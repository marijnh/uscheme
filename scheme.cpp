/* Copyright (c) 2004 Marijn Haverbeke
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any
 * damages arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose, including commercial applications, and to alter it and
 * redistribute it freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must
 *    not claim that you wrote the original software. If you use this
 *    software in a product, an acknowledgment in the product
 *    documentation would be appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must
 *    not be misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *    distribution.
 *
 * Marijn Haverbeke
 * marijn(at)haverbeke.nl
 */

// TODO:
//   check syntax-rules when they are defined instead of when expanded

#include <sstream>
#include <iostream>
#include <algorithm>
#include <map>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <csignal>

#include "scheme.hpp"
// Included again because some of the defines are undefined at the end of scheme.hpp
#include "error.hpp"
// Conversion between numbers and text
#include "number_io.hpp"
// Calculations with bignums
#include "bignum.hpp"
// Version number.
#include "version.hpp"

namespace uls{

// Global pointers to Interpreter and Mem_Manager (exists inside
// Interpreter). Are NULL if none are alive.
Interpreter* ip_ = NULL;
Mem_Manager* mp_ = NULL;
  
// ,MEMMANAGER

// When a cell is moved during garbage collection its new location is
// stored in the first data field of the old location.
inline Cell& Reference_To_Moved(Cell cell)
{
  return Compound_Info(cell).data[0];
}

// Used to convert cell sizes from 8-bit to 32-bit units
inline uintptr_t Fit_To_Four(uintptr_t num)
{
  uintptr_t retval = (num >> 2);
  if ((num & 3) != 0)
    ++retval;
  return retval;
}

// The method used for garbage collection is stop-and-copy. There are
// two blocks of equal size, only one is in use. When the block is
// full all 'live' cells get copied to the other block and the blocks
// switch roles. Allocation is simply a matter of putting the new cell
// at the current position in the live block and incrementing that
// position by the cell size. Big disadvantage of this method is that
// it moves stuff around when collecting.
Mem_Manager::Mem_Manager(size_t block_size)
  : _block_size(block_size),
    _block_position(0),
    _cell_header_size(Fit_To_Four(sizeof(Cell_Info) - sizeof(Cell))),
    _live_block(new size_t[block_size]),
    _dead_block(new size_t[block_size])
{
  mp_ = this;
}

Mem_Manager::~Mem_Manager()
{
  delete[] _live_block;
  delete[] _dead_block;
  mp_ = NULL;
}

// Allocate a new cell, collect garbage if needed.
Cell Mem_Manager::Allocate(size_t size, Cell_Type type, Pointer_Mask mask)
{
  size = Fit_To_Four(size) + _cell_header_size;
  if (size == _cell_header_size)
    size += 1;

  // Check if there is room, collect otherwise.
  if (size > (_block_size - _block_position)){
    Collect_Garbage();
    S_CHECK(size <= (_block_size - _block_position), "out of memory");
  }

  // Always collect garbage. this makes some easy-to-make errors
  // manifest themselves. Awfully slow though.
#ifdef ALWAYS_COLLECT
  Collect_Garbage();
#endif

  size_t* new_cell = _live_block + _block_position;
  _block_position += size;

  // Fill in the Cell_Info struct for the cell.
  Cell cell = (Cell)new_cell;
  Cell_Info& info = Compound_Info(cell);
  info.size = size;
  info.mask = mask;
  info.type = type;

  return cell;
}

void Mem_Manager::Collect_Garbage()
{
#ifdef WITH_DESTRUCTORS
  size_t old_block_position = _block_position;
#endif
  // switch the dead and the live blocks
  _block_position = 0;
  std::swap(_live_block, _dead_block);

  // move all reachable cells over to the new live block

  // Put the cells in that are marked by MCell or MStack objects in a
  // stack. They are the start from which all reachable cells are
  // reached.
  std::vector<Cell*> stack(_marked);
  for (std::vector<std::vector<Cell>*>::iterator i = _stacks.begin(); i != _stacks.end(); ++i){
    for (std::vector<Cell>::iterator j = (*i)->begin(); j != (*i)->end(); ++j)
      stack.push_back(&(*j));
  }

  // Handle every cell on the stack. Handling a cell may cause other
  // cells to be pushed onto it.
  while (!stack.empty()){
    Cell* current = stack.back();
    stack.pop_back();
    
    // first check current pointer - if it is not a compound cell
    // it is not a pointer, if it already points to the live block it
    // is already up to date
    if (Is_Compound(*current) && Is_In_Block(*current, _dead_block)){
      if (Compound_Info(*current).type == moved_cell_type){ // means the cell has already been moved
        *current = Reference_To_Moved(*current);
      }
      else{
        Move_Cell(current);
        Cell_Info& info = Compound_Info(*current);

        Pointer_Mask mask = info.mask;
        size_t words = info.size - _cell_header_size;

        // if mask is all 1's all fields get pushed
        if (mask == max_byte){
          for (size_t i = 0; i < words; ++i)
            stack.push_back(info.data + i);
        }
        // else if mask contains at least some 1's the fields that are
        // indicated get pushed.
        else if (mask != 0){
          for (size_t i = 0; i < words && (mask >> i) != 0; ++i){
            if (((mask >> i) & 1) != 0)
              stack.push_back(info.data + i);
          }
        }
      }
    }
  }

#ifdef WITH_DESTRUCTORS
  Call_Destructors(old_block_position);
#endif
}

// Moves a cell, sets a pointer to the new location at the old
// location and sets the size of the old location to 0 to indicate the
// cell has been moved.
void Mem_Manager::Move_Cell(Cell* cell)
{
  Cell_Info& info = Compound_Info(*cell);
  
  size_t* old_location = reinterpret_cast<uintptr_t*>(*cell);
  size_t* new_location = _live_block + _block_position;
  _block_position += info.size;

  std::copy(old_location, old_location + info.size, new_location);

  info.type = moved_cell_type;
  *cell = info.data[0] = (Cell)new_location;
}

// Check whether a cell is in a block
bool Mem_Manager::Is_In_Block(Cell cell, size_t* block) const
{
  size_t* b_cell = (size_t*)cell;
  return b_cell >= block && b_cell < block + _block_size;
}

// Remove a marked cell if it is not on top of the stack.
void Mem_Manager::Smart_Pop_Marked(Cell* cell)
{
  size_t new_size = _marked.size() - 1;
  size_t position = new_size - 1;
  while (_marked[position] != cell){
    S_ASSERT(position != 0);
    --position;
  }
  for (; position != new_size; ++position)
    _marked[position] = _marked[position + 1];
  _marked.pop_back();
}

#ifdef WITH_DESTRUCTORS
void Mem_Manager::Call_Destructors(size_t old_block_position)
{
  size_t* point = _dead_block;
  size_t* end = point + old_block_position;

  while (point < end)
  {
    Cell current = (Cell)point;
    Cell_Info& info = Compound_Info(current);
    if (info.type != moved_cell_type)
    {
      Destroy_Function destructor = ip_->type_manager.Get_Destructor(info.type);
      if (destructor != NULL)
        destructor(current);
    }
    point += info.size;
  }
}

void Mem_Manager::Call_All_Destructors()
{
  std::swap(_live_block, _dead_block);
  Call_Destructors(_block_position);
}
#endif

// ,INSTRUCTION

// Construct an enum of instructions with some macro magic.
#define INSTRUCTION(name) i_##name,
enum Instruction{
#include "instructions.i"
  i_last
};
#undef INSTRUCTION

inline bool Is_Instruction(Cell cell){
  return Match_Sixbit(cell, instruction_pattern);
}
inline Cell Make_Instruction(Instruction ins){
  return Encode_Sixbit(reinterpret_cast<uintptr_t&>(ins), instruction_pattern);
}

inline Instruction Instruction_Value(Cell cell){
  size_t temp = Extract_Sixbit(cell);
  return reinterpret_cast<Instruction&>(temp);
}

// A table associating instructions with their name is needed for
// reading and writing instructions.
struct Instruction_Info
{
  Instruction instruction;
  std::string name;
};
// instructions.i is included again, but this time to build the table.
#define INSTRUCTION(name) {i_##name, #name},
Instruction_Info* Instruction_List()
{
  static Instruction_Info instruction_info[] = {
#include "instructions.i"
    {i_last, ""}
  };
  return instruction_info;
}
#undef INSTRUCTION

Instruction_Info& Find_Instruction(const std::string& name){
  for (Instruction_Info* cur = Instruction_List(); cur->instruction != i_last; ++cur){
    if (cur->name == name)
      return *cur;
  }
  throw Scheme_Error("unknown instruction name: " + name);
}
Instruction_Info& Find_Instruction(Instruction instruction){
  for (Instruction_Info* cur = Instruction_List(); cur->instruction != i_last; ++cur){
    if (cur->instruction == instruction)
      return *cur;
  }
  S_THROW(std::runtime_error, "unknown instruction");
}

// ,TEMPNAME
// Temp names are used by the expander to rename local variables,
// which is needed to make hygienic macros behave properly. Sometimes
// it looks like half the code in this file is needed only for those
// awful hygienic macros...

inline Cell Make_Temp_Name(size_t id)
{
  return Encode_Fourbit(id, temp_name_pattern);
}
inline bool Is_Temp_Name(Cell cell)
{
  return Match_Fourbit(cell, temp_name_pattern);
}
inline size_t Temp_Name_ID(Cell cell)
{
  return Extract_Fourbit(cell);
}

// ,BIGNUM

// Bignums are stored as a set of unsigned digits with an extra field
// for the sign.
struct Bignum_Data
{
  unsigned short size;
  bool negative;
  digit data[1];
};

// Construct a bignum from an array of digits. Will remove trailing
// zero's but will not convert to a fixnum automatically.
inline Cell Make_Bignum(digit* value, size_t size, bool negative)
{
  while (size != 0 && value[size - 1] == 0)
    --size;
  Cell retval = Allocate(sizeof(Bignum_Data) - sizeof(digit) + size * sizeof(digit), bignum_type, 0);
  Bignum_Data& data = Extract<Bignum_Data>(retval);
  data.size = size;
  data.negative = negative;
  for (size_t i = 0; i != size; ++i)
    data.data[i] = value[i];
  return retval;
}
// Construct a bignum from a 64-bit int. Will create a bignum even if
// a fixnum would be possible (operations on bignums require two
// bignums, not a bignum and a fixnum).
Cell Make_Bignum(int64 value)
{
  bool negative = value < 0;
  if (negative)
    value = -value;
  if (value < static_cast<int64>(digit_radix)){
    digit val = value;
    return Make_Bignum(&val, 1, negative);
  }
  else{
    digit val[2];
    val[0] = value;
    val[1] = (value >> digit_size);
    return Make_Bignum(val, 2, negative);
  }
}
// Make an integer from an array of digits. This one will convert to
// fixnum if possible.
inline Cell Make_Integer(digit* value, size_t size, bool negative)
{
  while (size != 0 && value[size - 1] == 0)
    --size;
  if (size == 0)
    return zero_cell;
  else if (size == 1 && value[0] < max_fixnum)
    return Make_Fixnum(negative ? -value[0] : value[0]);
  else
    return Make_Bignum(value, size, negative);
}

inline Bignum_Data& Get_Bignum_Data(Cell cell)
{
  S_ASSERT(Is_Bignum(cell));
  return Extract<Bignum_Data>(cell);
}

// Convert a double to an integer (fixnum if possible)
Cell Double_To_Integer(double value)
{
  bool negative = value < 0;
  if (negative)
    value = -value;
  value = std::floor(value + .5);
  
  std::vector<digit> array;
  while (value != 0){
    double quotient = std::floor(value / digit_radix);
    double remainder = value - (quotient * digit_radix);
    array.push_back(static_cast<digit>(remainder + .5));
    value = quotient;
  }

  return Make_Integer(&array[0], array.size(), negative);
}

inline bool Double_Is_Int(double value)
{
  return std::floor(value) == value;
}
inline double Truncate_Double(double value)
{
  if (value < 0)
    return std::ceil(value);
  else
    return std::floor(value);
}

// Compare two bignums for equality.
bool Bignum_Equal(Cell one, Cell two)
{
  Bignum_Data& d_one = Get_Bignum_Data(one), & d_two = Get_Bignum_Data(two);
  if (d_one.negative != d_two.negative || d_one.size != d_two.size)
    return false;
  for (size_t i = 0; i != d_one.size; ++i){
    if (d_one.data[i] != d_two.data[i])
      return false;
  }
  return true;
}

// Compare two bignums to see if one is less than two.
bool Bignum_Less(Cell one, Cell two)
{
  Bignum_Data& d_one = Get_Bignum_Data(one), & d_two = Get_Bignum_Data(two);
  if (d_one.negative != d_two.negative)
    return d_one.negative;

  bool result = false;
  if (d_one.size != d_two.size)
    result = d_one.size < d_two.size;
  else{
    for (size_t i = d_one.size; i != 0; --i){
      digit i_one = d_one.data[i], i_two = d_two.data[i];
      if (i_one != i_two){
        result = i_one < i_two;
        break;
      }
    }
  }
  
  if (d_one.negative)
    result = -result;
  return result;
}

void Write_Bignum(Cell cell, std::ostream& str, bool display)
{
  Bignum_Data& data = Get_Bignum_Data(cell);
  Write_Array(str, data.data, data.size, data.negative);
}

// ,INTEGER

// Some operations that work on both bignums and fixnums.
inline bool Integer_Negative(Cell cell)
{
  if (Is_Fixnum(cell))
    return Fixnum_Value(cell) < 0;
  else
    return Get_Bignum_Data(cell).negative;
}
inline bool Integer_Equal(Cell one, Cell two)
{
  bool small = Is_Fixnum(one);
  if (small != Is_Fixnum(two))
    return false;
  else if (small)
    return one == two;
  else
    return Bignum_Equal(one, two);
}
inline bool Integer_Less(Cell one, Cell two)
{
  bool one_small = Is_Fixnum(one), two_small = Is_Fixnum(two);
  if (one_small && !two_small)
    return true;
  else if (!one_small && two_small)
    return false;
  else if (one_small)
    return Fixnum_Value(one) < Fixnum_Value(two);
  else
    return Bignum_Less(one, two);
}

Cell Integer_Quotient(Cell one, Cell two)
{
  S_CHECK(two != zero_cell, "division by zero");
  if (Is_Fixnum(one) && Is_Fixnum(two)){
    int64 result = static_cast<int64>(Fixnum_Value(one)) / Fixnum_Value(two);
    return Make_Integer(result);
  }
  else{
    MCell m_one = one, m_two = two;
    if (Is_Fixnum(one))
      m_one = Promote_Number(one, n_fixnum);
    else if (Is_Fixnum(two))
      m_two = Promote_Number(two, n_fixnum);

    Bignum_Data& d_one = Get_Bignum_Data(m_one), & d_two = Get_Bignum_Data(m_two);
    size_t s_max = std::max(d_one.size, d_two.size);
    Array_Buffer quotient(s_max), remainder(s_max);
    Divide_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, quotient.data, quotient.size, remainder.data, remainder.size);
    return Make_Integer(quotient.data, quotient.size, d_one.negative != d_two.negative);
  }
}
Cell Integer_Remainder(Cell one, Cell two)
{
  S_CHECK(two != zero_cell, "division by zero");
  if (Is_Fixnum(one) && Is_Fixnum(two)){
    int64 result = static_cast<int64>(Fixnum_Value(one)) % Fixnum_Value(two);
    return Make_Integer(result);
  }
  else{
    MCell m_one = one, m_two = two;
    if (Is_Fixnum(one))
      m_one = Promote_Number(one, n_fixnum);
    else if (Is_Fixnum(two))
      m_two = Promote_Number(two, n_fixnum);

    Bignum_Data& d_one = Get_Bignum_Data(m_one), & d_two = Get_Bignum_Data(m_two);
    size_t s_max = std::max(d_one.size, d_two.size);
    Array_Buffer quotient(s_max), remainder(s_max);
    Divide_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, quotient.data, quotient.size, remainder.data, remainder.size);
    return Make_Integer(remainder.data, remainder.size, d_one.negative);
  }
}
Cell Integer_Modulo(Cell one, Cell two)
{
  if (Is_Fixnum(one) && Is_Fixnum(two)){
    int i_one = Fixnum_Value(one), i_two = Fixnum_Value(two);
    int64 result = static_cast<int64>(i_one) % i_two;
    if (result != 0 && (i_one < 0) != (i_two < 0))
      result += i_two;
    return Make_Integer(result);
  }
  else{
    bool needs_add = Integer_Negative(one) != Integer_Negative(two);
    MCell m_two = two;
    Cell remainder = Integer_Remainder(one, two);
    if (remainder != zero_cell && needs_add)
      remainder = Number_Add(remainder, m_two);
    return remainder;
  }
}

// ,RATIONAL

void Write_Rational(Cell cell, std::ostream& str, bool display)
{
  S_ASSERT(Is_Rational(cell));
  str << (unsigned)Rational_Numerator(cell) << '/'
      << (unsigned)Rational_Denominator(cell);
}

Cell Make_Rational(const MCell& numerator, const MCell& denominator)
{
  S_ASSERT(denominator != zero_cell); // caller should make sure
  Cell retval = Allocate_Cell<Rational_Data>(rational_type);
  Rational_Numerator(retval) = numerator;
  Rational_Denominator(retval) = denominator;
  return retval;
}

Cell Greatest_Common_Divisor(Cell one, Cell two)
{
  S_ASSERT(Is_Integer(one) && Is_Integer(two));
  MCell a = one, b = two;
  if (Integer_Less(b, a))
    std::swap<Cell>(a, b);

  while (a != zero_cell){
    b = Integer_Remainder(b, a);
    std::swap<Cell>(a, b);
  }
  return b;
}

// Make as simple a rational possible from two integers. Will return
// an integer if the denominator ends up as one.
Cell Make_Simplified_Rational(Cell numerator, Cell denominator)
{
  S_ASSERT(Is_Integer(numerator) && Is_Integer(denominator));
  
  if (numerator == zero_cell)
    return zero_cell;
  S_CHECK(denominator != zero_cell, "invalid rational number (denominator equals 0)");

  MCell num = numerator, denom = denominator;
  if (Integer_Negative(denom)){
    num = Number_Subtract(zero_cell, num);
    denom = Number_Subtract(zero_cell, denom);
  }

  MCell common = Greatest_Common_Divisor(num, denom);
  if (common != one_cell){
    num = Integer_Quotient(num, common);
    denom = Integer_Quotient(denom, common);
  }

  if (denom == one_cell)
    return num;
  else
    return Make_Rational(num, denom);
}

// ,REAL

// Reals are easy.
void Write_Real(Cell cell, std::ostream& str, bool display)
{
  Write_Double(str, Real_Value(cell));
}

Cell Make_Real(double value){
  Cell retval = Allocate_Cell<double>(real_type, 0);
  Extract<double>(retval) = value;
  return retval;
}

// ,NUMBER

// A 'tower' system for numbers, the number types form a hierarchy an
// can be promoted upwards.
Num_Type Number_Type(Cell cell)
{
  S_ASSERT(Is_Number(cell));
  
  if (Is_Fixnum(cell)){
    return n_fixnum;
  }
  else{
    S_ASSERT(Is_Compound(cell));
    Cell_Type type = Get_Type(cell);
    if (type == bignum_type)
      return n_bignum;
    else if (type == rational_type)
      return n_rational;
    else
      return n_real;
  }
}

Cell Promote_Number(Cell num, Num_Type type)
{
  switch(type){
  case n_fixnum:
    return Make_Bignum(Fixnum_Value(num));
  case n_bignum:
    return Make_Rational(num, one_cell);
  case n_rational:
    return Make_Real(Number_To_Double(num));
  default:
    S_ASSERT(false);
    return invalid_cell;
  }
}

// Make two numbers have the same type. Takes the types as arguments
// because the caller usually already calculated those. (Talk about
// overactive optimization).
Num_Type Align_Number_Types(Cell& one, Cell& two, Num_Type t_one, Num_Type t_two)
{
  MCell m_one = one, m_two = two;
  while (t_one < t_two){
    m_one = Promote_Number(one, t_one);
    t_one = static_cast<Num_Type>(static_cast<size_t>(t_one) + 1);
  }
  while (t_two < t_one){
    m_two = Promote_Number(two, t_two);
    t_two = static_cast<Num_Type>(static_cast<size_t>(t_two) + 1);
  }
  one = m_one;
  two = m_two;
  return t_one;
}

// Convert any kind of number to a double.
double Number_To_Double(Cell cell)
{
  if (Is_Fixnum(cell)){
    return static_cast<double>(Fixnum_Value(cell));
  }
  else if (Is_Bignum(cell)){
    Bignum_Data& data = Get_Bignum_Data(cell);
    double accum = 0;
    for (size_t i = 0; i != data.size; ++i){
      double add = data.data[i];
      for (size_t j = 0; j != i; ++j)
        add *= digit_radix;
      accum += add;
    }
    if (data.negative)
      accum = -accum;
    return accum;
  }
  else if (Is_Rational(cell)){
    return Number_To_Double(Rational_Numerator(cell)) / Number_To_Double(Rational_Denominator(cell));
  }
  else{
    return Real_Value(cell);
  }
}

// Convert a string to the correct kind of number.
Cell String_To_Number(const std::string& buffer, size_t radix = 10)
{
  size_t slash = buffer.find('/');
  // When radix is more than 14 e means 14 so only E can be used for
  // exponent.
  size_t dot_or_exp = (radix > 14) ? buffer.find_first_of(".E") : buffer.find_first_of(".eE");
  if (slash != buffer.npos){
    MCell numerator = String_To_Number(std::string(buffer, 0, slash), radix),
      denominator = String_To_Number(std::string(buffer, slash + 1), radix);
    S_CHECK(Is_Integer(numerator) && Is_Integer(denominator), "invalid rational constant: " + buffer);
    return Make_Simplified_Rational(numerator, denominator);
  }
  else if (dot_or_exp != buffer.npos){
    return Make_Real(String_To_Double(buffer, radix));
  }
  else{ // integer
    // Read the integer as a bignum when it is bigger than 19 decimal
    // digits, otherwise read it (more efficiently) as an int64. When
    // the radix is not 10 we always use the bignum version, because
    // it got too complicated to calculate when those numbers were too
    // big to fit in an int64.
    if (buffer.size() > 19 || radix != 10){
      std::vector<digit> array;
      bool negative = String_To_Array(buffer, array, radix);
      return Make_Integer(&array[0], array.size(), negative);
    }
    else{
      return Make_Integer(String_To_Int(buffer, radix));
    }
  }
}
// Get a number from a string in any radix. Only used by the primitive
// number->string, the standard output functions for the number types
// already know their type and always use radix 10 so they can do this
// simpler.
std::string Number_To_String(Cell num, size_t radix = 10)
{
  std::ostringstream output;
  if (Is_Fixnum(num)){
    Write_Int(output, Fixnum_Value(num), radix);
  }
  else if (Is_Bignum(num)){
    Bignum_Data& data = Get_Bignum_Data(num);
    Write_Array(output, data.data, data.size, data.negative, radix);
  }
  else if (Is_Real(num)){
    Write_Double(output, Real_Value(num), radix);
  }
  else{
    S_ASSERT(Is_Rational(num));
    output << Number_To_String(Rational_Numerator(num), radix);
    output << '/';
    output << Number_To_String(Rational_Denominator(num), radix);
  }
  return output.str();
}

// The following functions add, subtract, multiply and divide number
// types. They first make the types the same by promoting the 'lower'
// argument, and then apply the operations that are needed for that
// type of number.
Cell Number_Add(Cell one, Cell two)
{
  Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
  if (t_one != t_two)
    t_one = Align_Number_Types(one, two, t_one, t_two);
  
  switch (t_one){
  case n_fixnum:
    {
      // Using 64-bit ints and Make_Integer, the numbers automatically
      // become bignums when they get too fat.
      int64 result = static_cast<int64>(Fixnum_Value(one)) + Fixnum_Value(two);
      return Make_Integer(result);
    }
  case n_bignum:
    {
      Bignum_Data& d_one = Get_Bignum_Data(one), & d_two = Get_Bignum_Data(two);
      Array_Buffer result(std::max(d_one.size, d_two.size) + 1);

      // This is a little icky, but necessary because I use unsigned
      // digits instead of a 2-complement system. An add of a negative
      // and a positive number becomes a subtract.
      if (d_one.negative == d_two.negative){
        Add_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, result.data, result.size);
        return Make_Integer(result.data, result.size, d_one.negative);
      }
      else if (Array_Smaller(d_one.data, d_one.size, d_two.data, d_two.size)){
        Subtract_Arrays(d_two.data, d_two.size, d_one.data, d_one.size, result.data, result.size);
        return Make_Integer(result.data, result.size, d_two.negative);
      }
      else{
        Subtract_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, result.data, result.size);
        return Make_Integer(result.data, result.size, d_one.negative);
      }
    }
  case n_rational:
    {
      MCell m_one = one, m_two = two;
      MCell numerator = Number_Multiply(Rational_Numerator(m_one), Rational_Denominator(m_two));
      MCell temp = Number_Multiply(Rational_Numerator(m_two), Rational_Denominator(m_one));
      numerator = Number_Add(numerator, temp);
      Cell denominator = Number_Multiply(Rational_Denominator(m_one), Rational_Denominator(m_two));
      return Make_Simplified_Rational(numerator, denominator);
    }
  case n_real:
    return Make_Real(Number_To_Double(one) + Number_To_Double(two));
  }
  return invalid_cell; // just to silence the compiler
}

Cell Number_Subtract(Cell one, Cell two)
{
  Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
  if (t_one != t_two)
    t_one = Align_Number_Types(one, two, t_one, t_two);

  switch (t_one){
  case n_fixnum:
    {
      int64 result = static_cast<int64>(Fixnum_Value(one)) - Fixnum_Value(two);
      return Make_Integer(result);
    }
  case n_bignum:
    {
      Bignum_Data& d_one = Get_Bignum_Data(one), & d_two = Get_Bignum_Data(two);
      Array_Buffer result(std::max(d_one.size, d_two.size) + 1);
  
      if (d_one.negative != d_two.negative){
        Add_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, result.data, result.size);
        return Make_Integer(result.data, result.size, d_one.negative);
      }
      else if (Array_Smaller(d_one.data, d_one.size, d_two.data, d_two.size)){
        Subtract_Arrays(d_two.data, d_two.size, d_one.data, d_one.size, result.data, result.size);
        return Make_Integer(result.data, result.size, !d_one.negative);
      }
      else{
        Subtract_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, result.data, result.size);
        return Make_Integer(result.data, result.size, d_one.negative);
      }
    }
  case n_rational:
    {
      MCell m_one = one, m_two = two;
      MCell numerator = Number_Multiply(Rational_Numerator(m_one), Rational_Denominator(m_two));
      MCell temp = Number_Multiply(Rational_Numerator(m_two), Rational_Denominator(m_one));
      numerator = Number_Subtract(numerator, temp);
      Cell denominator = Number_Multiply(Rational_Denominator(m_one), Rational_Denominator(m_two));
      return Make_Simplified_Rational(numerator, denominator);
    }
  case n_real:
    return Make_Real(Number_To_Double(one) - Number_To_Double(two));
  }
  return invalid_cell;
}

Cell Number_Multiply(Cell one, Cell two)
{
  Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
  if (t_one != t_two)
    t_one = Align_Number_Types(one, two, t_one, t_two);

  switch (t_one){
  case n_fixnum:
    {
      int64 result = static_cast<int64>(Fixnum_Value(one)) * Fixnum_Value(two);
      return Make_Integer(result);
    }
  case n_bignum:
    {
      Bignum_Data& d_one = Get_Bignum_Data(one), & d_two = Get_Bignum_Data(two);
      Array_Buffer result(d_one.size + d_two.size);
      Multiply_Arrays(d_one.data, d_one.size, d_two.data, d_two.size, result.data, result.size);
      return Make_Integer(result.data, result.size, d_one.negative != d_two.negative);
    }
  case n_rational:
    {
      MCell m_one = one, m_two = two;
      MCell numerator = Number_Multiply(Rational_Numerator(m_one), Rational_Numerator(m_two));
      Cell denominator = Number_Multiply(Rational_Denominator(m_one), Rational_Denominator(m_two));
      return Make_Simplified_Rational(numerator, denominator);
    }
  case n_real:
    return Make_Real(Number_To_Double(one) * Number_To_Double(two));
  }
  return invalid_cell;
}

Cell Number_Divide(Cell one, Cell two)
{
  Num_Type t_one = Number_Type(one), t_two = Number_Type(two);
  // Divide for integers means becoming a rational number
  if ((t_one == n_fixnum || t_one == n_bignum) && (t_two == n_fixnum || t_two == n_bignum))
    return Make_Simplified_Rational(one, two);
  
  if (t_one != t_two)
    t_one = Align_Number_Types(one, two, t_one, t_two);

  if (t_one == n_rational){
    MCell m_one = one, m_two = two;
    MCell numerator = Number_Multiply(Rational_Numerator(m_one), Rational_Denominator(m_two));
    Cell denominator = Number_Multiply(Rational_Denominator(m_one), Rational_Numerator(m_two));
    return Make_Simplified_Rational(numerator, denominator);
  }
  else{
    return Make_Real(Number_To_Double(one) / Number_To_Double(two));
  }
}

// ,PAIR

// Actually writes a whole list.
void Write_Pair(Cell cell, std::ostream& str, bool display)
{
  str << "(";
  while(true){
    Write(Car(cell), str, display);
    if (Cdr(cell) == null_cell)
      break;
    if (!Is_Pair(Cdr(cell))){
      str << " . ";
      Write(Cdr(cell), str, display);
      break;
    }
    str << ' ';
    cell = Cdr(cell);
  }
  str << ')';
}

// Utility functions
size_t List_Length(Cell list, const char* error){
  size_t result = 0;
  while (list != null_cell){
    S_CHECK(Is_Pair(list), error);
    ++result;
    list = Cdr(list);
  }
  return result;
}
bool Is_Proper_List(Cell list){
  for (; list != null_cell; list = Cdr(list)){
    if (!Is_Pair(list))
      return false;
  }
  return true;
}

// ,STRING

Cell Make_String(size_t size, char fill = ' ')
{
  Cell new_string = Allocate(sizeof(size_t) + size, string_type, 0);
  String_Data& data = Extract<String_Data>(new_string);
  data.size = size;
  for (size_t i = 0; i != size; ++i)
    data.data[i] = fill;
  return new_string;

}

Cell Make_String(const std::string& value)
{
  // Allocates a cell of a size dependant on the string size.
  Cell new_string = Allocate(sizeof(size_t) + value.size(), string_type, 0);
  String_Data& data = Extract<String_Data>(new_string);
  data.size = value.size();
  std::copy(value.begin(), value.end(), data.data);
  return new_string;
}

std::string String_Value(Cell cell)
{
  S_ASSERT(Is_String(cell));
  size_t size = String_Size(cell);
  if (size == 0)
    return "";
  else
    return std::string(&String_Ref(cell, 0), size);
}

void Write_String(Cell cell, std::ostream& str, bool display)
{
  if (!display)
    str << '"';
  size_t size = String_Size(cell);
  for (size_t i = 0; i < size; ++i){
    char c = String_Ref(cell, i);
    if (!display && (c == '\\' || c == '"'))
      str << '\\';
    str << c;
  }
  if (!display)
    str << '"';
}

// ,VECTOR

Cell Make_Vector(size_t size, Cell fill)
{
  Cell new_vector = Allocate(sizeof(Cell) * (size + 1), vector_type);
  Vector_Data& data = Extract<Vector_Data>(new_vector);
  data.size = Make_Fixnum(size);
  for (size_t i = 0; i < size; ++i)
    data.data[i] = fill;
  return new_vector;
}

Cell Make_Vector(const MStack& stack)
{
  Cell new_vector = Make_Vector(stack.Size());
  for (size_t i = 0; i != stack.Size(); ++i)
    Vector_Ref(new_vector, i) = stack[i];
  return new_vector;
}

Cell Make_Vector_From_List(Cell list)
{
  S_ASSERT(Is_Pair(list));
  size_t size = List_Length(list);
  MCell lst = list;
  Cell new_vec = Make_Vector(size);
  for (size_t i = 0; i != size; ++i, lst = Cdr(lst))
    Vector_Ref(new_vec, i) = Car(lst);
  return new_vec;
}

Cell Copy_Vector(Cell vec)
{
  S_ASSERT(Is_Vector(vec));
  size_t size = Vector_Size(vec);
  MCell old_vec = vec;
  Cell new_vec = Make_Vector(size);
  for (size_t i = 0; i != size; ++i)
    Vector_Ref(new_vec, i) = Vector_Ref(vec, i);
  return new_vec;
}

void Write_Vector(Cell cell, std::ostream& str, bool display)
{
  str << "#(";
  size_t size = Vector_Size(cell);
  for (size_t i = 0; i < size; ++i){
    Write(Vector_Ref(cell, i), str, display);
    if (i != size - 1)
      str << ' ';
  }
  str << ')';
}

// ,PORT

void Write_Inport(Cell cell, std::ostream& str, bool display)
{
  Inport_Data& data = Extract<Inport_Data>(cell);
  str << "#<inport";
  if (data.file_name != false_cell)
    str << ':' << String_Value(data.file_name);
  str << '>';
}
void Write_Outport(Cell cell, std::ostream& str, bool display)
{
  Outport_Data& data = Extract<Outport_Data>(cell);
  str << "#<outport";
  if (data.file_name != false_cell)
    str << ':' << String_Value(data.file_name);
  str << '>';
}

// File ports keep track of their filename.
Cell Make_Inport(const MCell& filename)
{
  Cell new_port = Allocate_Cell<Inport_Data>(inport_type, 1);
  Inport_Data& data = Extract<Inport_Data>(new_port);
  std::ifstream* new_stream = new std::ifstream(String_Value(filename).c_str(), std::ios::binary);
  S_CHECK(!new_stream->fail(), "could not open file " + String_Value(filename));
  data.file_name = filename;
  data.stream = new_stream;
  data.position = 0;
  data.line = 1;
  return new_port;
}
// Reopening file ports is a useful trick. It works as follows -
// whenever a file gets closed it records its position (for input
// files, output files can just start appending), it can then be
// reopened to start at that position. This combines very well with
// dynamic-wind, it allows code inside the wind to read or write front
// to back without worrying about it getting closed an reopened.
void Reopen_Inport(Cell port)
{
  S_ASSERT(Is_Inport(port));
  Inport_Data& data = Extract<Inport_Data>(port);
  S_CHECK(data.file_name != false_cell, "only file ports can be reopened");
  if (data.stream == NULL){
    std::ifstream* new_stream = new std::ifstream(String_Value(data.file_name).c_str(), std::ios::binary);
    S_CHECK(!new_stream->fail(), "could not open file " + String_Value(data.file_name));
    new_stream->seekg(data.position);
    if (new_stream->fail())
      new_stream->setstate(std::ios_base::eofbit);
    data.stream = new_stream;
  }
}
Cell Make_Outport(const MCell& filename)
{
  Cell new_port = Allocate_Cell<Outport_Data>(outport_type, 1);
  Outport_Data& data = Extract<Outport_Data>(new_port);
  std::ofstream* new_stream = new std::ofstream(String_Value(filename).c_str(), std::ios::binary);
  S_CHECK(!new_stream->fail(), "could not open file " + String_Value(filename));
  data.file_name = filename;
  data.stream = new_stream;
  return new_port;
}
void Reopen_Outport(Cell port)
{
  S_ASSERT(Is_Outport(port));
  Outport_Data& data = Extract<Outport_Data>(port);
  S_CHECK(data.file_name != false_cell, "only file ports can be reopened");
  if (data.stream == NULL){
    std::ofstream* new_stream = new std::ofstream(String_Value(data.file_name).c_str(), std::ios::binary | std::ios::app);
    S_CHECK(!new_stream->fail(), "could not open file " + String_Value(data.file_name));
    data.stream = new_stream;
  }
}

// Ports based on existing streams. The filename field is #f for
// these, closing or reopening them is a no-op.
Cell Make_Inport(std::istream& stream)
{
  Cell new_port = Allocate_Cell<Inport_Data>(inport_type, 1);
  Inport_Data& data = Extract<Inport_Data>(new_port);
  data.file_name = false_cell;
  data.stream = &stream;
  data.position = 0;
  data.line = 1;
  return new_port;
}
Cell Make_Outport(std::ostream& stream)
{
  Cell new_port = Allocate_Cell<Outport_Data>(outport_type, 1);
  Outport_Data& data = Extract<Outport_Data>(new_port);
  data.file_name = false_cell;
  data.stream = &stream;
  return new_port;
}

Cell Inport_Read_Char(Cell port)
{
  S_ASSERT(Is_Inport(port));
  Inport_Data& data = Extract<Inport_Data>(port);
  if (data.stream == NULL)
    return eof_cell;
  char c = data.stream->get();
  if (c == '\n')
    ++data.line;
  
  if (data.stream->eof())
    return eof_cell;
  else
    return Make_Character(c);
}
Cell Inport_Peek_Char(Cell port)
{
  S_ASSERT(Is_Inport(port));
  Inport_Data& data = Extract<Inport_Data>(port);
  if (data.stream == NULL)
    return eof_cell;
  char c = data.stream->peek();
  if (data.stream->eof())
    return eof_cell;
  else
    return Make_Character(c);
}

std::istream& Inport_Stream(Cell port)
{
  S_ASSERT(Is_Inport(port));
  Inport_Data& data = Extract<Inport_Data>(port);
  S_CHECK(data.stream != NULL, "taking input stream from a closed file");
  return *data.stream;
}
// Current line of the input port.
size_t Inport_Line(Cell port)
{
  S_ASSERT(Is_Inport(port));
  return Extract<Inport_Data>(port).line;
}

std::ostream& Outport_Stream(Cell cell){
  S_ASSERT(Is_Outport(cell));
  std::ostream* pointer = Extract<Outport_Data>(cell).stream;
  S_CHECK(pointer != NULL, "taking output stream from a closed file");
  return *pointer;
}

// This might work on some implementations, it does not work on GCC.
// Used to implement char-ready?, which is kind of hard to do with the
// blocking streams of the standard library.
bool Inport_Ready(Cell port){
  S_ASSERT(Is_Inport(port));
  std::istream* stream = Extract<Inport_Data>(port).stream;
  return stream != NULL && stream->rdbuf()->in_avail() > 0;
}

// Closing already closed ports or non-file ports does not do
// anything.
void Close_Inport(Cell cell)
{
  S_ASSERT(Is_Inport(cell));
  Inport_Data& data = Extract<Inport_Data>(cell);
  if (data.file_name != false_cell && data.stream != NULL){
    data.position = data.stream->tellg();
    static_cast<std::ifstream*>(data.stream)->close();
    delete data.stream;
    data.stream = NULL;
  }
}
void Close_Outport(Cell cell)
{
  S_ASSERT(Is_Outport(cell));
  Outport_Data& data = Extract<Outport_Data>(cell);
  if (data.file_name != false_cell && data.stream != NULL){
    static_cast<std::ofstream*>(data.stream)->close();
    delete data.stream;
    data.stream = NULL;
  }
}

// ,CLOSURE

struct Closure_Data
{
  int num_args;
  Cell code, environment, name;
};

Cell Make_Closure(const MCell& code, const MCell& environment, Cell num_args, Cell name)
{
  Cell retval = Allocate_Cell<Closure_Data>(closure_type, 6);
  Closure_Data& data = Extract<Closure_Data>(retval);
  data.code = code;
  data.environment = environment;
  data.num_args = Fixnum_Value(num_args);
  data.name = name;
  return retval;
}
// Unfinished closures are the things the compiler generates, the
// environment has to be added to them at run-time. They are simply a
// 3-element vector containing a code vector, a number of arguments
// and a name.
inline Cell Make_Closure(const MCell& unfinished_closure, const MCell& environment)
{
  Cell retval = Allocate_Cell<Closure_Data>(closure_type, 6);
  Closure_Data& data = Extract<Closure_Data>(retval);
  data.code = Vector_Ref(unfinished_closure, 0);
  data.environment = environment;
  data.num_args = Fixnum_Value(Vector_Ref(unfinished_closure, 1));
  data.name = Vector_Ref(unfinished_closure, 2);;
  return retval;
}
Cell Make_Unfinished_Closure(const MCell& code, Cell num_args, Cell name)
{
  Cell closure_vector = Make_Vector(3);
  Vector_Ref(closure_vector, 0) = code;
  Vector_Ref(closure_vector, 1) = num_args;
  Vector_Ref(closure_vector, 2) = name;
  return closure_vector;
}

inline Cell Closure_Code(Cell cell){
  return Extract<Closure_Data>(cell).code;
}
inline Cell Closure_Environment(Cell cell){
  return Extract<Closure_Data>(cell).environment;
}
inline int Closure_Num_Args(Cell cell){
  return Extract<Closure_Data>(cell).num_args;
}
inline Cell Closure_Name(Cell cell){
  return Extract<Closure_Data>(cell).name;
}

void Write_Closure(Cell cell, std::ostream& str, bool display)
{
  str << "#<closure";
  Cell name = Closure_Name(cell);
  if (Is_Symbol(name))
    str << ':' << Symbol_Name(name);
  str << '>';
}

std::string Function_Name(Cell name)
{
  if (name == false_cell){
    return "unnamed function";
  }
  else{
    S_ASSERT(Is_Symbol(name));
    return Symbol_Name(name);
  }
}

// ,PRIMITIVE

// The function is stored s a Primitive_Function_0 and cast to the
// proper type (based on num_args) when it is called.
struct Primitive_Data
{
  Primitive_Function_0 function;
  int num_args;
  Cell name;
};

// Do the same thing for each of the 9 types of primitive functions.
// It is not pretty but it is convenient for client code.
#define WRITE_FUNCTION(n) \
Cell Make_Primitive(Primitive_Function_##n function, const std::string& name, bool var_arg)\
{\
  Cell new_prim = Allocate_Cell<Primitive_Data>(primitive_type, 0);\
  Primitive_Data& data = Extract<Primitive_Data>(new_prim);\
  data.function = reinterpret_cast<Primitive_Function_0>(function);\
  data.num_args = n;\
  if (var_arg)\
    data.num_args = -data.num_args;\
  data.name = Make_Symbol(name);\
  return new_prim;\
}

WRITE_FUNCTION(0);
WRITE_FUNCTION(1);
WRITE_FUNCTION(2);
WRITE_FUNCTION(3);
WRITE_FUNCTION(4);
WRITE_FUNCTION(5);
WRITE_FUNCTION(6);
WRITE_FUNCTION(7);
WRITE_FUNCTION(8);

#undef WRITE_FUNCTION

// Ugly, repetetive Call_Primitive functions. This is where C++'s
// static function/type system shines (not). If you know any kind of
// template magic that can make this more elegant, tell me!
namespace{
  typedef Cell (*Call_Primitive) (Primitive_Function_0 function, Cell* arguments);

  Cell Call_Primitive_0(Primitive_Function_0 function, Cell* arguments)
  {
    return function();
  }
  Cell Call_Primitive_1(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_1>(function)(arguments[0]);
  }
  Cell Call_Primitive_2(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_2>(function)(arguments[0], arguments[1]);
  }
  Cell Call_Primitive_3(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_3>(function)(arguments[0], arguments[1], arguments[2]);
  }
  Cell Call_Primitive_4(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_4>(function)(arguments[0], arguments[1], arguments[2], arguments[3]);
  }
  Cell Call_Primitive_5(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_5>(function)(arguments[0], arguments[1], arguments[2], arguments[3],
                                                            arguments[4]);
  }
  Cell Call_Primitive_6(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_6>(function)(arguments[0], arguments[1], arguments[2], arguments[3],
                                                            arguments[4], arguments[5]);
  }
  Cell Call_Primitive_7(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_7>(function)(arguments[0], arguments[1], arguments[2], arguments[3],
                                                            arguments[4], arguments[5], arguments[6]);
  }
  Cell Call_Primitive_8(Primitive_Function_0 function, Cell* arguments)
  {
    return reinterpret_cast<Primitive_Function_8>(function)(arguments[0], arguments[1], arguments[2], arguments[3],
                                                            arguments[4], arguments[5], arguments[6], arguments[7]);
  }

  Call_Primitive call_primitive_table[] = {
    Call_Primitive_0,
    Call_Primitive_1,
    Call_Primitive_2,
    Call_Primitive_3,
    Call_Primitive_4,
    Call_Primitive_5,
    Call_Primitive_6,
    Call_Primitive_7,
    Call_Primitive_8
  };
}

inline int Primitive_Num_Args(Cell cell)
{
  return Extract<Primitive_Data>(cell).num_args;
}
inline Primitive_Function_0 Primitive_Function(Cell cell)
{
  return Extract<Primitive_Data>(cell).function;
}
inline Cell Primitive_Name(Cell cell)
{
  return Extract<Primitive_Data>(cell).name;
}
void Write_Primitive(Cell cell, std::ostream& str, bool display)
{
  str << "#<primitive:" << (unsigned)Primitive_Name(cell) << '>';
}

// ,SPECIAL FORM

enum Form_Name{
  form_if,
  form_define,
  form_set,
  form_quote,
  form_quasiquote,
  form_lambda,
  form_begin,
  form_define_macro,
  form_define_syntax,
  form_let_syntax,
  form_letrec_syntax,
  form_current_env
  // Current_env is a hack to make interaction-environment possible
  // (the virtual machine itself does not know much about
  // environments, so it has to be fetched at compile time, for which
  // a special form is needed.)
};

inline Cell Make_Special_Form(Form_Name name)
{
  return Encode_Sixbit(name, form_pattern);
}
inline Form_Name Special_Form_Name(Cell cell)
{
  size_t temp = Extract_Sixbit(cell);
  return reinterpret_cast<Form_Name&>(temp);
}

// ,RENAMED SYMBOLS

// Not to be confused with temp names. These are what macro-expansion
// makes of the symbols it finds in its template (the ones that are
// not expanded). They only live inside the expander - after the macro
// gets expanded the expander converts these to either temp names or
// normal symbols (based on whether they refer to a top level variable
// or a local one. They can nest (in case of a macro expanding to a
// macro definition).

struct Renamed_Symbol_Data
{
  Cell old_name, macro, unique_value;
};

Cell Make_Renamed_Symbol(const MCell& old_name, const MCell& macro, const MCell& unique)
{
  Cell new_cell = Allocate_Cell<Renamed_Symbol_Data>(renamed_symbol_type);
  Renamed_Symbol_Data& data = Extract<Renamed_Symbol_Data>(new_cell);
  data.old_name = old_name;
  data.macro = macro;
  data.unique_value = unique;
  return new_cell;
}

inline bool Is_Renamed_Symbol(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == renamed_symbol_type;
}
inline bool Renamed_Symbol_Equal(Cell one, Cell two)
{
  Renamed_Symbol_Data& d_one = Extract<Renamed_Symbol_Data>(one),
    & d_two = Extract<Renamed_Symbol_Data>(two);
  return d_one.old_name == d_two.old_name && d_one.unique_value == d_two.unique_value;
}

inline Cell Renamed_Symbol_Old_Name(Cell cell)
{
  S_ASSERT(Is_Renamed_Symbol(cell));
  return Extract<Renamed_Symbol_Data>(cell).old_name;
}
inline Cell Renamed_Symbol_Macro(Cell cell)
{
  S_ASSERT(Is_Renamed_Symbol(cell));
  return Extract<Renamed_Symbol_Data>(cell).macro;
}

void Write_Renamed_Symbol(Cell cell, std::ostream& str, bool display)
{
  str << "#<renamed:" << (unsigned)Renamed_Symbol_Old_Name(cell) << '>';
}

Cell Extract_Symbol(Cell identifier)
{
  while (Is_Renamed_Symbol(identifier))
    identifier = Renamed_Symbol_Old_Name(identifier);
  return identifier;
}

// Turn renamed symbols back into plain symbols inside an expression.
// Needed because the macro expander does not know about quoting and
// quasiquoting, and renames symbols inside quoted expressions. This
// is used to flatten those expressions out again. The function makes
// some attempt to not create new pairs and vectors unless necessary.
Cell Unrename_Expression(const MCell& expression)
{
  if (Is_Pair(expression)){
    MCell car = Car(expression), cdr = Cdr(expression);
    car = Unrename_Expression(car);
    cdr = Unrename_Expression(cdr);
    if (car == Car(expression) && cdr == Cdr(expression))
      return expression;
    else
      return Cons(car, cdr);
  }
  else if (Is_Vector(expression)){
    MStack new_values;
    bool changed = false;
    size_t size = Vector_Size(expression);
    for (size_t i = 0; i != size; ++i){
      new_values.Push(Unrename_Expression(Vector_Ref(expression, i)));
      if (new_values.Back() != Vector_Ref(expression, i))
        changed = true;
    }
    if (changed)
      return Make_Vector(new_values);
    else
      return expression;
  }
  else if (Is_Renamed_Symbol(expression)){
    return Extract_Symbol(expression);
  }
  else{
    return expression;
  }
}

// An identifier (some more misuse of words) means a symbol or a
// renamed symbol.
inline bool Is_Identifier(Cell cell){
  return Is_Symbol(cell) || Is_Renamed_Symbol(cell);
}

// ,EQUAL

// Because equal is used in C++ code quite a bit I implemented it in
// C++ instead of scheme.
bool Equal(Cell one, Cell two)
{
  if (one == two){
    return true;
  }
  else if (Is_Compound(one) && Is_Compound(two)){
    Cell_Type type = Get_Type(one);
    if (Get_Type(two) != type){
      return false;
    }
    else if (type == pair_type){
      return Equal(Car(one), Car(two)) && Equal(Cdr(one), Cdr(two));
    }
    else if (type == vector_type){
      if (Vector_Size(one) == Vector_Size(two)){
        size_t size = Vector_Size(one);
        for (size_t i = 0; i != size; ++i){
          if (!Equal(Vector_Ref(one, i), Vector_Ref(two, i)))
            return false;
        }
        return true;
      }
    }
    else if (type == string_type){
      if (String_Size(one) == String_Size(two)){
        size_t size = String_Size(one);
        for (size_t i = 0; i != size; ++i){
          if (String_Ref(one, i) != String_Ref(two, i))
            return false;
        }
        return true;
      }
    }
    else if (type == rational_type){
      return Equal(Rational_Numerator(one), Rational_Numerator(two)) and
        Equal(Rational_Denominator(one), Rational_Denominator(two));
    }
    else if (type == real_type){
      return Real_Value(one) == Real_Value(two);
    }
    else if (type == bignum_type){
      return Bignum_Equal(one, two);
    }
    else if (type == renamed_symbol_type){
      return Renamed_Symbol_Equal(one, two);
    }
  }
  return false;
}

// The standard assoc
Cell Assoc(Cell needle, Cell list)
{
  for (; list != null_cell; list = Cdr(list)){
    if (Equal(Caar(list), needle))
      return Car(list);
  }
  return null_cell;
}

// This is not entirely the same as the scheme function member - this
// returns a bool instead of a piece of list.
bool Member(Cell value, Cell list)
{
  for (; list != null_cell; list = Cdr(list)){
    S_ASSERT(Is_Pair(list));
    if (Equal(Car(list), value))
      return true;
  }
  return false;
}

// ,MACRO

// Displaying macro's should not actually happen (synax can not be
// dereferenced), but it is nice to have something comprehensive
// instead of a segfault when it happens.
void Write_Macro(Cell cell, std::ostream& str, bool display)
{
  str << "#<macro>";
}

struct Macro_Data
{
  Cell special_symbols, rules, name, local_env, name_space;
};

Cell Make_Macro(const MCell& specials, const MCell& rules, Cell name, const MCell& local_env, const MCell& name_space)
{
  Cell new_macro = Allocate_Cell<Macro_Data>(macro_type);
  Macro_Data& data = Extract<Macro_Data>(new_macro);
  data.special_symbols = specials;
  data.rules = rules;
  data.name = name;
  data.local_env = local_env;
  data.name_space = name_space;
  return new_macro;
}

inline Cell Macro_Name(Cell macro)
{
  S_ASSERT(Is_Macro(macro));
  return Extract<Macro_Data>(macro).name;
}
inline Cell Macro_Specials(Cell macro)
{
  S_ASSERT(Is_Macro(macro));
  return Extract<Macro_Data>(macro).special_symbols;
}
inline Cell Macro_Rules(Cell macro)
{
  S_ASSERT(Is_Macro(macro));
  return Extract<Macro_Data>(macro).rules;
}
inline Cell Macro_Environment(Cell macro)
{
  S_ASSERT(Is_Macro(macro));
  return Extract<Macro_Data>(macro).local_env;
}
inline Cell Macro_Namespace(Cell macro)
{
  S_ASSERT(Is_Macro(macro));
  return Extract<Macro_Data>(macro).name_space;
}

// The error checking for macros currently happens mostly at runtime
// (as opposed to compile time), this is not very nice but it saved me
// a lot of almost-duplicate code.
Cell Syntax_Rules_To_Macro(Cell syntax_rules, Cell name, const MCell& environment, const MCell& name_space)
{
  S_CHECK(List_Length(syntax_rules, "improper list in syntax-rules") >= 2, "not enough arguments to syntax-rules");
  
  List_Builder pattern;
  MCell rules = Cdr(syntax_rules), specials = Car(syntax_rules);
            
  for (; rules != null_cell; rules = Cdr(rules)){
    S_CHECK(Is_Pair(Car(rules)) && Caar(rules) != null_cell, "pattern rule in syntax-rules is not a valid rule");

    pattern.Add_Element(Cons(Cdaar(rules), Cdar(rules)));
  }
            
  return Make_Macro(specials, pattern.List(), name, environment, name_space);
}

// System to expand macro's. I never expected that ellipses pattern
// language to be so messy until I implemented it.
class Macro_Expander
{
public:
  // External interface. You create a Macro_Expander and then apply it
  // to an expression (a list of arguments to that macro). A
  // macro_expander can be used more than once but the current code
  // (in Expander::Macro_Expand) does not do this.
  Macro_Expander(Cell macro);
  Cell Apply(const MCell& input);
  
private:
  // Used to determine whether an expression matches to any
  // non-literals. This is used to figure out when to stop expanding
  // patterns with ellipses behind them.
  enum Match_Status { good, not_bad, bad };
  Match_Status Combine(Match_Status one, Match_Status two);
  Match_Status Contains_Another_Match(MStack& values, Cell pattern);

  // During expansion the matched expressions are stored in two
  // MStacks, one with the names of the variables (_var_names), and a
  // local one with their value trees. These trees get shorter as the
  // expander enters ellipses subtemplates. Var_Number gives the index
  // of a variable, Find_Bound_Vars finds all the meaningful variables
  // in a pattern and adds them to _var_names.
  int Var_Number(Cell var);
  void Find_Bound_Vars(MStack& target, Cell pattern);
  void Build_Subtree_Vector(MStack& target, MStack& source, size_t branch_num);

  // These do the actual matching and expanding. Add_Ellipses just
  // pastes some ellipses after a pattern and is used to expand
  // multiple ellipses in a row.
  Cell Add_Ellipses(const MCell& base, size_t amount);
  Cell Match_Pattern(const MCell& pattern, const MCell& input);
  Cell Fill_In_Template(MStack& var_values, const MCell& tmpl, bool use_ellipses);

  MStack _var_names;
  MCell _macro, _unique_value;
  const MCell dot_dot_dot;
};

Macro_Expander::Macro_Expander(Cell macro)
  : _macro(macro),
    // to uniquely recognize renamed symbols instantiated by this
    // macro expansion
    _unique_value(Make_String(0)),
    dot_dot_dot(Make_Symbol("..."))
{
}

// Find out which variables exist in a pattern as non-literal,
// non-ellipsis variables.
void Macro_Expander::Find_Bound_Vars(MStack& target, Cell pattern)
{
  if (Is_Pair(pattern)){
    Find_Bound_Vars(target, Car(pattern));
    Find_Bound_Vars(target, Cdr(pattern));
  }
  else if (Is_Vector(pattern)){
    size_t size = Vector_Size(pattern);
    for (size_t i = 0; i != size; ++i)
      Find_Bound_Vars(target, Vector_Ref(pattern, i));
  }
  else if (Is_Identifier(pattern) && pattern != dot_dot_dot && !Member(pattern, Macro_Specials(_macro))){
    target.Push(pattern);
  }
}

// -1 for not found
int Macro_Expander::Var_Number(Cell var)
{
  size_t size = _var_names.Size();
  for (size_t i = 0; i != size; ++i){
    if (Equal(var, _var_names[i]))
      return i;
  }
  return -1;
}

// Append a cell to the end of a list (cell can be a list).
Cell Append_To_List(Cell cell, Cell lst)
{
  if (lst == null_cell)
    return cell;

  Cell tmp = lst;
  for (; Cdr(tmp) != null_cell; tmp = Cdr(tmp));
  Cdr(tmp) = cell;
  return lst;
}

// Insert a matched expression obtained from matching an 'ellipsed'
// subpattern to an expression into a matched expression containing
// the result of earlier matchings of this subpattern.
void Integrate_Tree(Cell element, MCell& tree)
{
  for (MCell current = element; current != null_cell; current = Cdr(current)){
    MCell found = Assoc(Caar(current), tree);
    if (found == null_cell){
      Cdar(current) = Cons_Null(Cdar(current));
      tree = Cons(Car(current), tree);
    }
    else{
      MCell tmp = Cdar(current);
      tmp = Cons_Null(tmp);
      Cdr(found) = Append_To_List(tmp, Cdr(found));
    }
  }
}

// Extract a matched expression from a pattern and an input
// expression. Will return #f/false_cell if the two did not match, and
// a list of variable - value tree pairs if it does match. A value
// tree contains just a value if the variable appeared without
// ellipses, otherwise it is a list of value trees for each of the
// matched instances. All values are prefixed with an invalid_cell to
// be able to determine the difference between parts of the value tree
// and actual values. And this function is definitely too big, same
// goes for Fill_In_Tree.
Cell Macro_Expander::Match_Pattern(const MCell& pattern, const MCell& input)
{
  if (Is_Identifier(pattern)){
    if (Member(pattern, Macro_Specials(_macro))){ // literal
      if (Equal(Extract_Symbol(input), Extract_Symbol(pattern)))
        return null_cell;
      else
        return false_cell;
    }
    else{ // normal pattern var
      MCell tmp = invalid_cell;
      tmp = Cons(tmp, input);
      tmp = Cons(pattern, tmp);
      return Cons_Null(tmp); // ((pattern #<invalid> . input))
    }
  }
  else if (Is_Pair(pattern)){
    if (Is_Pair(Cdr(pattern)) && Cadr(pattern) == dot_dot_dot){ // (pattern ...)
      S_CHECK(Cddr(pattern) == null_cell, "'...' appearing in invalid position in macro pattern");
      MCell result = null_cell;

      // add all bound identifiers in pattern to result
      MStack bound;
      Find_Bound_Vars(bound, Car(pattern));
      for (size_t i = 0; i != bound.Size(); ++i)
        result = Cons(Cons_Null(bound[i]), result);
      
      // match the elements of input against the pattern
      for (MCell current = input; current != null_cell; current = Cdr(current)){
        if (!Is_Pair(current)) // ellipses only match proper lists
          return false_cell;
        Cell matched = Match_Pattern(Car(pattern), Car(current));
        if (matched == false_cell)
          return false_cell;
        Integrate_Tree(matched, result);
      }

      return result;
    }
    else if (!Is_Pair(input)){
      return false_cell;
    }
    else{ // (pattern1 . pattern2)
      MCell car = Car(input);
      car = Match_Pattern(Car(pattern), car);
      if (car == false_cell)
        return false_cell;
      MCell cdr = Cdr(input);
      cdr = Match_Pattern(Cdr(pattern), cdr);
      if (cdr == false_cell)
        return false_cell;
      return Append_To_List(cdr, car);
    }
  }
  else if (Is_Vector(pattern)){ // #(pattern1 - patternN)
    if (!Is_Vector(input))
      return false_cell;
    size_t pat_size = Vector_Size(pattern), input_size = Vector_Size(input);
    bool ellipses = pat_size > 1 && Vector_Ref(pattern, pat_size - 1) == dot_dot_dot;
    if (ellipses)
      pat_size -= 2;
    if ((!ellipses && pat_size != input_size) || (ellipses && input_size < pat_size))
      return false_cell;
    MCell result = null_cell;
    for (size_t i = 0; i != pat_size; ++i)
      result = Append_To_List(Match_Pattern(Vector_Ref(pattern, i), Vector_Ref(input, i)), result);

    if (ellipses){ // #(pattern1 - patternx ...)
      MCell dotted = Vector_Ref(pattern, pat_size);
      for (size_t i = pat_size; i != input_size; ++i)
        Integrate_Tree(Match_Pattern(dotted, Vector_Ref(input, i)), result);
    }
    return result;
  }
  // another type of value, must be equal to the input to match.
  else if (Equal(pattern, input)){ 
    return null_cell;
  }
  else{
    return false_cell;
  }
}

// Make a list containing base followed by amount ellipses. Needed in
// the process of matching templates where multiple ellipses follow a
// subtemplate.
Cell Macro_Expander::Add_Ellipses(const MCell& base, size_t amount)
{
  MCell result = null_cell;
  for (size_t i = 0; i != amount; ++i)
    result = Cons(dot_dot_dot, result);
  return Cons(base, result);
}

// Combine two Match_Status values. These indicate how a subtemplate
// related to the current value vector. If it contained identifiers
// that should match a value but there was no value for them in the
// vector, they get 'bad', if they contain only stuff that does not
// have to match anything (literal identifiers and other literals)
// they get 'not_bad', and if they have identifiers that match values
// they get 'good'.
Macro_Expander::Match_Status Macro_Expander::Combine(Macro_Expander::Match_Status one, Macro_Expander::Match_Status two)
{
  if (one == bad || two == bad)
    return bad;
  else if (one == good || two == good)
    return good;
  else
    return not_bad;
}

// Check whether a subtemplate contains any identifiers matching stuff
// in the value vector.
Macro_Expander::Match_Status Macro_Expander::Contains_Another_Match(MStack& values, Cell pattern)
{
  if (Is_Identifier(pattern)){
    int nr = Var_Number(pattern);
    if (nr == -1)
      return not_bad;
    else if (values[nr] == invalid_cell)
      return bad;
    else
      return good;
  }
  else if (Is_Pair(pattern)){
    return Combine(Contains_Another_Match(values, Car(pattern)), Contains_Another_Match(values, Cdr(pattern)));
  }
  else if (Is_Vector(pattern)){
    size_t size = Vector_Size(pattern);
    Match_Status accum = not_bad;
    for (size_t i = 0; i != size; ++i){
      accum = Combine(accum, Contains_Another_Match(values, Vector_Ref(pattern, i)));
      if (accum == bad)
        return bad;
    }
    return accum;
  }
  else{
    return not_bad;
  }
}

// Make a value vector for branch #branch_num from the values in
// source.
void Macro_Expander::Build_Subtree_Vector(MStack& target, MStack& source, size_t branch_num)
{
  size_t size = _var_names.Size();
  for (size_t i = 0; i != size; ++i){
    MCell current = source[i];
    if (Is_Pair(current) && Car(current) != invalid_cell){
      size_t j = 0;
      for (size_t j = 0; j != branch_num && current != null_cell; ++j)
        current = Cdr(current);
      if (current == null_cell){
        if (j == branch_num)
          target[i] = null_cell;
        else
          target[i] = invalid_cell;
      }
      else{
        target[i] = Car(current);
      }
    }
    else{
      target[i] = invalid_cell;
    }
  }
}

// Fill in a template with the elements that got matched. The third
// argument is there to support subtemplates in the form of (...
// template), which are treated as template except that ellipses do
// not have special meaning inside it.
Cell Macro_Expander::Fill_In_Template(MStack& var_values, const MCell& tmpl, bool use_ellipses)
{
  if (Is_Pair(tmpl)){
    MCell cdr = Cdr(tmpl);
    if (use_ellipses && Car(tmpl) == dot_dot_dot){ // (... templ)
      S_CHECK(Is_Pair(cdr) && Cdr(cdr) == null_cell, "invalid use of '...' at start of list in macro template");
      return Fill_In_Template(var_values, Car(cdr), false);
    }
    else if (use_ellipses && Is_Pair(cdr) && Car(cdr) == dot_dot_dot){ // (templ ... [...]* . other-stuff)
      cdr = Cdr(cdr);
      MCell dotted = Car(tmpl);
      size_t ellipses = 0, branch = 0;
      List_Builder result;

      while (Is_Pair(cdr) && Car(cdr) == dot_dot_dot){ // checking for multiple ellipses
        cdr = Cdr(cdr);
        ++ellipses;
      }
      if (ellipses > 0)
        dotted = Add_Ellipses(dotted, ellipses); // any extra ellipses get delegated to the next recursion

      while (true){
        MStack new_values(_var_names.Size()); // construct a new value vector
        Build_Subtree_Vector(new_values, var_values, branch);
        if (Contains_Another_Match(new_values, dotted) != good)
          break;
        MCell filled =  Fill_In_Template(new_values, dotted, true);
        
        if (ellipses == 0){ // if only one set of ellipses add the result as an atom
          result.Add_Element(filled);
        }
        else{ // otherwise splice it
          for (; filled != null_cell; filled = Cdr(filled))
            result.Add_Element(Car(filled));
        }
        ++branch;
      }

      // match the stuff after the ellipses
      result.Add_End(Fill_In_Template(var_values, cdr, true));
      return result.List();
    }
    else{ // (templ1 . templ2)
      MCell car = Car(tmpl);
      car = Fill_In_Template(var_values, car, use_ellipses);
      cdr = Fill_In_Template(var_values, cdr, use_ellipses);
      return Cons(car, cdr);
    }
  }
  else if (Is_Vector(tmpl)){
    MStack result;
    size_t size = Vector_Size(tmpl);
    for (size_t i = 0; i != size; ++i){
      MCell current = Vector_Ref(tmpl, i);
      size_t ellipses = 0;
      if (use_ellipses){
        while (i < size - 1 && Vector_Ref(tmpl, i + 1) == dot_dot_dot){ // immediately handle any ellipses after this element
          ++ellipses;
          ++i;
        }
      }
      if (ellipses > 0){
        MCell ellipsed = Add_Ellipses(current, ellipses); // pass the ellipses to the recursion
        Cell filled = Fill_In_Template(var_values, ellipsed, true);
        for (; filled != null_cell; filled = Cdr(filled)) // splice in the result
          result.Push(Car(filled));
      }
      else{
        result.Push(Fill_In_Template(var_values, current, use_ellipses));
      }
    }
    return Make_Vector(result);
  }
  else if (Is_Identifier(tmpl)){
    int nr = Var_Number(tmpl);
    if (nr == -1){ // if this is a literal symbol it gets renamed
      return Make_Renamed_Symbol(tmpl, _macro, _unique_value);
    }
    else{ // otherwise the value gets inserted
      Cell value = var_values[nr];
      if (value == null_cell)
        return null_cell;
      S_ASSERT(Is_Pair(value) && Car(value) == invalid_cell);
      return Cdr(value);
    }
  }
  else{
    // some kind of literal value
    return tmpl;
  }
}

// Expand a macro
Cell Macro_Expander::Apply(const MCell& expr)
{
  // for every pattern - template match there is
  for (MCell pattern = Macro_Rules(_macro); pattern != null_cell; pattern = Cdr(pattern)){
    // try a match
    Cell matched = Match_Pattern(Caar(pattern), expr);
    // if it matched
    if (matched != false_cell){
      // make a value vector out of the match
      _var_names.Clear();
      Find_Bound_Vars(_var_names, Caar(pattern));
      MStack values(_var_names.Size());
      for (; matched != null_cell; matched = Cdr(matched)){
        int num = Var_Number(Caar(matched));
        S_ASSERT(num != -1);
        values[num] = Cdar(matched);
      }
      // and expand the macro
      return Fill_In_Template(values, Cadar(pattern), true);
    }
  }
  // nothing matched
  throw Scheme_Error("invalid syntax for " + Symbol_Name(Macro_Name(_macro)));
}

Cell Compile(Cell expression, const MCell& name_space);
Cell Run_Code(Cell code, bool handle_errors);

// ,SIMPLE MACRO

// Old-style macros that just call a function to do the expansion.

Cell Make_Simple_Macro(const MCell& function)
{
  S_CHECK(Is_Closure(function), "define-macro requires a closure as it's second argument");
  Cell new_macro = Allocate_Cell<Cell>(simple_macro_type);
  Extract<Cell>(new_macro) = function;
  return new_macro;
}

inline Cell Simple_Macro_Function(Cell macro)
{
  S_ASSERT(Is_Simple_Macro(macro));
  return Extract<Cell>(macro);
}

Cell Run_Code(Cell code, bool handle_errors);

Cell Expand_Simple_Macro(const MCell& macro, const MCell& args)
{
  MCell code = Make_Vector(6);
  Vector_Ref(code, 0) = Make_Instruction(i_literal);
  Vector_Ref(code, 1) = args;
  Vector_Ref(code, 2) = Make_Instruction(i_as_arguments);
  Vector_Ref(code, 3) = Make_Instruction(i_literal);
  Vector_Ref(code, 4) = Simple_Macro_Function(macro);
  Vector_Ref(code, 5) = Make_Instruction(i_tail);
  return Run_Code(code, false);
}
  
// ,CONTINUATION

// One Live_Continuation lives inside the Run_Code function. Together
// with the stack that lives there it keeps track of the current state
// of the virtual machine. Note that this is only one of the three
// forms continuations take. They can be allocated as cells (needed
// for call/cc), they can be a bunch of elements on an MStack (used
// for normal function calling), and this one here is used to
// represent the active continuation.
class Live_Continuation
{
public:
  Live_Continuation(Cell start_code);
  
  MCell code, reg, arguments, environment;
  Cell function_name; // name of the function we are currently in
  size_t instruction_counter;
};

Live_Continuation::Live_Continuation(Cell start_code)
  : code(start_code),
    function_name(false_cell),
    instruction_counter(0)
{}

// This pushing and popping of continuations is slightly crummy,
// information about which field goes where is present here, in the
// Run_Code loop and in the Restore_Continuation function.
const size_t continuation_size = 5;

void Push_Continuation(Live_Continuation& cont, MStack& stack)
{
  stack.Push(cont.code);
  stack.Push(cont.arguments);
  stack.Push(cont.environment);
  stack.Push(cont.function_name);
  stack.Push(Make_Fixnum(cont.instruction_counter));
}

void Pop_Continuation(Live_Continuation& cont, MStack& stack)
{
  cont.instruction_counter = Fixnum_Value(stack.Pop());
  cont.function_name = stack.Pop();
  cont.environment = stack.Pop();
  cont.arguments = stack.Pop();
  cont.code = stack.Pop();
}

void Write_Continuation(Cell cell, std::ostream& str, bool display)
{
  str << "#<continuation>";
}

struct Continuation_Data
{
  Cell code, code_counter, continuation, arguments, environment, function_name;
};

Cell Make_Continuation(MStack& stack, size_t position, const MCell& parent)
{
  Cell new_c = Allocate_Cell<Continuation_Data>(continuation_type);
  Continuation_Data& data = Extract<Continuation_Data>(new_c);
  data.code = stack[position];
  data.arguments = stack[++position]; // ewwwwww... it is fast though
  data.environment = stack[++position];
  data.function_name = stack[++position];
  data.code_counter = stack[++position];
  data.continuation = parent;
  
  return new_c;
}

void Restore_Continuation(Cell continuation, MStack& stack) // TODO: copy argument vectors
{
  Continuation_Data& data = Extract<Continuation_Data>(continuation);
  stack.Push(data.code);
  stack.Push(data.arguments);
  stack.Push(data.environment);
  stack.Push(data.function_name);
  stack.Push(data.code_counter);
}

inline Cell Continuation_Function_Name(Cell continuation){
  return Extract<Continuation_Data>(continuation).function_name;
}
inline Cell Continuation_Parent(Cell continuation){
  return Extract<Continuation_Data>(continuation).continuation;
}

// ,ENVIRONMENT

void Write_Namespace(Cell cell, std::ostream& str, bool display)
{
  str << "#<namespace>";
}

struct Namespace_Data
{
  Cell size;
  Cell parent;
  Cell binding[1];
};

const size_t default_workspace_size = 503, null_env_size = 23, report_env_size = 233;

Cell Make_Namespace(const MCell& parent, size_t size)
{
  Cell retval = Allocate(sizeof(Namespace_Data) + sizeof(Cell) * (size - 1), namespace_type);
  Namespace_Data& data = Extract<Namespace_Data>(retval);
  data.parent = parent;
  data.size = Make_Fixnum(size);
  for (size_t i = 0; i != size; ++i)
    data.binding[i] = null_cell;
  return retval;
}

void Bootstrap_Namespace(const MCell& name_space)
{
  Eval_String("(define (interaction-environment) (impl:current-env))", name_space, false);
  Eval_String("(define (load file) (_load file (interaction-environment)))", name_space, false);
}

Cell Namespace_Parent(Cell space)
{
  S_ASSERT(Is_Namespace(space));
  return Extract<Namespace_Data>(space).parent;
}
size_t Namespace_Size(Cell space)
{
  S_ASSERT(Is_Namespace(space));
  return Fixnum_Value(Extract<Namespace_Data>(space).size);
}
Cell& Namespace_Ref(Cell space, size_t index)
{
  S_ASSERT(Is_Namespace(space));
  S_ASSERT(index < Namespace_Size(space));
  return Extract<Namespace_Data>(space).binding[index];
}

inline size_t Hash_Symbol(Cell symbol, Cell name_space)
{
  return Symbol_Value(symbol) % Namespace_Size(name_space);
}

// Get a pair (symbol . value) that corresponds to the binding of
// symbol in the namespace. If no such pair is present a new one is
// created with the value of the symbol in underlying namespaces (or
// invalid_cell if no value is found).
Cell Get_Binding(const MCell& name_space, Cell symbol)
{
  size_t hashed = Hash_Symbol(symbol, name_space);
  for (Cell element = Namespace_Ref(name_space, hashed); element != null_cell; element = Cdr(element)){
    if (Caar(element) == symbol)
      return Car(element);
  }
  MCell new_binding = Cons(symbol, Get_Value(Namespace_Parent(name_space), symbol));
  MCell old_list = Namespace_Ref(name_space, hashed);
  Namespace_Ref(name_space, hashed) = Cons(new_binding, old_list);
  return new_binding;
}

// Get a value from a namespace and the namespaces below it without
// creating a new pair if it does not exist.
Cell Get_Value(Cell name_space, Cell symbol)
{
  for (; name_space != null_cell; name_space = Namespace_Parent(name_space)){
    for (Cell element = Namespace_Ref(name_space, Hash_Symbol(symbol, name_space)); element != null_cell; element = Cdr(element)){
      if (Caar(element) == symbol)
        return Cdar(element);
    }
  }
  return invalid_cell;
}

// Define a symbol in a namespace
void Define_Symbol(const MCell& name_space, const std::string& name, const MCell& value)
{
  Cell slot = Get_Binding(name_space, Make_Symbol(name));
  Cdr(slot) = value;
}

// These are used to represent offsets in an environment (list of
// vectors). Depth indicates which vector, distance indicates the
// element of that vector.
struct Environment_Ref
{
  unsigned short depth, distance;
};
const unsigned short not_found = max_short;

// Find symbol in vector.
unsigned short Environment_Offset(Cell symbol, Cell env_vector)
{
  unsigned short retval = 0;
  size_t size = Vector_Size(env_vector);
  for (size_t i = 0; i != size; ++i){
    if (Vector_Ref(env_vector, i) == symbol)
      return retval;
    ++retval;
  }
  return not_found;
}

// Get ref from environment.
Environment_Ref Find_In_Environment(Cell symbol, Cell environment)
{
  Environment_Ref retval = {0, 0};
  while (environment != null_cell){
    retval.distance = Environment_Offset(symbol, Car(environment));
    if (retval.distance != not_found)
      return retval;
    environment = Cdr(environment);
    ++retval.depth;
  }
  retval.depth = not_found;
  return retval;
}

// This is how refs are represented in code vectors (pair of fixnums).
inline Cell Make_Env_Ref(size_t depth, size_t distance)
{
  return XCons(Make_Fixnum(depth), Make_Fixnum(distance));
}

// Does two things. Checks whether a correct number of arguments was
// given, and in case the last argument is a 'rest' argument it
// changes the vector around to move the rest arguments into a list.
Cell Adjust_Arguments(Cell argument_vector, int num_values, int num_args, Cell function_name)
{
  if (num_args >= 0){ // fixed argument number
    if (num_values == num_args)
      return argument_vector;
    else if (num_values < num_args)
      throw Scheme_Error("not enough arguments to function " + Function_Name(function_name));
    else
      throw Scheme_Error("too many arguments to function " + Function_Name(function_name));
  }
  else{ // variable argument number
    num_args = -num_args;
    if (num_values < num_args - 1){
      throw Scheme_Error("not enough arguments to function " + Function_Name(function_name));
    }
    else{ // construct a new argument vector from the old one, putting all the arguments past num_args - 1 into a list
      MCell old_arg_vec = argument_vector, new_arg_vec = Make_Vector(num_args);
      for (int i = 0; i < num_args - 1; ++i)
        Vector_Ref(new_arg_vec, i) = Vector_Ref(old_arg_vec, i);
      List_Builder var_arg_list;
      for (int i = num_args - 1; i < num_values; ++i)
        var_arg_list.Add_Element(Vector_Ref(old_arg_vec, i));
      Vector_Ref(new_arg_vec, num_args - 1) = var_arg_list.List();
      return new_arg_vec;
    }
  }
}

// Increase the size of the top vector of an environment. Used for
// local defines.
void Grow_Environment(const MCell& environment, size_t new_size)
{
  Cell new_vec = Make_Vector(new_size, invalid_cell);
  Cell old_vector = Car(environment);
  if (old_vector != null_cell){
    size_t old_size = Vector_Size(old_vector);
    S_ASSERT(new_size >= old_size);

    for (size_t i = 0; i != old_size; ++i)
      Vector_Ref(new_vec, i) = Vector_Ref(old_vector, i);
  }
  Car(environment) = new_vec;
}

// ,READER

// First some utility functions to recognize the various kinds of
// chars. Symbol start characters unambiguously mean the rest of the
// element is a symbol.
bool Is_Symbol_Start(char c)
{
  static const char specials[] = {'!', '$', '%', '&', '*', '/', ':', '<',
                                  '=', '>', '?', '@', '^', '_', '~', 0};
  if (std::isalpha(c))
    return true;
  for (const char* i = specials; *i != 0; ++i){
    if (c == *i)
      return true;
  }
  return false;
}

// These represent a symbol when they are all by themselves, otherwise
// they are a number.
inline bool Is_Ambiguous_Char(char c)
{
  return (c == '+' || c == '-' || c == '.');
}
inline bool Is_Number_Char(char c)
{
  return (std::isdigit(c) || Is_Ambiguous_Char(c));
}
// Inside numbers slashes and exponents can appear.
inline bool Is_Internal_Number_Char(char c)
{
  return Is_Number_Char(c) || c == 'e' || c == 'E' || c == '/';
}
// Inside symbols anything that is a symbol start or a number can
// appear.
inline bool Is_Symbol_Char(char c)
{
  return (Is_Symbol_Start(c) || Is_Number_Char(c));
}

// for comparing character names
bool String_Compare_Insensitive(const std::string& one, const std::string& two)
{
  if (one.size() != two.size())
    return false;
  for (size_t i = 0; i != one.size(); ++i){
    if (std::tolower(one[i]) != std::tolower(two[i]))
      return false;
  }
  return true;
}

// Convert elements from an Input_Splitter into cells. Reads one whole
// expression. This is a bit of an overgrown monster function... It is
// important that whenever something throws because of invalid input,
// it FIRST advances past that invalid input - otherwise the system
// will go into an infinite loop over that same invalid input.
Cell Read(Input_Splitter& input, bool allow_eof = true)
{
  const std::string& cur = input.Current();
  
  if (cur == "("){ // a list
    input.Advance(false);
    List_Builder new_list;
    
    while (true){
      if (input.Current() == ")"){
        input.Advance(allow_eof);
        break;
      }
      else{
        new_list.Add_Element(Read(input, false));

        if (input.Current() == "."){
          input.Advance(false);
          new_list.Add_End(Read(input, false));
          S_CHECK(input.Current() == ")", "not a valid pair");
          input.Advance(false);
          break;
        }
      }
    }
    return new_list.List();
  }
  else if (cur[0] == '"'){ // a string
    S_ASSERT(cur.size() >= 2);
    std::string value;
    bool escaped = false;
    for (size_t i = 1; i < cur.size() - 1; ++i){
      if (cur[i] != '\\' || escaped){
        value += cur[i];
        escaped = false;
      }
      else{
        escaped = true;
      }
    }
    input.Advance(allow_eof);
    return Make_String(value);
  }
  else if (cur == "'"){ // a quoted thingy
    input.Advance(false);
    static const Cell quote = Make_Symbol("quote");
    return Cons(quote, Cons_Null(Read(input)));
  }
  else if (cur == "`"){ // quasiquoted
    input.Advance(false);
    static const Cell quasiquote = Make_Symbol("quasiquote");
    return Cons(quasiquote, Cons_Null(Read(input)));
  }
  else if (cur == ","){ // unquoted
    input.Advance(false);
    static const Cell unquote = Make_Symbol("unquote");
    return Cons(unquote, Cons_Null(Read(input)));
  }
  else if (cur == ",@"){ // splicing-unquoted
    input.Advance(false);
    static const Cell unquote_splicing = Make_Symbol("unquote-splicing");
    return Cons(unquote_splicing, Cons_Null(Read(input)));
  }
  else if (cur == "#("){ // a vector
    input.Advance(false);
    MStack elements;
    while(input.Current() != ")")
      elements.Push(Read(input, false));
    input.Advance(allow_eof);
    Cell new_vec = Make_Vector(elements);
    return new_vec;
  }
  else if (cur[0] == '#'){
    if (cur.size() < 2){ // single #
      input.Advance(allow_eof);
      throw Scheme_Error("invalid input: #");
    }
    char c = cur[1];
    if (cur.size() == 2){ // special
      input.Advance(allow_eof);
      if (c == 't')
        return true_cell;
      else if (c == 'f')
        return false_cell;
      else if (c == 'v')
        return void_cell;
      else
        throw Scheme_Error(std::string("unrecognized #-expression: #") + c);
    }
    else if (c == '\\'){ // character
      std::string charname(cur, 2);
      input.Advance(allow_eof);
      if (charname.size() == 1){
        return Make_Character(charname[0]);
      }
      else if (String_Compare_Insensitive(charname, "space")){
        return Make_Character(' ');
      }
      else if (String_Compare_Insensitive(charname, "newline")){
        return Make_Character('\n');
      }
      else if (String_Compare_Insensitive(charname, "eof")){
        return eof_cell;
      }
      throw Scheme_Error("unknown character constant: #\\" + charname);
    }
    else if (c == '%'){ // instruction
      std::string ins_name(cur, 2);
      input.Advance(allow_eof);
      return Make_Instruction(Find_Instruction(ins_name).instruction);
    }
    else{
      std::string error = "unrecognized #-expression: " + cur;
      input.Advance();
      throw Scheme_Error(error);
    }
  }
  else if (Is_Symbol_Start(cur[0]) || (Is_Ambiguous_Char(cur[0]) && cur.size() == 1) || cur == "..."){ // symbol
    std::string buffer;
    for(std::string::const_iterator i = cur.begin(); i != cur.end(); ++i){
      if (!Is_Symbol_Char(*i)){
        buffer = "invalid character in symbol name: " + cur;
        input.Advance();
        throw Scheme_Error(buffer);
      }
      buffer += std::tolower(*i);
    }
    input.Advance(allow_eof);
    return Make_Symbol(buffer);
  }
  else if (Is_Number_Char(cur[0])){ // number
    std::string saved = cur;
    input.Advance(allow_eof);
    return String_To_Number(saved);
  }
  else{ // wrong
    std::string error = "unrecognized input: " + cur;
    input.Advance();
    throw Scheme_Error(error);
  }
}

Cell Read(std::istream& stream){
  Stream_Input_Splitter splitter(stream);
  return Read(splitter);
}

// Read from an input port. Takes care to update the line number after
// reading.
Cell Read(Cell inport){
  S_CHECK(Inport_Is_Open(inport), "can not read from closed port");
  MCell port = inport;
  Stream_Input_Splitter splitter(*Extract<Inport_Data>(port).stream);
  try{
    Cell retval = Read(splitter);
    Extract<Inport_Data>(port).line += splitter.Lines_Read();
    return retval;
  }
  catch (Scheme_Error& e){
    Extract<Inport_Data>(port).line += splitter.Lines_Read();
    throw;
  }
}

// ,TYPES

// Thingy to keep track of what print functions belong to what types.
// Registers all the standard types on startup.
Type_Manager::Type_Manager()
  : _functions(available_type, NULL),
#ifdef WITH_DESTRUCTORS
    _destructors(available_type, NULL),
#endif
    _current(available_type)
{
  struct Association
  {
    Cell_Type type;
    Write_Function write;
  };

  static const Association init[] = {
    {renamed_symbol_type, Write_Renamed_Symbol},
    {pair_type, Write_Pair},
    {vector_type, Write_Vector},
    {string_type, Write_String},
    {closure_type, Write_Closure},
    {primitive_type, Write_Primitive},
    {continuation_type, Write_Continuation},
    {inport_type, Write_Inport},
    {outport_type, Write_Outport},
    {rational_type, Write_Rational},
    {real_type, Write_Real},
    {bignum_type, Write_Bignum},
    {macro_type, Write_Macro},
    {simple_macro_type, Write_Macro},
    {namespace_type, Write_Namespace},
    {available_type, NULL}
  };
  for (const Association* cur = init; cur->write != NULL; ++cur)
    _functions[cur->type] = cur->write;
#ifdef WITH_DESTRUCTORS
  _destructors[inport_type] = Close_Inport;
  _destructors[outport_type] = Close_Outport;
#endif
}

Cell_Type Type_Manager::Make_Type(Write_Function write
#ifdef WITH_DESTRUCTORS
                                  , Destroy_Function destruct
#endif
                                  )
{
  _functions.resize(_current + 1);
  _functions[_current] = write;
#ifdef WITH_DESTRUCTORS
  _destructors.resize(_current + 1);
  _destructors[_current] = destruct;
#endif

  S_ASSERT(_current != max_byte);
  ++_current;
  return _current - 1;
}

#ifdef WITH_DESTRUCTORS
Cell_Type Make_Type(Write_Function write, Destroy_Function destruct)
{
  return ip_->type_manager.Make_Type(write, destruct);
}
#else
Cell_Type Make_Type(Write_Function write)
{
  return ip_->type_manager.Make_Type(write);
}
#endif

// ,WRITE

// Write a cell to a stream. Display indicates whether it is printed
// in s-expression or pretty format (with display on strings have no
// "'s and characters have no #\)
void Write(Cell cell, std::ostream& str, bool display)
{
  // Compounds get printed with the function that is associated with their type
  if (Is_Compound(cell)){
    ip_->type_manager.Get_Function(Get_Type(cell))(cell, str, display);
  }
  // Non-compounds get handled by this function
  else if (Is_Symbol(cell)){
    bool normal_symbol = false;
    std::string name = Symbol_Name(cell);
    // Check whether the symbol contains any weird characters.
    if ((name.size() > 0 && Is_Symbol_Start(name[0])) || (Is_Ambiguous_Char(name[0]) && name.size() == 1)
        || name == "..."){
      for (std::string::iterator i = name.begin() + 1; i != name.end(); ++i){
        if (!Is_Symbol_Char(*i))
          goto outside;
      }
      normal_symbol = true;
    }
  outside:
    // add pipes around weird symbols
    if (!normal_symbol && !display)
      str << '|';
    str << name;
    if (!normal_symbol && !display)
      str << '|';
  }
  else if (Is_Special(cell)){
    if (cell == null_cell)
      str << "()";
    else if (cell == true_cell)
      str << "#t";
    else if (cell == false_cell)
      str << "#f";
    else if (cell == void_cell)
      str << "#v";
    else if (cell == invalid_cell)
      str << "#<INVALID>";
    else
      S_ASSERT(false);
  }
  else if (Is_Fixnum(cell)){
    str << Fixnum_Value(cell);
  }
  else if (Is_Character(cell)){
    int c = Character_Value(cell);
    if (!display)
      str << "#\\";
    if (c == '\n' && !display)
      str << "newline";
    else if (c == ' ' && !display)
      str << "space";
    else if (c == 256 && !display)
      str << "eof";
    else if (c == 256) // eof always keeps its #\.
      str << "#\\eof";
    else
      str << static_cast<char>(c);
  }
  else if (Is_Instruction(cell)){
    str << "#%" << Find_Instruction(Instruction_Value(cell)).name;
  }
  else if (Is_Special_Form(cell)){
    str << "#<special form>";
  }
  else if (Is_Temp_Name(cell)){
    str << "#<temp:" << (unsigned)Temp_Name_ID(cell) << '>';
  }
  else{
    S_ASSERT(false);
  }
}

std::string Cell_To_String(Cell cell, bool display)
{
  std::ostringstream result;
  Write(cell, result, display);
  return result.str();
}

// ,EXPANDER

// Looks up a binding in a list of lists that represent the current
// environment for the expander. Expanders bind two things - local
// vars get bound to temp names and vars introduced by let-syntax or
// letrec-syntax get bound to macro's. Takes care of renamed symbols
// and their embedded environments. If nothing matches the symbol
// itself is returned.
Cell Resolve_Name(Cell identifier, Cell env, Cell name_space, bool syntax_value, bool one_deep = false)
{
  S_ASSERT(Is_Identifier(identifier));
  Cell id = identifier;

  Cell found = null_cell;
  for(; env != null_cell; env = Cdr(env)){
    found = Assoc(id, Car(env));
    if (found != null_cell && (syntax_value || !Is_Syntax(Cdr(found)))){
      found = Cdr(found);
      break;
    }
    if (one_deep)
      break;
  }

  if (found != null_cell){
    return found;
  }
  else if (Is_Renamed_Symbol(identifier) && !one_deep){
    Cell macro = Renamed_Symbol_Macro(identifier);
    return Resolve_Name(Renamed_Symbol_Old_Name(identifier), Macro_Environment(macro), Macro_Namespace(macro), syntax_value);
  }
  else if (syntax_value){
    Cell ns_value = Get_Value(name_space, identifier);
    if (Is_Syntax(ns_value))
      return ns_value;
    else
      return identifier;
  }
  else{
    return identifier;
  }
}

// Extract the unquoted parts of a quasiquoted expression.
void Find_Unquoted_Parts(MStack& unquoted, Cell expression, size_t depth)
{
  const static Cell unquote = Make_Symbol("unquote");
  const static Cell unquote_splicing = Make_Symbol("unquote-splicing");
  const static Cell quasiquote = Make_Symbol("quasiquote");

  if (Is_Pair(expression)){
    if (Car(expression) == unquote || Car(expression) == unquote_splicing){
      S_CHECK(Is_Pair(Cdr(expression)) && Cddr(expression) == null_cell, "invalid unquoted expression");
      if (depth == 0)
        unquoted.Push(Cadr(expression));
      else
        Find_Unquoted_Parts(unquoted, Cadr(expression), depth - 1);
    }
    else if (Car(expression) == quasiquote){
      Find_Unquoted_Parts(unquoted, Cadr(expression), depth + 1);
    }
    else{
      Find_Unquoted_Parts(unquoted, Car(expression), depth);
      Find_Unquoted_Parts(unquoted, Cdr(expression), depth);
    }
  }
  else if (Is_Vector(expression)){
    size_t size = Vector_Size(expression);
    for(size_t i = 0; i != size; ++i)
      Find_Unquoted_Parts(unquoted, Vector_Ref(expression, i), depth);
  }
}

// Check whether an expression resembles a syntax-rules expression.
bool Is_Syntax_Rules(Cell element)
{
  static const Cell syntax_rules = Make_Symbol("syntax-rules");
  return Is_Pair(element) && Is_Identifier(Car(element)) && Extract_Symbol(Car(element)) == syntax_rules;
}

// Before code is compiled it is run through the expander. This has a
// number of effects. The most important effect is that macro's get
// expanded, related to that embedded let-syntax and letrec-syntax
// expressions vanish. To make hygiene workable all local symbols get
// new unique names (temp-names). All listst starting with a symbol
// referring to a special form get their first element dereferenced.
// On top of that a few special forms get transformed. If expressions
// get a #v element added if no 'else expresson' was given. Defines
// that have a list as their first argument get transformed to normal
// defines with a lambda a second argument. Lambda's get a fixnum
// representing the amount of internal defines as a first argument.
// Begins that contain defines get transformed to lambda expressions
// so that remaining begins can be inlined. Quasiquote expressions get
// a vector of expanded unquoted parts as a first argument.
class Expander
{
public:
  Expander(Cell name_space, Cell local_env, size_t cur_name);
  Cell Expand(Cell expression);

private:
  Cell Expand_Body(Cell expression, bool wrap);
  Cell Expand_Form(Cell syntax, Cell tail, bool define_allowed);
  Cell Macro_Expand(Cell expression);

  bool Look_For_Defines(Cell expression, MStack& results, MStack& defined_symbols);
  Cell Make_Name();
  
  size_t _cur_name;
  MCell _name_space, _local_env;
};

Expander::Expander(Cell name_space, Cell local_env, size_t cur_name)
  : _cur_name(cur_name),
    _name_space( name_space),
    _local_env(local_env)
{}

// Generate a new temp name that is unique in this branch.
Cell Expander::Make_Name()
{
  Cell name = Make_Temp_Name(_cur_name);
  ++_cur_name;
  return name;
}

// checks for (define ....) || (begin [(define ...)]*)
bool Expander::Look_For_Defines(Cell expression, MStack& results, MStack& defined_symbols)
{
  static const Cell define_cell = Make_Special_Form(form_define);
  static const Cell begin_cell = Make_Special_Form(form_begin);

  MCell expanded = Macro_Expand(expression);
  Cell head = null_cell;
  if (Is_Pair(expanded) && Is_Identifier(Car(expanded)))
    head = Resolve_Name(Car(expanded), _local_env, _name_space, true);
  
  // if it is a define expression
  if (head == define_cell){
    S_CHECK(Cdr(expanded) != null_cell, "invalid define expression");
    Cell symbol = Cadr(expanded);
    if (Is_Pair(symbol)) // lambda define
      symbol = Car(symbol);

    S_CHECK(Is_Identifier(symbol), "invalid define expression - defining a non-symbol value");
    defined_symbols.Push(symbol);
    results.Push(expanded);
    return true;
  }
  // begin, does it only contain defines?
  else if (head == begin_cell){
    MStack parts, defined;
    for (MCell part = Cdr(expanded); part != null_cell; part = Cdr(part)){
      S_CHECK(Is_Pair(part), "malformed begin expression - not a proper list");
      MCell current = Macro_Expand(Car(part));
      if (!Look_For_Defines(current, parts, defined)){
        results.Push(expanded);
        return false;
      }
    }
    for (size_t i = 0; i != parts.Size(); ++i)
      results.Push(parts[i]);
    for (size_t i = 0; i != defined.Size(); ++i)
      defined_symbols.Push(defined[i]);
    return true;
  }
  // other expression, is not a define
  else{
    results.Push(expanded);
    return false;
  }
}

// Expand a lambda body. Checks for internal defines. If wrap is true
// it will assume the lambda had no arguments and wrap it into a begin
// if no defines were present or a lambda with no args if there were
// defines.
Cell Expander::Expand_Body(Cell expression, bool wrap)
{
  static const Cell begin_cell = Make_Special_Form(form_begin);
  static const Cell define_cell = Make_Special_Form(form_define);
  static const Cell lambda_cell = Make_Special_Form(form_lambda);

  size_t defines = 0;
  bool in_defines = true;
  MStack result, defined_symbols;
  MCell expr = expression;

  for (; expr != null_cell; expr = Cdr(expr)){
    S_CHECK(Is_Pair(expr), "improper list in begin/lambda block: " + Cell_To_String(expr));

    if (in_defines){
      defines = result.Size();
      in_defines = Look_For_Defines(Car(expr), result, defined_symbols);
    }
    else{
      result.Push(Macro_Expand(Car(expr)));
    }
  }
  
  // update the environment and gather new names
  MStack new_names;
  for (size_t i = 0; i != defined_symbols.Size(); ++i){
    Cell symbol = defined_symbols[i];
    Cell current_binding = Resolve_Name(symbol, _local_env, _name_space, false, true);
    if (current_binding == symbol){
      Cell name = Make_Name();
      Car(_local_env) = Cons(Cons(symbol, name), Car(_local_env));
      new_names.Push(name);
    }
  }

  // now that the environment is up to date we can expand all the
  // expression.
  List_Builder expanded;
  for (size_t i = 0; i != result.Size(); ++i){
    if (i < defines)
      expanded.Add_Element(Expand_Form(define_cell, Cdr(result[i]), true));
    else
      expanded.Add_Element(Expand(result[i]));
  }
  
  if (!wrap){
    return Cons(Make_Vector(new_names), expanded.List()); // (#(new defs) body ...)
  }
  else if (new_names.Empty()){ // (begin body ...)
    return Cons(begin_cell, expanded.List());
  }
  else{ // ((lambda () #(new defs) body ...))
    MCell tmp = Make_Vector(new_names);
    tmp = Cons(tmp, expanded.List());
    tmp = Cons(null_cell, tmp);
    tmp = Cons(lambda_cell, tmp);
    return Cons_Null(tmp);
  }
}

// keep expanding macros as long as expression is a list and its first
// element refers to a macro
Cell Expander::Macro_Expand(Cell expression)
{
  if (Is_Pair(expression) && Is_Identifier(Car(expression))){
    Cell val = Resolve_Name(Car(expression), _local_env, _name_space, true);
    if (Is_Macro(val)){
      MCell expr = Cdr(expression);
      Macro_Expander m_expander(val);
      return Macro_Expand(m_expander.Apply(expr));
    }
    else if (Is_Simple_Macro(val))
    {
      MCell expr = Cdr(expression);
      return Macro_Expand(Expand_Simple_Macro(val, expr));
    }
  }
  return expression;
}

// handle special forms. this does a lot of error checking and other
// stuff to make the compiler's life a little simple. main function is
// to properly replace symbols with temp names when they are local.
Cell Expander::Expand_Form(Cell syntax, Cell tail, bool define_allowed)
{
  static const Cell lambda = Make_Symbol("lambda");

  MCell back = tail;
  size_t args = List_Length(back, "attempt to evaluate a special form with an improper list as arguments");
  List_Builder result;
  result.Add_Element(syntax);
  
  switch(Special_Form_Name(syntax)){
    // if - a third argument gets added if only two were given
  case form_if:
    {
      S_CHECK(args > 1 && args < 4, "if takes either 2 or 3 arguments");
      result.Add_Element(Expand(Car(back)));
      result.Add_Element(Expand(Cadr(back)));
      if (args == 2)
        result.Add_Element(void_cell);
      else
        result.Add_Element(Expand(Caddr(back)));
      return result.List();
    } break;

    // define - (define (a b ...) body ...) becomes (define new-a old-a (lambda (b ...) body ...))
    // the environment is already updated by Expand_Body if this is a
    // local define. a new argument is added to indicate the old name
    // of the symbol, so that the compiler can add names to functions.
  case form_define:
    {
      S_CHECK(args > 1, "define takes at least two arguments");
      S_CHECK(define_allowed || _local_env == null_cell, "use of define in incorrect position");
      MCell symbol, value;

      if (!Is_Pair(Car(back))){ // normal define
        symbol = Car(back);
        S_CHECK(args < 3, "too many arguments for define");
        value = Cadr(back);
      }
      else{ // lambda define
        value = Cdar(back);
        value = Cons(value, Cdr(back));
        value = Cons(lambda, value);
        symbol = Caar(back);
      }
      S_CHECK(Is_Identifier(symbol), "attempt to define non-symbol value");

      if (_local_env != null_cell){
        Cell current_binding = Resolve_Name(symbol, _local_env, _name_space, false, true);
        S_ASSERT(current_binding != symbol);
        S_CHECK(Is_Temp_Name(current_binding), "invalid syntax for define");
        result.Add_Element(current_binding);
      }
      else{
        result.Add_Element(Extract_Symbol(symbol));
      }
      result.Add_Element(Extract_Symbol(symbol));
      
      result.Add_Element(Expand(value));
      return result.List();
    } break;

    // set - finds out whether the symbol given is local or top-level,
    // and renames it properly if it is local
  case form_set:
    {
      S_CHECK(args == 2, "set! takes two arguments");
      MCell symbol = Car(back);
      S_CHECK(Is_Identifier(symbol), "first argument to set! must be a symbol");
      MCell value = Cadr(back);

      symbol = Resolve_Name(symbol, _local_env, _name_space, false);
      result.Add_Element(symbol);
      result.Add_Element(Expand(value));
      return result.List();
    } break;

    // lambda - used expand_body to handle internal defines. an extra
    // argument is added to indicate the number of defines. arguments
    // are renamed.
  case form_lambda:
    {
      S_CHECK(args >= 2, "lambda takes at least two arguments");
      
      // Change the argument symbols to temp names.
      List_Builder names, new_env;
      for (MCell arg_list = Car(back); arg_list != null_cell; arg_list = Cdr(arg_list)){
        bool stop = false;
        MCell cur_arg;
        if (Is_Pair(arg_list)){
          cur_arg = Car(arg_list);
        }
        else{
          cur_arg = arg_list;
          stop = true;
        }

        S_CHECK(Is_Identifier(cur_arg), "non-symbol value in argument list");
        Cell name = Make_Name();
        new_env.Add_Element(Cons(cur_arg, name));
        if (stop){
          names.Add_End(name);
          break;
        }
        else{
          names.Add_Element(name);
        }
      }

      // a new expander with the arguments in its environment is used
      // to expand the body.
      Cell new_local_env = Cons(new_env.List(), _local_env);
      Expander new_expander(_name_space, new_local_env, _cur_name);

      result.Add_Element(names.List());
      result.Add_End(new_expander.Expand_Body(Cdr(back), false));
      return result.List();
    } break;

    // begin - Expand_Body is used to make this either a begin or a
    // lambda based on whether there are internal defines.
  case form_begin:
    {
      // top level begin
      if (_local_env == null_cell){
        for (; back != null_cell; back = Cdr(back))
          result.Add_Element(Expand(Car(back)));
        return result.List();
      }
      // non-inline-define begin inside a lambda
      else{
        Expander new_expander(_name_space, Cons(null_cell, _local_env), _cur_name);
        return new_expander.Expand_Body(back, true);
      }
    } break;

    // quote - identifiers renamed by macro-expansion have to be
    // un-renamed.
  case form_quote:
    {
      S_CHECK(args == 1, "quote takes exactly one argument");
      result.Add_Element(Unrename_Expression(Car(back)));
      return result.List();
    } break;

    // quasiquote - gathers the unquoted parts into a vector,
    // unrenames the quoted expression.
  case form_quasiquote:
    {
      S_CHECK(args == 1, "quasiquote only takes one argument");
      MStack unquoted;
      Find_Unquoted_Parts(unquoted, Car(back), 0);

      for (size_t i = 0; i != unquoted.Size(); ++i)
        unquoted[i] = Expand(unquoted[i]);

      result.Add_Element(Make_Vector(unquoted));
      result.Add_Element(Unrename_Expression(Car(back)));
      return result.List();
    } break;

    // old-style macro
  case form_define_macro:
    {
      S_CHECK(args == 2, "define-macro takes exactly one argument");
      Cell symbol = Car(back);
      S_CHECK(Is_Identifier(symbol), "define-syntax requires a symbol as first argument");
      symbol = Extract_Symbol(symbol);
      result.Add_Element(symbol);
      
      result.Add_Element(Expand(Cadr(back)));
      return result.List();
    }
    
    // define-syntax - already turns the second argument into a macro
  case form_define_syntax:
    {
      S_CHECK(args == 2, "define-syntax takes exactly two arguments");
      Cell symbol = Car(back);
      S_CHECK(Is_Identifier(symbol), "define-syntax requires a symbol as first argument");
      symbol = Extract_Symbol(symbol);
      result.Add_Element(symbol);
      
      back = Cadr(back);
      S_CHECK(Is_Syntax_Rules(back), "define-syntax requires a syntax-rules expression as second argument");
      result.Add_Element(Syntax_Rules_To_Macro(Cdr(back), symbol, null_cell, _name_space));
      return result.List();
    } break;

    // let-syntax and letrec-syntax - compiles its body in a new expander with an
    // environment that contains the new macros
  case form_let_syntax:
  case form_letrec_syntax:
    {
      bool letrec = Special_Form_Name(syntax) == form_letrec_syntax;
      std::string form_name = letrec ? "letrec-syntax" : "let-syntax";
      S_CHECK(args > 1, form_name + " needs more than one argument");
      MCell defs = Car(back);
      S_CHECK(Is_Pair(defs), "invalid definition list for " + form_name);
      List_Builder new_macros;
      MCell env_used;
      if (letrec)
        env_used = Cons(null_cell, _local_env);
      else
        env_used = _local_env;
      
      for (; defs != null_cell; defs = Cdr(defs)){
        S_CHECK(Is_Pair(defs), "definition list in " + form_name + " not a valid list");
        S_CHECK(Is_Pair(Car(defs)) && Is_Identifier(Caar(defs)), "improper element in definition list for " + form_name);
        S_CHECK(Is_Pair(Cdar(defs)) && Is_Syntax_Rules(Cadar(defs)), "values for all definitions in " + form_name + " must be syntax-rules expressions");

        Cell symbol = Caar(defs);
        MCell macro = Syntax_Rules_To_Macro(Cdr(Cadar(defs)), Extract_Symbol(symbol), env_used, _name_space);
        if (letrec)
          Car(env_used) = Cons(Cons(symbol, macro), Car(env_used));
        else
          new_macros.Add_Element(Cons(symbol, macro));
      }

      if (!letrec)
        env_used = Cons(new_macros.List(), _local_env);
      Expander new_expander(_name_space, env_used, _cur_name);
      return new_expander.Expand_Body(Cdr(back), true);
    } break;

    // does nothing
  case form_current_env:
    S_CHECK(args == 0, "current-env takes no arguments");
    return result.List();
    break;
    
  default:
    throw Scheme_Error("unknown special form");
  }
}

// Expand an arbitraty expression
Cell Expander::Expand(Cell expression)
{
  // First macro-expand it
  expression = Macro_Expand(expression);
  if (Is_Pair(expression)){
    // decide whether it is a special form or a function call
    Cell head = Car(expression);
    if (Is_Identifier(head))
      head = Resolve_Name(head, _local_env, _name_space, true);
    
    if (Is_Special_Form(head)){
      return Expand_Form(head, Cdr(expression), false);
    }
    // Simply expand every element of a list an return a list of those
    // elements. Checks for improper lists.
    else{
      MCell expr = expression;
      List_Builder new_list;
      for (MCell lst = expression; lst != null_cell; lst = Cdr(lst)){
        S_CHECK(Is_Pair(lst), "can not evaluate improper list: " + Cell_To_String(expr));
        new_list.Add_Element(Expand(Car(lst)));
      }
      return new_list.List();
    }
  }
  // identifiers get resolved
  else if (Is_Identifier(expression)){
    Cell resolved = Resolve_Name(expression, _local_env, _name_space, true);
    S_CHECK(!Is_Syntax(resolved), "invalid syntax for " + Symbol_Name(Extract_Symbol(expression)));
    return resolved;
  }
  // nothing happens to other stuff
  else{
    return expression;
  }
}

// ,COMPILE

// Compiler. Passes over expanded expressions and turns them into code
// vectors.
class Compiler
{
public:
  Compiler(Cell name_space, Cell environment);
  void Compile(Cell expression, bool tail);
  Cell Code() const{return Make_Vector(_code);}
  
private:
  void Compile_Function_Call(Cell expression, bool tail);
  void Compile_Special_Form(Cell syntax, Cell arguments, bool tail);
  
  MStack _code;
  MCell _name_space, _environment;
  Cell _defining_symbol;
};

Compiler::Compiler(Cell name_space, Cell environment)
  : _name_space(name_space),
    _environment(environment),
    _defining_symbol(false_cell)
{
}

// macro to make adding instructions simple
#define INS(name) _code.Push(Make_Instruction(i_##name))

// compile any expression. note that this assumes it has already been
// expanded. will not do much good on non-expanded expressions.
void Compiler::Compile(Cell expression, bool tail)
{
  if (Is_Pair(expression)){
    if (Is_Special_Form(Car(expression)))
      Compile_Special_Form(Car(expression), Cdr(expression), tail);
    else
      Compile_Function_Call(expression, tail);
  }
  // because local symbols have become temp names, symbols always mean
  // top-level bindings
  else if (Is_Symbol(expression)){
    Cell slot = Get_Binding(_name_space, expression);
    // Symbols pointing to syntax can not be dereferenced
    S_CHECK(!Is_Syntax(Cdr(slot)), "invalid syntax for " + Symbol_Name(Car(slot)));
    INS(deref_ref);
    _code.Push(slot);
    if (tail)
      INS(return);
  }
  // a temp name means a local variable
  else if (Is_Temp_Name(expression)){
    Environment_Ref ref = Find_In_Environment(expression, _environment);
    S_ASSERT(ref.depth != not_found);
    INS(deref_env);
    _code.Push(Make_Env_Ref(ref.depth, ref.distance));
    if (tail)
      INS(return);
  }
  // other stuff is literal
  else{
    S_CHECK(expression != null_cell, "attempt to evaluate empty list");
    INS(literal);
    _code.Push(expression);
    if (tail)
      INS(return);
  }
}

// Compile a function call. First the arguments get compiled left to
// right and added to the arg list, and then the first element of the
// list is compiled and the function is called.
void Compiler::Compile_Function_Call(Cell expression, bool tail)
{
  MCell head = Car(expression), back = Cdr(expression);

  INS(setup_arg_list);
  _code.Push(Make_Fixnum(List_Length(back)));
  size_t arg_num = 0;
  while (back != null_cell){
    Compile(Car(back), false);
    back = Cdr(back);
    
    INS(add_arg);
    _code.Push(Make_Fixnum(arg_num));
    ++arg_num;
  }
  Compile(head, false);
  if (tail)
    INS(tail);
  else
    INS(call);
}

// compile special forms
void Compiler::Compile_Special_Form(Cell syntax, Cell arguments, bool tail)
{
  static const Cell lambda = Make_Special_Form(form_lambda);
  static const Cell fill_in_quasiquoted = Make_Symbol("impl:fill-in-quasiquoted");
  
  MCell back = arguments;

  switch(Special_Form_Name(syntax)){
  case form_if:
    // straightforward jump construction
    {
      Compile(Car(back), false);
      INS(jump_if_false);
      size_t first_jump = _code.Size();
      _code.Push(invalid_cell);
      Compile(Cadr(back), tail);
      size_t second_jump = 0;
      if (!tail){
        INS(jump);
        second_jump = _code.Size();
        _code.Push(invalid_cell);
      }
      _code[first_jump] = Make_Fixnum(_code.Size());
      Compile(Caddr(back), tail);
      if (!tail){
        _code[second_jump] = Make_Fixnum(_code.Size());
      }
    } break;
        
  case form_define:
    // depening on whether this is at top level either a top level var
    // or a var in the current local environment gets defined. if the
    // value is a lambda expression _defining_symbol gets set, which
    // causes the next compiled lambda to set the name of the new
    // closure.
    {
      MCell plain_symbol = Cadr(back), renamed_symbol = Car(back), value = Caddr(back);

      if (Is_Pair(value) && Car(value) == lambda)
        _defining_symbol = plain_symbol;

      Compile(value, false);

      if (_environment == null_cell){
        S_ASSERT(Is_Symbol(renamed_symbol));
        INS(define_ref);
        _code.Push(Get_Binding(_name_space, renamed_symbol));
      }
      else{
        S_ASSERT(Is_Temp_Name(renamed_symbol));
        INS(set_env);
        _code.Push(Make_Env_Ref(0, Environment_Offset(renamed_symbol, Car(_environment))));
      }
      if (tail)
        INS(return);
    } break;

  case form_set:
    // if symbol is a symbol this is a top level var, otherwise it is
    // some local var
    {
      Cell symbol = Car(back), value = Cadr(back);
      Compile(value, false);

      if (Is_Symbol(symbol)){
        INS(set_ref);
        _code.Push(Get_Binding(_name_space, symbol));
      }
      else{
        Environment_Ref ref = Find_In_Environment(symbol, _environment);
        S_ASSERT(ref.depth != not_found);
        INS(set_env);
        _code.Push(Make_Env_Ref(ref.depth, ref.distance));
      }
      if (tail)
        INS(return);
    } break;

  case form_lambda:
    // actually creates an unfinished closure that gets finished at
    // runtime.
    {
      size_t num_args = 0;
      bool var_args = false;
      Cell arg_list = Car(back);
      // count the arguments and decide whether there is a 'rest'
      // argument
      while (arg_list != null_cell){
        ++num_args;
        if (!Is_Pair(arg_list)){
          var_args = true;
          break;
        }
        arg_list = Cdr(arg_list);
      }

      // put the arguments into a vector, this will be part of the
      // environment in which the body gets compiled.
      MCell arg_vec = Make_Vector(num_args);
      arg_list = Car(back);
      for (size_t i = 0; i != num_args; ++i){
        if (Is_Pair(arg_list)){
          Vector_Ref(arg_vec, i) = Car(arg_list);
          arg_list = Cdr(arg_list);
        }
        else{
          S_ASSERT(i == num_args - 1);
          Vector_Ref(arg_vec, i) = arg_list;
        }
      }

      // the body gets compiled in a new compiler
      Compiler new_code(_name_space, Cons(arg_vec, _environment));

      MCell new_defines = Cadr(back), body = Cddr(back);

      // adjust the environment for the defines in the body
      if (!Vector_Size(new_defines) == 0){
        size_t old_size = Vector_Size(Car(new_code._environment)),
          added_size = Vector_Size(new_defines),
          new_size = old_size + added_size;
        new_code._code.Push(Make_Instruction(i_grow_env));
        new_code._code.Push(Make_Fixnum(new_size));

        Grow_Environment(new_code._environment, new_size);
        for (size_t i = 0; i != added_size; ++i)
          Vector_Ref(Car(new_code._environment), old_size + i) = Vector_Ref(new_defines, i);
      }

      // compile the body itself
      for (; body != null_cell; body = Cdr(body))
        new_code.Compile(Car(body), Cdr(body) == null_cell);

      // some code for finishing the closure
      INS(literal);
      MCell new_code_vector = new_code.Code();
      int arg_code = num_args;
      if (var_args) arg_code = -arg_code;
      _code.Push(Make_Unfinished_Closure(new_code_vector, Make_Fixnum(arg_code), _defining_symbol));
      _defining_symbol = false_cell;
      INS(finish_lambda);
      
      if (tail)
        INS(return);
    } break;

  case form_begin:
    // begins get inlined
    if (back == null_cell){
      INS(literal);
      _code.Push(void_cell);
    }
    else{
      for (; back != null_cell; back = Cdr(back))
        Compile(Car(back), tail && Cdr(back) == null_cell);
    }
    if (tail)
      INS(return);
    break;  
    
  case form_quote:
    INS(literal);
    _code.Push(Car(back));
    if (tail)
      INS(return);
    break;

  case form_quasiquote:
    // this becomes a call to impl:fill-in-quasiquoted, with the
    // expression as a first argument and the expressions resulting
    // from evaluating the unquoted parts as remaning arguments.
    {
      // the expander already gathered the unquoted parts
      MCell unquoted = Car(back);
      size_t size = Vector_Size(unquoted);

      // pass the quoted expression
      INS(setup_arg_list);
      _code.Push(Make_Fixnum(size + 1));
      INS(literal);
      _code.Push(Cadr(back));
      INS(add_arg);
      _code.Push(zero_cell);

      // and the unquoted parts
      for (size_t part = 0; part != size; ++part){
        Compile(Vector_Ref(unquoted, part), false);
        INS(add_arg);
        _code.Push(Make_Fixnum(part + 1));
      }

      // call impl:fill-in-quasiquoted
      INS(deref_ref);
      _code.Push(Get_Binding(_name_space, fill_in_quasiquoted));
      if (tail)
        INS(tail);
      else
        INS(call);
    } break;

  case form_define_macro:
    // uh-oh
    {
      Cell symbol = Car(back), value = Cadr(back);
      Compile(value, false);
      INS(make_macro);
      INS(define_ref);
      _code.Push(Get_Binding(_name_space, symbol));
      if (tail)
        INS(return);
    } break;
    
  case form_define_syntax:
    // most of the work was done by the expander. just bind arg 1 to
    // arg 2.
    {
      Cell symbol = Car(back);
      INS(literal);
      _code.Push(Cadr(back));

      INS(define_ref);
      _code.Push(Get_Binding(_name_space, symbol));
      if (tail)
        INS(return);
    } break;

  case form_current_env:
    // just put the current env in the code vector
    INS(literal);
    _code.Push(_name_space);
    if (tail)
      INS(return);
    break;
    
  default:
    throw Scheme_Error("unknown special form");
  }
}

#undef INS

// compile actually does both expanding and compiling. returns a code
// vector. the third argument is needed to support evaluating in
// different environments.
Cell Compile(Cell expression, const MCell& name_space)
{
  Expander expander(name_space, null_cell, 0);
  Cell expanded = expander.Expand(expression);
  Compiler compiler(name_space, null_cell);
  compiler.Compile(expanded, true);
  return compiler.Code();
}

// ,SIGINT CATCHER

// Sets a flag when ctrl-C is pressed. If ctrl-C is pressed twice
// without this thing polling in between the second signal is let
// through.
class SigINT_Catcher
{
public:
  SigINT_Catcher()
  {
    interrupted = false;
    Register();
  }
  ~SigINT_Catcher()
  {
    Unregister();
  }
  
  void Poll()
  {
    if (interrupted){
      interrupted = false;
      Register();
      throw Scheme_Error("user break");
    }
  }

private:
  static void Register()
  {
    old_handler = std::signal(SIGINT, Handler);
  }
  static void Unregister()
  {
    std::signal(SIGINT, old_handler);
  }
  
  static void (*old_handler)(int);
  static volatile bool interrupted;
  static void Handler(int signal);
};

void (*SigINT_Catcher::old_handler)(int) = NULL;
volatile bool SigINT_Catcher::interrupted = false;
void SigINT_Catcher::Handler(int signal)
{
  interrupted = true;
  Unregister();
}

// ,EVAL

// The 'virtual machine', fetches instructions from its current code
// vector and executes them.
Cell Run_Code(Cell code, bool handle_errors)
{
  SigINT_Catcher catcher;
  
  S_ASSERT(Is_Vector(code));
  Live_Continuation lc(code);
  MStack stack; // continuations are kept on this
  bool done = false;

  // the eval loop, fetches instructions and executes code based on them
  while(!done){
    try{
      catcher.Poll();
      
      S_ASSERT(lc.instruction_counter < Vector_Size(lc.code));
      Cell current = Vector_Ref(lc.code, lc.instruction_counter);
      S_ASSERT(Is_Instruction(current));
      ++lc.instruction_counter;
      Instruction current_instruction = Instruction_Value(current);

      switch(current_instruction){
      case i_return: // return from current continuation (reg is return value)
        if (stack.Empty())
          done = true;
        else
          Pop_Continuation(lc, stack);
        break;

      case i_literal: // put the next element in the code vector in reg
        lc.reg = Vector_Ref(lc.code, lc.instruction_counter);
        ++lc.instruction_counter;
        break;

      case i_jump: // jump to the instruction indicated by the next element in the code vector
        lc.instruction_counter = Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
        break;

      case i_jump_if_false: // if reg is false, jump to the instruction indicated by the next element in the code vector
        if (lc.reg == false_cell)
          lc.instruction_counter = Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
        else
          ++lc.instruction_counter;
        break;

      case i_tail: // perform a tail call with the closure or primitive in reg
      case i_call: // call the closure or primitive in reg
      // these had so much code in common I made them one case, it is
      // checked again inside the code whether a normal call or a tail
      // call is happening
        {
          S_ASSERT(lc.arguments != null_cell);
          Cell arguments = Car(lc.arguments);
          lc.arguments = Cdr(lc.arguments);
          int values = (arguments == null_cell ? 0 : Vector_Size(arguments));

          if (Is_Primitive(lc.reg)){ // primitive call
            int args = Primitive_Num_Args(lc.reg);
            Cell arg_vector = Adjust_Arguments(arguments, values, args, Primitive_Name(lc.reg));
            Cell* arg_array = NULL;
            if (args != 0)
              arg_array = &Vector_Ref(arg_vector, 0);
            if (args < 0)
              args = -args;
            S_ASSERT(args <= 8);
            lc.reg = call_primitive_table[args](Primitive_Function(lc.reg), arg_array);

            // return
            if (current_instruction == i_tail){
              if (stack.Empty())
                done = true;
              else
                Pop_Continuation(lc, stack);
            }
          }
          else if (Is_Closure(lc.reg)){ // closure call
            MCell arg_vec = Adjust_Arguments(arguments, values, Closure_Num_Args(lc.reg), Closure_Name(lc.reg));
            if (current_instruction == i_call)
              Push_Continuation(lc, stack);

            // change the live environment to the start of this closure
            lc.environment = Cons(arg_vec, Closure_Environment(lc.reg));
            lc.code = Closure_Code(lc.reg);
            lc.instruction_counter = 0;
            lc.function_name = Closure_Name(lc.reg);
          }
          else{ // invalid call
            throw Scheme_Error("attempt to call non-function object: " + Cell_To_String(lc.reg));
          }
        } break;

      case i_finish_lambda: // make a closure with the current environment and the pair (args . code) in reg
        S_ASSERT(Is_Vector(lc.reg));
        lc.reg = Make_Closure(lc.reg, lc.environment);
        break;

      case i_grow_env: // increase the size of the local environment to make room for newly defined symbols
        {
          int new_size = Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
          ++lc.instruction_counter;
          Grow_Environment(lc.environment, new_size);
        } break;
        
      case i_deref_env: // lookup the value indicated by the next element in the code vector in the environment and put in in reg
        {
          lc.reg = Vector_Ref(lc.code, lc.instruction_counter);
          ++lc.instruction_counter;
          S_ASSERT(Is_Pair(lc.reg));
          Cell target_env = lc.environment;
        
          for (size_t depth = Fixnum_Value(Car(lc.reg)); depth != 0; --depth){
            S_ASSERT(Cdr(target_env) != null_cell);
            target_env = Cdr(target_env);
          }
          target_env = Car(target_env);
          size_t distance = Fixnum_Value(Cdr(lc.reg));
          S_ASSERT(Is_Vector(target_env));
          S_ASSERT(distance < Vector_Size(target_env));

          lc.reg = Vector_Ref(target_env, distance);
          S_CHECK(lc.reg != invalid_cell, "accessing locally-defined symbol before its initialization");
        } break;
      
      case i_deref_ref: // look up the value indicated the next element on the code vector in the top environment and put it in reg
        lc.reg = Vector_Ref(lc.code, lc.instruction_counter);
        ++lc.instruction_counter;
        S_ASSERT(Is_Pair(lc.reg));
        S_CHECK(Cdr(lc.reg) != invalid_cell, "undefined symbol: " + Symbol_Name(Car(lc.reg)));
        lc.reg = Cdr(lc.reg);
        break;

      case i_set_env: // set environment var indicated by next element in code vector to the value of reg
        {
          Cell ref = Vector_Ref(lc.code, lc.instruction_counter);
          ++lc.instruction_counter;
          size_t depth = Fixnum_Value(Car(ref)), distance = Fixnum_Value(Cdr(ref));
          S_ASSERT(Is_Pair(ref));

          Cell target_env = lc.environment;
          for (; depth > 0; --depth){
            S_ASSERT(Cdr(target_env) != null_cell);
            target_env = Cdr(target_env);
          }

          Vector_Ref(Car(target_env), distance) = lc.reg;
          lc.reg = void_cell;
        } break;

      case i_define_ref: // define cell indicated by next element in code vector to value of reg
      case i_set_ref: // set cell indicated by next element in code vector to value of reg
        {
          Cell ref = Vector_Ref(lc.code, lc.instruction_counter);
          ++lc.instruction_counter;
          S_ASSERT(Is_Pair(ref));
          if (current_instruction == i_set_ref) { // for set!, check whether the symbol is bound
            S_CHECK(Cdr(ref) != invalid_cell, "attempt to set undefined symbol: " + Symbol_Name(Car(ref)));
          }
          Cdr(ref) = lc.reg;
          lc.reg = void_cell;
        } break;

      case i_setup_arg_list: // push a new arg list whose size is indicated in the next element of the code vector
        {
          size_t list_size = Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
          ++lc.instruction_counter;
          MCell new_vector = null_cell;
          if (list_size != 0)
            new_vector = Make_Vector(list_size, invalid_cell);
          lc.arguments = Cons(new_vector, lc.arguments);
        } break;
      
      case i_add_arg: // add the value in reg to the top arg list at the position indicated by the next element
        {
          size_t arg_num = Fixnum_Value(Vector_Ref(lc.code, lc.instruction_counter));
          ++lc.instruction_counter;
          S_ASSERT(lc.arguments != null_cell);
          S_ASSERT(Car(lc.arguments) != null_cell);
          Vector_Ref(Car(lc.arguments), arg_num) = lc.reg;
        } break;

      case i_as_arguments: // set the top arg list to the elements of the list in reg (used by apply)
        if (lc.reg != null_cell)
          lc.reg = Make_Vector_From_List(lc.reg);
        lc.arguments = Cons(lc.reg, lc.arguments);
        break;

      case i_make_macro: // turn the closure in reg into a macro
        lc.reg = Make_Simple_Macro(lc.reg);
        break;
        
      case i_current_continuation: // load the current continuation to reg
        {
          MCell current = null_cell;
          for (size_t i = 0; i != stack.Size(); i += continuation_size)
            current = Make_Continuation(stack, i, current);
          lc.reg = current;
        } break;

      case i_set_continuation: // restore the continuation found in reg
        {
          S_ASSERT(lc.reg == null_cell || Is_Continuation(lc.reg));
          stack.Clear();
          std::vector<Cell> continuations;
          while (lc.reg != null_cell){
            continuations.push_back(lc.reg);
            lc.reg = Continuation_Parent(lc.reg);
          }
          for (size_t i = continuations.size(); i != 0; --i){
            Restore_Continuation(continuations[i - 1], stack);
          }
        } break;
        
      default:
        throw Scheme_Error("invalid instruction");
        break;
      }
    }
    catch (Scheme_Error& e){
      const static Cell handle_error = Make_Symbol("impl:handle-error");
      const static Cell quote_cell = Make_Symbol("quote");
      
      // when an error occurs impl:handle-error is called with some
      // context information if handl_errors is true and
      // impl:handle-error is defined
      if (handle_errors && Get_Value(ip_->report_env, handle_error) != invalid_cell){
        MCell instructions = lc.code;
        size_t current_instruction = (lc.instruction_counter == 0) ? 0 : (lc.instruction_counter - 1);

        // Make a list of function names for simple stack tracing.
        List_Builder trace;
        trace.Add_Element(lc.function_name);
        for (size_t pos = stack.Size(); pos != 0; pos -= 5)
          trace.Add_Element(stack[pos - 2]);

        // Reset the context
        stack.Clear();
        lc.arguments = null_cell;
        lc.environment = null_cell;
        lc.function_name = void_cell;
        lc.instruction_counter = 0;

        // Run (impl:handle-error <error message> <stack trace> <current instruction> <instruction vector>)
        MCell command = Cons_Null(instructions);
        command = Cons(Make_Fixnum(current_instruction), command);
        command = Cons(Cons(quote_cell, Cons_Null(trace.List())), command);
        command = Cons(Make_String(e.what()), command);
        command = Cons(handle_error, command);
        lc.code = Compile(command, ip_->report_env);
      }
      // otherwise the exception is not handled here
      else{
        throw;
      }
    }
  }

  return lc.reg;
}

// ,REPL

// run a repl using in.input and in.output as in and output.
void Run_REPL(bool welcome_message)
{
  if (welcome_message)
    Outport_Stream(ip_->output) << "Welcome to Unlikely Scheme v" << VERSION << "\n";

  Eval_String("(run-repl (impl:current-env))", ip_->work_env, true);
}

// load a file
void Load_File(const std::string& filename)
{
  Eval_String("(load \"" + filename + "\")", true);
}

// evaluate a string. only the first expression in the string is
// evaluated ("1 2 3" => 1)
Cell Eval_String(const std::string& str, const MCell& name_space, bool handle_errors)
{
  String_Input_Splitter splitter;
  splitter.Add_Line(str);
  S_CHECK(splitter.Full_Expression(), "attempt to eval unfinished expression");
  Cell expr = Read(splitter);
  return Eval_Expression(expr, name_space, handle_errors);
}

// evaluate a cell
Cell Eval_Expression(Cell expression, Cell name_space, bool handle_errors)
{
  Cell code = Compile(expression, name_space);
  return Run_Code(code, handle_errors);
}

// non-iostream repl
bool String_REPL::Add_Line(const std::string& str)
{
  bool retval = false;
  _input.Add_Line(str);
  try{
    while(_input.Full_Expression()){
      retval = true;
      Cell next = Read(_input);
      next = Run_Code(Compile(next, ip_->work_env), false);
      if (next != void_cell){
        std::ostream& output = Outport_Stream(ip_->output);
        Write(next, output);
        output << '\n';
      }
    }
  }
  catch (const Scheme_Error& e){
    _input.Reset();
    throw;
  }
  return retval;
}

// capture the output to in.out
Output_Catcher::Output_Catcher()
  : _old_stream(ip_->output)
{
  ip_->output = Make_Outport(_stream);
}

Output_Catcher::~Output_Catcher()
{
  ip_->output = _old_stream;
}

std::string Output_Catcher::Get_New_Output()
{
  std::string new_output = _stream.str();
  _stream.str("");
  return new_output;
}

// ,PRIMITIVES

// Some stuff to make error checking easier. These are a little odd
// (especially the type system of CHECK_TYPE), but the idea is to have
// as little clutter as possible in the primitives and to minimize the
// amount of strings that have to be kept in the executable.

std::string Make_Arg_Type_Error(const std::string& function, const std::string& num, const std::string& type)
{
  static const char* arg = "argument ", * to = " to ", * must = " must be of type ";
  return arg + num + to + function + must + type;
}
#define FUN_NAME(name) static const char* function_name = name
#define CHECK_TYPE(var, type, num) S_CHECK(Is_##type(var), Make_Arg_Type_Error(function_name, #num, #type))
#define CHECK_N_TYPE(var, type, function, num) S_CHECK(Is_##type(var), Make_Arg_Type_Error(function, #num, #type))
inline bool Single_Optional_Argument(Cell optional, const char* function)
{
  if (optional == null_cell)
    return false;
  S_CHECK(Cdr(optional) == null_cell, "too many arguments to " + std::string(function));
  return true;
}
#define OPTIONAL(var) Single_Optional_Argument(var, function_name)

// The primitives are mostly a lot of really simple functions
// implemented in terms of other functions in really straightforward
// ways.

// general

Cell p_eqp(Cell one, Cell two){
  return Make_Bool(one == two);
}
Cell p_eqvp(Cell one, Cell two){
  if (Is_Bignum(one) && Is_Bignum(two))
    return Make_Bool(Bignum_Equal(one, two));
  else if (Is_Real(one) && Is_Real(two))
    return Make_Bool(Real_Value(one) == Real_Value(two));
  else if (Is_Rational(one) && Is_Rational(two))
    return Make_Bool(Integer_Equal(Rational_Numerator(one), Rational_Numerator(two)) and
                     Integer_Equal(Rational_Denominator(one), Rational_Denominator(two)));
  else
    return Make_Bool(one == two);
}
Cell p_equalp(Cell one, Cell two){
  return Make_Bool(Equal(one, two));
}

// pairs and lists

Cell p_car(Cell pair)
{
  CHECK_N_TYPE(pair, Pair, "car", 1);
  return Car(pair);
}
Cell p_cdr(Cell pair)
{
  CHECK_N_TYPE(pair, Pair, "cdr", 1);
  return Cdr(pair);
}
Cell p_cons(Cell car, Cell cdr)
{
  return Cons(car, cdr);
}

Cell p_nullp(Cell cell)
{
  return Make_Bool(cell == null_cell);
}
Cell p_pairp(Cell cell)
{
  return Make_Bool(Is_Pair(cell));
}
Cell p_listp(Cell cell)
{
  Cell runner_one = cell, runner_two = cell;
  while (Is_Pair(runner_two)){
    runner_one = Cdr(runner_one);
    runner_two = Cdr(runner_two);
    if (!Is_Pair(runner_two))
      break;
    runner_two = Cdr(runner_two);
    if (runner_one == runner_two)
      return false_cell;
  }
  return Make_Bool(runner_two == null_cell);
}

Cell p_set_car(Cell pair, Cell value)
{
  CHECK_N_TYPE(pair, Pair, "set-car!", 1);
  Car(pair) = value;
  return void_cell;
}
Cell p_set_cdr(Cell pair, Cell value)
{
  CHECK_N_TYPE(pair, Pair, "set-cdr!", 1);
  Cdr(pair) = value;
  return void_cell;
}

// numbers

Cell p_numberp(Cell cell)
{
  return Make_Bool(Is_Number(cell));
}
// To make things simple, the concept integer in the C++ code means a
// Bignum or Fixnum, while in scheme it can also be a Real that
// contains an integer value.
Cell p_integerp(Cell cell)
{
  return Make_Bool(Is_Integer(cell) || (Is_Real(cell) && Double_Is_Int(Real_Value(cell))));
}
Cell p_rationalp(Cell cell)
{
  return Make_Bool(Is_Integer(cell) || Is_Rational(cell));
}
Cell p_realp(Cell cell)
{
  return Make_Bool(Is_Number(cell));
}

Cell p_exactp(Cell cell)
{
  CHECK_N_TYPE(cell, Number, "exact?", 1);
  return Make_Bool(!Is_Real(cell));
}
Cell p_inexactp(Cell cell)
{
  CHECK_N_TYPE(cell, Number, "inexact?", 1);
  return Make_Bool(Is_Real(cell));
}

Cell p_equals(Cell one, Cell two){
  FUN_NAME("=");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Make_Bool(Number_To_Double(one) == Number_To_Double(two));
}
Cell p_less(Cell one, Cell two){
  FUN_NAME("<");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Make_Bool(Number_To_Double(one) < Number_To_Double(two));
}
Cell p_less_equal(Cell one, Cell two){
  FUN_NAME("<=");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Make_Bool(Number_To_Double(one) <= Number_To_Double(two));
}
Cell p_greater(Cell one, Cell two){
  FUN_NAME(">");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Make_Bool(Number_To_Double(one) > Number_To_Double(two));
}
Cell p_greater_equal(Cell one, Cell two){
  FUN_NAME(">=");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Make_Bool(Number_To_Double(one) >= Number_To_Double(two));
}

Cell p_plus(Cell one, Cell two)
{
  FUN_NAME("+");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Number_Add(one, two);
}
Cell p_minus(Cell one, Cell two)
{
  FUN_NAME("-");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Number_Subtract(one, two);
}
Cell p_times(Cell one, Cell two)
{
  FUN_NAME("*");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Number_Multiply(one, two);
}
Cell p_divide(Cell one, Cell two)
{
  FUN_NAME("/");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  return Number_Divide(one, two);
}

Cell p_number_to_string(Cell number, Cell custom_radix)
{
  FUN_NAME("number->string");
  CHECK_TYPE(number, Number, 1);
  int radix = 10;
  if (OPTIONAL(custom_radix)){
    CHECK_TYPE(Car(custom_radix), Fixnum, 2);
    radix = Fixnum_Value(Car(custom_radix));
    S_CHECK(radix > 1 && radix < 21, "radix for number->string can only range from 2 to 20");
  }
  return Make_String(Number_To_String(number, radix));
}
Cell p_string_to_number(Cell str, Cell custom_radix)
{
  FUN_NAME("string->number");
  CHECK_TYPE(str, String, 1);
  int radix = 10;
  if (OPTIONAL(custom_radix)){
    CHECK_TYPE(Car(custom_radix), Fixnum, 2);
    radix = Fixnum_Value(Car(custom_radix));
    S_CHECK(radix > 1 && radix < 21, "radix for string->number can only range from 2 to 20");
  }
  try{
    return String_To_Number(String_Value(str), radix);
  }
  catch(const Scheme_Error& e){
    return false_cell;
  }
}

Cell p_quotient(Cell one, Cell two)
{
  FUN_NAME("quotient");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  if (Is_Integer(one) && Is_Integer(two)){
    return Integer_Quotient(one, two);
  }
  else{
    double val_one = Number_To_Double(one),
      val_two = Number_To_Double(two);
    S_CHECK(Double_Is_Int(val_one) && Double_Is_Int(val_two), "arguments to quotient must be integers");
    return Make_Real(Truncate_Double(val_one / val_two));
  }
}
Cell p_remainder(Cell one, Cell two)
{
  FUN_NAME("remainder");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  if (Is_Integer(one) && Is_Integer(two)){
    return Integer_Remainder(one, two);
  }
  else{
    double val_one = Number_To_Double(one),
      val_two = Number_To_Double(two);
    S_CHECK(Double_Is_Int(val_one) && Double_Is_Int(val_two), "arguments to remainder must be integers");
    return Make_Real(val_one - val_two * Truncate_Double(val_one / val_two));
  }
}
Cell p_modulo(Cell one, Cell two)
{
  FUN_NAME("modulo");
  CHECK_TYPE(one, Number, 1);
  CHECK_TYPE(two, Number, 2);
  if (Is_Integer(one) && Is_Integer(two)){
    return Integer_Modulo(one, two);
  }
  else{
    double val_one = Number_To_Double(one),
      val_two = Number_To_Double(two);
    S_CHECK(Double_Is_Int(val_one) && Double_Is_Int(val_two), "arguments to modulo must be integers");
    double result = val_one - val_two * Truncate_Double(val_one / val_two);
    if (result != 0 && (val_one < 0) != (val_two < 0))
      result += val_two;
    return Make_Real(result);
  }
}

Cell p_numerator(Cell num)
{
  if (Is_Integer(num))
    return num;
  else if (Is_Rational(num))
    return Rational_Numerator(num);
  else
    throw Scheme_Error("argument to numerator must be a rational number");
}
Cell p_denominator(Cell num)
{
  if (Is_Integer(num))
    return one_cell;
  else if (Is_Rational(num))
    return Rational_Denominator(num);
  else
    throw Scheme_Error("argument to denominator must be a rational number");
}

Cell p_ceiling(Cell num)
{
  CHECK_N_TYPE(num, Number, "ceiling", 1);
  if (Is_Integer(num))
    return num;
  else{
    double value = std::ceil(Number_To_Double(num));
    if (Is_Rational(num))
      return Double_To_Integer(value);
    else
      return Make_Real(value);
  }
}
Cell p_floor(Cell num)
{
  CHECK_N_TYPE(num, Number, "floor", 1);
  if (Is_Integer(num))
    return num;
  else{
    double value = std::floor(Number_To_Double(num));
    if (Is_Rational(num))
      return Double_To_Integer(value);
    else
      return Make_Real(value);
  }
}
Cell p_truncate(Cell num)
{
  CHECK_N_TYPE(num, Number, "truncate", 1);
  if (Is_Integer(num))
    return num;
  else{
    double value = Truncate_Double(Number_To_Double(num));
    if (Is_Rational(num))
      return Double_To_Integer(value);
    else
      return Make_Real(value);
  }
}
Cell p_round(Cell num)
{
  CHECK_N_TYPE(num, Number, "round", 1);
  if (Is_Integer(num))
    return num;
  else{
    double value = Number_To_Double(num);
    bool negative = value < 0;
    if (negative)
      value = -value;
    double floor = std::floor(value), fraction = value - floor;
    if (fraction < .5)
      value = floor;
    else if (fraction > .5)
      value = floor + 1;
    else if (Double_Is_Int(floor * .5))
      value = floor;
    else
      value = floor + 1;
    if (negative)
      value = -value;
    
    if (Is_Rational(num))
      return Double_To_Integer( value);
    else
      return Make_Real(value);
  }
}

Cell p_exp(Cell num)
{
  CHECK_N_TYPE(num, Number, "exp", 1);
  return Make_Real(std::exp(Number_To_Double(num)));
}
Cell p_log(Cell num)
{
  CHECK_N_TYPE(num, Number, "log", 1);
  double val = Number_To_Double(num);
  S_CHECK(val > 0, "invalid value for log");
  return Make_Real(std::log(val));
}
Cell p_sqrt(Cell num)
{
  CHECK_N_TYPE(num, Number, "sqrt", 1);
  double val = Number_To_Double(num);
  S_CHECK(val >= 0, "invalid value for sqrt");
  val = std::sqrt(val);
  if (Double_Is_Int(val))
    return Double_To_Integer(val);
  else
    return Make_Real(val);
}
Cell p_expt(Cell num, Cell exp)
{
  FUN_NAME("expt");
  CHECK_TYPE(num, Number, 1);
  CHECK_TYPE(exp, Number, 2);
  if (Is_Integer(num) && Is_Fixnum(exp)){
    int expon = Fixnum_Value(exp);
    bool negative = expon < 0;
    if (negative)
      expon = -expon;
    MCell accum = one_cell, base = num;
    for (size_t i = expon; i != 0; --i)
      accum = Number_Multiply(accum, base);
    if (negative)
      accum = Make_Simplified_Rational(one_cell, accum);
    return accum;
  }
  else{
    double base = Number_To_Double(num), expon = Number_To_Double(exp);
    S_CHECK(base != 0 || expon >= 0, "raising 0 to a negative power");
    return Make_Real(std::pow(base, expon));
  }
}

Cell p_sin(Cell num)
{
  CHECK_N_TYPE(num, Number, "sin", 1);
  return Make_Real(std::sin(Number_To_Double(num)));
}
Cell p_cos(Cell num)
{
  CHECK_N_TYPE(num, Number, "cos", 1);
  return Make_Real(std::cos(Number_To_Double(num)));
}
Cell p_tan(Cell num)
{
  CHECK_N_TYPE(num, Number, "tan", 1);
  return Make_Real(std::tan(Number_To_Double(num)));
}
Cell p_asin(Cell num)
{
  CHECK_N_TYPE(num, Number, "asin", 1);
  double val = Number_To_Double(num);
  S_CHECK(val <= 1.0 && val >= -1.0, "invalid value for asin");
  return Make_Real(std::asin(val));
}
Cell p_acos(Cell num)
{
  CHECK_N_TYPE(num, Number, "acos", 1);
  double val = Number_To_Double(num);
  S_CHECK(val <= 1.0 && val >= -1.0, "invalid value for acos");
  return Make_Real(std::acos(val));
}
Cell p_atan(Cell num, Cell opt)
{
  const double pi = 3.14159265358979323846;
  FUN_NAME("atan");
  CHECK_TYPE(num, Number, 1);
  if (OPTIONAL(opt)){
    CHECK_TYPE(Car(opt), Number, 2);
    double y = Number_To_Double(num),
      x = Number_To_Double(Car(opt));
    double retval;
    if (x == 0){
      if (y > 0)
	retval = pi * .5;
      else if (y < 0)
	retval = pi * -.5;
      else
        throw Scheme_Error("atan is undefined when both arguments are 0");
    }
    else{
      retval = std::atan(y / x);
      if (x <= 0)
        retval += pi;
    }
    return Make_Real(retval);
  }
  else{
    return Make_Real(std::atan(Number_To_Double(num)));
  }
}

Cell p_exact_to_inexact(Cell num)
{
  CHECK_N_TYPE(num, Number, "exact->inexact", 1);
  return Make_Real(Number_To_Double(num));
}
Cell p_inexact_to_exact(Cell num)
{
  const size_t mult_factor = 232792560; // divisible by everything under 22
  CHECK_N_TYPE(num, Number, "inexact->exact", 1);
  if (Is_Real(num)){
    Cell numerator = Double_To_Integer(Real_Value(num) * mult_factor);
    return Make_Simplified_Rational(numerator, Make_Fixnum(mult_factor));
  }
  else{
    return num;
  }
}

// symbols

Cell p_symbolp(Cell cell)
{
  return Make_Bool(Is_Symbol(cell));
}
Cell p_symbol_to_string(Cell cell)
{
  CHECK_N_TYPE(cell, Symbol, "symbol->string", 1);
  return Make_String(Symbol_Name(cell));
}
Cell p_string_to_symbol(Cell cell)
{
  CHECK_N_TYPE(cell, String, "string->symbol", 1);
  return Make_Symbol(String_Value(cell));
}
Cell p_symbol_to_integer(Cell cell)
{
  CHECK_N_TYPE(cell, Symbol, "symbol->integer", 1);
  return Make_Fixnum(Symbol_Value(cell));
}
Cell p_integer_to_symbol(Cell cell)
{
  CHECK_N_TYPE(cell, Fixnum, "integer->symbol", 1);
  return Make_Symbol(Fixnum_Value(cell));
}

// characters

Cell p_charp(Cell cell)
{
  return Make_Bool(Is_Character(cell));
}

Cell p_char_equal(Cell one, Cell two){
  FUN_NAME("char=?");
  CHECK_TYPE(one, Character, 1);
  CHECK_TYPE(two, Character, 2);
  return Make_Bool(Character_Value(one) == Character_Value(two));
}
Cell p_char_less(Cell one, Cell two){
  FUN_NAME("char<?");
  CHECK_TYPE(one, Character, 1);
  CHECK_TYPE(two, Character, 2);
  return Make_Bool(Character_Value(one) < Character_Value(two));
}
Cell p_char_less_equal(Cell one, Cell two){
  FUN_NAME("char<=?");
  CHECK_TYPE(one, Character, 1);
  CHECK_TYPE(two, Character, 2);
  return Make_Bool(Character_Value(one) <= Character_Value(two));
}
Cell p_char_greater(Cell one, Cell two){
  FUN_NAME("char>?");
  CHECK_TYPE(one, Character, 1);
  CHECK_TYPE(two, Character, 2);
  return Make_Bool(Character_Value(one) > Character_Value(two));
}
Cell p_char_greater_equal(Cell one, Cell two){
  FUN_NAME("char>=?");
  CHECK_TYPE(one, Character, 1);
  CHECK_TYPE(two, Character, 2);
  return Make_Bool(Character_Value(one) >= Character_Value(two));
}

Cell p_char_to_integer(Cell cell)
{
  CHECK_N_TYPE(cell, Character, "char->integer", 1);
  return Make_Fixnum(Character_Value(cell));
}
Cell p_integer_to_char(Cell cell)
{
  CHECK_N_TYPE(cell, Fixnum, "integer->char", 1);
  return Make_Character(Fixnum_Value(cell));
}

Cell p_char_upcase(Cell cell)
{
  CHECK_N_TYPE(cell, Character, "char-upcase", 1);
  return Make_Character(std::toupper(Character_Value(cell)));
}
Cell p_char_downcase(Cell cell)
{
  CHECK_N_TYPE(cell, Character, "char-downcase", 1);
  return Make_Character(std::tolower(Character_Value(cell)));
}

Cell p_char_alphabetic(Cell cell){
  CHECK_N_TYPE(cell, Character, "char-alphabetic", 1);
  return Make_Bool(std::isalpha(Character_Value(cell)));
}
Cell p_char_numeric(Cell cell){
  CHECK_N_TYPE(cell, Character, "char-numeric", 1);
  return Make_Bool(std::isdigit(Character_Value(cell)));
}
Cell p_char_whitespace(Cell cell){
  CHECK_N_TYPE(cell, Character, "char-whitespace", 1);
  return Make_Bool(Is_Whitespace(Character_Value(cell)));
}

// strings

Cell p_stringp(Cell cell)
{
  return Make_Bool(Is_String(cell));
}

Cell p_make_string(Cell length, Cell fill)
{
  FUN_NAME("make-string");
  CHECK_TYPE(length, Fixnum, 1);
  S_CHECK(Fixnum_Value(length) >= 0, "first argument to make-string must be non-negative");
  Cell fill_char = Make_Character(' ');
  if (OPTIONAL(fill)){
    CHECK_TYPE(Car(fill), Character, 2);
    fill_char = Car(fill);
  }
  return Make_String(Fixnum_Value(length), Character_Value(fill_char));
}
Cell p_string_length(Cell str)
{
  CHECK_N_TYPE(str, String, "string-length", 1);
  return Make_Fixnum(String_Size(str));
}
Cell p_string_ref(Cell str, Cell index)
{
  FUN_NAME("string-ref");
  CHECK_TYPE(str, String, 1);
  CHECK_TYPE(index, Fixnum, 2);
  int i = Fixnum_Value(index);
  S_CHECK(i >= 0 && static_cast<size_t>(i) < String_Size(str), "index out of range in string-ref");
  return Make_Character(String_Ref(str, i));
}
Cell p_string_set(Cell str, Cell index, Cell value)
{
  FUN_NAME("string-set");
  CHECK_TYPE(str, String, 1);
  CHECK_TYPE(index, Fixnum, 2);
  CHECK_TYPE(value, Character, 3);
  int i = Fixnum_Value(index);
  S_CHECK(i >= 0 && static_cast<size_t>(i) < String_Size(str), "index out of range in string-set!");
  String_Ref(str, i) = Character_Value(value);
  return void_cell;
}

// vector

Cell p_vectorp(Cell cell)
{
  return Make_Bool(Is_Vector(cell));
}

Cell p_make_vector(Cell length, Cell fill)
{
  FUN_NAME("make-vector");
  CHECK_TYPE(length, Fixnum, 1);
  S_CHECK(Fixnum_Value(length) >= 0, "first argument to make-vector must be non-negative");
  Cell fill_value = false_cell;
  if (OPTIONAL(fill))
    fill_value = Car(fill);
  return Make_Vector(Fixnum_Value(length), fill_value);
}
Cell p_vector_length(Cell vector)
{
  CHECK_N_TYPE(vector, Vector, "vector-length", 1);
  return Make_Fixnum(Vector_Size(vector));
}
Cell p_vector_ref(Cell vector, Cell index)
{
  FUN_NAME("vector-ref");
  CHECK_TYPE(vector, Vector, 1);
  CHECK_TYPE(index, Fixnum, 2);
  int i = Fixnum_Value(index);
  S_CHECK(i >= 0 && static_cast<size_t>(i) < Vector_Size(vector), "index out of range in vector-ref");
  return Vector_Ref(vector, i);
}
Cell p_vector_set(Cell vector, Cell index, Cell value)
{
  FUN_NAME("vector-set!");
  CHECK_TYPE(vector, Vector, 1);
  CHECK_TYPE(index, Fixnum, 2);
  int i = Fixnum_Value(index);
  S_CHECK(i >= 0 && static_cast<size_t>(i) < Vector_Size(vector), "index out of range in vector-set!");
  Vector_Ref(vector, i) = value;
  return void_cell;
}

// i/o

Cell p_input_portp(Cell cell)
{
  return Make_Bool(Is_Inport(cell));
}
Cell p_output_portp(Cell cell)
{
  return Make_Bool(Is_Outport(cell));
}

// Common pattern of functions taking an optional output port arg
#define GET_OUTPORT(n) \
  Cell output = ip_->output;\
  if (OPTIONAL(port)){\
    CHECK_TYPE(Car(port), Outport, n);\
    output = Car(port);\
  }

Cell p_write(Cell value, Cell port)
{
  FUN_NAME("write");
  GET_OUTPORT(2);
  Write(value, Outport_Stream(output));
  return void_cell;
}
Cell p_write_char(Cell cell, Cell port)
{
  FUN_NAME("write-char");
  CHECK_TYPE(cell, Character, 1);
  GET_OUTPORT(2);
  Write(cell, Outport_Stream(output), true);
  return void_cell;
}
Cell p_display(Cell value, Cell port)
{
  FUN_NAME("display");
  GET_OUTPORT(2);
  Write(value, Outport_Stream(output), true);
  return void_cell;
}
Cell p_newline(Cell port)
{
  FUN_NAME("newline");
  GET_OUTPORT(1);
  Outport_Stream(output) << std::endl;
  return void_cell;
}
Cell p_flush_output(Cell port)
{
  FUN_NAME("flush-output");
  GET_OUTPORT(1);
  Outport_Stream(output).flush();
  return void_cell;
}
#undef GET_OUTPORT

#define GET_INPORT() \
  Cell input = ip_->input;\
  if (OPTIONAL(port)){\
    CHECK_TYPE(Car(port), Inport, 1);\
    input = Car(port);\
  }

Cell p_read(Cell port)
{
  FUN_NAME("read");
  GET_INPORT();
  return Read(input);
}
Cell p_read_char(Cell port)
{
  FUN_NAME("read-char");
  GET_INPORT();
  return Inport_Read_Char(input);
}
Cell p_peek_char(Cell port)
{
  FUN_NAME("peek-char");
  GET_INPORT();
  return Inport_Peek_Char(input);
}
Cell p_eof_objectp(Cell cell)
{
  return Make_Bool(cell == eof_cell);
}
Cell p_char_readyp(Cell port)
{
  FUN_NAME("char-ready?");
  GET_INPORT();
  return Make_Bool(Inport_Ready(input));
}

#undef GET_INPORT

Cell p_open_input_file(Cell filename)
{
  CHECK_N_TYPE(filename, String, "open-input-file", 1);
  return Make_Inport(filename);
}
Cell p_reopen_input_file(Cell port)
{
  CHECK_N_TYPE(port, Inport, "reopen-input-file", 1);
  Reopen_Inport(port);
  return void_cell;
}
Cell p_open_output_file(Cell filename)
{
  CHECK_N_TYPE(filename, String, "open-output-file", 1);
  return Make_Outport(filename);
}
Cell p_reopen_output_file(Cell port)
{
  CHECK_N_TYPE(port, Outport, "reopen-output-file", 1);
  Reopen_Outport(port);
  return void_cell;
}
Cell p_close_input_port(Cell port)
{
  CHECK_N_TYPE(port, Inport, "close-input-port", 1);
  Close_Inport(port);
  return void_cell;
}
Cell p_close_output_port(Cell port)
{
  CHECK_N_TYPE(port, Outport, "close-output-port", 1);
  Close_Outport(port);
  return void_cell;
}
Cell p_current_input_port()
{
  return ip_->input;
}
Cell p_current_output_port()
{
  return ip_->output;
}

Cell p_file_existsp(Cell filename)
{
  CHECK_N_TYPE(filename, String, "file-exists?", 1);
  std::ifstream test(String_Value(filename).c_str());
  return Make_Bool(!test.fail());
}
Cell p_input_port_line(Cell port)
{
  CHECK_N_TYPE(port, Inport, "input-port-line", 1);
  return Make_Integer(Inport_Line(port));
}

// procedure

Cell p_procedurep(Cell cell){
  return Make_Bool(Is_Closure(cell) || Is_Primitive(cell));
}

// non-standard functions

// used to make closures using instruction vectors
Cell p_make_closure(Cell code, Cell num_args){
  CHECK_N_TYPE(code, Vector, "make-closure", 1);
  CHECK_N_TYPE(num_args, Fixnum, "make-closure", 2);
  return Make_Closure(code, null_cell, num_args, false_cell);
}
// raise an error
Cell p_raise(Cell message){
  CHECK_N_TYPE(message, String, "raise", 1);
  throw Scheme_Error(String_Value(message));
}
Cell p_collect_garbage(){
  mp_->Collect_Garbage();
  return void_cell;
}
Cell p_definedp(Cell symbol, Cell name_space)
{
  FUN_NAME("defined?");
  CHECK_TYPE(symbol, Symbol, 1);
  CHECK_TYPE(name_space, Namespace, 2);
  return Make_Bool(Get_Value(name_space, symbol) != invalid_cell);
}
Cell p_environmentp(Cell cell)
{
  return Make_Bool(Is_Namespace(cell));
}

Cell p_random(Cell number){
  FUN_NAME("random");
  int max = 100;
  if (OPTIONAL(number)){
    CHECK_TYPE(Car(number), Fixnum, 1);
    max = Fixnum_Value(Car(number));
    S_CHECK(max > 0, "argument 1 to random must be positive");
  }
  return Make_Fixnum(std::abs(rand() % max));
}
Cell p_seed_random(Cell number){
  CHECK_N_TYPE(number, Fixnum, "seed-random", 1);
  srand(Fixnum_Value(number));
  return void_cell;
}
Cell p_current_time(){
  return Make_Integer(time(NULL));
}

// convert any object to string
Cell p_object_to_string(Cell cell, Cell opt_display)
{
  FUN_NAME("object->string");
  bool display = false;
  if (OPTIONAL(opt_display))
    display = Car(opt_display) != false_cell;
  std::string str = Cell_To_String(cell, display);
  return Make_String(str);
}

// environments

Cell p_scheme_report_environment(Cell version){
  CHECK_N_TYPE(version, Fixnum, "scheme-report-environment", 1);
  S_CHECK(Fixnum_Value(version) == 5, "argument 1 to scheme-report-environment must be 5");
  MCell retval = Make_Namespace(ip_->report_env, default_workspace_size);
  Bootstrap_Namespace(retval);
  return retval;
}
Cell p_null_environment(Cell version){
  CHECK_N_TYPE(version, Fixnum, "null-environment", 1);
  S_CHECK(Fixnum_Value(version) == 5, "argument 1 to null-environment must be 5");
  MCell retval = Make_Namespace(ip_->null_env, default_workspace_size);
  Bootstrap_Namespace(retval);
  return retval;
}

// used by eval, creates a closure of 0 arguments containing the code
// from expression and optionally a name
Cell p_compile(Cell expression, Cell name_space, Cell opt_name)
{
  FUN_NAME("compile");
  CHECK_TYPE(name_space, Namespace, 2);
  Cell name = false_cell;
  if (OPTIONAL(opt_name)){
    CHECK_TYPE(Car(opt_name), Symbol, 3);
    name = Car(opt_name);
  }
  return Make_Closure(Compile(expression, name_space), null_cell, zero_cell, name);
}

#undef CHECK_N_TYPE
#undef CHECK_TYPE
#undef OPTIONAL
#undef FUN_NAME

// ,INITIALIZATION

#define FORM(name, value) Define_Symbol(name_space, name, Make_Special_Form(value))

void Initialize_Syntax(const MCell& name_space)
{
  FORM("if", form_if);
  FORM("define", form_define);
  FORM("set!", form_set);
  FORM("quote", form_quote);
  FORM("quasiquote", form_quasiquote);
  FORM("begin", form_begin);
  FORM("lambda", form_lambda);
  FORM("let-syntax", form_let_syntax);
  FORM("letrec-syntax", form_letrec_syntax);
  FORM("define-macro", form_define_macro);
  FORM("define-syntax", form_define_syntax);
  FORM("impl:current-env", form_current_env);
}

#undef FORM
#define PRIMITIVE(name, function) Define_Symbol(name_space, name, Make_Primitive(function, name))
#define PRIMITIVE_V(name, function) Define_Symbol(name_space, name, Make_Primitive(function, name, true))

void Initialize_Primitives(const MCell& name_space)
{
  PRIMITIVE("eq?", p_eqp);
  PRIMITIVE("eqv?", p_eqvp);
  PRIMITIVE("equal?", p_equalp);
  
  PRIMITIVE("car", p_car);
  PRIMITIVE("cdr", p_cdr);
  PRIMITIVE("cons", p_cons);
  PRIMITIVE("set-car!", p_set_car);
  PRIMITIVE("set-cdr!", p_set_cdr);
  PRIMITIVE("null?", p_nullp);
  PRIMITIVE("pair?", p_pairp);
  PRIMITIVE("list?", p_listp);

  PRIMITIVE("number?", p_numberp);
  PRIMITIVE("integer?", p_integerp);
  PRIMITIVE("rational?", p_rationalp);
  PRIMITIVE("real?", p_realp);
  PRIMITIVE("complex?", p_realp);
  PRIMITIVE("exact?", p_exactp);
  PRIMITIVE("inexact?", p_inexactp);

  // two-arg versions. the arbitrary-arg versions are implemented in
  // init.scm
  PRIMITIVE("_=", p_equals);
  PRIMITIVE("_<", p_less);
  PRIMITIVE("_<=", p_less_equal);
  PRIMITIVE("_>", p_greater);
  PRIMITIVE("_>=", p_greater_equal);
  
  PRIMITIVE("_+", p_plus);
  PRIMITIVE("_-", p_minus);
  PRIMITIVE("_*", p_times);
  PRIMITIVE("_/", p_divide);

  PRIMITIVE("quotient", p_quotient);
  PRIMITIVE("remainder", p_remainder);
  PRIMITIVE("modulo", p_modulo);

  PRIMITIVE_V("number->string", p_number_to_string);
  PRIMITIVE_V("string->number", p_string_to_number);

  PRIMITIVE("numerator", p_numerator);
  PRIMITIVE("denominator", p_denominator);

  PRIMITIVE("ceiling", p_ceiling);
  PRIMITIVE("floor", p_floor);
  PRIMITIVE("truncate", p_truncate);
  PRIMITIVE("round", p_round);

  PRIMITIVE("exp", p_exp);
  PRIMITIVE("log", p_log);
  PRIMITIVE("sqrt", p_sqrt);
  PRIMITIVE("expt", p_expt);
  
  PRIMITIVE("sin", p_sin);
  PRIMITIVE("cos", p_cos);
  PRIMITIVE("tan", p_tan);
  PRIMITIVE("asin", p_asin);
  PRIMITIVE("acos", p_acos);
  PRIMITIVE_V("atan", p_atan);

  PRIMITIVE("exact->inexact", p_exact_to_inexact);
  PRIMITIVE("inexact->exact", p_inexact_to_exact);
  
  PRIMITIVE("symbol?", p_symbolp);
  PRIMITIVE("string->symbol", p_string_to_symbol);
  PRIMITIVE("symbol->string", p_symbol_to_string);
  PRIMITIVE("symbol->integer", p_symbol_to_integer);
  PRIMITIVE("integer->symbol", p_integer_to_symbol);
  
  PRIMITIVE("char?", p_charp);
  PRIMITIVE("char_=?", p_char_equal);
  PRIMITIVE("char_<?", p_char_less);
  PRIMITIVE("char_<=?", p_char_less_equal);
  PRIMITIVE("char_>?", p_char_greater);
  PRIMITIVE("char_>=?", p_char_greater_equal);

  PRIMITIVE("char->integer", p_char_to_integer);
  PRIMITIVE("integer->char", p_integer_to_char);
  PRIMITIVE("char-upcase", p_char_upcase);
  PRIMITIVE("char-downcase", p_char_downcase);
  PRIMITIVE("char-alphabetic?", p_char_alphabetic);
  PRIMITIVE("char-numeric?", p_char_numeric);
  PRIMITIVE("char-whitespace?", p_char_whitespace);

  PRIMITIVE("string?", p_stringp);
  PRIMITIVE_V("make-string", p_make_string);
  PRIMITIVE("string-length", p_string_length);
  PRIMITIVE("string-ref", p_string_ref);
  PRIMITIVE("string-set!", p_string_set);
  
  PRIMITIVE("vector?", p_vectorp);
  PRIMITIVE_V("make-vector", p_make_vector);
  PRIMITIVE("vector-length", p_vector_length);
  PRIMITIVE("vector-ref", p_vector_ref);
  PRIMITIVE("vector-set!", p_vector_set);

  PRIMITIVE("input-port?", p_input_portp);
  PRIMITIVE("output-port?", p_output_portp);

  PRIMITIVE_V("write", p_write);
  PRIMITIVE_V("write-char", p_write_char);
  PRIMITIVE_V("display", p_display);
  PRIMITIVE_V("newline", p_newline);

  PRIMITIVE_V("read", p_read);
  PRIMITIVE_V("read-char", p_read_char);
  PRIMITIVE_V("char-ready?", p_char_readyp);
  PRIMITIVE_V("peek-char", p_peek_char);
  PRIMITIVE("eof-object?", p_eof_objectp);
  PRIMITIVE_V("flush-output", p_flush_output);
  
  PRIMITIVE("open-input-file", p_open_input_file);
  PRIMITIVE("open-output-file", p_open_output_file);
  PRIMITIVE("reopen-input-file", p_reopen_input_file);
  PRIMITIVE("reopen-output-file", p_reopen_output_file);
  PRIMITIVE("close-input-port", p_close_input_port);
  PRIMITIVE("close-output-port", p_close_output_port);
  PRIMITIVE("current-input-port", p_current_input_port);
  PRIMITIVE("current-output-port", p_current_output_port);
  PRIMITIVE("file-exists?", p_file_existsp);
  PRIMITIVE("input-port-line", p_input_port_line);
  
  PRIMITIVE("procedure?", p_procedurep);
  PRIMITIVE("scheme-report-environment", p_scheme_report_environment);
  PRIMITIVE("null-environment", p_null_environment);
  PRIMITIVE_V("compile", p_compile);
  
  PRIMITIVE("make-closure", p_make_closure);
  PRIMITIVE("raise", p_raise);
  PRIMITIVE("collect-garbage", p_collect_garbage);
  PRIMITIVE("defined?", p_definedp);
  PRIMITIVE("environment?", p_environmentp);
  
  PRIMITIVE_V("random", p_random);
  PRIMITIVE("seed-random", p_seed_random);
  PRIMITIVE("current-time", p_current_time);
  PRIMITIVE_V("object->string", p_object_to_string);
}

#undef PRIMITIVE
#undef PRIMITIVE_V

// ,INTERPRETER

std::string Find_Init_File()
{
  // With ALWAYS_COLLECT turned on the interpreter is way too slow for
  // the full init file, so we use a smaller one (still awfully slow
  // though).
  const char* filename =
#ifdef ALWAYS_COLLECT
    "init-light.scm"
#else
# ifdef INIT_FILE
    INIT_FILE
# else
    "uscheme-init.scm"
# endif
#endif
    ;
  const char* env_name = std::getenv("USCHEME_INIT_FILE");
  std::string retval;
  if (env_name != NULL && !std::ifstream(env_name).fail()){
    retval = env_name;
  }
  else if (!std::ifstream(filename).fail()){
    retval = filename;
  }
#ifdef PREFIX
#ifndef _WIN32
  else{
    std::string long_name = PREFIX + std::string("/share/") + filename;
    if (!std::ifstream(long_name.c_str()).fail())
      retval = long_name;
  }
#endif
#endif
  S_CHECK(retval.size() != 0, "could not locate init file");
  return retval;
}

Interpreter::Interpreter(size_t memory)
  : mem_manager(memory * 128),
    null_env(Make_Namespace(null_cell, null_env_size)),
    report_env(Make_Namespace(null_env, report_env_size)),
    work_env(Make_Namespace(report_env, default_workspace_size)),
    input(Make_Inport(std::cin)),
    output(Make_Outport(std::cout))
{
  S_CHECK(ip_ == NULL, "only one interpreter can be alive at any time");
  ip_ = this;

  Initialize_Syntax(null_env);
  Initialize_Primitives(report_env);

  // this is to get the value of null_env to the init code
  Define_Symbol(report_env, "*null-env*", null_env);
  // bootstrap code. loads init file.
  const char* open =
    "((lambda (file target-env)"
    "  (define (loop expr)"
    "    (if (eof-object? expr)"
    "        #v"
    "        (begin"
    "          (if (eq? ((compile expr target-env)) 'goto-report-env)"
    "            (set! target-env (impl:current-env)))"
    "          (loop (read file)))))"
    "  (loop (read file))"
    "  (close-input-port file)) (open-input-file \"";
  const char* close = "\") *null-env*)";
  Eval_String(open + Find_Init_File() + close, report_env, false);
  // undef *null-env* again
  Define_Symbol(report_env, "*null-env*", invalid_cell);
  Bootstrap_Namespace(work_env);
}

Interpreter::~Interpreter()
{
#ifdef WITH_DESTRUCTORS
  mem_manager.Call_All_Destructors();
#endif
  ip_ = NULL;
}

}
