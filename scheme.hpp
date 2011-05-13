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

#ifndef SCHEME_HPP
#define SCHEME_HPP

#include <cstddef>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>

#include "type.hpp"
#include "noncopyable.hpp"
// Various error reporting things.
#include "error.hpp"
// Used to split input into tokens.
#include "inputsplitter.hpp"
// Associate strings with numbers.
#include "symbol.hpp"

namespace uls{

// ,CELL

// These are a bunch of patterns used to determine the type of
// non-pointer cell types. The top bits of the cell indicate the type.
// When the top bit is 1 it is an integer, when the top two bits are 0
// it is a pointer, etc.
  
// 1 bit patterns
const size_t int_pattern = 1;
// 2 bit patterns
const size_t compound_pattern = 0;
const size_t fourbit_pattern = 2;
// 4 bit patterns
const size_t symbol_pattern = 2;
const size_t temp_name_pattern = 6;
const size_t instruction_pattern = 10;
const size_t sixbit_pattern = 14;
// 6-bit patterns
const size_t char_pattern = 14;
const size_t instuction_pattern = 30;
const size_t form_pattern = 46;
const size_t special_pattern = 62;

// Some cells have their values defined right here in the enum. These
// are the cells with special values and the fixnums 0 and 1 (it is
// convenient to have those available like this).
//
// The invalid cell is used for a few different purposes, the most
// important purpose is to indicate a value that has not been defined
// yet. User code should not be able to create invalid cells, and
// primitives should never return them. Having invalid cells running
// around in user code will lead to weird results (most likely strange
// errors about a variable being used before it is defined).
enum Cell{
  false_cell = ((1 << 6) | special_pattern),
  true_cell = ((2 << 6) | special_pattern),
  null_cell = ((3 << 6) | special_pattern),
  void_cell = ((4 << 6) | special_pattern),
  invalid_cell = ((5 << 6) | special_pattern),
  eof_cell = ((256 << 6) | char_pattern),
  zero_cell = int_pattern,
  one_cell = (1 << 1) | int_pattern
};

// Some helper functions for encoding and extracting values with the
// top 4 or 6 bits used as type identification.
inline size_t Extract_Fourbit(Cell cell){
  return reinterpret_cast<size_t&>(cell) >> 4;
}
inline Cell Encode_Fourbit(size_t value, size_t pattern){
  size_t temp = (value << 4) | pattern;
  return reinterpret_cast<Cell&>(temp);
}
inline bool Match_Fourbit(Cell cell, size_t pattern){
  return (reinterpret_cast<size_t&>(cell) & 15) == pattern;
}
inline size_t Extract_Sixbit(Cell cell){
  return reinterpret_cast<size_t&>(cell) >> 6;
}
inline Cell Encode_Sixbit(size_t value, size_t pattern){
  size_t temp = (value << 6) | pattern;
  return reinterpret_cast<Cell&>(temp);
}
inline bool Match_Sixbit(Cell cell, size_t pattern){
  return (reinterpret_cast<size_t&>(cell) & 63) == pattern;
}

// ,SPECIALS

inline bool Is_Special(Cell cell)
{
  return Match_Sixbit(cell, special_pattern);
}

// Convenience function to make a scheme-boolean out of a c boolean.
inline Cell Make_Bool(bool value)
{
  return value ? true_cell : false_cell;
}

// ,CELLTYPE

// Cell types. A cell type is an 8 bit value identifying a compound
// cell type. Non-compound cell types are identified by their patterns
// (see above). All user-defined types are compound. A cell type
// always has a write function associated with it to convert cells of
// that type to text.
typedef byte Cell_Type;
const Cell_Type
  pair_type = 0,
  vector_type = 1,
  string_type = 2,
  closure_type = 3,
  primitive_type = 4,
  continuation_type = 5,
  inport_type = 6,
  outport_type = 7,
  rational_type = 8,
  real_type = 9,
  bignum_type = 10,
  macro_type = 11,
  simple_macro_type = 12,
  renamed_symbol_type = 13,
  namespace_type = 14,
  moved_cell_type = 15, // used by GC
  available_type = 16;

// New types can be created with the Make_Type function. It is up to
// the user code to keep track of this value and pass it to
// Allocate_Cell when creating cells of this type. The write function
// will be invoked when a cell of that type is written, displayed or
// converted to a string in some other way. The third argument
// indicates whether display or write was used, currently only strings
// and characters write differently when display is true.

#ifdef WITH_DESTRUCTORS
typedef void (*Destroy_Function)(Cell cell);
#endif
typedef void (*Write_Function)(Cell cell, std::ostream& str, bool display);
Cell_Type Make_Type(Write_Function write
#ifdef WITH_DESTRUCTORS
                    , Destroy_Function destroy = NULL
#endif
                    );
  
// ,CELLINFO

// Used to tell the memory manager which part of a struct contains
// cells, and which part contains other data. Starting from the least
// significant bit, every bit describes a 32-bit part of the cell
// data. 1 means it is a cell and must be examined when collecting
// garbage, 0 means other data. The default mask for Allocate_Cell has
// all bits set to 1, which means all fields are cells. Only the first
// 8 fields can be specified like this. The rest is assumed to be
// non-cell data unless all bits were 1, in which case everything is
// cell data.
typedef byte Pointer_Mask;

// This is the header for a memory block used by a cell. The top two
// are only used by the memory manager, type can be looked at by all
// code (use the Get_Type function), and data contains the data for
// the cell.
struct Cell_Info
{
  unsigned short size;
  Pointer_Mask mask;
  Cell_Type type;
  Cell data[1];
};

inline bool Is_Compound(Cell cell)
{
  return (reinterpret_cast<size_t&>(cell) & 3) == 0;
}

inline Cell_Info& Compound_Info(Cell cell)
{
  S_ASSERT(Is_Compound(cell));
  return *reinterpret_cast<Cell_Info*>(cell);
}

// Get a reference to the guts of a compound cell represented as a
// certain type. Make sure you are actually using the right type with
// the right kind of cell.
template<class Data>
inline Data& Extract(const Cell cell)
{
  return *reinterpret_cast<Data*>(Compound_Info(cell).data);
}

// Get the type of a compound cell. Does not work on non-compound
// cells!
inline Cell_Type Get_Type(Cell cell)
{
  return Compound_Info(cell).type;
}

// ,MEMMANAGER

// The memory manager. Every interpreter has one of these. They are
// used to allocate cells and they take care of the garbage
// collection.
class Mem_Manager: public noncopyable
{
public:
  Mem_Manager(size_t block_size = 500000);
  ~Mem_Manager();

  // Allocate a cell, you might want to consider using the free
  // function Allocate_Cell if you know exactly how big the cell has
  // to be. This can trigger garbage collection.
  Cell Allocate(size_t size, Cell_Type type, Pointer_Mask mask = max_byte);

  // Discard all cells that are not pointed to by the content of
  // MCells and MStacks
  void Collect_Garbage();

  // Just for debugging, checks whether a cell was missed in the last collection
  bool Is_Valid(Cell cell)
  {
    return !Is_Compound(cell) || Is_In_Block(cell, _live_block);
  }
  
  // These are used by MCell and MStack to protect their contents from
  // being collected. You are advised to use those classes instead of
  // using these functions directly.
  void Push_Marked(Cell* cell)
  {
    _marked.push_back(cell);
  }
  void Pop_Marked(Cell* cell)
  {
    if(_marked.back() == cell)
      _marked.pop_back();
    else
      Smart_Pop_Marked(cell);
  }
  void Push_Stack(std::vector<Cell>& stack)
  {
    _stacks.push_back(&stack);
  }
  void Pop_Stack()
  {
    _stacks.pop_back();
  }
  
private:
  void Move_Cell(Cell* cell);
  bool Is_In_Block(Cell cell, size_t* block) const;
  void Smart_Pop_Marked(Cell* cell);
#ifdef WITH_DESTRUCTORS
  friend class Interpreter;
  void Call_Destructors(size_t old_block_position);
  void Call_All_Destructors();
#endif

  const size_t _block_size;
  size_t _block_position, _cell_header_size;
  size_t* _live_block, * _dead_block;

  std::vector<Cell*> _marked;
  std::vector<std::vector<Cell>*> _stacks;
};

// Points to the memory manager if one is alive. You are encouraged to
// just stay away from this pointer, since the top-level Allocate
// functions supply a perfectly good way to allocate stuff.
extern Mem_Manager* mp_;

// MCell is used to contain one cell (objects of this class can be
// implicitly converted from and to cells) and protect it from being
// garbage collected.
class MCell
{
public:
  MCell(Cell cell = null_cell)
    : _cell(cell)
  {
    mp_->Push_Marked(&_cell);
  }
  MCell(const MCell& other)
    : _cell(other._cell)
  {
    mp_->Push_Marked(&_cell);
  }
  inline ~MCell()
  {
    mp_->Pop_Marked(&_cell);
  }

  operator Cell&() {return _cell;}
  operator Cell() const{return _cell;}
  void operator=(Cell cell){_cell = cell;}
  void operator=(const MCell& mcell){_cell = mcell._cell;}
  
private:
  Cell _cell;
};

// MStack is like MCell but instead it protects a whole stack of
// cells. Has a std::vector-like interface. You must not allocate
// these as function statics or on the heap, they rely on being
// destructed in the same order they were created.
class MStack
{
public:
  MStack(){mp_->Push_Stack(_cells);}
  explicit MStack(size_t size) : _cells(size, null_cell){mp_->Push_Stack(_cells);}
  ~MStack(){mp_->Pop_Stack();}
  
  Cell& operator[](size_t n){return _cells[n];}
  Cell operator[](size_t n) const{return _cells[n];}
  void Push(Cell cell){_cells.push_back(cell);}
  Cell Pop(){Cell temp = _cells.back(); _cells.pop_back(); return temp;}
  bool Empty() const{return _cells.empty();}
  size_t Size() const{return _cells.size();}
  Cell& Back(){return _cells.back();}
  void Clear(){_cells.clear();}
  
private:
  std::vector<Cell> _cells;
};

// ,TYPE MANAGER

// Associates write functions with cell types. Just use Make_Type and
// ignore this class.
class Type_Manager: public noncopyable
{
public:
  Type_Manager();
  Cell_Type Make_Type(Write_Function write
#ifdef WITH_DESTRUCTORS
                    , Destroy_Function destroy
#endif
                      );
  Write_Function Get_Function(Cell_Type type){
    S_ASSERT(type < _functions.size());
    S_ASSERT(_functions[type] != NULL);
    return _functions[type];
  }
#ifdef WITH_DESTRUCTORS
  Destroy_Function Get_Destructor(Cell_Type type){
    S_ASSERT(type < _destructors.size());
    return _destructors[type];
  }
#endif

private:
  std::vector<Write_Function> _functions;
#ifdef WITH_DESTRUCTORS
  std::vector<Destroy_Function> _destructors;
#endif  
  Cell_Type _current;
};


// ,INTERPRETER

// This is what you create an instance of to start working with
// scheme. Everything is public, and the only member function is the
// constructor.
// 
// The argument to the constructor gives the amount of kilobytes a
// memory block must contain. The memory allocated by the mem manager
// is twice this, because of the garbage collection method used.
//
// Messing with the variables in this struct should be rather safe.
// You can call functions on the mem_manager and type_manager if you
// must, change the standard input and output, messing with the
// environments is probably a bad idea.
struct Interpreter
{
  explicit Interpreter(size_t memory = 2000);
  ~Interpreter();
  
  Type_Manager type_manager;
  Mem_Manager mem_manager;

  MCell null_env, report_env, work_env;
  MCell input, output;
};

// Pointer to a live Interpreter if one exists. Don't touch.
extern Interpreter* ip_;

// Convenient ways of allocating a cell. Use the template argument of
// Allocate_Cell to specify what kind of data you want to store in the
// cell (and then get access to that with Extract<Data> after it has
// been allocated).
inline Cell Allocate(size_t size, Cell_Type type, Pointer_Mask mask = max_short)
{
  return mp_->Allocate(size, type, mask);
}
template <class Data>
inline Cell Allocate_Cell(Cell_Type type, Pointer_Mask mask = max_short)
{
  return Allocate(sizeof(Data), type, mask);
}

// ,FIXNUM

// Fixnums range from -max_fixnum to +max_fixnum
const size_t max_fixnum = (max_int >> 2);

inline bool Is_Fixnum(Cell cell)
{
  return (reinterpret_cast<size_t&>(cell) & 1) == int_pattern;
}
inline Cell Make_Fixnum(int value)
{
  S_ASSERT(std::abs(value) < max_fixnum);
  size_t temp = (value << 1) | int_pattern;
  return reinterpret_cast<Cell&>(temp);
}
inline int Fixnum_Value(Cell cell)
{
  S_ASSERT(Is_Fixnum(cell));
  size_t bits = reinterpret_cast<size_t&>(cell) >> 1;
  // This is needed to restore the sign, it basically takes the
  // almost-most-significant bit and copies it to the most significant
  // bit (which got trampled by the shifting)
  bits |= ((bits & (1 << (sizeof(int) * byte_size - 2))) << 1);
  return reinterpret_cast<int&>(bits);
}

// ,BIGNUM
// Bignums internally contain:
// - a sign
// - a size
// - a series of 32-bit values that make up the digits of the number in
//   radix 2^32

inline bool Is_Bignum(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == bignum_type;
}
Cell Make_Bignum(int64 value);

// ,INTEGER

inline bool Is_Integer(Cell cell)
{
  return Is_Fixnum(cell) || Is_Bignum(cell);
}
inline Cell Make_Integer(int64 value)
{
  int64 abs_value = (value < 0) ? -value : value;
  if (abs_value >= max_fixnum)
    return Make_Bignum(value);
  else
    return Make_Fixnum(value);
}

// Bignums are tricky to work with, here are some basic numeric
// operations that can be applied to them (more can be found in
// ,NUMBER)
bool Integer_Negative(Cell cell);
bool Integer_Equal(Cell one, Cell two);
bool Integer_Less(Cell one, Cell two);

Cell Integer_Quotient(Cell one, Cell two);
Cell Integer_Remainder(Cell one, Cell two);
Cell Integer_Modulo(Cell one, Cell two);

// ,RATIONAL
// Rational number are implemented as two integer (fixnum or bignum)
// values. They are always simplified on creation.

struct Rational_Data
{
  Cell numerator, denominator;
};

inline bool Is_Rational(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == rational_type;
}
Cell Make_Simplified_Rational(Cell numerator, Cell denominator);

inline Cell& Rational_Numerator(const Cell cell)
{
  return Extract<Rational_Data>(cell).numerator;
}
inline Cell& Rational_Denominator(const Cell cell)
{
  return Extract<Rational_Data>(cell).denominator;
}

// ,REAL
// Reals are C++ doubles wrapped up in a cell

inline bool Is_Real(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == real_type;
}
Cell Make_Real(double value);
inline double Real_Value(Cell cell)
{
  return Extract<double>(cell);
}

// ,NUMBER

// The Num_Type system is used to conveniently 'promote' numbers to
// other number types.
enum Num_Type {
  n_fixnum = 0,
  n_bignum = 1,
  n_rational = 2,
  n_real = 3
};

Num_Type Number_Type(Cell cell);
// Be careful with Promote_Number, promoting fixnums leads to bignums
// that should be fixnums and promoting bignums leads to rationals
// that have a denominator of 1 - other procedures can get confused by
// such objects.
Cell Promote_Number(Cell num, Num_Type type);

inline bool Is_Number(Cell cell)
{
  return Is_Integer(cell) || Is_Real(cell) || Is_Rational(cell);
}

// Get the double value of any type of number, can be convenient with
// all those different types confusing your code.
double Number_To_Double(Cell cell);

// Basic operations on any kind of number cells.
Cell Number_Add(Cell one, Cell two);
Cell Number_Subtract(Cell one, Cell two);
Cell Number_Multiply(Cell one, Cell two);
Cell Number_Divide(Cell one, Cell two);

// ,SYMBOL
// Symbol cells, see symbol.hpp and symbol.cpp for the implementation
// of the symbol table.

inline bool Is_Symbol(Cell cell)
{
  return Match_Fourbit(cell, symbol_pattern);
}
inline Cell Make_Symbol(Symbol symbol)
{
  return Encode_Fourbit(symbol, symbol_pattern);
}
inline Cell Make_Symbol(const std::string& name)
{
  return Make_Symbol(Get_Symbol(name));
}
inline Symbol Symbol_Value(Cell cell)
{
  return Extract_Fourbit(cell);
}
inline const std::string& Symbol_Name(Cell cell)
{
  return Get_Symbol_Name(Symbol_Value(cell));
}

// ,CHARACTER

inline bool Is_Character(Cell cell)
{
  return Match_Sixbit(cell, char_pattern);
}
inline Cell Make_Character(int c)
{
  return Encode_Sixbit(c, char_pattern);
}
inline int Character_Value(Cell cell)
{
  return Extract_Sixbit(cell);
}

// ,PAIR

struct Pair
{
  Cell car, cdr;
};

inline bool Is_Pair(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == pair_type;
}

// Car and Cdr are used both for getting and setting values.
inline Cell& Car(const Cell cell)
{
  S_ASSERT(Is_Pair(cell));
  return Extract<Pair>(cell).car;
}
inline Cell& Cdr(const Cell cell)
{
  S_ASSERT(Is_Pair(cell));
  return Extract<Pair>(cell).cdr;
}

// A number of cxr variants.
inline Cell& Caar(const Cell cell)
{
  return Car(Car(cell));
}
inline Cell& Cdar(const Cell cell)
{
  return Cdr(Car(cell));
}
inline Cell& Cadr(const Cell cell)
{
  return Car(Cdr(cell));
}
inline Cell& Cddr(const Cell cell)
{
  return Cdr(Cdr(cell));
}
inline Cell& Caddr(const Cell cell)
{
  return Car(Cdr(Cdr(cell)));
}
inline Cell& Cdaar(const Cell cell)
{
  return Cdr(Car(Car(cell)));
}
inline Cell& Cadar(const Cell cell)
{
  return Car(Cdr(Car(cell)));
}
inline Cell& Cddar(const Cell cell)
{
  return Cdr(Cdr(Car(cell)));
}
inline Cell& Caadr(const Cell cell)
{
  return Car(Car(Cdr(cell)));
}
inline Cell& Caaar(const Cell cell)
{
  return Car(Car(Car(cell)));
}

inline Cell Cons(const MCell& car, const MCell& cdr)
{
  Cell retval = Allocate_Cell<Pair>(pair_type);
  Car(retval) = car;
  Cdr(retval) = cdr;
  return retval;
}
// Only use this when car and cdr are NOT compounds
inline Cell XCons(Cell car, Cell cdr)
{
  Cell retval = Allocate_Cell<Pair>(pair_type);
  Car(retval) = car;
  Cdr(retval) = cdr;
  return retval;
}
// Conses null onto a cell
inline Cell Cons_Null(const MCell& car)
{
  Cell retval = Allocate_Cell<Pair>(pair_type);
  Car(retval) = car;
  Cdr(retval) = null_cell;
  return retval;
}
    
size_t List_Length(Cell list, const char* error = "improper list");
bool Is_Proper_List(Cell list);

bool Equal(Cell one, Cell two);
bool Member(Cell value, Cell list);
Cell Assoc(Cell needle, Cell list);

// Easy way of building a list front to end.
class List_Builder
{
public:
  void Add_Element(const MCell& cell)
  {
    if (_start == null_cell){
      _start = Cons(cell, _start);
      _tail = _start;
    }
    else{
      S_ASSERT(_tail != null_cell);
      Cdr(_tail) = Cons_Null(cell);
      _tail = Cdr(_tail);
    }
  }
  void Add_End(Cell cell)
  {
    if (_start == null_cell){
      _start = cell;
    }
    else{
      S_ASSERT(_tail != null_cell);
      Cdr(_tail) = cell;
    }
    _tail = null_cell;
  }
  const MCell& List()
  {
    return _start;
  }

private:
  MCell _start, _tail;
};

// ,STRING

inline bool Is_String(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == string_type;
}
Cell Make_String(const std::string& value);
std::string String_Value(Cell cell);

struct String_Data
{
  size_t size;
  char data[1];
};

inline size_t String_Size(Cell cell)
{
  S_ASSERT(Is_String(cell));
  return Extract<String_Data>(cell).size;
}
inline char& String_Ref(Cell cell, size_t n)
{
  S_ASSERT(Is_String(cell));
  S_ASSERT(n < String_Size(cell));
  return Extract<String_Data>(cell).data[n];
}

// ,VECTOR

inline bool Is_Vector(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == vector_type;
}

// Some different ways of constructing vectors
Cell Make_Vector(size_t size, Cell fill = null_cell);
Cell Make_Vector(const MStack& stack);
Cell Make_Vector(Cell list);

struct Vector_Data
{
  Cell size;
  Cell data[1];
};

inline size_t Vector_Size(Cell cell)
{
  S_ASSERT(Is_Vector(cell));
  return Fixnum_Value(Extract<Vector_Data>(cell).size);
}
inline Cell& Vector_Ref(Cell cell, size_t n)
{
  S_ASSERT(Is_Vector(cell));
  return Extract<Vector_Data>(cell).data[n];
}

// ,PORT

inline bool Is_Inport(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == inport_type;
}
inline bool Is_Outport(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == outport_type;
}

// Ports made with a filename have to be closed before they are
// collected or they will leak memory.
Cell Make_Inport(const MCell& filename);
Cell Make_Outport(const MCell& filename);
Cell Make_Inport(std::istream& stream);
Cell Make_Outport(std::ostream& stream);

Cell Inport_Read_Char(Cell port);
Cell Inport_Peek_Char(Cell port);

struct Inport_Data
{
  Cell file_name;
  std::istream* stream;
  size_t position, line;
};
struct Outport_Data
{
  Cell file_name;
  std::ostream* stream;
};

std::istream& Inport_Stream(Cell port);
std::ostream& Outport_Stream(Cell cell);
size_t Inport_Line(Cell port);

inline bool Inport_Is_Open(Cell cell)
{
  S_ASSERT(Is_Inport(cell));
  return Extract<Inport_Data>(cell).stream != NULL;
}
inline bool Outport_Is_Open(Cell cell)
{
  S_ASSERT(Is_Outport(cell));
  return Extract<Outport_Data>(cell).stream != NULL;
}

void Close_Inport(Cell cell);
void Close_Outport(Cell cell);

// ,NAMESPACE

inline bool Is_Namespace(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == namespace_type;
}

// ,CLOSURE

// Closures contain compiled code, the number of arguments they take,
// the environment in which they were created and optionally name.
// This is not exposed in this header though, client code shouldn't
// need to manipulate closures directly.
inline bool Is_Closure(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == closure_type;
}
Cell Closure_Name(Cell cell);

// ,PRIMITIVE

// Primitives contain a function pointer pointing to a function of one
// of the 9 types shown below, a number of arguments and a name. This
// looks very ugly but the advantage is that you can easily define
// primitives with up to 8 arguments. The Make_Primitive will make a
// primitive with the correct number of arguments based on the type of
// function you pass it. If var_arg is true the last argument will
// behave like z in (lambda (x y . z) ....)
typedef Cell (*Primitive_Function_0) ();
typedef Cell (*Primitive_Function_1) (Cell one);
typedef Cell (*Primitive_Function_2) (Cell one, Cell two);
typedef Cell (*Primitive_Function_3) (Cell one, Cell two, Cell three);
typedef Cell (*Primitive_Function_4) (Cell one, Cell two, Cell three, Cell four);
typedef Cell (*Primitive_Function_5) (Cell one, Cell two, Cell three, Cell four, Cell five);
typedef Cell (*Primitive_Function_6) (Cell one, Cell two, Cell three, Cell four, Cell five, Cell six);
typedef Cell (*Primitive_Function_7) (Cell one, Cell two, Cell three, Cell four, Cell five, Cell six, Cell seven);
typedef Cell (*Primitive_Function_8) (Cell one, Cell two, Cell three, Cell four, Cell five, Cell six, Cell seven, Cell eight);
Cell Make_Primitive(Primitive_Function_0 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_1 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_2 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_3 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_4 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_5 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_6 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_7 function, const std::string& name, bool var_arg = false);
Cell Make_Primitive(Primitive_Function_8 function, const std::string& name, bool var_arg = false);

inline bool Is_Primitive(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == primitive_type;
}

// ,SYNTAX

// 'Special form' means primitive syntax here - stuff like if, lambda,
// quote are special forms.
inline bool Is_Special_Form(Cell cell)
{
  return Match_Sixbit(cell, form_pattern);
}
// Stuff defined by syntax-rules expressions are macros
inline bool Is_Macro(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == macro_type;
}
inline bool Is_Simple_Macro(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == simple_macro_type;
}
inline bool Is_Syntax(Cell cell)
{
  return Is_Special_Form(cell) || Is_Macro(cell) || Is_Simple_Macro(cell);
}

// ,CONTINUATION

// These should not show up much on the outside. The things that
// call/cc returns are actually closures with a continuation inside.
// The only way to directly work with these is through the
// #%current_continuation and #%set_continuation instructions.
inline bool Is_Continuation(Cell cell)
{
  return Is_Compound(cell) && Get_Type(cell) == continuation_type;
}

// ,INTERFACE

// Define a top-level symbol to have a certain value.
void Define_Symbol(const MCell& name_space, const std::string& name, const MCell& value);
// Convenience function for defining primitives.
template <typename Function_Type>
inline void Define_Primitive(const std::string& name, Function_Type function, bool var_arg = false)
{
  Define_Symbol(ip_->work_env, name, Make_Primitive(function, name, var_arg));
}

// Get the value that a binding has in a namespace. Looks through
// parent namespaces too.
Cell Get_Value(Cell name_space, Cell symbol);

// Method to output a cell to an output stream. The display argument
// indicates whether this is a 'display' or a 'write' action
// (influences the way strings and characters are outputted).
void Write(Cell cell, std::ostream& str, bool display = false);

// Some convenience functions related to Write.
inline std::ostream& operator<<(std::ostream& os, Cell cell)
{
  Write(cell, os);
  return os;
}
std::string Cell_To_String(Cell cell, bool display = false);

// Read a cell from a stream.
Cell Read(std::istream& stream);
inline std::istream& operator>>(std::istream& is, Cell& cell)
{
  cell = Read(is);
  return is;
}

// Starts a read-eval-print loop. This will not return until in.input
// is at eof.
void Run_REPL(bool welcome_message = true);
// Loads a file (just executes the load function defined in init.scm
// with the file as argument)
void Load_File(const std::string& filename);
// Evaluate a string or an expression. Only the first expression in
// the given string is evaluated.
Cell Eval_String(const std::string& str, const MCell& name_space, bool handle_errors = false);
inline Cell Eval_String(const std::string& str, bool handle_errors = false)
{
  return Eval_String(str, ip_->work_env, handle_errors);
}
Cell Eval_Expression(Cell expression, Cell name_space, bool handle_errors = false);
inline Cell Eval_Expression(Cell expression, bool handle_errors = false)
{
  return Eval_Expression(expression, ip_->work_env, handle_errors);
}

// A read eval print loop that does not wait for input to come from a
// stream but has to be fed strings to run. The return value of
// Add_Line indicates whether anything got evaluated (if the new
// string did not finish a full expression it is false).
class String_REPL
{
public:
  bool Add_Line(const std::string& str);
  
private:
  String_Input_Splitter _input;
};

// This is useful if you want to poll the output instead of have it go
// into a stream, basically just attaches itself to in.output and
// gives you any new output every time you call Get_New_Output. Makes
// the assumption that no one else messes with in.output while it is
// alive.
class Output_Catcher
{
public:
  Output_Catcher();
  ~Output_Catcher();
  std::string Get_New_Output();

private:
  std::ostringstream _stream;
  MCell _old_stream;
};

// Pointer wrappers are a convenient way of wrapping C++ objects in
// scheme cells. Make a class (T) inherit from Pointer_Wrapper<T>, and
// then you can call Init_Type on it somewhere in your initialization
// code, passing it the name of the constructor function for this
// type, the actual function you want to use for constructing such
// object, and optionally a specialized write function. The class
// provides a convenient function for creating the actual scheme cells
// from a pointer - Wrap_Object. The reason you still have to write
// the actual constructor function yourself is that you may want to
// have it take arguments, or behave in some special way.
#ifdef WITH_DESTRUCTORS
template<class T>
class Pointer_Wrapper
{
public:
  static Cell_Type type_id;

  static void Default_Write(Cell cell, std::ostream& str, bool display)
  {
    str << "#<wrapped pointer>";
  }
  static void Destroy(Cell data)
  {
    delete Extract<T*>(data);
  }

  template<typename Function_Type>
  static void Init_Type(std::string constructor_name, Function_Type create, Write_Function write = Default_Write)
  {
    type_id = Make_Type(write, Destroy);
    Define_Primitive(constructor_name, create);
  }
  
  static Cell Wrap_Object(T* object)
  {
    Cell new_cell = Allocate_Cell<T*>(type_id, 0);
    Extract<T*>(new_cell) = object;
    return new_cell;
  }
};

template<class T>
Cell_Type Pointer_Wrapper<T>::type_id = 0;
#endif

}

#include "undef_error.hpp"

#endif //SCHEME_HPP
