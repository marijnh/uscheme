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

#include <vector>
#include <memory>

#include "bignum.hpp"
#include "error.hpp"

namespace uls{

const size_t bignum_buffer_size = 400;

class Array_Manager
{
public:
  Array_Manager();
  ~Array_Manager();

  digit* Allocate(int size);
  
private:
  void Clear_Temp_Buffers();
  
  std::auto_ptr<digit> _buffer;
  std::vector<digit*> _temp_buffers;
  size_t _used, _desired_size, _current_size;
};

Array_Manager::Array_Manager()
  : _buffer(new digit[bignum_buffer_size]),
    _used(0),
    _desired_size(bignum_buffer_size),
    _current_size(bignum_buffer_size)
{}

Array_Manager::~Array_Manager()
{
  Clear_Temp_Buffers();
}

digit* Array_Manager::Allocate(int size)
{
  digit* retval = _buffer.get() + _used;
  _used += size;
  if (size > 0 && _used >= _current_size){
    retval = new digit[size];
    _temp_buffers.push_back(retval);
    _desired_size = _used;
  }
  else if (_used == 0){
    Clear_Temp_Buffers();
    if (_desired_size > _current_size){
      _buffer = std::auto_ptr<digit>(new digit[_desired_size]);
      _current_size = _desired_size;
    }
  }
  return retval;
}

void Array_Manager::Clear_Temp_Buffers()
{
  while (!_temp_buffers.empty()){
    delete[] _temp_buffers.back();
    _temp_buffers.pop_back();
  }
}

digit* Allocate_Array(int size)
{
  static Array_Manager manager;
  return manager.Allocate(size);
}
  
bool Array_Zero(const digit* one, size_t s_one)
{
  for (size_t i = 0; i != s_one; ++i){
    if (one[i] != 0)
      return false;
  }
  return true;
}

bool Array_Smaller(const digit* one, size_t s_one, const digit* two, size_t s_two)
{
  int s_big, s_small;
  const digit* big;
  if (s_one < s_two){
    s_big = s_two - 1;
    s_small = s_one - 1;
    big = two;
  }
  else{
    s_big = s_one - 1;
    s_small = s_two - 1;
    big = one;
  }
  for (int i = s_big; i != s_small; --i){
    if (big[i] != 0)
      return (s_one < s_two);
  }
  for (int i = s_small; i != -1; --i){
    if (one[i] != two[i])
      return one[i] < two[i];
  }
  return false;
}

void Add_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* result, size_t s_result)
{
  size_t carry = 0;
  for (size_t i = 0; i != s_result; ++i){
    int64 hold = carry;
    if (i < s_one)
      hold += one[i];
    if (i < s_two)
      hold += two[i];
    
    carry = (hold >> digit_size);
    result[i] = hold;
  }
  S_ASSERT(carry == 0);
}

void Subtract_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* result, size_t s_result)
{
  int carry = 0;
  for (size_t i = 0; i != s_result; ++i){
    int64 hold = carry;
    if (i < s_one)
      hold += one[i];
    if (i < s_two)
      hold -= two[i];

    if (hold < 0){
      hold += digit_radix;
      carry = -1;
    }
    else{
      carry = 0;
    }
    result[i] = hold;
  }
  S_ASSERT(carry == 0);
}

// Some of this bignum code is quite mind-boggling. There is a bug in
// it that causes it not to work on mac systems (something to do with
// the fact that those machines store bytes in a different order I
// assume). I have to admit I can't quite figure it all out anymore.

void Multiply_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* result, size_t s_result)
{
  for (size_t i = 0; i != s_result; ++i)
    result[i] = 0;

  for (size_t i = 0; i != s_two; ++i){
    size_t carry = 0;
    for (size_t j = 0; j < s_one || carry != 0; ++j){
      uint64 hold = (j < s_one) ? static_cast<uint64>(one[j]) * two[i] : 0;
      hold += carry;
      carry = (hold >> digit_size);

      size_t carry2 = hold;
      size_t pos = i + j;
      do {
        S_ASSERT(pos < s_result);
        uint64 hold2 = result[pos];
        hold2 += carry2;
        carry2 = (hold2 >> digit_size);
        result[pos] = hold2;
        ++pos;
      } while (carry2 != 0);
    }
    S_ASSERT(carry == 0);
  }
}

void Divide_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* quotient, size_t s_quotient, digit* remain, size_t s_remain)
{
  for (size_t i = 0; i != s_remain; ++i){
    if (i < s_one)
      remain[i] = one[i];
    else
      remain[i] = 0;
  }
  for (size_t i = 0; i != s_quotient; ++i)
    quotient[i] = 0;
  
  while (!Array_Smaller(remain, s_remain, two, s_two)){
    size_t hidig_remain = s_remain - 1;
    while (remain[hidig_remain] == 0) --hidig_remain;
    size_t hidig_two = s_two - 1;
    while (two[hidig_two] == 0) --hidig_two;
    size_t shift = hidig_remain - hidig_two;
    uint64 val_remain = remain[hidig_remain];
    uint64 val_two = two[hidig_two];
    if (hidig_remain != 0){
      val_remain = digit_radix * val_remain + remain[hidig_remain - 1];
      --shift;
    }
    uint64 guess = (val_remain - 1) / val_two;
    if (guess > digit_mask){
      guess = (guess >> digit_size);
      ++shift;
    }
    if (guess == 0)
      guess = 1;
    for (size_t j = 0; j != s_two; ++j){
      int64 carry = guess * two[j];
      size_t pos = j + shift;
      do {
        S_ASSERT(pos < s_remain);
        int64 hold = remain[pos];
        hold -= carry;
        carry = 0;
        if (hold < 0){
          carry = ((-hold) >> digit_size) + 1;
          hold = -((-hold) & digit_mask) + digit_radix;
        }
        remain[pos] = hold;
        ++pos;
      } while (carry != 0);
    }
    uint64 carry = guess, pos = shift;
    do {
      S_ASSERT(pos < s_quotient);
      uint64 hold = quotient[pos];
      hold += carry;
      carry = (hold >> digit_size);
      quotient[pos] = hold;
      ++pos;
    } while (carry != 0);
  }
}

}
