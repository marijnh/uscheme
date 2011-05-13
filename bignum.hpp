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

#ifndef BIGNUM_HPP
#define BIGNUM_HPP

#include "type.hpp"

// Methods for operations on bignums. The interface of passing
// pointers and sizes is a little clunky but this was necessary
// because there are two representations of bignums - inside cells and
// as Array_Buffers.

namespace uls{

// The digit type and some conts to make messing with it easier.
typedef unsigned int digit;
const size_t digit_size = byte_size * sizeof(digit);
const uint64 digit_radix = (static_cast<uint64>(1) << digit_size), digit_mask = digit_radix - 1;

// Buffers to keep bignums in during calculations.
digit* Allocate_Array(int size);
struct Array_Buffer
{
  explicit Array_Buffer(int s)
    : size(s),
      data(Allocate_Array(size))
  {}
  ~Array_Buffer()
  {
    Allocate_Array(-size);
  }
  size_t size;
  digit* data;
};

// The operations. Add and subtract allow the result buffer to be the
// same as one of the source buffers, with multiply and divide this
// does not work.
bool Array_Zero(const digit* one, size_t s_one);
bool Array_Smaller(const digit* one, size_t s_one, const digit* two, size_t s_two);
void Add_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* result, size_t s_result);
void Subtract_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* result, size_t s_result);
void Multiply_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* result, size_t s_result);
void Divide_Arrays(const digit* one, size_t s_one, const digit* two, size_t s_two, digit* quotient, size_t s_quotient, digit* remain, size_t s_remain);

}

#endif //BIGNUM_HPP
