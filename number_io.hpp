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

#ifndef NUMBER_IO_HPP
#define NUMBER_IO_HPP

#include <ostream>
#include <vector>
#include <string>

#include "type.hpp"
#include "bignum.hpp"

// Used to convert numbers to and from text. This was needed for two
// reasons - Firstly I wanted to support a lot of radixes, and
// secondly bignums needed custom i/o methods anyway.
//
// The reason input is done through strings and output through streams
// is that this was convenient. The functions that require input in
// scheme.cpp already had strings, and strings are easy to read
// backwards, while the output functions worked with streams.

namespace uls{

double String_To_Double(const std::string& str, size_t radix = 10);
int64 String_To_Int(const std::string& str, size_t radix = 10);
// Return value indicates the sign of the number, true is negative
bool String_To_Array(const std::string& str, std::vector<digit>& array, size_t radix = 10);

void Write_Double(std::ostream& stream, double value, size_t radix = 10);
void Write_Int(std::ostream& stream, int value, size_t radix = 10);
void Write_Array(std::ostream& stream, const digit* array, size_t size, bool negative, size_t radix = 10);

}

#endif //NUMBER_IO_HPP
