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

#ifndef TYPE_HPP
#define TYPE_HPP

#include <cstddef>

// Some typedefs and constant to make some bit-twiddling operations
// and typenames a little less ugly.

namespace uls{

typedef unsigned char byte;
typedef long long int64;
typedef unsigned long long uint64;
const size_t byte_size = 8;
const unsigned char max_byte = 0xFF;
const unsigned short max_short = 0xFFFF;
const size_t max_int = 0xFFFFFFFF;

}

#endif //TYPE_HPP
