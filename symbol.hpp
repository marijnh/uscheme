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

#ifndef SYMBOL_HPP
#define SYMBOL_HPP

#include <string>

// Associate integer values with strings. Get_Symbol will create a new
// association if none exist for that string, Find_Symbol will return
// null_symbol in that case. Note that this system never forgets
// associations, this has the disadvantage that it can get memory
// intensive if you generate large amounts of temporary symbols
// (string->symbol and integer->symbol can do that), but the advantage
// that this module is not dependant on any Interpreter and that the
// symbols can be used for other purposes than scheme symbols.

namespace uls{

typedef size_t Symbol;
const Symbol null_symbol = 0;

Symbol Get_Symbol(const std::string& word);
Symbol Find_Symbol(const std::string& word);
const std::string& Get_Symbol_Name(Symbol s);

}

#endif //SYMBOL_HPP
