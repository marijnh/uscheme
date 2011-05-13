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

#ifndef ERROR_MACROS_HPP
#define ERROR_MACROS_HPP

#include <stdexcept>

#ifdef NDEBUG

#define S_THROW(error_type, message) throw error_type(message)

#define S_ASSERT(expr) 

#else // NDEBUG

#include <sstream>

inline std::string Macro_Mark_String(std::string target, const char* file, int line)
{
  std::ostringstream new_string;
  new_string << target << " (" << file << " at line " << line << ")";
  return new_string.str();
}

#define S_THROW(error_type, message) throw error_type(Macro_Mark_String(message, __FILE__, __LINE__))

#ifdef _WIN32
#define S_DIE() exit(1)
#else
#include <csignal>
#define S_DIE() kill(0, SIGTERM)
#endif
#include <iostream>
#define S_ASSERT(expr) if (!(expr)){std::cout << Macro_Mark_String("Assertion (" #expr ") failed.", __FILE__, __LINE__) << std::endl;\
                                  S_DIE();} else

#endif // NDEBUG

#ifndef SCHEME_ERROR_DEFINED
#define SCHEME_ERROR_DEFINED
namespace uls{
class Scheme_Error: public std::exception
{
public:
  Scheme_Error(const std::string& message): _message(message){}
  virtual ~Scheme_Error() throw(){}
  
  virtual const char* what() const throw(){return _message.c_str();}
  
private:
  std::string _message;
};
}
#endif //SCHEME_ERROR_DEFINED

#define S_CHECK(test, message) if (!(test)) throw Scheme_Error(message); else

#endif //ERROR_MACROS_HPP
