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

#ifndef INPUT_SPLITTER_HPP
#define INPUT_SPLITTER_HPP

#include <string>
#include <deque>
#include <istream>

// System to split input into element-sized strings. An abstract base
// class with two implementations is provided - one for strings and
// one for streams. The reason there are two implemenations is that
// stream output is blocking, it just keeps looking ahead as long as
// it has to for new input, while string output has to look ahead to
// see if a full expression has been entered before an expression can
// be read.

namespace uls{

inline bool Is_Whitespace(char c)
{
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
}

class Input_Splitter
{
public:
  virtual ~Input_Splitter(){}

  virtual const std::string& Current() = 0;
  virtual void Advance(bool allow_eof = true) = 0;
};

class String_Input_Splitter: public Input_Splitter
{
public:
  String_Input_Splitter();

  virtual const std::string& Current();
  virtual void Advance(bool allow_eof = true);

  void Add_Line(const std::string& str);
  bool Full_Expression();
  void Reset();
  
private:
  void Add_Part(const std::string& part, bool can_be_finished = true);
  
  std::deque<std::string> _parts;
  std::string _unfinished;
  bool _in_string;
  int _open_parens;
  size_t _finished_part;
};

class Stream_Input_Splitter: public Input_Splitter
{
public:
  Stream_Input_Splitter(std::istream& stream);

  virtual const std::string& Current();
  virtual void Advance(bool allow_eof = true);

  bool Eof(){return _stream.eof();}
  size_t Lines_Read(){return _lines_read;}

private:
  void Read_Part();
  char Get_Char()
  {
    char retval = _stream.get();
    if (retval == '\n')
      ++_lines_read;
    return retval;
  }
  
  std::istream& _stream;
  std::string _current;
  size_t _lines_read;
  bool _allow_eof;
};

}

#endif //INPUT_SPLITTER_HPP
