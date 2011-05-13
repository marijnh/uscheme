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

#include <sstream>
#include <cmath>

#include "number_io.hpp"
#include "error.hpp"

// TODO : maybe add #-stuff to number reading?
 
namespace uls{

const char* Digit_Table()
{
  const static char table[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'};
  return table;
}
const char zero_digit = '0', minus_digit = '-';
  
int Digit_To_Int(char dig, size_t radix = 10)
{
  int retval = -1;
  if (dig >= '0' && dig < '0' + 10)
    retval = dig - '0';
  else if (dig >= 'a' && dig < 'a' + 10)
    retval = dig - 'a' + 10;
  else if (dig >= 'A' && dig < 'A' + 10)
    retval = dig - 'A' + 10;
  S_CHECK(retval != -1 && retval < static_cast<int>(radix), "invalid digit in numeric constant: " + std::string(1, dig));
  return retval;
}

double String_To_Double(const std::string& str, size_t radix)
{
  double number = 0;
  S_CHECK(str.size() > 0, "invalid floating-point constant: " + str);
  
  size_t dot = str.find('.');
  size_t exp = (radix < 15) ? str.find_first_of("eE") : str.find('E');
  S_CHECK(dot != str.npos || exp != str.npos, "invalid floating-point constant: " + str);
  bool negative = str[0] == minus_digit, read_something = false;
  size_t pos = negative ? 1 : 0;
  if (str[0] == '+')
    pos = 1;
  size_t end = dot == str.npos ? exp : dot;

  if (pos != end){
    read_something = true;
    int64 integer = String_To_Int(std::string(str, pos, end - pos), radix);
    number += integer;
  }
  pos = end + 1;
  
  if (dot != str.npos){
    end = exp == str.npos ? str.size() : exp;
    if (pos != end){
      read_something = true;
      double fraction = static_cast<double>(String_To_Int(std::string(str, pos, end - pos), radix));
      size_t frac_width = end - pos;
      for (size_t i = frac_width; i != 0; --i)
        fraction /= radix;
      number += fraction;
    }
    pos = end + 1;
  }
  if (exp != str.npos){
    int64 expo = String_To_Int(std::string(str, pos), radix);
    while (expo < 0){
      number /= radix;
      ++expo;
    }
    while (expo > 0){
      number *= radix;
      --expo;
    }
  }
  if (negative)
    number = -number;

  S_CHECK(read_something, "empty numeric string");
  return number;
}

int64 String_To_Int(const std::string& str, size_t radix)
{
  S_CHECK(str.size() != 0, "empty numeric string");
  bool negative = str[0] == minus_digit;
  int64 number = 0, factor = 1;
  int end = negative ? 0 : -1;
  if (str[0] == '+')
    end = 0;
  S_CHECK(static_cast<int>(str.size()) - 1 != end, "empty numeric string");
    
  for (int i = str.size() - 1; i != end; --i){
    number += factor * Digit_To_Int(str[i], radix);
    factor *= radix;
  }
  if (negative)
    number = -number;
  
  return number;
}

bool String_To_Array(const std::string& str, std::vector<digit>& array, size_t radix)
{
  S_CHECK(str.size() != 0, "empty numeric string");
  bool negative = str[0] == minus_digit;
  uint64 radix_factor = 0;
  for (uint64 counter = radix; counter < digit_radix; counter *= radix)
    ++radix_factor;

  size_t size = (str.size() / radix_factor) + 1;
  array.resize(size);
  Array_Buffer product(size), factor(size);
  for (size_t i = 1; i != size; ++i)
    factor.data[i] = 0;
  factor.data[0] = 1;
  digit radix_digit = radix;
  
  int end = negative ? 0 : -1;
  if (str[0] == '+')
    end = 0;
  S_CHECK(static_cast<int>(str.size()) - 1 != end, "empty numeric string");
  for (int i = str.size() - 1; i != end; --i){
    digit current = Digit_To_Int(str[i], radix);
    
    Multiply_Arrays(&current, 1, factor.data, size, product.data, size);
    Add_Arrays(product.data, size, &array[0], size, &array[0], size);
    Multiply_Arrays(&radix_digit, 1, factor.data, size, product.data, size);
    std::copy(product.data, product.data + size, factor.data);
  }
  
  return negative;
}

void Split_To_Digits(std::vector<char>& buffer, uint64 number, size_t radix)
{
  const char* dig = Digit_Table();
  
  while (number != 0){
    buffer.push_back(dig[number % radix]);
    number /= radix;
  }
}

void Write_Double(std::ostream& stream, double value, size_t radix)
{
  const int frac_width = 16;

  if (value == 0){
    stream << zero_digit << '.' << zero_digit;
    return;
  }
  if (value < 0){
    stream << minus_digit;
    value = -value;
  }
  
  int exp = 0;
  while (value >= radix){
    value /= radix;
    ++exp;
  }
  while (value < 1){
    value *= radix;
    --exp;
  }

  size_t full = static_cast<size_t>(value);
  double fraction = value - static_cast<double>(full);

  S_ASSERT(full < radix);
  stream << Digit_Table()[full];
  stream << '.';

  uint64 i_frac = static_cast<uint64>(fraction * std::pow(static_cast<double>(radix), frac_width) + .5);
  if (i_frac == 0){
    stream << zero_digit;
  }
  else{
    std::vector<char> buffer;
    Split_To_Digits(buffer, i_frac, radix);
    while (buffer.size() != static_cast<size_t>(frac_width))
      buffer.push_back(zero_digit);
    size_t last = 0;
    while(buffer[last] == zero_digit)
      ++last;
    for (size_t i = buffer.size(); i != last; --i)
      stream << buffer[i - 1];
  }
  
  if (exp != 0){
    char exp_char = (radix < 15) ? 'e' : 'E';
    stream << exp_char;
    Write_Int(stream, exp, radix);
  }
}

void Write_Int(std::ostream& stream, int value, size_t radix)
{
  if (value == 0){
    stream << zero_digit;
    return;
  }
  if (value < 0){
    stream << minus_digit;
    value = -value;
  }

  std::vector<char> buffer;
  Split_To_Digits(buffer, value, radix);

  for (size_t i = buffer.size(); i != 0; --i)
    stream << buffer[i - 1];
}

void Write_Array(std::ostream& stream, const digit* array, size_t size, bool negative, size_t radix)
{
  if (negative)
    stream << minus_digit;
  
  Array_Buffer quotient(size), remainder(size), value(size);
  std::copy(array, array + size, value.data);
  
  if (Array_Zero(array, size)){
    stream << zero_digit;
    return;
  }

  digit factor = radix, factor_width = 1;
  while (true){
    uint64 temp = static_cast<uint64>(factor) * radix;
    if (temp >= digit_radix)
      break;
    factor = temp;
    ++factor_width;
  }

  std::vector<char> buffer;
  size_t width = 0;
  while (!Array_Zero(value.data, value.size)){
    Divide_Arrays(value.data, size, &factor, 1, quotient.data, size, remainder.data, size);
    Split_To_Digits(buffer, remainder.data[0], radix);
    width += factor_width;
    while (buffer.size() != width)
      buffer.push_back(zero_digit);
    std::copy(quotient.data, quotient.data + size, value.data);
  }

  size_t end = buffer.size();
  while (buffer[end - 1] == zero_digit)
    --end;
  for (size_t i = end; i != 0; --i)
    stream << buffer[i - 1];
}

}
