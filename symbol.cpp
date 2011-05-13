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

#include <algorithm>
#include <vector>
#include "error.hpp"
#include "symbol.hpp"
#include "type.hpp"

namespace uls{

namespace{
  // Symbols consist of 10 bit (the lowest 10) of hash, and the rest
  // of the value is an ID that is used to distinguish the symbol from
  // other symbols with the same hash code. Symbols and strings can
  // both be converted to hash numbers this way, which makes both
  // symbols and their names useful keys for the hash table.
  const size_t table_size = 521;
  const size_t hash_width = 10, hash_mask = 1023;
  
  inline Symbol Make_Symbol(size_t hash, size_t id)
  {
    return hash + (id << hash_width);
  }
  inline size_t Symbol_Hash(Symbol sym)
  {
    return (sym & hash_mask);
  }
  inline size_t Symbol_ID(Symbol sym)
  {
    return (sym >> hash_width);
  }
  size_t Hash_String(const std::string& str)
  {
    size_t max = std::min(str.size(), static_cast<size_t>(4));
    size_t accum = 0;
    for (size_t i = 0; i != max; ++i)
      accum += (str[i] << i * byte_size);
    return accum % table_size;
  }
  
  class Symbol_Table
  {
  public:
    Symbol_Table();
    ~Symbol_Table();

    Symbol Get_Symbol(const std::string& str);
    Symbol Has_Symbol(const std::string& str) const;
    const std::string& Get_Name(Symbol s) const;

  private:
    struct Entry
    {
      Entry(size_t id, const std::string& name, Entry* next)
        : id(id), name(name), next(next) {}
      size_t id;
      std::string name;
      Entry* next;
    };
    std::vector<Entry*> _table;
  };

  Symbol_Table::Symbol_Table()
    : _table(table_size, NULL)
  {}

  Symbol_Table::~Symbol_Table()
  {
    for (size_t i = 0; i != table_size; ++i){
      for (Entry* cur = _table[i]; cur != NULL;){
        Entry* temp = cur;
        cur = cur->next;
        delete temp;
      }
    }
  }

  Symbol Symbol_Table::Get_Symbol(const std::string& str)
  {
    size_t hash = Hash_String(str);
    size_t id = 0;
    for (Entry* current = _table[hash]; current != NULL; current = current->next){
      if (str == current->name){
        id = current->id;
        break;
      }
    }
    if (id == 0){
      Entry* front = _table[hash];
      id = (front == NULL) ? 1 : front->id + 1;
      Entry* new_entry = new Entry(id, str, front);
      _table[hash] = new_entry;
    }

    return Make_Symbol(hash, id);
  }

  Symbol Symbol_Table::Has_Symbol(const std::string& str) const
  {
    size_t hash = Hash_String(str);
    size_t id = 0;
    for (Entry* current = _table[hash]; current != NULL; current = current->next){
      if (str == current->name){
        id = current->id;
        break;
      }
    }
    if (id == 0)
      return null_symbol;
    else
      return Make_Symbol(hash, id);
  }

  const std::string& Symbol_Table::Get_Name(Symbol s) const
  {
    const static std::string unnamed("unnamed symbol");
    size_t hash = Symbol_Hash(s);
    if (hash >= table_size)
      return unnamed;
    size_t id = Symbol_ID(s);
    
    for (Entry* current = _table[hash]; current != NULL; current = current->next){
      if (current->id == id)
        return current->name;
    }
    return unnamed;
  }

  // The exists only one Symbol_Table, and this function is how the
  // functions below access it.
  Symbol_Table& Get_Table()
  {
    static Symbol_Table table;
    return table;
  }
}

Symbol Get_Symbol(const std::string& word)
{
  return Get_Table().Get_Symbol(word);
}

Symbol Find_Symbol(const std::string& word)
{
  return Get_Table().Has_Symbol(word);
}

const std::string& Get_Symbol_Name(Symbol s)
{
  return Get_Table().Get_Name(s);
}

}
