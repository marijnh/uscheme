#ifndef NONCOPYABLE_HPP
#define NONCOPYABLE_HPP

// stolen from boost/noncopyable.hpp

class noncopyable
{
protected:
  noncopyable() {}
  ~noncopyable() {}
private:
  noncopyable( const noncopyable& );
  const noncopyable& operator=( const noncopyable& );
};

#endif //NONCOPYABLE_HPP
