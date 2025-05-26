#ifndef FS_VECTOR_H
#define FS_VECTOR_H
#include "fs.h"
#include "../vector.h"

class fs_vector {
public:
  struct Head {
    pos_t pos;
    size_t size, cap;
  };

private:
  Bfsp &bf;

  template<class T> void extend(Head &hd, size_t sz);

public:
  explicit fs_vector(Bfsp &bf_);
  template<class T> void push_back(Head &hd, const T &d);
  template<class T> void resize(Head &hd, size_t sz);
  template<class T> sjtu::vector<T> getAll(const Head &hd);
};

template<class T>
void fs_vector::extend(Head &hd, size_t sz) {
  if (sz <= hd.cap)
    return;
  sz = std::max(sz, hd.cap * 2);
  pos_t newpos = bf.allocEmpty(sizeof(T) * sz);
  bf.memcpy(newpos, hd.pos, sizeof(T) * hd.size);
  hd.cap = sz;
  hd.pos = newpos;
}

template<class T>
void fs_vector::resize(Head &hd, size_t sz) {
  if (sz <= hd.cap)
    return hd.size = sz, void();
  extend<T>(hd, sz);
  hd.size = sz;
}

template<class T>
void fs_vector::push_back(Head &hd, const T &d) {
  resize<T>(hd, hd.size + 1);
  bf.putT(hd.pos + (hd.size - 1) * sizeof(T), d);
}

template<class T>
sjtu::vector<T> fs_vector::getAll(const Head &hd) {
  sjtu::vector<T> ret;
  ret.resize(hd.size);
  bf.get(hd.pos, reinterpret_cast<char*>(ret.data()), sizeof(T) * hd.size);
  return ret;
}

#endif //FS_VECTOR_H
