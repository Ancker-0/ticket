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
  template<class T> void update(Head &hd, size_t pos, const T &val);
  template<class T> sjtu::vector<T> getAll(const Head &hd);
  template<class T> T get(const Head &hd, size_t pos);
  template<class T> void putAll(Head &hd, const sjtu::vector<T> &vec);
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

template<class T> void fs_vector::update(Head &hd, size_t pos, const T &val) {
  assert(pos < hd.size);
  bf.putT(hd.pos + pos * sizeof(T), val);
}

template<class T>
T fs_vector::get(const Head &hd, size_t pos) {
  T ret;
  bf.getT(hd.pos + pos * sizeof(T), ret);
  return ret;
}

template<class T>
void fs_vector::putAll(Head &hd, const sjtu::vector<T> &vec) {
  resize<T>(hd, vec.size());
  bf.put(hd.pos, reinterpret_cast<const char*>(vec.data()), sizeof(T) * vec.size());
}

#endif //FS_VECTOR_H
