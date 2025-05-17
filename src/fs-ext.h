//
// Created by user on 25-4-30.
//

#ifndef FS_EXT_H
#define FS_EXT_H

#include <vector>

#include "fs.h"

class Dumpable {
public:
  virtual void Dump(Bfsp &bf, pos_t pos) = 0;
  virtual void Read(Bfsp &bf, pos_t pos) = 0;
  virtual ~Dumpable() = default;

protected:
  Dumpable() = default;
};

template<class T>
class StaticDumpable : public Dumpable {
public:
  // StaticDumpable() = default;
  template<typename... Args>
  explicit StaticDumpable(Args &&... args)
    : value(std::forward<Args>(args)...) {
  }
  using value_type = T;
  value_type value;
  static constexpr size_t size = sizeof(T);
  void Dump(Bfsp &bf, pos_t pos) override;
  void Read(Bfsp &bf, pos_t pos) override;
};

template<class T>
void StaticDumpable<T>::Dump(Bfsp &bf, pos_t pos) {
  bf.putT(pos, value);
}

template<class T>
void StaticDumpable<T>::Read(Bfsp &bf, pos_t pos) {
  bf.getT(pos, value);
}

template <typename T>
concept DynamicDumpableConcept = requires(T x, const std::vector<std::byte> &buf) {
  { x.from_bytes(buf) };
  { x.to_bytes() } -> std::same_as<std::vector<std::byte>>;
};

template <class T>
requires DynamicDumpableConcept<T>
class DynamicDumpable : public Dumpable {
public:
  template<typename... Args>
  explicit DynamicDumpable(Args &&... args)
    : value(std::forward<Args>(args)...) {
  }
  using value_type = T;
  value_type value;
  void Dump(Bfsp &bf, pos_t pos) override;
  void Read(Bfsp &bf, pos_t pos) override;
};

template<class T> requires DynamicDumpableConcept<T>
void DynamicDumpable<T>::Dump(Bfsp &bf, pos_t pos) {
  std::vector<std::byte> buf = value.to_bytes();
  bf.putT(pos, (size_t)buf.size());
  bf.put(pos + sizeof(size_t), reinterpret_cast<const char *>(buf.data()), buf.size());
}

template<class T> requires DynamicDumpableConcept<T>
void DynamicDumpable<T>::Read(Bfsp &bf, pos_t pos) {
  size_t len;
  bf.getT(pos, len);
  std::vector<std::byte> buf(len);
  bf.get(pos + sizeof(size_t), reinterpret_cast<char *>(buf.data()), len);
  value.from_bytes(buf);
}

struct DumpableStringImplement {
  std::string value;
  std::vector<std::byte> to_bytes() {
    std::vector<std::byte> buf(value.size() + 1);
    strcpy(reinterpret_cast<char *>(buf.data()), value.data());
    return buf;
  }
  void from_bytes(const std::vector<std::byte> &buf) {
    Massert(!buf.empty() and buf.back() == std::byte{0}, "Unexpected bytes");
    value.resize(buf.size() - 1);
    strcpy(value.data(), reinterpret_cast<const char *>(buf.data()));
  }
};

template <class T>
requires DynamicDumpableConcept<T>
struct DynamicDumpableVectorImplement {
  using value_type = T;
  std::vector<value_type> value;

  template<typename... Args>
  explicit DynamicDumpableVectorImplement(Args &&... args)
    : value(std::forward<Args>(args)...) {
  }

  std::vector<std::byte> to_bytes() {
    std::vector<std::vector<std::byte>> buf(value.size());
    std::vector<std::byte> ret;
    for (size_t i = 0; i < value.size(); ++i)
      buf[i] = value[i].to_bytes();
    for (size_t i = 0; i < value.size(); ++i) {
      size_t len = buf[i].size();
      static_assert(sizeof(size_t) == 8);
      for (int j = 0; j < 8; ++j, len >>= 8)
        ret.push_back(std::byte{len});
      for (auto b : buf[i])
        ret.push_back(b);
    }
    return ret;
  }

  void from_bytes(std::vector<std::byte> buf) {
    value.clear();
    size_t p = 0;
    std::vector<std::byte> tmp;
    while (p < buf.size()) {
      Massert(p + 8 <= buf.size(), "Unexpected length");
      size_t len = 0;
      for (int j = 0; j < 8; ++j)
        len |= (size_t)buf[p + j] << (j * 8);
      Massert(p + 8 + len <= buf.size(), "Unexpected length");
      tmp.clear();
      for (size_t j = 0; j < len; ++j)
        tmp.push_back(buf[p + 8 + j]);
      value.push_back(value_type{});
      value.back().from_bytes(tmp);
      p += 8 + len;
    }
  }
};

using DumpableString = DynamicDumpable<DumpableStringImplement>;
template <DynamicDumpableConcept T> using DumpableVector = DynamicDumpableVectorImplement<T>;

#endif //FS_EXT_H