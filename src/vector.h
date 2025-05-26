#ifndef SJTU_VECTOR_HPP
#define SJTU_VECTOR_HPP

#include <climits>
#include <cassert>
#include <memory>
#include <cstring>
#include <cstddef>

namespace sjtu
{
/**
 * a data container like std::vector
 * store data in a successive memory and support random access.
 */
template<typename T>
class vector
{
	char *p;
	int _size, capacity;

	void check_bound(int pos) const {
		if (pos < 0 or pos >= _size)
			assert(false);
	}

	void check_unempty() const {
		if (!_size)
			assert(false);
	}

	T *Tptr() {
		return reinterpret_cast<T*>(p);
	}

	const T *Tptr() const {
		return reinterpret_cast<const T*>(p);
	}

	void map(const auto &f) {
		for (int i = 0; i < _size; ++i)
			Tptr()[i] = f(Tptr()[i]);
	}

	void extend(int nsz) {
		if (nsz > capacity) {
			if (capacity == 0) {
				capacity = nsz;
				p = new char[sizeof(T) * capacity];
			} else {
				while (capacity < nsz)
					capacity <<= 1;
				char *np = new char[sizeof(T) * capacity];
        for (size_t i = 0; i < _size; ++i) {
          new (np + sizeof(T) * i) T(std::move(Tptr()[i]));
          Tptr()[i].~T();
        }
				delete[] p;
				p = np;
			}
		}
		_size = nsz;
	}

public:
	/**
	 * TODO
	 * a type for actions of the elements of a vector, and you should write
	 *   a class named const_iterator with same interfaces.
	 */
	/**
	 * you can see RandomAccessIterator at CppReference for help.
	 */
	class const_iterator;
	class iterator
	{
	// The following code is written for the C++ type_traits library.
	// Type traits is a C++ feature for describing certain properties of a type.
	// For instance, for an iterator, iterator::value_type is the type that the
	// iterator points to.
	// STL algorithms and containers may use these type_traits (e.g. the following
	// typedef) to work properly. In particular, without the following code,
	// @code{std::sort(iter, iter1);} would not compile.
	// See these websites for more information:
	// https://en.cppreference.com/w/cpp/header/type_traits
	// About value_type: https://blog.csdn.net/u014299153/article/details/72419713
	// About iterator_category: https://en.cppreference.com/w/cpp/iterator
	public:
		using difference_type = std::ptrdiff_t;
		using value_type = T;
		using pointer = T*;
		using reference = T&;
		using iterator_category = std::output_iterator_tag;
		friend class vector;
		friend class const_iterator;

	private:
		/**
		 * TODO add data members
		 *   just add whatever you want.
		 */
		vector &self;
		int pos;
		iterator(vector &_self, int _pos) : self(_self), pos(_pos) {}
		// iterator() = default;

	public:
		/**
		 * return a new iterator which pointer n-next elements
		 * as well as operator-
		 */
		iterator operator+(const int &n) const
		{
			return { self, pos + n };
		}
		iterator operator-(const int &n) const
		{
			return { self, pos - n };
		}
		// return the distance between two iterators,
		// if these two iterators point to different vectors, assert(false);
		int operator-(const iterator &rhs) const
		{
			if (&self != &rhs.self)
				assert(false);
			return pos - rhs.pos;
		}
		iterator& operator+=(const int &n)
		{
			pos += n;
			return *this;
		}
		iterator& operator-=(const int &n)
		{
			pos -= n;
			return *this;
		}
		/**
		 * TODO iter++
		 */
		iterator operator++(int) { pos++; return iterator(self, pos - 1); }
		/**
		 * TODO ++iter
		 */
		iterator& operator++() { ++pos; return *this; }
		/**
		 * TODO iter--
		 */
		iterator operator--(int) { pos--; return iterator(self, pos - 1); }
		/**
		 * TODO --iter
		 */
		iterator& operator--() { --pos; return *this; }
		/**
		 * TODO *it
		 */
		T& operator*() const{ return self[pos]; }
		/**
		 * a operator to check whether two iterators are same (pointing to the same memory address).
		 */
		bool operator==(const iterator &rhs) const { return &self == &rhs.self and pos == rhs.pos; }
		bool operator==(const const_iterator &rhs) const { return &self == &rhs.self and pos == rhs.pos; }
		/**
		 * some other operator for iterator.
		 */
		bool operator!=(const iterator &rhs) const { return not(*this == rhs); }
		bool operator!=(const const_iterator &rhs) const { return not(*this == rhs); }
	};
	/**
	 * TODO
	 * has same function as iterator, just for a const object.
	 */
	class const_iterator
	{
	public:
		using difference_type = std::ptrdiff_t;
		using value_type = T;
		using pointer = T*;
		using reference = T&;
		using iterator_category = std::output_iterator_tag;
		friend class iterator;
		friend class vector;

		const_iterator operator+(const int &n) const
		{
			return { self, pos + n };
		}
		const_iterator operator-(const int &n) const
		{
			return { self, pos - n };
		}

		int operator-(const const_iterator &rhs) const
		{
			if (&self != &rhs.self)
				assert(false);
			return pos - rhs.pos;
		}
		const_iterator& operator+=(const int &n)
		{
			pos += n;
			return *this;
		}
		const_iterator& operator-=(const int &n)
		{
			pos -= n;
			return *this;
		}
		const_iterator operator++(int) { pos++; return const_iterator(self, pos - 1); }
		const_iterator& operator++() { ++pos; return *this; }
		const_iterator operator--(int) { pos--; return const_iterator(self, pos - 1); }
		const_iterator& operator--() { --pos; return *this; }
		const T& operator*() const{ return self[pos]; }

		bool operator==(const iterator &rhs) const { return &self == &rhs.self and pos == rhs.pos; }
		bool operator==(const const_iterator &rhs) const { return &self == &rhs.self and pos == rhs.pos; }
		/**
		 * some other operator for iterator.
		 */
		bool operator!=(const iterator &rhs) const { return not(*this == rhs); }
		bool operator!=(const const_iterator &rhs) const { return not(*this == rhs); }

	private:
		const vector &self;
		int pos;
		const_iterator(const vector &_self, int _pos) : self(_self), pos(_pos) {}
		/*TODO*/

	};

  void resize(int nsz) { extend(nsz); }
  T *data() { return reinterpret_cast<T*>(p); }
  const T *data() const { return reinterpret_cast<T*>(p); }

	/**
	 * TODO Constructs
	 * At least two: default constructor, copy constructor
	 */
	vector() : p(nullptr), _size(0), capacity(0) {}
	vector(const vector &other) {
		_size = capacity = other.size();
		if (other.size()) {
			p = new char[other.size() * sizeof(T)];
			for (int i = 0; i < other.size(); ++i)
				std::construct_at(Tptr() + i, other.Tptr()[i]);
		} else
			p = nullptr;
	}
	/**
	 * TODO Destructor
	 */
	~vector() {
		if (_size)
			std::destroy_n(Tptr(), _size);
		if (capacity)
			delete[] p;
	}
	/**
	 * TODO Assignment operator
	 */
	vector &operator=(const vector &other) {
		if (this == &other)
			return *this;
		std::destroy_n(Tptr(), _size);
		extend(other.size());
		for (int i = 0; i < other.size(); ++i)
			std::construct_at(Tptr() + i, other.Tptr()[i]);
		return *this;
	}
	/**
	 * assigns specified element with bounds checking
	 * assert(false);
	 */
	T & at(const size_t &pos) {
		check_bound(pos);
		return operator[](pos);
	}
	const T & at(const size_t &pos) const {
		check_bound(pos);
		return operator[](pos);
	}
	/**
	 * assigns specified element with bounds checking
	 * assert(false);
	 * !!! Pay attentions
	 *   In STL this operator does not check the boundary but I want you to do.
	 */
	T & operator[](const size_t &pos) {
		return Tptr()[pos];
	}
	const T & operator[](const size_t &pos) const {
		return Tptr()[pos];
	}
	/**
	 * access the first element.
	 * assert(false);
	 */
	const T & front() const {
		check_unempty();
		return operator[](0);
	}
	/**
	 * access the last element.
	 * assert(false);
	 */
	const T & back() const {
		check_unempty();
		return operator[](_size - 1);
	}
	/**
	 * returns an iterator to the beginning.
	 */
	iterator begin() { return iterator(*this, 0); }
	const_iterator begin() const { return const_iterator(*this, 0); }
	const_iterator cbegin() const { return const_iterator(*this, 0); }
	/**
	 * returns an iterator to the end.
	 */
	iterator end() { return iterator(*this, _size); }
	const_iterator end() const { return const_iterator(*this, _size); }
	const_iterator cend() const { return const_iterator(*this, _size); }
	/**
	 * checks whether the container is empty
	 */
	bool empty() const {
		return _size == 0;
	}
	/**
	 * returns the number of elements
	 */
	size_t size() const {
		return _size;
	}
	/**
	 * clears the contents
	 */
	void clear() {
		std::destroy_n(Tptr(), _size);
		_size = 0;
	}
	/**
	 * inserts value before pos
	 * returns an iterator pointing to the inserted value.
	 */
	iterator insert(iterator pos, const T &value) {
		return insert(pos.pos, value);
	}
	/**
	 * inserts value at index ind.
	 * after inserting, this->at(ind) == value
	 * returns an iterator pointing to the inserted value.
	 * assert(false);
	 */
	iterator insert(const size_t &ind, const T &value) {
		extend(_size + 1);
		check_bound(ind);
		// std::memmove(Tptr() + ind + 1, Tptr() + ind, sizeof(T) * (_size - ind - 1));
		std::construct_at(Tptr() + ind, value);
		return iterator(*this, ind);
	}
	/**
	 * removes the element at pos.
	 * return an iterator pointing to the following element.
	 * If the iterator pos refers the last element, the end() iterator is returned.
	 */
	iterator erase(iterator pos) {
		return erase(pos.pos);
	}
	/**
	 * removes the element with index ind.
	 * return an iterator pointing to the following element.
	 * assert(false);
	 */
	iterator erase(const size_t &ind) {
		check_bound(ind);
		std::destroy_at(Tptr() + ind);
		std::memmove(Tptr() + ind, Tptr() + ind + 1, sizeof(T) * (_size - ind - 1));
		extend(_size - 1);
		return iterator(*this, ind);
	}
	/**
	 * adds an element to the end.
	 */
	void push_back(const T &value) {
		insert(_size, value);
	}
	/**
	 * remove the last element from the end.
	 * assert(false);
	 */
	void pop_back() {
		check_unempty();
		erase(_size - 1);
	}
};

template <typename T, typename F>
void sort(T *l, T *r, const F &cmp) {
  if (l + 1 >= r)
    return;
  int idx = rand() % (r - l);
  std::swap(*l, *(l + idx));
  vector<T> left, right;
  for (T *p = l + 1; p < r; ++p)
    if (cmp(*l, *p))
      right.push_back(*p);
    else
      left.push_back(*p);
  std::swap(*l, *(l + left.size()));
  T *p = l;
  for (int i = 0; i < (int)left.size(); ++i)
    *(p++) = left[i];
  ++p;
  for (int i = 0; i < (int)right.size(); ++i)
    *(p++) = right[i];
  assert(p == r);
  sort(l, l + left.size(), cmp);
  sort(l + left.size() + 1, r, cmp);
}

}

#endif
