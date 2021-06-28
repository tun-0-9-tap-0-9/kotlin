/*
 * Copyright 2010-2021 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

#ifndef RUNTIME_CPP_SUPPORT_SPAN_H
#define RUNTIME_CPP_SUPPORT_SPAN_H

#include <array>
#include <iterator>
#include <limits>
#include <type_traits>

#include "KAssert.h"

// Modelling https://en.cppreference.com/w/cpp/container/span from C++20.

namespace kotlin {
namespace std_support {

inline constexpr std::size_t dynamic_extent = std::numeric_limits<std::size_t>::max();

template <typename T, std::size_t Extent = dynamic_extent>
class span {
public:
    using element_type = T;
    using value_type = std::remove_cv_t<T>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using iterator = T*;
    using reverse_iterator = std::reverse_iterator<iterator>;

    static constexpr std::size_t extent = Extent;

    template <std::size_t Size = Extent, std::enable_if_t<Size == 0, std::nullptr_t> = nullptr>
    constexpr span() noexcept : data_(nullptr) {}

    // TODO: Instead of T* it should just be a `contiguos_iterator`.
    constexpr explicit span(T* first, size_type count) : data_(first) {
        RuntimeAssert(count == Extent, "Mismatched count=%zu, expected %zu", count, Extent);
    }

    // TODO: Instead of T* it should just be a `contiguos_iterator`.
    constexpr explicit span(T* first, T* last) : data_(first) {
        auto count = std::distance(first, last);
        RuntimeAssert(count == Extent, "Mismatched count=%zu, expected %zu", count, Extent);
    }

    constexpr span(element_type (&arr)[Extent]) noexcept : data_(arr) {}

    template <typename U, std::enable_if_t<std::is_convertible_v<U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr span(std::array<U, Extent>& arr) noexcept : data_(arr.data()) {}

    template <typename U, std::enable_if_t<std::is_convertible_v<const U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr span(const std::array<U, Extent>& arr) noexcept : data_(arr.data()) {}

    // TODO: Constructor from a range.

    template <typename U, std::enable_if_t<std::is_convertible_v<U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr span(const span<U, Extent>& source) noexcept : data_(source.data()) {}

    template <typename U, std::enable_if_t<std::is_convertible_v<U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr explicit span(const span<U, dynamic_extent>& source) noexcept : data_(source.data()) {
        RuntimeAssert(source.size() == Extent, "Mismatched count=%zu, expected %zu", source.size(), Extent);
    }

    constexpr span(const span& other) noexcept = default;

    constexpr span& operator=(const span& other) noexcept = default;

    constexpr iterator begin() const noexcept { return data_; }
    constexpr iterator end() const noexcept { return data_ + size(); }

    constexpr reverse_iterator rbegin() const noexcept { return std::reverse_iterator(end()); }
    constexpr reverse_iterator rend() const noexcept { return std::reverse_iterator(begin()); }

    constexpr reference front() const { return *begin(); }
    constexpr reference back() const { return *(end() - 1); }

    constexpr reference operator[](size_type idx) const { return data()[idx]; }

    constexpr pointer data() const noexcept { return data_; }

    constexpr size_type size() const noexcept { return Extent; }
    constexpr size_type size_bytes() const noexcept { return Extent * sizeof(element_type); }
    [[nodiscard]] constexpr bool empty() const noexcept { return Extent == 0; }

    template <std::size_t Count>
    constexpr span<element_type, Count> first() const {
        static_assert(Count <= Extent, "Count must be smaller than Extent");
        return span<element_type, Count>(data_, Count);
    }

    constexpr span<element_type, dynamic_extent> first(size_type count) const {
        RuntimeAssert(count <= size(), "count=%zu must be smaller than size()=%zu", count, size());
        return span<element_type, dynamic_extent>(data_, count);
    }

    template <std::size_t Count>
    constexpr span<element_type, Count> last() const {
        static_assert(Count <= Extent, "Count must be smaller than Extent");
        return span<element_type, Count>(data_ + size() - Count, Count);
    }

    constexpr span<element_type, dynamic_extent> last(size_type count) const {
        RuntimeAssert(count <= size(), "count=%zu must be smaller than size()=%zu", count, size());
        return span<element_type, dynamic_extent>(data_ + size() - count, count);
    }

    template <std::size_t Offset, std::size_t Count = dynamic_extent>
    constexpr span<element_type, Count != dynamic_extent ? Count : Extent - Offset> subspan() const {
        static_assert(Offset <= Extent, "Offset must be smaller than Extent");
        static_assert(Count == dynamic_extent || Count <= Extent - Offset, "Count must be smaller than Extent - Offset");

        return span<element_type, Count != dynamic_extent ? Count : Extent - Offset>(data_ + Offset, Count == dynamic_extent ? size() - Offset : Count);
    }

    constexpr span<element_type, dynamic_extent> subspan(size_type offset, size_type count = dynamic_extent) const {
        RuntimeAssert(offset <= size(), "offset=%zu must be smaller than size()=%zu", offset, size());
        RuntimeAssert(count == dynamic_extent || count <= size() - offset, "count=%zu must be smaller than size()=%zu - offset=%zu", count, size(), offset);

        if (count == dynamic_extent) {
            return span<element_type, dynamic_extent>(data() + offset, size() - offset);
        }

        return span<element_type, dynamic_extent>(data() + offset, count);
    }

private:
    T* data_;
};

template <typename T>
class span<T, dynamic_extent> {
public:
    using element_type = T;
    using value_type = std::remove_cv_t<T>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using iterator = T*;
    using reverse_iterator = std::reverse_iterator<iterator>;

    static constexpr std::size_t extent = dynamic_extent;

    constexpr span() noexcept : data_(nullptr), size_(0) {}

    // TODO: Instead of T* it should just be a `contiguos_iterator`.
    constexpr span(T* first, size_type count) : data_(first), size_(count) {}

    // TODO: Instead of T* it should just be a `contiguos_iterator`.
    constexpr span(T* first, T* last) : data_(first), size_(std::distance(first, last)) {}

    template <std::size_t N>
    constexpr span(element_type (&arr)[N]) noexcept : data_(arr), size_(N) {}

    template <typename U, std::size_t N, std::enable_if_t<std::is_convertible_v<U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr span(std::array<U, N>& arr) noexcept : data_(arr.data()), size_(N) {}

    template <typename U, std::size_t N, std::enable_if_t<std::is_convertible_v<const U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr span(const std::array<U, N>& arr) noexcept : data_(arr.data()), size_(N) {}

    // TODO: Constructor from a range.

    template <typename U, std::size_t N, std::enable_if_t<std::is_convertible_v<U(*)[], element_type(*)[]>, std::nullptr_t> = nullptr>
    constexpr span(const span<U, N>& source) noexcept : data_(source.data()), size_(source.size()) {}

    constexpr span(const span& other) noexcept = default;

    constexpr span& operator=(const span& other) noexcept = default;

    constexpr iterator begin() const noexcept { return data_; }
    constexpr iterator end() const noexcept { return data_ + size(); }

    constexpr reverse_iterator rbegin() const noexcept { return std::reverse_iterator(end()); }
    constexpr reverse_iterator rend() const noexcept { return std::reverse_iterator(begin()); }

    constexpr reference front() const { return *begin(); }
    constexpr reference back() const { return *(end() - 1); }

    constexpr reference operator[](size_type idx) const { return data()[idx]; }

    constexpr pointer data() const noexcept { return data_; }

    constexpr size_type size() const noexcept { return size_; }
    constexpr size_type size_bytes() const noexcept { return size_ * sizeof(element_type); }
    [[nodiscard]] constexpr bool empty() const noexcept { return size_ == 0; }

    template <std::size_t Count>
    constexpr span<element_type, Count> first() const {
        RuntimeAssert(Count <= size(), "Count=%zu must be smaller than size()=%zu", Count, size());
        return span<element_type, Count>(data_, Count);
    }

    constexpr span<element_type, dynamic_extent> first(size_type count) const {
        RuntimeAssert(count <= size(), "count=%zu must be smaller than size()=%zu", count, size());
        return span<element_type, dynamic_extent>(data_, count);
    }

    template <std::size_t Count>
    constexpr span<element_type, Count> last() const {
        RuntimeAssert(Count <= size(), "Count=%zu must be smaller than size()=%zu", Count, size());
        return span<element_type, Count>(data_ + size() - Count, Count);
    }

    constexpr span<element_type, dynamic_extent> last(size_type count) const {
        RuntimeAssert(count <= size(), "count=%zu must be smaller than size()=%zu", count, size());
        return span<element_type, dynamic_extent>(data_ + size() - count, count);
    }

    template <std::size_t Offset, std::size_t Count = dynamic_extent>
    constexpr span<element_type, Count> subspan() const {
        RuntimeAssert(Offset <= size(), "Offset=%zu must be smaller than size()=%zu", Offset, size());
        RuntimeAssert(Count == dynamic_extent || Count <= size() - Offset, "Count=%zu must be smaller than size()=%zu - Offset=%zu", Count, size(), Offset);

        return span<element_type, Count>(data_ + Offset, Count == dynamic_extent ? size() - Offset : Count);
    }

    constexpr span<element_type, dynamic_extent> subspan(size_type offset, size_type count = dynamic_extent) const {
        RuntimeAssert(offset <= size(), "offset=%zu must be smaller than size()=%zu", offset, size());
        RuntimeAssert(count == dynamic_extent || count <= size() - offset, "count=%zu must be smaller than size()=%zu - offset=%zu", offset, size(), offset);

        if (count == dynamic_extent) {
            return span<element_type, dynamic_extent>(data() + offset, size() - offset);
        }

        return span<element_type, dynamic_extent>(data() + offset, count);
    }

private:
    T* data_;
    size_type size_;
};

} // namespace std_support
} // namespace kotlin

#endif // RUNTIME_CPP_SUPPORT_SPAN_H
