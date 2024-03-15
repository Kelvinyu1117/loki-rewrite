
module;
#include <cstddef>
#include <cstdint>
#include <string>
#include <type_traits>

export module type_list;

namespace loki {

namespace type_list {

namespace __detail {

struct EndOfList {};
template <typename T1, typename... Ts> struct TypeList;

template <> struct TypeList<EndOfList> {
  using head_t = EndOfList;
  using tail_t = EndOfList;
};

using EmptyList = TypeList<EndOfList>;

template <typename... Ts> consteval auto to_type_list() {
  if constexpr (sizeof...(Ts) == 0)
    return EmptyList{};
  else
    return TypeList<Ts...>{};
}

template <typename T1, typename... Ts> struct TypeList {
  using head_t = T1;
  using tail_t = decltype(to_type_list<Ts...>());
};

template <typename T>
concept is_type_list = requires(T) {
  typename T::head_t;
  typename T::tail_t;
};

template <is_type_list TList> consteval bool is_empty() {
  return std::is_same_v<typename TList::head_t, EndOfList> &&
         std::is_same_v<typename TList::tail_t, EndOfList>;
}

struct LengthFn {
private:
  template <is_type_list TList> consteval size_t impl() {
    if constexpr (is_empty<TList>())
      return 0;
    else
      return impl<typename TList::tail_t>() + 1;
  }

public:
  template <is_type_list TList> consteval size_t operator()() {
    return impl<TList>();
  }
};

template <is_type_list TList>
inline constexpr auto Length = LengthFn{}.template operator()<TList>();

struct AtFn {
private:
  template <is_type_list TList, size_t CurrentIndex, size_t Index>
  consteval auto impl() {
    if constexpr (CurrentIndex == Index)
      return TList::head_t;
    else
      return impl<TList::tail_t, CurrentIndex + 1, Index>();
  }

public:
  template <is_type_list TList, size_t Index, bool BoundCheck = true>
  consteval auto operator()() {
    auto len = Length<TList>;

    if constexpr (BoundCheck)
      static_assert(Index < len, "Index is out of range.");

    if constexpr (Index >= len)
      return EndOfList{};

    return impl<TList, 0, Index>;
  }
};

template <is_type_list TList, size_t I, bool BoundCheck = true>
using At = decltype(AtFn{}.template operator()<TList, I, BoundCheck>());

struct IndexofFn {
private:
  template <is_type_list TList, typename T, size_t CurrentIndex>
  consteval std::int64_t impl() {
    if constexpr (is_empty<TList>())
      return -1;
    else if constexpr (std::is_same_v<typename TList::head_t, T>)
      return CurrentIndex;
    else
      return impl<TList, T, CurrentIndex + 1>();
  }

public:
  template <is_type_list TList, typename T>
  consteval std::int64_t operator()() {
    if constexpr (is_empty<TList>())
      return -1;
    else if constexpr (std::is_same_v<typename TList::head_t, T>)
      return impl<TList, T, 0>();
  }
};

template <is_type_list TList, typename T>
inline constexpr auto IndexOf = IndexofFn{}.template operator()<TList, T>();

struct AppendFn {
public:
  template <typename T, typename... Ts>
  consteval auto operator()(TypeList<Ts...>) {
    if constexpr (is_empty<TypeList<Ts...>>())
      return TypeList<T>{};
    else
      return TypeList<Ts..., T>{};
  }
};

template <is_type_list TList, typename T>
using Append = decltype(AppendFn{}.template operator()<T>(TList{}));

struct PrependFn {
public:
  template <typename T, typename... Ts>
  consteval auto operator()(TypeList<Ts...>) {
    if constexpr (is_empty<TypeList<Ts...>>())
      return TypeList<T>{};
    else
      return TypeList<T, Ts...>{};
  }
};
template <is_type_list TList, typename T>
using Prepend = decltype(PrependFn{}.template operator()<T>(TList{}));

struct EraseFn {
private:
  template <is_type_list TList, typename T, bool All, is_type_list ResultList,
            bool IsRemoveDone>
  consteval auto impl() {
    if constexpr (is_empty<TList>()) {
      return ResultList{};
    } else {
      if constexpr (std::is_same_v<typename TList::head_t, T> &&
                    !IsRemoveDone) {
        if constexpr (All)
          return impl<typename TList::tail_t, T, All, ResultList, false>();
        else
          return impl<typename TList::tail_t, T, All, ResultList, true>();
      } else {
        return impl<typename TList::tail_t, T, All,
                    Append<ResultList, typename TList::head_t>, IsRemoveDone>();
      }
    }
  }

public:
  template <is_type_list TList, typename T, bool All>
  consteval auto operator()() {
    if constexpr (is_empty<TList>())
      return TList{};
    else
      return impl<TList, T, All, EmptyList, false>();
  }
};

template <is_type_list TList, typename T>
using Erase = decltype(EraseFn{}.template operator()<TList, T, false>());

template <is_type_list TList, typename T>
using EraseAll = decltype(EraseFn{}.template operator()<TList, T, true>());

struct NoDuplicatesFn {
private:
  template <is_type_list TList> consteval auto impl() {
    if constexpr (is_empty<TList>()) {
      return TList{};
    } else {
      using L1 = decltype(impl<typename TList::tail_t>());
      using L2 = Erase<L1, typename TList::head_t>;

      return Prepend<L2, typename TList::head_t>{};
    }
  }

public:
  template <is_type_list TList> consteval auto operator()() {
    if constexpr (is_empty<TList>())
      return TList{};
    else
      return impl<TList>();
  }
};

template <is_type_list TList>
using NoDuplicates = decltype(NoDuplicatesFn{}.template operator()<TList>());

struct ReplaceFn {
  template <is_type_list TList, typename T, typename U, bool All,
            is_type_list ResultList, bool isReplaceDone>
  consteval auto impl() {
    if constexpr (is_empty<TList>())
      return ResultList{};

    if (std::is_same_v<typename TList::head_t, T> && !isReplaceDone)
      if constexpr (All)
        return impl<TList::tail_t, T, U, All, Append<ResultList, U>, false>();
      else
        return impl<TList::tail_t, T, U, All, Append<ResultList, U>, true>();
    else
      return impl<TList::tail_t, T, U,
                  Append<ResultList, typename TList::head_t>, All>();
  }

  template <is_type_list TList, typename T, typename U, bool All>
  consteval auto operator()() {
    if constexpr (is_empty<TList>())
      return TList{};
    else
      return impl<TList, T, U, EmptyList, All>();
  }
};

template <is_type_list TList, typename T, typename U>
using Replace = decltype(ReplaceFn{}.template operator()<TList, T, U, false>());

template <is_type_list TList, typename T, typename U>
using ReplaceAll =
    decltype(ReplaceFn{}.template operator()<TList, T, U, true>());

struct ReverseFn {
  template <is_type_list TList> consteval auto impl() {
    if constexpr (is_empty<TList>())
      return TList{};
    else
      return Append<typename TList::tail_t, typename TList::head_t>{};
  }
  template <is_type_list TList> consteval auto operator()() {
    if constexpr (is_empty<TList>())
      return TList{};
    else
      return impl<TList>();
  }
};

template <is_type_list TList>
using Reverse = decltype(ReverseFn{}.template operator()<TList>());

struct MostDerivedFn {
  template <is_type_list TList, typename T> consteval auto impl() {
    if constexpr (is_empty<TList>())
      return T{};
    else if constexpr (std::is_same_v<typename TList::head_t, T> ||
                       std::is_base_of_v<T, typename TList::head_t>)
      return typename TList::head_t{};
    else
      return Append<typename TList::tail_t, typename TList::head_t>{};
  }

  template <is_type_list TList, typename T> consteval auto operator()() {
    if constexpr (is_empty<TList>())
      return T{};
    else
      return impl<TList, T>();
  }
};

template <is_type_list TList, typename T>
using MostDerived = decltype(MostDerivedFn{}.template operator()<TList, T>());

struct DerivedToFrontFn {
  template <is_type_list TList> consteval auto imp() {
    if constexpr (is_empty<TList>())
      return TList{};
    else {
      using TheMostDerived =
          MostDerived<typename TList::tail_t, typename TList::head_t>;

      using Temp = Replace<typename TList::tail_t, TheMostDerived,
                           typename TList::head_t>;

      return Prepend<TheMostDerived, decltype(imp<Temp>())>{};
    }
  }
  template <is_type_list TList> consteval auto operator()() {
    if constexpr (is_empty<TList>())
      return TList{};
    else
      return impl<TList>();
  }
};

template <is_type_list TList>
using DerivedToFront =
    decltype(DerivedToFrontFn{}.template operator()<TList>());

} // namespace __detail

template <typename... Ts> using TypeList = __detail::TypeList<Ts...>;

template <typename T> constexpr inline auto Length{__detail::Length<T>};

template <typename T, size_t I> using TypeAt = __detail::At<T, I, true>;

template <typename T, size_t I>
using TypeAtNonStrict = __detail::At<T, I, false>;

template <typename TList, typename T>
constexpr static inline auto IndexOf = __detail::IndexOf<TList, T>;

template <typename TList, typename T> using Append = __detail::Append<TList, T>;

template <typename TList, typename T> using Erase = __detail::Erase<TList, T>;

template <typename TList, typename T>
using EraseAll = __detail::EraseAll<TList, T>;

template <typename TList> using NoDuplicates = __detail::NoDuplicates<TList>;

template <typename TList> using Reverse = __detail::Reverse<TList>;

// test

static_assert(Length<TypeList<int>> == 1);
static_assert(Length<TypeList<int, double>> == 2);
static_assert(std::is_same_v<Append<TypeList<int>, int>, TypeList<int, int>>);
static_assert(std::is_same_v<Erase<TypeList<int, int>, int>, TypeList<int>>);
static_assert(
    std::is_same_v<EraseAll<TypeList<int, int>, int>, __detail::EmptyList>);
static_assert(std::is_same_v<EraseAll<TypeList<int, int, double>, int>,
                             TypeList<double>>);
static_assert(std::is_same_v<EraseAll<TypeList<int, double, int>, int>,
                             TypeList<double>>);
static_assert(std::is_same_v<EraseAll<TypeList<int, double, int, double>, int>,
                             TypeList<double, double>>);
static_assert(std::is_same_v<Append<TypeList<double, double>, std::string>,
                             TypeList<double, double, std::string>>);
static_assert(std::is_same_v<
              EraseAll<TypeList<int, double, int, double, std::string>, int>,
              TypeList<double, double, std::string>>);

static_assert(std::is_same_v<
              NoDuplicates<TypeList<int, double, int, double, std::string>>,
              TypeList<int, double, std::string>>);

static_assert(
    std::is_same_v<Reverse<TypeList<int, double>>, TypeList<double, int>>);

} // namespace type_list
} // namespace loki
