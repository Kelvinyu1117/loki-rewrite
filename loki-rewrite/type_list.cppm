module;

#include <cstddef>

export module type_list;

namespace loki {

namespace __detail {

template <typename... Ts> struct TypeList {};

template <typename... Ts> consteval size_t size(TypeList<Ts...> t) {
  return sizeof...(Ts);
}

} // namespace __detail

} // namespace loki
