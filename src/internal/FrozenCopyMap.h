#ifndef FROZEN_COPY_MAP_H
#define FROZEN_COPY_MAP_H

#include <unordered_map>
#include <functional>
#include <optional>

template <typename K, typename V, typename Hash = std::hash<K>, typename Pred = std::equal_to<K>>
class FrozenCopyMap {
public:
    FrozenCopyMap() : map() {}

    std::optional<V> insert_copy(const K& k, const V& v) {
        auto it = map.insert({k, v});
        if (!it.second) {
            return it.first->second;
        }
        return std::nullopt;
    }

    std::optional<V> get_copy(const K& k) {
        auto it = map.find(k);
        if (it != map.end()) {
            return it->second;
        }
        return std::nullopt;
    }

private:
    std::unordered_map<K, V, Hash, Pred> map;
};

#endif // FROZEN_COPY_MAP_H