#ifndef FROZEN_MAP_H
#define FROZEN_MAP_H

#include <unordered_map>
#include <memory>
#include <vector>
#include <utility>
#include <optional>

template<typename K, typename V>
class FrozenMap {
public:
    FrozenMap() : map(std::make_shared<std::unordered_map<K, V>>()) {}

    FrozenMap(std::shared_ptr<std::unordered_map<K, V>> map) : map(map) {}

    void insert(K key, V value) {
        map->insert(std::make_pair(key, value));
    }

    std::optional<V> get(const K& key) const {
        auto it = map->find(key);
        if (it != map->end()) {
            return it->second;
        }
        return std::nullopt;
    }
private:
    std::shared_ptr<std::unordered_map<K, V>> map;
};


#endif // FROZEN_MAP_H