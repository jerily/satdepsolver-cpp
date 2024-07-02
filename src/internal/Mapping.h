#ifndef MAPPING_H
#define MAPPING_H

#include <vector>
#include <array>
#include <optional>
#include <stdexcept>

template<typename TId, typename TValue>
class Mapping {
public:
    static const int VALUES_PER_CHUNK = 128;

    Mapping() : len(0) {}

    static std::pair<int, int> chunk_and_offset(int id) {
        int chunk = id / VALUES_PER_CHUNK;
        int offset = id % VALUES_PER_CHUNK;
        return {chunk, offset};
    }

    void insert(TId id, TValue value) {
        auto [chunk, offset] = chunk_and_offset(id.to_usize());

        if (chunk >= chunks.size()) {
            chunks.resize(chunk + 1);
        }
        chunks[chunk][offset] = value;
        len++;
    }

    std::optional<TValue> get(TId id) const {
        auto [chunk, offset] = chunk_and_offset(id.to_usize());
        if (chunk >= chunks.size()) {
            return std::nullopt;
        }
        return chunks[chunk][offset];
    }

    TValue& get_unchecked(TId id) {
        auto [chunk, offset] = chunk_and_offset(id.to_usize());
        return chunks[chunk][offset];
    }

    int size() const {
        return len;
    }

    bool empty() const {
        return len == 0;
    }

    class MappingIter {
    public:
        MappingIter(const Mapping<TId, TValue>* mapping) : mapping(mapping), offset(0) {}

        bool has_next() const {
            return offset < mapping->size();
        }

        std::optional<std::pair<TId, TValue>> next() {
            while (offset < mapping->size()) {
                auto [chunk, offset] = Mapping<TId, TValue>::chunk_and_offset(this->offset);
                TId id = TId::from_usize(this->offset);
                this->offset += 1;

                if (auto value = mapping->chunks[chunk][offset]) {
                    return std::make_pair(id, value.value());
                }
            }
            return std::nullopt;
        }

    private:
        const Mapping<TId, TValue>* mapping;
        int offset;
    };


    MappingIter iter() const {
        return MappingIter(this);
    }

private:
    std::vector<std::array<std::optional<TValue>, VALUES_PER_CHUNK>> chunks;
    int len;
};

#endif // MAPPING_H