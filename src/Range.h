#ifndef RANGE_h
#define RANGE_h

// This file was originally taken from:
// <https://raw.githubusercontent.com/pubgrub-rs/pubgrub/dev/src/range.rs>
// SPDX-License-Identifier: MPL-2.0

// Ranges are constraints defining sets of versions. They are a convenience struct that implements
// [`VersionSet`] for any version type that implements [`Ord`] + [`Clone`].
//
// Concretely, those constraints correspond to any set of versions representable as the
// concatenation, union, and complement of the ranges building blocks.
//
// Those building blocks are:
//  - [empty()](Range::empty): the empty set
//  - [full()](Range::full): the set of all possible versions
//  - [singleton{v}](Range::singleton): the set containing only the version v
//  - [higher_than{v}](Range::higher_than): the set defined by `v <= versions`
//  - [strictly_higher_than{v}](Range::strictly_higher_than): the set defined by `v < versions`
//  - [lower_than{v}](Range::lower_than): the set defined by `versions <= v`
//  - [strictly_lower_than{v}](Range::strictly_lower_than): the set defined by `versions < v`
//  - [between(v1, v2)](Range::between): the set defined by `v1 <= versions < v2`
//
// Ranges can be created from any type that implements [`Ord`] + [`Clone`].

#include <utility>
#include <variant>
#include <vector>
#include <stdexcept>
#include <optional>

template <typename V>
struct Excluded {
    V value;

    size_t hash() const {
        return std::hash<V>{}(value);
    }

    bool operator==(const Excluded &other) const {
        return value == other.value;
    }
};

template <typename V>
struct Included {
    V value;

    size_t hash() const {
        return std::hash<V>{}(value);
    }

    bool operator==(const Included &other) const {
        return value == other.value;
    }
};

struct Unbounded {
    size_t hash() const {
        return 0;
    }

    bool operator==(const Unbounded &other) const {
        return true;
    }
};

template <typename V>
using BoundVariant = std::variant<Excluded<V>, Included<V>, Unbounded>;


namespace std {
    template<typename V>
    struct hash<BoundVariant<V>> {
        std::size_t operator()(const BoundVariant<V> &bound) const {
            return std::visit([](const auto &v) {
                return v.hash();
            }, bound);
        }
    };
}

template <typename V>
using Interval = std::pair<BoundVariant<V>, BoundVariant<V>>;

template <typename V>
class Range {
public:

    using ValueType = V;

    std::vector<Interval<V>> segments;

    Range() = default;

    explicit Range(std::vector<Interval<V>> segments) : segments(std::move(segments)) {}

    // Empty set of versions
    static Range empty() {
        return Range();
    }

    // Set of all possible versions
    static Range full() {
        return Range({{Unbounded(), Unbounded()}});
    }

    // Set of all versions higher or equal to some version
    static Range higher_than(const V &v) {
        return Range({{Included<V>{v}, Unbounded()}});
    }

    // Set of all versions higher to some version
    static Range strictly_higher_than(const V &v) {
        return Range({{Excluded<V>{v}, Unbounded()}});
    }

    // Set of all versions lower to some version
    static Range strictly_lower_than(const V &v) {
        return Range({{Unbounded(), Excluded<V>{v}}});
    }

    // Set of all versions lower or equal to some version
    static Range lower_than(const V &v) {
        return Range({{Unbounded(), Included<V>{v}}});
    }

    // Set of versions greater or equal to `v1` but less than `v2`.
    static Range between(V v1, V v2) {
        return Range({{Included<V>{v1}, Excluded<V>{v2}}});
    }

    // Set containing exactly one version
    static Range singleton(const V &v) {
        return Range({{Included<V>{v}, Included<V>{v}}});
    }

    // Returns the complement of this Range.
    Range complement() const {

        if (segments.empty()) {
            return full();
        }

        return std::visit([this](const auto &segment) {
            auto [low, high] = segment;
            using T_LOW = std::decay_t<decltype(low)>;
            using T_HIGH = std::decay_t<decltype(high)>;

            if constexpr (std::is_same_v<T_LOW, Unbounded> && std::is_same_v<T_HIGH, Unbounded>) {
                return empty();
            } else if constexpr (std::is_same_v<T_LOW, Included<V>> && std::is_same_v<T_HIGH, Unbounded>) {
                return strictly_lower_than(low.value);
            } else if constexpr (std::is_same_v<T_LOW, Excluded<V>> && std::is_same_v<T_HIGH, Unbounded>) {
                return lower_than(low.value);
            } else if constexpr (std::is_same_v<T_LOW, Unbounded> && std::is_same_v<T_HIGH, Included<V>>) {
                return negate_segments(Excluded<V>(high.value),
                                       std::vector<Interval<V>>(segments.begin() + 1, segments.end()));
            } else if constexpr (std::is_same_v<T_LOW, Unbounded> && std::is_same_v<T_HIGH, Excluded<V>>) {
                return negate_segments(Included<V>(high.value),
                                       std::vector<Interval<V>>(segments.begin() + 1, segments.end()));
            } else {
                return negate_segments(Unbounded(), segments);
            }
        }, segments[0]);
    }

    // Helper function performing the negation of intervals in segments.
    Range negate_segments(BoundVariant<V> start, std::vector<Interval<V>> segments_to_negate) {
        std::vector<Interval<V>> complement_segments;
        for (auto [v1, v2] : segments_to_negate) {
            auto bound = std::visit([](const auto &v) {
                if constexpr (std::is_same_v<decltype(v), Included<V>>) {
                    return Excluded<V>(v.value);
                } else if constexpr (std::is_same_v<decltype(v), Excluded<V>>) {
                    return Included<V>(v.value);
                } else {
                    throw std::runtime_error("Unreachable");
                }
            }, v1);
            complement_segments.push_back({start, bound});
            start = std::visit([](const auto &v) {
                if constexpr (std::is_same_v<decltype(v), Included<V>>) {
                    return Excluded<V>(v.value);
                } else if constexpr (std::is_same_v<decltype(v), Excluded<V>>) {
                    return Included<V>(v.value);
                } else {
                    return Unbounded();
                }
            }, v2);
        }
        if (!std::holds_alternative<Unbounded>(start)) {
            complement_segments.push_back({start, Unbounded()});
        }

        return Range(complement_segments);
    }

    // Convert to something that can be used with
    // [BTreeMap::range](std::collections::BTreeMap::range).
    // All versions contained in self, will be in the output,
    // but there may be versions in the output that are not contained in self.
    // Returns None if the range is empty.

    std::optional<std::pair<BoundVariant<V>, BoundVariant<V>>> bounding_range() const {
        if (segments.empty()) {
            return std::nullopt;
        }
        auto start = segments.front().first;
        auto end = segments.back().second;
        return std::make_pair(start, end);
    }

    // Returns true if the this Range contains the specified value.
    bool contains(const V &v) const {
        for (auto [start, end]: segments) {
            auto result = std::visit([&v](const auto &start_arg, const auto &end_arg) {
                using T_start = std::decay_t<decltype(start_arg)>;
                using T_end = std::decay_t<decltype(end_arg)>;

                if constexpr (std::is_same_v<T_start, Unbounded> && std::is_same_v<T_end, Unbounded>) {
                    return true;
                } else if constexpr (std::is_same_v<T_start, Unbounded> && std::is_same_v<T_end, Included<V>>) {
                    auto high = std::any_cast<Included<V>>(end_arg);
                    return v <= high.value;
                } else if constexpr (std::is_same_v<T_start, Unbounded> && std::is_same_v<T_end, Excluded<V>>) {
                    auto high = std::any_cast<Excluded<V>>(end_arg);
                    return v < high.value;
                } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Unbounded>) {
                    auto low = std::any_cast<Included<V>>(start_arg);
                    return v >= low.value;
                } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Included<V>>) {
                    auto low = std::any_cast<Included<V>>(start_arg);
                    auto high = std::any_cast<Included<V>>(end_arg);
                    return v >= low.value && v <= high.value;
                } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Excluded<V>>) {
                    auto low = std::any_cast<Included<V>>(start_arg);
                    auto high = std::any_cast<Excluded<V>>(end_arg);
                    return v >= low.value && v < high.value;
                } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Unbounded>) {
                    auto low = std::any_cast<Excluded<V>>(start_arg);
                    return v > low.value;
                } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Included<V>>) {
                    auto low = std::any_cast<Excluded<V>>(start_arg);
                    auto high = std::any_cast<Included<V>>(end_arg);
                    return v > low.value && v <= high.value;
                } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Excluded<V>>) {
                    auto low = std::any_cast<Excluded<V>>(start_arg);
                    auto high = std::any_cast<Excluded<V>>(end_arg);
                    return v > low.value && v < high.value;
                }
                return false;
            }, start, end);

            if (result) {
                return true;
            }
        }
        return false;
    }

    // Computes the union of this `Range` and another.
    Range union_with(const Range &other) const {
        return complement().intersection(other.complement()).complement();
    }

    // Computes the intersection of two sets of versions.
    Range intersection_with(const Range &other) const {
        std::vector<Interval<V>> result_segments;
        auto left_iter = segments.cbegin();
        auto right_iter = other.segments.cbegin();
        while (left_iter != segments.cend() && right_iter != other.segments.cend()) {
            auto [left_start, left_end] = *left_iter;
            auto [right_start, right_end] = *right_iter;

            auto start = std::visit([&left_start, &right_start](const auto &l, const auto &r) {
                if constexpr (std::is_same_v<decltype(l), Included<V>> && std::is_same_v<decltype(r), Included<V>>) {
                    return Included<V>(std::max(l.value, r.value));
                } else if constexpr (std::is_same_v<decltype(l), Excluded<V>> && std::is_same_v<decltype(r), Excluded<V>>) {
                    return Excluded<V>(std::max(l.value, r.value));
                } else if constexpr (std::is_same_v<decltype(l), Included<V>> && std::is_same_v<decltype(r), Excluded<V>>) {
                    if (l.value <= r.value) {
                        return Excluded<V>(r.value);
                    } else {
                        return l;
                    }
                } else if constexpr (std::is_same_v<decltype(l), Excluded<V>> && std::is_same_v<decltype(r), Included<V>>) {
                    if (r.value < l.value) {
                        return Included<V>(r.value);
                    } else {
                        return l;
                    }
                } else if constexpr (std::is_same_v<decltype(l), Unbounded> && std::is_same_v<decltype(r), Included<V>>) {
                    return r;
                } else if constexpr (std::is_same_v<decltype(l), Included<V>> && std::is_same_v<decltype(r), Unbounded>) {
                    return l;
                } else {
                    throw std::runtime_error("Unreachable");
                }
            }, left_start, right_start);

            auto end = std::visit([&left_end, &right_end](const auto &l, const auto &r) {
                if constexpr (std::is_same_v<decltype(l), Included<V>> && std::is_same_v<decltype(r), Included<V>>) {
                    return Included<V>(std::min(l.value, r.value));
                } else if constexpr (std::is_same_v<decltype(l), Excluded<V>> && std::is_same_v<decltype(r), Excluded<V>>) {
                    return Excluded<V>(std::min(l.value, r.value));
                } else if constexpr (std::is_same_v<decltype(l), Included<V>> && std::is_same_v<decltype(r), Excluded<V>>) {
                    if (l.value >= r.value) {
                        return Excluded<V>(r.value);
                    } else {
                        return l;
                    }
                } else if constexpr (std::is_same_v<decltype(l), Excluded<V>> && std::is_same_v<decltype(r), Included<V>>) {
                    if (r.value > l.value) {
                        return Included<V>(r.value);
                    } else {
                        return l;
                    }
                } else if constexpr (std::is_same_v<decltype(l), Unbounded> && std::is_same_v<decltype(r), Included<V>>) {
                    return r;
                } else if constexpr (std::is_same_v<decltype(l), Included<V>> && std::is_same_v<decltype(r), Unbounded>) {
                    return l;
                } else {
                    throw std::runtime_error("Unreachable");
                }
            }, left_end, right_end);

            if (valid_segment(start, end)) {
                result_segments.push_back({start, end});
            }

            if (end == left_end) {
                ++left_iter;
            }

            if (end == right_end) {
                ++right_iter;
            }
        }
        return Range(result_segments);
    }

    Range clone() const {
        return Range(segments);
    }

    bool operator==(const Range &other) const {
        return segments == other.segments;
    }

    friend std::ostream &operator<<(std::ostream &os, const Range<V> &range) {
        if (range.segments.empty()) {
            os << "∅";
        } else {
            for (size_t i = 0; i < range.segments.size(); ++i) {
                auto [start, end] = range.segments[i];
                os << std::visit([](auto &&start_arg, auto &&end_arg) -> std::string {
                    using T_start = std::decay_t<decltype(start_arg)>;
                    using T_end = std::decay_t<decltype(end_arg)>;

                    if constexpr (std::is_same_v<T_start, Unbounded> && std::is_same_v<T_end, Unbounded>) {
                        return "*";
                    } else if constexpr (std::is_same_v<T_start, Unbounded> && std::is_same_v<T_end, Included<V>>) {
                        auto e = std::any_cast<Included<V>>(end_arg);
                        return "<=" + e.value;
                    } else if constexpr (std::is_same_v<T_start, Unbounded> && std::is_same_v<T_end, Excluded<V>>) {
                        auto e = std::any_cast<Excluded<V>>(end_arg);
                        return "<" + e.value;
                    } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Unbounded>) {
                        auto s = std::any_cast<Included<V>>(start_arg);
                        return ">=" + s.value;
                    } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Included<V>>) {
                        auto s = std::any_cast<Included<V>>(start_arg);
                        auto e = std::any_cast<Included<V>>(end_arg);
                        auto v_start = s.value;
                        auto v_end = e.value;
                        if (v_start == v_end) {
                            return "" + v_start;
                        } else {
                            return ">=" + v_start + ", <=" + v_end;
                        }
                    } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Excluded<V>>) {
                        auto s = std::any_cast<Included<V>>(start_arg);
                        auto e = std::any_cast<Excluded<V>>(end_arg);
                        return ">=" + s.value + ", <" + e.value;
                    } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Unbounded>) {
                        auto s = std::any_cast<Excluded<V>>(start_arg);
                        return ">" + s.value;
                    } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Included<V>>) {
                        auto s = std::any_cast<Excluded<V>>(start_arg);
                        auto e = std::any_cast<Included<V>>(end_arg);
                        return ">" + s.value + ", <=" + e.value;
                    } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Excluded<V>>) {
                        auto s = std::any_cast<Excluded<V>>(start_arg);
                        auto e = std::any_cast<Excluded<V>>(end_arg);
                        return ">" + s.value + ", <" + e.value;
                    }
                    return "";
                }, start, end);
            }
        }
        return os;
    }
private:

    void check_invariants() {
        for (size_t i = 0; i < segments.size() - 1; ++i) {
            auto [l_end, r_start] = std::make_pair(segments[i].second, segments[i + 1].first);
            if (std::holds_alternative<Included<V>>(l_end) && std::holds_alternative<Included<V>>(r_start)) {
                if (std::get<Included<V>>(l_end).value >= std::get<Included<V>>(r_start).value) {
                    throw std::runtime_error("Invalid range");
                }
            } else if (std::holds_alternative<Included<V>>(l_end) && std::holds_alternative<Excluded<V>>(r_start)) {
                if (std::get<Included<V>>(l_end).value >= std::get<Excluded<V>>(r_start).value) {
                    throw std::runtime_error("Invalid range");
                }
            } else if (std::holds_alternative<Excluded<V>>(l_end) && std::holds_alternative<Included<V>>(r_start)) {
                if (std::get<Excluded<V>>(l_end).value >= std::get<Included<V>>(r_start).value) {
                    throw std::runtime_error("Invalid range");
                }
            } else if (std::holds_alternative<Excluded<V>>(l_end) && std::holds_alternative<Excluded<V>>(r_start)) {
                if (std::get<Excluded<V>>(l_end).value > std::get<Excluded<V>>(r_start).value) {
                    throw std::runtime_error("Invalid range");
                }
            } else {
                throw std::runtime_error("Invalid range");
            }
        }
        for (auto [s, e] : segments) {
            if (!valid_segment(s, e)) {
                throw std::runtime_error("Invalid range");
            }
        }
    }

    bool valid_segment(const BoundVariant<V> &start, const BoundVariant<V> &end) {
        return std::visit([](const auto &start_arg, const auto &end_arg) {
            using T_start = std::decay_t<decltype(start_arg)>;
            using T_end = std::decay_t<decltype(end_arg)>;

            if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Included<V>>) {
                auto low = std::any_cast<Included<V>>(start_arg);
                auto high = std::any_cast<Included<V>>(end_arg);
                return low.value <= high.value;
            } else if constexpr (std::is_same_v<T_start, Included<V>> && std::is_same_v<T_end, Excluded<V>>) {
                auto low = std::any_cast<Included<V>>(start_arg);
                auto high = std::any_cast<Excluded<V>>(end_arg);
                return low.value < high.value;
            } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Included<V>>) {
                auto low = std::any_cast<Excluded<V>>(start_arg);
                auto high = std::any_cast<Included<V>>(end_arg);
                return low.value < high.value;
            } else if constexpr (std::is_same_v<T_start, Excluded<V>> && std::is_same_v<T_end, Excluded<V>>) {
                auto low = std::any_cast<Excluded<V>>(start_arg);
                auto high = std::any_cast<Excluded<V>>(end_arg);
                return low.value < high.value;
            } else {
                return true;
            }
        }, start, end);
    }

};

namespace std {
    template<typename V>
    struct hash<Range<V>> {
        std::size_t operator()(const Range<V> &range) const {
            std::size_t seed = 0;
            for (auto [start, end] : range.segments) {
                seed ^= std::visit([](const auto &start_arg, const auto &end_arg) {
                    std::size_t seed = 0;
                    seed ^= std::hash<BoundVariant<V>>{}(start_arg) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
                    seed ^= std::hash<BoundVariant<V>>{}(end_arg) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
                    return seed;
                }, start, end);
            }
            return seed;
        }
    };
}

#endif // RANGE_H