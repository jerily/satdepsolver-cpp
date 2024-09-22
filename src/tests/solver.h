#ifndef SOLVER_H
#define SOLVER_H

#include "../internal/NameId.h"
#include "../internal/VersionSetId.h"
#include "../Common.h"
#include "../Range.h"
#include "../Pool.h"
#include "../DisplaySolvable.h"
#include "../solver/Solver.h"
#include "../DisplayUnsat.h"
#include <cstdint>
#include <unordered_set>
#include <sstream>

// Let's define our own packaging version system and dependency specification.
// This is a very simple version system, where a package is identified by a name and a version
// in which the version is just an integer. The version is a range so can be noted as 0..2
// or something of the sorts, we also support constrains which means it should not use that
// package version this is also represented with a range.
//
// You can also use just a single number for a range like `package 0` which means the range from 0..1 (excluding the end)
//
// Lets call the tuples of (Name, Version) a `Pack` and the tuples of (Name, Range<u32>) a `Spec`
//
// We also need to create a custom provider that tells us how to sort the candidates. This is unique to each
// packaging ecosystem. Let's call our ecosystem 'BundleBox' so that how we call the provider as well.

// This is `Pack` which is a unique version and name in our bespoke packaging system
struct Pack {
    uint32_t version;
    bool unknown_deps;
    bool cancel_during_get_dependencies;

    explicit Pack(uint32_t p_version, bool p_unknown_deps = false, bool p_cancel_during_get_dependencies = false)
            : version(p_version), unknown_deps(p_unknown_deps),
              cancel_during_get_dependencies(p_cancel_during_get_dependencies) {
    }

    // assignment operator
    Pack &operator=(const Pack &other) {
        version = other.version;
        unknown_deps = other.unknown_deps;
        cancel_during_get_dependencies = other.cancel_during_get_dependencies;
        return *this;
    }

    // copy constructor
    Pack(const Pack &other) {
        version = other.version;
        unknown_deps = other.unknown_deps;
        cancel_during_get_dependencies = other.cancel_during_get_dependencies;
    }

    Pack with_unknown_deps() const {
        return Pack(version, true, cancel_during_get_dependencies);
    }

    Pack with_cancel_during_get_dependencies() const {
        return Pack(version, unknown_deps, true);
    }

    Pack offset(uint32_t offset) const {
        return Pack(version + offset);
    }

    bool operator==(const Pack &other) const {
        return version == other.version;
    }

    bool operator<(const Pack &other) const {
        return version < other.version;
    }

    bool operator>(const Pack &other) const {
        return version > other.version;
    }

    bool operator<=(const Pack &other) const {
        return version <= other.version;
    }

    bool operator>=(const Pack &other) const {
        return version >= other.version;
    }

    friend std::string operator+(const std::string &lhs, const Pack &rhs) {
        return lhs + std::to_string(rhs.version);
    }

    friend std::string operator+(const Pack &lhs, const std::string &rhs) {
        return std::to_string(lhs.version) + rhs;
    }

    friend std::ostream &operator<<(std::ostream &os, const Pack &pack) {
        os << pack.version;
        return os;
    }
};

namespace std {
    template<>
    struct hash<Pack> {
        std::size_t operator()(const Pack &pack) const {
            return std::hash<uint32_t>()(pack.version);
        }
    };
}

std::vector<std::string> split_string(const std::string &str, const char *split_str) {
    std::vector<std::string> split;
    size_t start = 0;
    size_t end = str.find(split_str);
    while (end != std::string::npos) {
        split.push_back(str.substr(start, end - start));
        start = end + 1;
        end = str.find(split_str, start);
    }
    split.push_back(str.substr(start, end));
    return split;
}

struct Spec {
    std::string name;
    Range<Pack> versions;

    static Spec from_str(const std::string &s) {
        auto split = split_string(s, " ");
        auto spec_name = split[0];

        auto version_range = [](std::optional<std::string> s) -> Range<Pack> {
            if (s.has_value()) {
                std::string start, end;
                size_t pos = s->find("..");
                if (pos != std::string::npos) {
                    start = s->substr(0, pos);
                    end = s->substr(pos + 2);
                } else {
                    start = s.value();
                    end = ""; // This will be handled later
                }
                Pack startPack(std::stoi(start));
                Pack endPack = !end.empty() ? Pack(std::stoi(end)) : startPack.offset(1);
//                std::cout << "startPack: " << startPack.version << std::endl;
//                std::cout << "endPack: " << endPack.version << std::endl;
                return Range<Pack>::between(startPack, endPack);
            } else {
                return Range<Pack>::full();
            }
        };

//        std::cout << spec_name << std::endl;

        auto spec_versions = version_range(split.size() > 1 ? std::optional(split[1]) : std::nullopt);
        return Spec{spec_name, spec_versions};
    }
};

struct BundleBoxPackageDependencies {
    std::vector<Spec> dependencies;
    std::vector<Spec> constrains;

};


template <typename Key, typename Value>
class OrderedMap {
public:
    // Insert a key-value pair
    void insert(const Key& key, const Value& value) {
        if (map.find(key) == map.end()) {
            order.push_back(key);
        }
        map[key] = value;
    }

    // Access value by key
    Value& operator[](const Key& key) {
        return map[key];
    }

    Value& at(const Key& key) {
        return map.at(key);
    }

    // Check if key exists
    bool contains(const Key& key) const {
        return map.find(key) != map.end();
    }

    // Get the insertion order
    const std::vector<Key>& get_order() const {
        return order;
    }

    // Print the map in insertion order
    void print() const {
        for (const auto& key : order) {
            std::cout << key << ": " << map.at(key) << std::endl;
        }
    }

    // Get the size of the map
    size_t size() const {
        return map.size();
    }

    auto keys() const {
        return order;
    }

private:
    std::unordered_map<Key, Value> map;
    std::vector<Key> order;
};



class BundleBoxProvider : public DependencyProvider<Range<Pack>, std::string> {
public:

    std::shared_ptr<Pool<Range<Pack>>> pool = std::make_shared<Pool<Range<Pack>>>(Pool<Range<Pack>>());
    std::unordered_map<std::string, OrderedMap<Pack, BundleBoxPackageDependencies>> packages;
    std::unordered_map<std::string, Pack> favored;
    std::unordered_map<std::string, Pack> locked;
    std::unordered_map<std::string, std::unordered_map<Pack, std::string>> excluded;
    bool cancel_solving;
//    std::atomic<size_t> concurrent_requests;
//    std::shared_ptr<std::atomic<size_t>> concurrent_requests_max;
    bool sleep_before_return;
    std::unordered_set<NameId> requested_candidates;
    std::unordered_set<NameId> requested_dependencies;

    std::vector<VersionSetId> requirements(const std::vector<std::string> &requirements) {
        std::vector<VersionSetId> version_set_ids;
        for (const auto &dep: requirements) {
            auto spec = Spec::from_str(dep);
            auto dep_name = pool->intern_package_name(spec.name);
            version_set_ids.push_back(pool->intern_version_set(dep_name, spec.versions));
        }
        return version_set_ids;
    }

    static BundleBoxProvider
    from_packages(const std::vector<std::tuple<std::string, uint32_t, std::vector<std::string>>> &p_packages) {
        auto result = BundleBoxProvider();
        for (const auto &package: p_packages) {
            auto name = std::get<0>(package);
            auto version = std::get<1>(package);
            auto deps = std::get<2>(package);
            result.add_package(name, Pack(version), deps, {});
        }
        return result;
    }

    void set_favored(const std::string &package_name, uint32_t version) {
        favored.insert(std::make_pair(package_name, Pack(version)));
    }

    void exclude(const std::string &package_name, uint32_t version, const std::string &reason) {
        excluded[package_name].insert(std::make_pair(Pack(version), reason));
    }

    void set_locked(const std::string &package_name, uint32_t version) {
        locked.insert(std::make_pair(package_name, Pack(version)));
    }

    void add_package(const std::string &package_name, const Pack& package_version,
                     const std::vector<std::string> &package_dependencies,
                     const std::vector<std::string> &package_constrains) {
        std::vector<Spec> deps;
        deps.reserve(package_dependencies.size());
        for (const auto &dep: package_dependencies) {
            deps.push_back(Spec::from_str(dep));
        }

        std::vector<Spec> cons;
        cons.reserve(package_constrains.size());
        for (const auto &con: package_constrains) {
            cons.push_back(Spec::from_str(con));
        }

        packages[package_name].insert(package_version, BundleBoxPackageDependencies{deps, cons});
    }

    void sort_candidates(std::vector<SolvableId> &solvables) override {
        std::sort(solvables.begin(), solvables.end(), [this](const SolvableId &a, const SolvableId &b) {
            auto a_pack = pool->resolve_solvable(a).get_inner();
            auto b_pack = pool->resolve_solvable(b).get_inner();
            // We want to sort with highest version on top
            return a_pack.version > b_pack.version;
        });
    }

    std::optional<PackageCandidates> get_candidates(NameId name_id) override {
        assert(requested_candidates.insert(name_id).second);        //            "duplicate get_candidates request"
        auto package_name = pool->resolve_package_name(name_id);
        if (packages.find(package_name) == packages.end()) {
            return std::nullopt;
        }

        const auto &package = packages.at(package_name);
        PackageCandidates package_candidates;
        package_candidates.candidates.reserve(package.size());


        auto favored_pack =
                favored.find(package_name) != favored.cend() ? std::optional(favored.at(package_name)) : std::nullopt;
        auto locked_pack =
                locked.find(package_name) != locked.cend() ? std::optional(locked.at(package_name)) : std::nullopt;
        auto excluded_packs = excluded.find(package_name) != excluded.cend() ? std::optional(excluded.at(package_name))
                                                                             : std::nullopt;

        for (const auto &pack : package.keys()) {

            auto solvable_id = pool->intern_solvable(name_id, pack);

            package_candidates.candidates.push_back(solvable_id);
            if (favored_pack.has_value() && favored_pack.value() == pack) {
                package_candidates.favored = std::optional(solvable_id);
            }
            if (locked_pack.has_value() && locked_pack.value() == pack) {
                package_candidates.locked = std::optional(solvable_id);
            }

            if (excluded_packs.has_value() && excluded_packs.value().find(pack) != excluded_packs.value().cend()) {
                package_candidates.excluded.emplace_back(solvable_id, pool->intern_string(excluded_packs.value().at(pack)));
            }
        }

        return package_candidates;
    }

    DependenciesVariant get_dependencies(SolvableId solvable) override {
        auto candidate = pool->resolve_solvable(solvable);
        auto package_name = pool->resolve_package_name(candidate.get_name_id());

        tracing::info(
                "get dependencies for %s\n",
                        package_name.c_str()
        );

        auto pack = candidate.get_inner();

        if (pack.cancel_during_get_dependencies) {
            cancel_solving = true;
            return Dependencies::Unknown{pool->intern_string("cancelled")};
        }

        if (pack.unknown_deps) {
            fprintf(stderr, "##################### could not retrieve deps\n");
            return Dependencies::Unknown{pool->intern_string("could not retrieve deps")};
        }

        if (packages.find(package_name) == packages.end()) {
            return Dependencies::Known{KnownDependencies{}};
        }

        auto deps = packages.at(package_name).at(pack);
        KnownDependencies result;
        for (const auto &req: deps.dependencies) {
            auto dep_name = pool->intern_package_name(req.name);
            auto dep_spec = pool->intern_version_set(dep_name, req.versions);

            result.requirements.push_back(dep_spec);
        }

        for (const auto &req: deps.constrains) {
            auto dep_name = pool->intern_package_name(req.name);
            auto dep_spec = pool->intern_version_set(dep_name, req.versions);
            result.constrains.push_back(dep_spec);
        }

        return Dependencies::Known{result};
    }

    std::optional<std::string> should_cancel_with_value() override {
        if (cancel_solving) {
            return std::optional("cancelled!");
        } else {
            return std::nullopt;
        }
    }

private:
    std::vector<Spec> dependencies;
    std::vector<Spec> constrains;
};


// Create a string from a [`Transaction`]
template <typename VS>
std::string transaction_to_string(const std::shared_ptr<Pool<VS>>& pool, const std::vector<SolvableId> &solvables) {
    std::stringstream output;
    for (const auto &solvable: solvables) {
        auto display_solvable = DisplaySolvable(pool,
                                                pool->resolve_internal_solvable(solvable));
        output << display_solvable << "\n";
    }
    return output.str();
}

// Solve the problem and returns either a solution represented as a string or an error string.
std::string solve_snapshot(BundleBoxProvider &provider, const std::vector<std::string> &specs) {
    auto requirements = provider.requirements(specs);
    auto pool = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [steps, err] = solver.solve(requirements);
    if (err.has_value()) {
        auto problem = std::get<UnsolvableOrCancelled::Unsolvable>(err.value());
        std::stringstream output;
        output << "UNSOLVABLE:\n";
        return output.str();
    } else {
        return transaction_to_string(pool, steps);
    }
}


std::string solve(BundleBoxProvider &provider, const std::vector<std::string> &specs, std::vector<SolvableId> &result) {
    auto requirements = provider.requirements(specs);
    auto pool = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [steps, err] = solver.solve(requirements);
    if (err.has_value()) {
        std::string message = std::visit([&](auto &&arg) -> std::string {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, UnsolvableOrCancelled::Unsolvable>) {
                auto unsolvable = std::any_cast<UnsolvableOrCancelled::Unsolvable>(arg);
                auto optional_problem_graph = solver.graph(unsolvable.problem);
                std::stringstream output;
                const auto& graph = optional_problem_graph.value();
                std::cout << graph.graphviz(pool, true) << std::endl;

                DisplayUnsat<Range<Pack>, std::string> display_unsat(pool, graph);
                output << display_unsat.to_string();
                return output.str();
            } else if constexpr (std::is_same_v<T, UnsolvableOrCancelled::Cancelled>) {
//                auto cancelled = std::any_cast<UnsolvableOrCancelled::Cancelled>(arg);
                return "cancelled!";
            }
            return "unreachable!";
        }, err.value());

        return message;
    } else {
//        auto reason = provider.should_cancel_with_value();
//        return reason.value_or(transaction_to_string(pool, steps));
        for (const auto &solvable: steps) {
            result.push_back(solvable);
        }
        return "";
    }
}


std::string solve_unsat(BundleBoxProvider &provider, const std::vector<std::string> &specs) {
    reset_problem_count();
    std::vector<SolvableId> result;
    std::string message = solve(provider, specs, result);
    assert(result.empty());
    return message;
}

#endif // SOLVER_H