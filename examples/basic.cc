#include <satdepsolver.h>
#include <Range.h>
#include <algorithm>
#include <iostream>
#include <unordered_map>

using Versions = Range<unsigned>;

class Provider : public DependencyProvider<Versions> {
    std::unordered_map<NameId, PackageCandidates> packages;
    std::unordered_map<SolvableId, KnownDependencies> dependencies;

public:
    Provider() { pool = std::make_shared<Pool<Versions>>(); }

    VersionSetId requirement(const std::string& name, Versions versions) {
        return pool->intern_version_set(pool->intern_package_name(name), versions);
    }

    void add(const std::string& name, unsigned version, std::vector<Requirement> requirements = {}) {
        auto name_id = pool->intern_package_name(name);
        auto id = pool->intern_solvable(name_id, version);
        packages[name_id].candidates.push_back(id);
        packages[name_id].hint_dependencies_available.push_back(id);
        dependencies.emplace(id, KnownDependencies{std::move(requirements), {}});
    }

    std::optional<PackageCandidates> get_candidates(NameId name) override {
        auto found = packages.find(name);
        return found == packages.end() ? std::nullopt : std::optional(found->second);
    }

    void sort_candidates(std::vector<SolvableId>& candidates) override {
        std::sort(candidates.begin(), candidates.end(), [this](auto a, auto b) {
            return pool->resolve_solvable(a).get_inner() > pool->resolve_solvable(b).get_inner();
        });
    }

    DependenciesVariant get_dependencies(SolvableId id) override {
        return Dependencies::Known{dependencies.at(id)};
    }

    std::optional<std::string> should_cancel_with_value() override { return std::nullopt; }
};

int main() {
    Provider provider;
    provider.add("library", 1);
    provider.add("library", 2);
    provider.add("library", 3);
    provider.add("app", 1, {provider.requirement("library", Versions::between(2, 4))});
    auto app = provider.requirement("app", Versions::full());

    Solver<Versions, std::string, Provider> solver(provider);
    auto [selected, error] = solver.solve(Problem().requirements(std::vector<VersionSetId>{app}));
    if (error) {
        if (auto* unsolvable = std::get_if<UnsolvableOrCancelled::Unsolvable>(&*error)) {
            auto graph = solver.graph(unsolvable->conflict);
            if (graph) { std::cerr << DisplayUnsat<Versions, std::string>(provider.pool, *graph); }
        } else {
            std::cerr << std::any_cast<std::string>(std::get<UnsolvableOrCancelled::Cancelled>(*error).data) << '\n';
        }
        return 1;
    }
    for (auto id : selected) {
        auto solvable = provider.pool->resolve_solvable(id);
        std::cout << provider.pool->resolve_package_name(solvable.get_name_id()) << '=' << solvable.get_inner() << '\n';
    }
    return 0;
}
