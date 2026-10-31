#include "solver.h"
#include <array>
#include <random>

// Exercise both eager metadata hints (used by ttrek) and lazy providers.
class CountingProvider : public BundleBoxProvider {
public:
    bool hints = false;
    bool cancel_in_candidates = false;
    bool fail_in_dependencies = false;
    size_t sorts = 0;
    std::unordered_map<SolvableId, size_t> dependency_calls;

    std::optional<PackageCandidates> get_candidates(NameId name) override {
        auto candidates = BundleBoxProvider::get_candidates(name);
        if (candidates && hints) {
            candidates->hint_dependencies_available = candidates->candidates;
        }
        if (cancel_in_candidates) { cancel_solving = true; }
        return candidates;
    }

    void sort_candidates(std::vector<SolvableId>& ids) override {
        ++sorts;
        BundleBoxProvider::sort_candidates(ids);
    }

    DependenciesVariant get_dependencies(SolvableId id) override {
        ++dependency_calls[id];
        if (fail_in_dependencies) { throw std::runtime_error("registry transport failed"); }
        return BundleBoxProvider::get_dependencies(id);
    }
};

using TestSolver = Solver<Range<Pack>, std::string, CountingProvider>;

static void test_shared_candidates_and_hints() {
    CountingProvider provider;
    provider.hints = true;
    provider.add_package("a", Pack(1), {"shared"}, {});
    provider.add_package("b", Pack(1), {"shared"}, {"shared 1"});
    provider.add_package("shared", Pack(1), {}, {});
    provider.add_package("shared", Pack(2), {}, {});
    const auto requirement = provider.requirements({"shared"}).front();
    TestSolver solver(provider);
    const auto& first = solver.cache.get_or_cache_sorted_candidates(requirement);
    assert(first.size() == 2);
    for (auto id : first) { assert(solver.cache.are_dependencies_available_for(id)); }
    // Populate many cache entries, forcing rehashes. References must remain valid.
    for (int i = 3; i < 100; ++i) {
        solver.cache.get_or_cache_sorted_candidates(provider.requirements({"shared " + std::to_string(i)}).front());
    }
    assert(&first == &solver.cache.get_or_cache_sorted_candidates(requirement));
    const auto sorts = solver.cache.provider.sorts;
    auto problem = Problem().requirements(provider.requirements({"a", "b"}));
    for (int repeat = 0; repeat < 2; ++repeat) {
        auto [selected, error] = solver.solve(problem);
        assert(!error && selected.size() == 3);
        assert(std::find(selected.begin(), selected.end(), solver.cache.provider.solvable_id("shared", 1)) != selected.end());
        assert(solver.cache.provider.dependency_calls.size() == 4); // includes unselected shared=2
        for (auto [id, calls] : solver.cache.provider.dependency_calls) { assert(calls == 1); }
    }
    assert(solver.cache.provider.sorts == sorts + 2); // a and b; shared reused by both

    auto overlap = provider.requirements({"shared 1 | shared | missing"}).front();
    const auto& candidates = solver.cache.get_or_cache_sorted_candidates(overlap);
    assert(candidates.size() == 2);
    assert(candidates.front() == solver.cache.provider.solvable_id("shared", 1)); // preserve alternative preference
}

static void test_cancelled_result_and_transport_errors() {
    for (int phase = 0; phase < 3; ++phase) {
        CountingProvider provider;
        provider.add_package("a", phase == 2 ? Pack(1).with_cancel_during_get_dependencies() : Pack(1), {}, {});
        provider.cancel_solving = phase == 0;
        provider.cancel_in_candidates = phase == 1;
        TestSolver solver(provider);
        auto [selected, error] = solver.solve(provider.requirements({"a"}));
        assert(selected.empty() && error);
        auto* cancelled = std::get_if<UnsolvableOrCancelled::Cancelled>(&*error);
        assert(cancelled && std::any_cast<std::string>(cancelled->data) == "cancelled!");
    }
    CountingProvider provider;
    provider.add_package("a", Pack(1), {}, {});
    provider.cancel_solving = true;
    TestSolver solver(provider);
    assert(solver.solve(provider.requirements({"a"})).second);
    solver.cache.provider.cancel_solving = false;
    assert(!solver.solve(provider.requirements({"a"})).second);
    solver.cache.provider.cancel_solving = true;
    // Cancellation on a solve with entirely cached metadata uses the same result payload.
    auto [selected, error] = solver.solve(provider.requirements({"a"}));
    assert(error && selected.empty());
    assert(std::any_cast<std::string>(std::get<UnsolvableOrCancelled::Cancelled>(*error).data) == "cancelled!");

    provider.cancel_solving = false;
    provider.fail_in_dependencies = true;
    TestSolver failing(provider);
    bool caught = false;
    try { failing.solve(provider.requirements({"a"})); }
    catch (const std::runtime_error& error) { caught = std::string(error.what()) == "registry transport failed"; }
    assert(caught); // provider failures must not be swallowed or mistaken for cancellation
}

static void test_upstream_diagnostics() {
    auto provider = BundleBoxProvider::from_packages({
        {"a", 1, {"b 41..42"}}, {"a", 2, {"b 41..42"}},
        {"a", 3, {"b 42..43"}}, {"a", 4, {"b 43..44"}}});
    auto error = solve_unsat(provider, {"a"});
    assert(error.find("a 1 | 2 would require") != std::string::npos);
    assert(error.find("a 3 would require") != std::string::npos);
    assert(error.find("a 4 would require") != std::string::npos);
    for (const auto* range : {">=41, <42", ">=42, <43", ">=43, <44"}) {
        assert(error.find(range) != std::string::npos);
    }

    auto locked = BundleBoxProvider::from_packages({{"c", 1, {}}, {"c", 2, {}}, {"c", 3, {}}, {"c", 4, {}}});
    locked.set_locked("c", 1);
    locked.exclude("c", 1, "it is not available locally");
    error = solve_unsat(locked, {"c"});
    const std::string explanation = "c 1 is locked, but it is excluded because it is not available locally";
    auto position = error.find(explanation);
    assert(position != std::string::npos);
    assert(error.find("is locked", position + explanation.size()) == std::string::npos);
    auto id = locked.solvable_id("c", 2);
    assert((DisplayMergedSolvable<Range<Pack>, std::string>(locked.pool, {id, id}).to_string() == "2"));
}

static void test_self_dependencies_and_numeric_ranges() {
    auto provider = BundleBoxProvider::from_packages({{"a", 1, {"a | b"}}, {"b", 1, {}}});
    provider.add_package("a", Pack(2), {}, {"a 1"});
    assert(solve_snapshot(provider, {"a"}) == "a=1\n");
    auto error = solve_unsat(provider, {"a 2"});
    assert(error.find("would constrain") != std::string::npos);
    assert(error.find("Error:") == std::string::npos);

    using R = Range<unsigned>;
    std::ostringstream output;
    output << R::full() << ';' << R::empty() << ';' << R::singleton(7) << ';'
           << R::between(1, 4) << ';' << R::lower_than(2) << ';' << R::higher_than(2) << ';'
           << R::strictly_lower_than(3) << ';' << R::strictly_higher_than(3);
    assert(output.str() == "*;∅;7;>=1, <4;<=2;>=2;<3;>3");
}

static void test_undo_preserves_pending_propagation() {
    DecisionTracker tracker;
    auto root = SolvableId::root();
    auto child = SolvableId::from_usize(1);
    tracker.try_add_decision(Decision(root, true, ClauseId::install_root()), 1);
    tracker.try_add_decision(Decision(child, true, ClauseId::install_root()), 2);
    tracker.undo_last();
    assert(tracker.next_unpropagated()->solvable_id == root);
    auto [decision, level] = tracker.undo_last();
    assert(decision.solvable_id == root && level == 0 && tracker.is_empty());
    assert(!tracker.next_unpropagated());
}

// Independent exhaustive oracle: enumerate all choices of absent/version for a
// small registry and compare satisfiability and every returned assignment.
struct OracleSpec {
    size_t name;
    int version; // zero means any present version
    std::string text() const {
        return std::string(1, static_cast<char>('a' + name)) + (version ? " " + std::to_string(version) : "");
    }
    bool matches(const std::array<int, 4>& chosen) const {
        return chosen[name] && (!version || chosen[name] == version);
    }
};
using OracleRequirement = std::vector<OracleSpec>;
struct OraclePackage {
    std::vector<OracleRequirement> requirements;
    std::vector<OracleSpec> constraints;
    bool excluded = false;
};
static std::string text(const OracleRequirement& requirement) {
    std::string result;
    for (const auto& alternative : requirement) {
        if (!result.empty()) { result += " | "; }
        result += alternative.text();
    }
    return result;
}
static bool matches(const OracleRequirement& requirement, const std::array<int, 4>& chosen) {
    return std::any_of(requirement.begin(), requirement.end(), [&](const auto& alternative) { return alternative.matches(chosen); });
}

static void test_against_exhaustive_oracle() {
    std::mt19937 random(0x12'02'24);
    for (size_t trial = 0; trial < 400; ++trial) {
        std::array<std::array<OraclePackage, 3>, 4> packages;
        std::array<int, 4> locks{};
        auto spec = [&]() { return OracleSpec{random() % 4, static_cast<int>(random() % 5)}; };
        auto requirement = [&]() {
            OracleRequirement result{spec()};
            if (random() % 3 == 0) { result.push_back(spec()); }
            return result;
        };
        for (size_t name = 0; name < 4; ++name) {
            if (random() % 5 == 0) { locks[name] = 1 + random() % 3; }
            for (auto& package : packages[name]) {
                package.excluded = random() % 10 == 0;
                for (size_t n = random() % 3; n; --n) { package.requirements.push_back(requirement()); }
                if (random() % 3 == 0) { package.constraints.push_back(spec()); }
            }
        }
        std::vector<OracleRequirement> roots{requirement(), requirement()};
        std::vector<OracleSpec> constraints{spec()};
        auto valid = [&](const std::array<int, 4>& chosen) {
            for (const auto& root : roots) { if (!matches(root, chosen)) { return false; } }
            for (const auto& constraint : constraints) {
                if (chosen[constraint.name] && !constraint.matches(chosen)) { return false; }
            }
            for (size_t name = 0; name < 4; ++name) {
                if (!chosen[name]) { continue; }
                if (locks[name] && chosen[name] != locks[name]) { return false; }
                const auto& package = packages[name][chosen[name] - 1];
                if (package.excluded) { return false; }
                for (const auto& req : package.requirements) { if (!matches(req, chosen)) { return false; } }
                for (const auto& constraint : package.constraints) {
                    if (chosen[constraint.name] && !constraint.matches(chosen)) { return false; }
                }
            }
            return true;
        };
        bool satisfiable = false;
        for (size_t mask = 0; mask < 256; ++mask) {
            std::array<int, 4> chosen{};
            for (size_t name = 0; name < 4; ++name) { chosen[name] = (mask >> (2 * name)) & 3; }
            satisfiable |= valid(chosen);
        }
        for (bool hints : {false, true}) {
            CountingProvider provider;
            provider.hints = hints;
            for (size_t name = 0; name < 4; ++name) {
                std::string package_name(1, static_cast<char>('a' + name));
                if (locks[name]) { provider.set_locked(package_name, locks[name]); }
                for (int version = 1; version <= 3; ++version) {
                    const auto& package = packages[name][version - 1];
                    std::vector<std::string> deps, cons;
                    for (const auto& req : package.requirements) { deps.push_back(text(req)); }
                    for (const auto& constraint : package.constraints) { cons.push_back(constraint.text()); }
                    provider.add_package(package_name, Pack(version), deps, cons);
                    if (package.excluded) { provider.exclude(package_name, version, "unavailable"); }
                }
            }
            std::vector<std::string> root_text, constraint_text;
            for (const auto& root : roots) { root_text.push_back(text(root)); }
            for (const auto& constraint : constraints) { constraint_text.push_back(constraint.text()); }
            TestSolver solver(provider);
            auto problem = Problem().requirements(provider.requirements(root_text)).constraints(provider.version_sets(constraint_text));
            for (int repeat = 0; repeat < 2; ++repeat) {
                auto [selected, error] = solver.solve(problem);
                std::array<int, 4> chosen{};
                for (auto id : selected) {
                    auto solvable = provider.pool->resolve_solvable(id);
                    auto name = provider.pool->resolve_package_name(solvable.get_name_id()).front() - 'a';
                    assert(!chosen[name]);
                    chosen[name] = solvable.get_inner().version;
                }
                if (error.has_value() == satisfiable || (!error && !valid(chosen))) {
                    std::cerr << "Oracle mismatch: trial " << trial << ", hints " << hints << ", repeat " << repeat << std::endl;
                    std::abort();
                }
            }
        }
    }
}

int main() {
    test_shared_candidates_and_hints();
    test_cancelled_result_and_transport_errors();
    test_upstream_diagnostics();
    test_self_dependencies_and_numeric_ranges();
    test_undo_preserves_pending_propagation();
    test_against_exhaustive_oracle();
}
