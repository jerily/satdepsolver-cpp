#include "solver.h"
//#include "resolvo/internal/tracing.h"

void test_unit_propagation_1() {
    // test_unit_propagation_1
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 1, std::vector<std::string>()}});
    auto root_requirements = provider.requirements({"asdf"});
    auto pool_ptr = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(root_requirements);

    assert(!err.has_value());

    auto solvable = pool_ptr->resolve_solvable(solved[0]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "asdf");
    assert(solvable.get_inner().version == 1);
    fprintf(stdout, "success\n");
}

// Test if we can also select a nested version
void test_unit_propagation_nested() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 1, std::vector<std::string>{"efgh"}},
                                                      {{"efgh"}, 4, std::vector<std::string>()},
                                                      {{"dummy"}, 6, std::vector<std::string>()}});
    auto root_requirements = provider.requirements({"asdf"});
    auto pool_ptr = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(root_requirements);

    assert(!err.has_value());
    assert(solved.size() == 2);

    auto solvable = pool_ptr->resolve_solvable(solved[0]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "asdf");
    assert(solvable.get_inner().version == 1);

    solvable = pool_ptr->resolve_solvable(solved[1]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "efgh");
    assert(solvable.get_inner().version == 4);
    fprintf(stdout, "success\n");
}

// Test if we can resolve multiple versions at once
void test_resolve_multiple() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 1, std::vector<std::string>()},
                                                      {{"asdf"}, 2, std::vector<std::string>()},
                                                      {{"efgh"}, 4, std::vector<std::string>()},
                                                      {{"efgh"}, 5, std::vector<std::string>()}});
    auto root_requirements = provider.requirements({"asdf", "efgh"});
    auto pool_ptr = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(root_requirements);

    assert(!err.has_value());
    assert(solved.size() == 2);

    auto solvable = pool_ptr->resolve_solvable(solved[0]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "asdf");
    assert(solvable.get_inner().version == 2);

    solvable = pool_ptr->resolve_solvable(solved[1]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "efgh");
    assert(solvable.get_inner().version == 5);
    fprintf(stdout, "success\n");
}

#define assert_snapshot(snapshot) assert(check_equal_to_snapshot(__func__, snapshot))

std::string dirname(const char* path) {
    std::string path_str(path);
    auto pos = path_str.find_last_of('/');
    return path_str.substr(0, pos);
}

template <typename S>
bool check_equal_to_snapshot(const char* func, const S& snapshot) {

//char filename[256];
//snprintf(filename, sizeof(filename), "%s/snapshots/%s.snap", dirname(__FILE__).c_str(), func);
//FILE *fp = fopen(filename, "r");
//if (fp == NULL) {
//fprintf(stderr, "Failed to open snapshot file\n");
//return false;
//}
//
//// read file
//char buffer[1024];
//std::string snapshot_from_file;
//while (fgets(buffer, sizeof(buffer), fp) != NULL) {
//snapshot_from_file += buffer;
//}
//
//std::set<std::string> snapshot_lines;
//std::set<std::string> snapshot_from_file_lines;
//
//std::istringstream snapshot_stream(snapshot);
//std::string line;
//while (std::getline(snapshot_stream, line)) {
//snapshot_lines.insert(line);
//}
//
//std::istringstream snapshot_from_file_stream(snapshot_from_file);
//while (std::getline(snapshot_from_file_stream, line)) {
//snapshot_from_file_lines.insert(line);
//}
//
//if (snapshot_lines != snapshot_from_file_lines) {
//fprintf(stderr, "Snapshot from file: %s\n", snapshot_from_file.c_str());
//fprintf(stderr, "Snapshot: %s\n", snapshot.c_str());
//return false;
//}

return true;
}

void test_resolve_with_concurrent_metadata_fetching() {
    auto provider = BundleBoxProvider::from_packages({{{"parent"}, 4, std::vector<std::string>{"child1", "child2"}},
                                                      {{"child1"}, 3, std::vector<std::string>()},
                                                      {{"child2"}, 2, std::vector<std::string>()}});
//    auto max_concurrent_requests = provider.concurrent_requests_max;
    auto result = solve_snapshot(provider, {"parent"});
    assert_snapshot(result);

//    assert(max_concurrent_requests.get() == 2);
    fprintf(stdout, "success\n");
}

// In case of a conflict the version should not be selected with the conflict
void test_resolve_with_conflict() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 4, std::vector<std::string>{"conflicting 1"}},
                                                      {{"asdf"}, 3, std::vector<std::string>{"conflicting 0"}},
                                                      {{"efgh"}, 7, std::vector<std::string>{"conflicting 0"}},
                                                      {{"efgh"}, 6, std::vector<std::string>{"conflicting 0"}},
                                                      {{"conflicting"}, 1, std::vector<std::string>()},
                                                      {{"conflicting"}, 0, std::vector<std::string>()}});
    auto result = solve_snapshot(provider, {"asdf", "efgh"});
    assert_snapshot(result);

}

void test_literal_satisfying_value() {
    auto lit = Literal {
        SolvableId::root(),
                true,
    };
    assert(lit.satisfying_value() == false);

    lit = Literal {
        SolvableId::root(),
                false,
    };
    assert(lit.satisfying_value() == true);
}

void test_literal_eval() {
    auto decision_map = DecisionMap();
    auto lit = Literal {
        SolvableId::root(),
                false,
    };
    auto negated_lit = Literal {
        SolvableId::root(),
                true,
    };

    // Undecided
    assert(lit.eval(decision_map) == std::nullopt);
    assert(negated_lit.eval(decision_map) == std::nullopt);

    // Decided
    decision_map.set(SolvableId::root(), true, 1);
    assert(lit.eval(decision_map) == true);
    assert(negated_lit.eval(decision_map) == false);

    decision_map.set(SolvableId::root(), false, 1);
    assert(lit.eval(decision_map) == false);
    assert(negated_lit.eval(decision_map) == true);
}

ClauseState clause(std::array<ClauseId, 2> next_clauses, std::array<SolvableId, 2> watched_solvables) {
return ClauseState(
        std::move(watched_solvables),
        std::move(next_clauses),

// the kind is irrelevant here
Clause::InstallRoot{}
);
}

void test_unlink_clause_different() {
    auto clause1 = clause(
            {ClauseId::from_usize(2), ClauseId::from_usize(3)},
            {SolvableId::from_usize(1596), SolvableId::from_usize(1211)}
    );
    auto clause2 = clause(
            {ClauseId::null(), ClauseId::from_usize(3)},
            {SolvableId::from_usize(1596), SolvableId::from_usize(1208)}
    );
    auto clause3 = clause(
            {ClauseId::null(), ClauseId::null()},
            {SolvableId::from_usize(1211), SolvableId::from_usize(42)}
    );

    // Unlink 0
    {
        auto clause1_copy = clause1;
        clause1_copy.unlink_clause(clause2, SolvableId::from_usize(1596), 0);
        auto vec1 = std::vector<SolvableId>{SolvableId::from_usize(1596), SolvableId::from_usize(1211)};
        auto vec2 = std::vector<ClauseId>{ClauseId::null(), ClauseId::from_usize(3)};
        assert(std::vector(clause1_copy.watched_literals_.begin(), clause1_copy.watched_literals_.end()) == vec1);
        assert(std::vector(clause1_copy.next_watches_.begin(), clause1_copy.next_watches_.end()) == vec2);
    }

    // Unlink 1
    {
        auto clause1_copy = clause1;
        clause1_copy.unlink_clause(clause3, SolvableId::from_usize(1211), 0);
        auto vec1 = std::vector<SolvableId>{SolvableId::from_usize(1596), SolvableId::from_usize(1211)};
        auto vec2 = std::vector<ClauseId>{ClauseId::from_usize(2), ClauseId::null()};
        assert(std::vector(clause1_copy.watched_literals_.begin(), clause1_copy.watched_literals_.end()) == vec1);
        assert(std::vector(clause1_copy.next_watches_.begin(), clause1_copy.next_watches_.end()) == vec2);
    }
}

void test_unlink_clause_same() {
    auto clause1 = clause(
            {ClauseId::from_usize(2), ClauseId::from_usize(2)},
            {SolvableId::from_usize(1596), SolvableId::from_usize(1211)}
    );
    auto clause2 = clause(
            {ClauseId::null(), ClauseId::null()},
            {SolvableId::from_usize(1596), SolvableId::from_usize(1211)}
    );

    // Unlink 0
    {
        auto clause1_copy = clause1;
        clause1_copy.unlink_clause(clause2, SolvableId::from_usize(1596), 0);
        auto vec1 = std::vector<SolvableId>{SolvableId::from_usize(1596), SolvableId::from_usize(1211)};
        auto vec2 = std::vector<ClauseId>{ClauseId::null(), ClauseId::from_usize(2)};
        assert(std::vector(clause1_copy.watched_literals_.begin(), clause1_copy.watched_literals_.end()) == vec1);
        assert(std::vector(clause1_copy.next_watches_.begin(), clause1_copy.next_watches_.end()) == vec2);
    }

    // Unlink 1
    {
        auto clause1_copy = clause1;
        clause1_copy.unlink_clause(clause2, SolvableId::from_usize(1211), 1);
        auto vec1 = std::vector<SolvableId>{SolvableId::from_usize(1596), SolvableId::from_usize(1211)};
        auto vec2 = std::vector<ClauseId>{ClauseId::from_usize(2), ClauseId::null()};
        assert(std::vector(clause1_copy.watched_literals_.begin(), clause1_copy.watched_literals_.end()) == vec1);
        assert(std::vector(clause1_copy.next_watches_.begin(), clause1_copy.next_watches_.end()) == vec2);
    }
}


void test_requires_with_and_without_conflict() {
    auto decisions = DecisionTracker();
    auto parent = SolvableId::from_usize(1);
    auto candidate1 = SolvableId::from_usize(2);
    auto candidate2 = SolvableId::from_usize(3);

    // No conflict, all candidates available
    auto [clause, conflict] = Clause::requires(
            parent,
            VersionSetId::from_usize(0),
            {candidate1, candidate2},
            decisions
    );
    assert(!conflict);
    assert(clause.watched_literals_[0] == parent);
    assert(clause.watched_literals_[1] == candidate1);

    // No conflict, still one candidate available
    decisions.get_map().set(candidate1, false, 1);
    auto [clause2, conflict2] = Clause::requires(
            parent,
            VersionSetId::from_usize(0),
            {candidate1, candidate2},
            decisions
    );
    assert(!conflict2);
    assert(clause2.watched_literals_[0] == parent);
    assert(clause2.watched_literals_[1] == candidate2);

    // Conflict, no candidates available
    decisions.get_map().set(candidate2, false, 1);
    auto [clause3, conflict3] = Clause::requires(
            parent,
            VersionSetId::from_usize(0),
            {candidate1, candidate2},
            decisions
    );
    assert(conflict3);
    assert(clause3.watched_literals_[0] == parent);
    assert(clause3.watched_literals_[1] == candidate1);

    // Panic
    decisions.get_map().set(parent, false, 1);
    bool panicked = false;
    try {
        Clause::requires(
                parent,
                VersionSetId::from_usize(0),
                {candidate1, candidate2},
                decisions
        );
    } catch (const std::exception& e) {
        panicked = true;
    }
    assert(panicked);
}

void test_constrains_with_and_without_conflict() {
    auto decisions = DecisionTracker();

    auto parent = SolvableId::from_usize(1);
    auto forbidden = SolvableId::from_usize(2);

    // No conflict, forbidden package not installed
    auto [clause, conflict] = Clause::constrains(parent, forbidden, VersionSetId::from_usize(0), decisions);
    assert(!conflict);
    assert(clause.watched_literals_[0] == parent);
    assert(clause.watched_literals_[1] == forbidden);

    // Conflict, forbidden package installed
    decisions.try_add_decision(Decision{forbidden, true, ClauseId::null()}, 1);
    auto [clause2, conflict2] = Clause::constrains(parent, forbidden, VersionSetId::from_usize(0), decisions);
    assert(conflict2);
    assert(clause2.watched_literals_[0] == parent);
    assert(clause2.watched_literals_[1] == forbidden);

    // Panic
    decisions.try_add_decision(Decision{parent, false, ClauseId::null()}, 1);
    bool panicked = false;
    try {
        Clause::constrains(parent, forbidden, VersionSetId::from_usize(0), decisions);
    } catch (const std::exception& e) {
        panicked = true;
    }
    assert(panicked);
}

void test_clause_size() {
    fprintf(stderr, "ClauseState size: %lu - ClauseVariant size: %lu\n", sizeof(ClauseState), sizeof(ClauseVariant));
    assert(sizeof(ClauseState) == 32);
}

void test_resolve_with_nonexisting() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 4, std::vector<std::string>{"b"}},
                                                      {{"asdf"}, 3, std::vector<std::string>()},
                                                      {{"b"}, 1, std::vector<std::string>{"idontexist"}}});
    auto root_requirements = provider.requirements({"asdf"});
    auto pool_ptr = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(root_requirements);

    assert(!err.has_value());
    assert(solved.size() == 1);

    auto solvable = pool_ptr->resolve_solvable(solved[0]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "asdf");
    assert(solvable.get_inner().version == 3);
    fprintf(stdout, "success\n");
}


void test_resolve_with_nested_deps() {
    auto provider = BundleBoxProvider::from_packages({{{"apache-airflow"}, 3, std::vector<std::string>{"opentelemetry-api 2..4", "opentelemetry-exporter-otlp"}},
                                                      {{"apache-airflow"}, 2, std::vector<std::string>{"opentelemetry-api 2..4", "opentelemetry-exporter-otlp"}},
                                                      {{"apache-airflow"}, 1, std::vector<std::string>()},
                                                      {{"opentelemetry-api"}, 3, std::vector<std::string>{"opentelemetry-sdk"}},
                                                      {{"opentelemetry-api"}, 2, std::vector<std::string>()},
                                                      {{"opentelemetry-api"}, 1, std::vector<std::string>()},
                                                      {{"opentelemetry-exporter-otlp"}, 1, std::vector<std::string>{"opentelemetry-grpc"}},
                                                      {{"opentelemetry-grpc"}, 1, std::vector<std::string>{"opentelemetry-api 1"}}});
    auto root_requirements = provider.requirements({"apache-airflow"});
    auto pool_ptr = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(root_requirements);

    assert(!err.has_value());
    assert(solved.size() == 1);

    auto solvable = pool_ptr->resolve_solvable(solved[0]);
    assert(pool_ptr->resolve_package_name(solvable.get_name_id()) == "apache-airflow");
    assert(solvable.get_inner().version == 1);
    fprintf(stdout, "success\n");
}

void test_resolve_with_unknown_deps() {
    auto provider = BundleBoxProvider();
    provider.add_package(
            "opentelemetry-api",
            Pack(3).with_unknown_deps(),
            {},
            {}
    );
    provider.add_package("opentelemetry-api", Pack(2), {}, {});
    auto requirements = provider.requirements({"opentelemetry-api"});
    auto pool = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(requirements);

    assert(!err.has_value());

    assert(solved.size() == 1);

    auto solvable = pool->resolve_solvable(solved[0]);

    assert(pool->resolve_package_name(solvable.get_name_id()) == "opentelemetry-api");

    assert(solvable.get_inner().version == 2);
}

void test_resolve_and_cancel() {
    auto provider = BundleBoxProvider();
    provider.add_package(
            "opentelemetry-api",
            Pack(3).with_unknown_deps(),
            {},
            {}
    );
    provider.add_package(
            "opentelemetry-api",
            Pack(2).with_cancel_during_get_dependencies(),
            {},
            {}
    );
    auto error = solve_unsat(provider, {"opentelemetry-api"});

    assert_snapshot(error);
}

// Locking a specific package version in this case a lower version namely `3` should result
// in the higher package not being considered
void test_resolve_locked_and_top_level() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 4, std::vector<std::string>()},
                                                      {{"asdf"}, 3, std::vector<std::string>()}});
    provider.set_locked("asdf", 3);

    auto requirements = provider.requirements({"asdf"});

    auto pool = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(requirements);

    assert(!err.has_value());
    assert(solved.size() == 1);
    auto solvable_id = solved[0];
    assert(pool->resolve_solvable(solvable_id).get_inner().version == 3);
}

// Should ignore lock when it is not a top level package and a newer version exists without it
void test_resolve_ignored_locked_top_level() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 4, std::vector<std::string>()},
                                                      {{"asdf"}, 3, std::vector<std::string>{"fgh"}},
                                                      {{"fgh"}, 1, std::vector<std::string>()}});
    provider.set_locked("fgh", 1);

    auto requirements = provider.requirements({"asdf"});
    auto pool = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(requirements);

    assert(!err.has_value());
    assert(solved.size() == 1);

    auto solvable = pool->resolve_solvable(solved[0]);
    assert(pool->resolve_package_name(solvable.get_name_id()) == "asdf");
    assert(solvable.get_inner().version == 4);
}

// Test checks if favoring without a conflict results in a package upgrade
//    insta::assert_snapshot!(result, @r###"
//        a=1
//        b=2
//        "###);
void test_resolve_favor_without_conflict() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 1, std::vector<std::string>()},
                                                      {{"a"}, 2, std::vector<std::string>()},
                                                      {{"b"}, 1, std::vector<std::string>()},
                                                      {{"b"}, 2, std::vector<std::string>()}});
    provider.set_favored("a", 1);
    provider.set_favored("b", 1);

    auto result = solve_snapshot(provider, {"a", "b 2"});
    assert_snapshot(result);

}

//    insta::assert_snapshot!(result, @r###"
//        a=2
//        b=2
//        c=2
//        "###);
void test_resolve_favor_with_conflict() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 1, std::vector<std::string>{"c 1"}},
                                                      {{"a"}, 2, std::vector<std::string>()},
                                                      {{"b"}, 1, std::vector<std::string>{"c 1"}},
                                                      {{"b"}, 2, std::vector<std::string>{"c 2"}},
                                                      {{"c"}, 1, std::vector<std::string>()},
                                                      {{"c"}, 2, std::vector<std::string>()}});
    provider.set_favored("a", 1);
    provider.set_favored("b", 1);
    provider.set_favored("c", 1);

    auto result = solve_snapshot(provider, {"a", "b 2"});
    assert_snapshot(result);
}

//    insta::assert_snapshot!(result, @r###"
//        a=2
//        b=5
//        "###);
void test_resolve_cyclic() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 2, std::vector<std::string>{"b 0..10"}},
                                                      {{"b"}, 5, std::vector<std::string>{"a 2..4"}}});
    auto requirements = provider.requirements({"a 0..100"});
    auto pool = provider.pool;
    auto solver = Solver<Range<Pack>, std::string, BundleBoxProvider>(provider);
    auto [solved, err] = solver.solve(requirements);

    auto result = transaction_to_string(pool, solved);
    assert_snapshot(result);
}

void test_unsat_locked_and_excluded() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 1, std::vector<std::string>{"c 2"}},
                                                      {{"c"}, 2, std::vector<std::string>()},
                                                      {{"c"}, 1, std::vector<std::string>()}});
    provider.set_locked("c", 1);
    auto result = solve_unsat(provider, {"asdf"});
    fprintf(stderr, "result: %s\n", result.c_str());
    assert_snapshot(result);
}

void test_unsat_no_candidates_for_child_1() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 1, std::vector<std::string>{"c 2"}},
                                                      {{"c"}, 1, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"asdf"});
    assert_snapshot(error);
}

void test_unsat_no_candidates_for_child_2() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 41, std::vector<std::string>{"b 0..20"}}});
    auto error = solve_unsat(provider, {"a 0..1000"});
    assert_snapshot(error);
}

void test_unsat_missing_top_level_dep_1() {
    auto provider = BundleBoxProvider::from_packages({{{"asdf"}, 1, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"fghj"});
    assert_snapshot(error);
}

void test_unsat_missing_top_level_dep_2() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 41, std::vector<std::string>{"b 15"}},
                                                      {{"b"}, 15, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"a 41", "b 14"});
    assert_snapshot(error);
}

void test_unsat_after_backtracking() {
    auto provider = BundleBoxProvider::from_packages({{{"b"}, 7, std::vector<std::string>{"d 1"}},
                                                      {{"b"}, 6, std::vector<std::string>{"d 1"}},
                                                      {{"c"}, 1, std::vector<std::string>{"d 2"}},
                                                      {{"c"}, 2, std::vector<std::string>{"d 2"}},
                                                      {{"d"}, 2, std::vector<std::string>()},
                                                      {{"d"}, 1, std::vector<std::string>()},
                                                      {{"e"}, 1, std::vector<std::string>()},
                                                      {{"e"}, 2, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"b", "c", "e"});
    assert_snapshot(error);
}

void test_unsat_incompatible_root_requirements() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 2, std::vector<std::string>()},
                                                      {{"a"}, 5, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"a 0..4", "a 5..10"});
    assert_snapshot(error);
}

void test_unsat_bluesky_conflict() {
    auto provider = BundleBoxProvider::from_packages({{{"suitcase-utils"}, 54, std::vector<std::string>()},
                                                      {{"suitcase-utils"}, 53, std::vector<std::string>()},
                                                      {{"bluesky-widgets"}, 42, std::vector<std::string>{"bluesky-live 0..10", "numpy 0..10", "python 0..10", "suitcase-utils 0..54"}},
                                                      {{"bluesky-live"}, 1, std::vector<std::string>()},
                                                      {{"numpy"}, 1, std::vector<std::string>()},
                                                      {{"python"}, 1, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"bluesky-widgets 0..100", "suitcase-utils 54..100"});
    assert_snapshot(error);
}

void test_unsat_pubgrub_article() {
    auto provider = BundleBoxProvider::from_packages({{{"menu"}, 15, std::vector<std::string>{"dropdown 2..3"}},
                                                      {{"menu"}, 10, std::vector<std::string>{"dropdown 1..2"}},
                                                      {{"dropdown"}, 2, std::vector<std::string>{"icons 2"}},
                                                      {{"dropdown"}, 1, std::vector<std::string>{"intl 3"}},
                                                      {{"icons"}, 2, std::vector<std::string>()},
                                                      {{"icons"}, 1, std::vector<std::string>()},
                                                      {{"intl"}, 5, std::vector<std::string>()},
                                                      {{"intl"}, 3, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"menu", "icons 1", "intl 5"});
    assert_snapshot(error);
}

void test_unsat_applies_graph_compression() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 10, std::vector<std::string>{"b"}},
                                                      {{"a"}, 9, std::vector<std::string>{"b"}},
                                                      {{"b"}, 100, std::vector<std::string>{"c 0..100"}},
                                                      {{"b"}, 42, std::vector<std::string>{"c 0..100"}},
                                                      {{"c"}, 103, std::vector<std::string>()},
                                                      {{"c"}, 101, std::vector<std::string>()},
                                                      {{"c"}, 100, std::vector<std::string>()},
                                                      {{"c"}, 99, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"a", "c 101..104"});
    assert_snapshot(error);
}

void test_unsat_constrains() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 10, std::vector<std::string>{"b 50..100"}},
                                                      {{"a"}, 9, std::vector<std::string>{"b 50..100"}},
                                                      {{"b"}, 50, std::vector<std::string>()},
                                                      {{"b"}, 42, std::vector<std::string>()}});
    provider.add_package("c", Pack(10), {}, {"b 0..50"});
    provider.add_package("c", Pack(8), {}, {"b 0..50"});
    auto error = solve_unsat(provider, {"a", "c"});
    assert_snapshot(error);
}

void test_unsat_constrains_2() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 1, std::vector<std::string>{"b"}},
                                                      {{"a"}, 2, std::vector<std::string>{"b"}},
                                                      {{"b"}, 1, std::vector<std::string>{"c 1"}},
                                                      {{"b"}, 2, std::vector<std::string>{"c 2"}}});

    provider.add_package("c", Pack(1), {}, {"a 3"});
    provider.add_package("c", Pack(2), {}, {"a 3"});
    auto error = solve_unsat(provider, {"a"});
    assert_snapshot(error);
}

void test_missing_dep() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 2, std::vector<std::string>{"missing"}},
                                                      {{"a"}, 1, std::vector<std::string>()}});
    auto error = solve_unsat(provider, {"a"});
    assert_snapshot(error);
}

void test_no_backtracking() {
    auto provider = BundleBoxProvider::from_packages({{{"quetz-server"}, 2, std::vector<std::string>{"pydantic 10..20"}},
                                                      {{"quetz-server"}, 1, std::vector<std::string>{"pydantic 0..10"}},
                                                      {{"pydantic"}, 1, std::vector<std::string>()},
                                                      {{"pydantic"}, 2, std::vector<std::string>()},
                                                      {{"pydantic"}, 3, std::vector<std::string>()},
                                                      {{"pydantic"}, 4, std::vector<std::string>()},
                                                      {{"pydantic"}, 5, std::vector<std::string>()},
                                                      {{"pydantic"}, 6, std::vector<std::string>()},
                                                      {{"pydantic"}, 7, std::vector<std::string>()},
                                                      {{"pydantic"}, 8, std::vector<std::string>()},
                                                      {{"pydantic"}, 9, std::vector<std::string>()},
                                                      {{"pydantic"}, 10, std::vector<std::string>()},
                                                      {{"pydantic"}, 11, std::vector<std::string>()},
                                                      {{"pydantic"}, 12, std::vector<std::string>()},
                                                      {{"pydantic"}, 13, std::vector<std::string>()},
                                                      {{"pydantic"}, 14, std::vector<std::string>()}});
    auto result = solve_snapshot(provider, {"quetz-server", "pydantic 0..10"});
    assert_snapshot(result);
}

void test_incremental_crash() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 3, std::vector<std::string>{"missing"}},
                                                      {{"a"}, 2, std::vector<std::string>{"missing"}},
                                                      {{"a"}, 1, std::vector<std::string>{"b"}},
                                                      {{"b"}, 2, std::vector<std::string>{"a 2..4"}},
                                                      {{"b"}, 1, std::vector<std::string>()}});
    auto result = solve_snapshot(provider, {"a"});
    assert_snapshot(result);
}

void test_excluded() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 2, std::vector<std::string>{"b"}},
                                                      {{"a"}, 1, std::vector<std::string>{"c"}},
                                                      {{"b"}, 1, std::vector<std::string>()},
                                                      {{"c"}, 1, std::vector<std::string>()}});
    provider.exclude("b", 1, "it is externally excluded");
    provider.exclude("c", 1, "it is externally excluded");
    auto result = solve_snapshot(provider, {"a"});
    assert_snapshot(result);
}

void test_merge_excluded() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 1, std::vector<std::string>()},
                                                      {{"a"}, 2, std::vector<std::string>()}});
    provider.exclude("a", 1, "it is externally excluded");
    provider.exclude("a", 2, "it is externally excluded");
    auto result = solve_snapshot(provider, {"a"});
    assert_snapshot(result);
}

void test_merge_installable() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 1, std::vector<std::string>()},
                                                      {{"a"}, 2, std::vector<std::string>()},
                                                      {{"a"}, 3, std::vector<std::string>()},
                                                      {{"a"}, 4, std::vector<std::string>()}});
    auto result = solve_snapshot(provider, {"a 0..3", "a 3..5"});
    assert_snapshot(result);
}

void test_root_excluded() {
    auto provider = BundleBoxProvider::from_packages({{{"a"}, 1, std::vector<std::string>()}});
    provider.exclude("a", 1, "it is externally excluded");
    auto result = solve_snapshot(provider, {"a"});
    assert_snapshot(result);
}

void test_indenter_without_top_level_indent() {
    auto indenter = Indenter(false);

    indenter.push_level_with_order(ChildOrder::Last);
    assert(indenter.get_indent() == "");

    indenter.push_level_with_order(ChildOrder::Last);
    assert(indenter.get_indent() == "└─ ");
}

void test_indenter_with_multiple_siblings() {
    auto indenter = Indenter(true);

    indenter.push_level_with_order(ChildOrder::Last);
    assert(indenter.get_indent() == "└─ ");

    indenter.push_level_with_order(ChildOrder::HasRemainingSiblings);
    assert(indenter.get_indent() == "   ├─ ");

    indenter.push_level_with_order(ChildOrder::Last);
    assert(indenter.get_indent() == "   │  └─ ");

    indenter.push_level_with_order(ChildOrder::Last);
    assert(indenter.get_indent() == "   │     └─ ");

    indenter.push_level_with_order(ChildOrder::HasRemainingSiblings);
    assert(indenter.get_indent() == "   │        ├─ ");
}

int main(int argc, char *argv[]) {

//    test_literal_satisfying_value();
//    test_literal_eval();
//    test_unlink_clause_different();
//    test_unlink_clause_same();
//    test_requires_with_and_without_conflict();
//    test_constrains_with_and_without_conflict();
//
//    test_unit_propagation_1();
//    test_unit_propagation_nested();
//    test_resolve_with_concurrent_metadata_fetching();
//    test_resolve_with_conflict();
//    test_resolve_with_nonexisting();
//    test_resolve_with_nested_deps();
//    test_resolve_with_unknown_deps();
////    test_resolve_and_cancel();
//    test_resolve_locked_and_top_level();
//    test_resolve_ignored_locked_top_level();
//    test_resolve_favor_without_conflict();
//    test_resolve_favor_with_conflict();
//    test_resolve_cyclic();
//    test_unsat_locked_and_excluded();
//    test_unsat_no_candidates_for_child_1();
//    test_unsat_no_candidates_for_child_2();
//    test_unsat_missing_top_level_dep_1();
//    test_unsat_missing_top_level_dep_2();
//
//    test_unsat_after_backtracking();
//    test_unsat_incompatible_root_requirements();
//    test_unsat_bluesky_conflict();
    test_unsat_pubgrub_article();
//
//    test_unsat_applies_graph_compression();
//    test_unsat_constrains();
//    test_unsat_constrains_2();
//    test_missing_dep();
//    test_no_backtracking();
//    test_incremental_crash();
//    test_merge_installable();
//
//
//    test_excluded();
//    test_merge_excluded();
//    test_root_excluded();
//
//    test_resolve_multiple();
////    test_clause_size();
//
////    test_indenter_without_top_level_indent();
////    test_indenter_with_multiple_siblings();
    return 0;
}

