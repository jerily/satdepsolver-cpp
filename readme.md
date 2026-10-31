# satdepsolver-cpp

A C++17 dependency solver ported from [resolvo](https://github.com/mamba-org/resolvo).

## Rules and principles

- Keep the SAT algorithm independent of package registries, semantic versions, installation, and Tcl.
- Consumers implement a version set and a dependency provider. They own ranking, locks, exclusions, and transport failures.
- The library is header-only: templates are compiled into each consumer, with no Rust, shared library, or runtime service dependency.
- Preserve the existing solver architecture. The 0.6 release synchronizes the maintained Jerily copy and packages it for independent consumers.

## Build and consume

Requires a C++17 compiler and CMake 3.22.1 or newer.

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/your/prefix
cmake --build build --parallel
ctest --test-dir build --output-on-failure
cmake --install build
```

Tests default on for a standalone build and off when used as a subdirectory.
Set `SATDEPSOLVER_BUILD_TESTS=OFF` to install only the headers and CMake package.
Tests retain assertions in Release builds.

```cmake
find_package(satdepsolver 0.6 CONFIG REQUIRED)
target_link_libraries(my_program PRIVATE satdepsolver::satdepsolver)
```

Set `CMAKE_PREFIX_PATH` to the installation prefix. Alternatively use
`add_subdirectory(path/to/satdepsolver-cpp)` and the same target.
The target supplies C++17, the public include path, and `Threads::Threads`.
It does not change compiler flags, build types, or definitions on other targets.
Headers install under `include/satdepsolver-cpp`; consumers linking the target
include `<satdepsolver.h>`. Optional `<Range.h>` provides the original generic
range type. Applications may instead provide their own version set.

## Provider contract

Derive from `DependencyProvider<VS, std::string>`, initialize its `pool` with
`std::make_shared<Pool<VS>>()`, and implement `get_candidates`, `sort_candidates`,
`get_dependencies`, and `should_cancel_with_value`. Do not shadow the base `pool`.
A version set supplies `ValueType`, `contains`, `clone`, equality, hashing, and
stream formatting. Version values used by the bundled range must also be ordered
and hashable. All IDs must come from the same pool; root solvable ID zero is reserved.

```cpp
#include <satdepsolver.h>

// provider and version_set_type are supplied by the application.
Solver<version_set_type, std::string, provider_type> solver(provider);
auto [selected, error] = solver.solve(
    Problem().requirements(requirements).constraints(constraints));
```

`Solver` owns a copy of the provider and shares its pool. Metadata accumulated
by callbacks lives in `solver.cache.provider`. Consumers needing that metadata
after resolution must retain shared state or retrieve the provider copy.
Requirements install matching packages; constraints restrict packages without
requiring installation. Soft requirements attempt particular solvables without
invalidating the already chosen hard solution. Union requirements represent
alternatives, including alternatives with different names.

The original `solve(vector<VersionSetId>)` convenience overload remains available.
`UnsolvableOrCancelled::Unsolvable` now contains `conflict` (previously `problem`).
Call `solver.graph(error.conflict)` and `DisplayUnsat` while the solver and pool
remain alive. Conflict graphs have independent IDs and need no global reset.
Repeated solves reset SAT decisions and clauses but retain cached provider metadata;
construct a fresh solver if the underlying registry data changes.

## Synchronization summary and migration plan

Source: `jerily-webserver/core/satdepsolver` at
`5359d63cc17f7e3a82d540c0332852d95c84417a` (the subsequent README-only change
is not part of the algorithm import).

| Imported change | Effect |
| --- | --- |
| `Requirement`, `VersionSetUnionId`, pool union storage | OR requirements over one or more version sets |
| `Problem` input, `Conflict` diagnostics | Separate requests, root constraints, and optional solvables from failures |
| Requirement-aware clauses and candidate cache | Propagate unions through solving and conflict display |
| Complete solve-state reset and `undo_until(0)` | Support repeated solves and correct backtracking |
| Watch relocation to previously unwatched solvables | Avoid the former “linking to unknown solvable” failure |
| Provider virtual destructor and test-provider initialization | Correct provider lifetime and deterministic tests |
| Snapshot comparisons and new feature regressions | Actually validate the existing expected solver results |

The imported core changes are extensions and fixes to the original port, not an
algorithm replacement. Additional standalone fixes make header definitions safe
across translation units, add missing includes and an include guard, make graph
IDs local, and return arena iterator references to stored values rather than
temporaries. Conflict formatting checks the end of locked-dependency edge lists
and handles root constraints. Snapshot comparisons preserve duplicate lines.

The integration sequence is:

1. Synchronize the core and its regression suite in this repository.
2. Export and install the CMake interface target, and validate public headers,
   repeated solves, multi-file linking, Debug/Release, and sanitizers.
3. Adapt `ttrek`'s existing provider to the pool, requirement, and result APIs;
   retain package-manager policy in `ttrek`.
4. Remove its vendored Rust resolver, pin this revision in its build script,
   and validate registry-backed resolution and installation/update behavior.
5. Merge the library PR before the dependent `ttrek` PR. Future core fixes should
   land here first; synchronizing the embedded webserver copy is a separate change.

## Security

### Surface

In-process C++ APIs receive caller-owned package metadata and IDs. This library
performs no network requests and executes no installation scripts.

### Protections

Pools own interned data. Providers have a virtual destructor. Header definitions
support multiple translation units, and graph counters are not global state.
Tests exercise conflicts, exclusions, locks, unions, constraints, soft requirements,
backtracking, repeated solves, and iterator lifetime.

### Pending

This remains an actively developed port, not complete feature parity with current
Rust resolvo. IDs are trusted inputs; some bounds checks are assertions. A solver
and its pool are not safe for concurrent mutation. Cancellation still includes
exception paths, so callers must handle provider/solver exceptions. Fuzzing and
performance comparisons on large registries remain future work.
