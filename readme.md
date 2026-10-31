# satdepsolver-cpp

A header-only C++17 dependency solver, ported from
[resolvo](https://github.com/prefix-dev/resolvo). Give it the available packages
and their dependencies; it finds a compatible set or explains why none exists.
No Rust toolchain is needed.

The solver supports version requirements, alternative dependencies (`A OR B`),
constraints, preferred and locked versions, exclusions, and optional packages.
Your application controls version comparison, candidate preference, and how
package metadata is loaded.

## Build and try it

Requires a C++17 compiler and CMake 3.22.1 or newer.

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
./build/example_basic
```

The [complete example](examples/basic.cc) resolves an application that needs
version 2 or 3 of a library and prints:

```text
app=1
library=3
```

## Add it to your project

Install the headers and CMake package:

```sh
cmake --install build --prefix /your/prefix
```

Then use the exported target in your `CMakeLists.txt`:

```cmake
find_package(satdepsolver 0.6 CONFIG REQUIRED)
target_link_libraries(my_app PRIVATE satdepsolver::satdepsolver)
```

Configure your project with `-DCMAKE_PREFIX_PATH=/your/prefix`. Alternatively,
use `add_subdirectory(path/to/satdepsolver-cpp)` and link the same target.
Include `<satdepsolver.h>` in your code; `<Range.h>` is an optional version-range
implementation. Tests and the example build by default for standalone builds;
set `SATDEPSOLVER_BUILD_TESTS=OFF` to disable them.

## Provide package metadata

Implement `DependencyProvider<YourVersionSet>` and initialize its inherited
`pool` with `std::make_shared<Pool<YourVersionSet>>()`. The provider supplies:

- `get_candidates(name)`: available versions, with optional preferences, locks,
  exclusions, and hints that dependencies are already available.
- `sort_candidates(candidates)`: your preferred order, best candidate first.
- `get_dependencies(id)`: requirements and constraints for one candidate.
- `should_cancel_with_value()`: `std::nullopt` to continue, or a cancellation reason.

Use the pool to intern package names, versions, and requirements. Keep all IDs
in the same pool. A custom version set supplies `ValueType`, `contains`, `clone`,
equality, hashing, and stream formatting; the example uses `Range<unsigned>`.

## Resolve dependencies

```cpp
Solver<YourVersionSet, std::string, YourProvider> solver(provider);
auto [selected, error] = solver.solve(
    Problem().requirements(requirements).constraints(constraints));
```

Requirements select packages. Constraints restrict versions only when a package
is selected. `.soft_requirements(solvable_ids)` attempts optional packages while
preserving the hard solution.

On success, `selected` contains solvable IDs that you can look up in the pool.
On failure, inspect `error`: `Unsolvable` contains a `conflict` for
`solver.graph()` and `DisplayUnsat`; `Cancelled` contains the reason as a
`std::string` in `data`. Provider exceptions propagate to the caller. See the
[example](examples/basic.cc) for result handling.

The solver copies the provider and shares its pool. Callback-populated metadata
lives in `solver.cache.provider`. Repeated solves reuse cached metadata; create
a new solver when the available packages change. A solver and its pool must not
be mutated concurrently.

See the [upstream comparison and maintenance notes](docs/maintenance.md) for
ported fixes, compatibility details, and features that differ from resolvo.
