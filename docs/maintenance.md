# Upstream status

Reviewed against [resolvo 0.12.2](https://github.com/prefix-dev/resolvo/releases/tag/resolvo-v0.12.2)
(October 8, 2026), commit `f8a5147b1d25399bda9a5b15efd8aa9c48d512b9`.
This is an independent C++ port; it does not have full feature or performance
parity with the Rust implementation.

## Included in this update

- Reuse cached candidates and avoid repeated dependency scans
  ([#307](https://github.com/prefix-dev/resolvo/pull/307)).
- Improve learned-clause watches after backtracking
  ([#245](https://github.com/prefix-dev/resolvo/pull/245)).
- Keep distinct dependency ranges visible in conflict reports
  ([#233](https://github.com/prefix-dev/resolvo/pull/233)).
- Explain why a locked version is excluded and remove duplicate lock messages
  ([#255](https://github.com/prefix-dev/resolvo/pull/255)).
- Deduplicate displayed versions
  ([#106](https://github.com/prefix-dev/resolvo/pull/106)).

The update also fixes metadata hints, cancellation reasons, overlapping union
candidates, self-dependencies, decision-stack handling, and numeric range display.
Root constraints, unions, soft requirements, and preference for explicit root
requests were already supported.

## Remaining differences

- Conditional requirements and multiple installed versions per package are not
  supported. Applications can choose dependencies from their own configuration.
- The port retains direct requirement clauses and pairwise version exclusions.
  Upstream's newer encodings, decision queue, and watch representation can be
  more efficient on large registries; performance parity has not been measured.
- Rust async runtimes, serialization, instrumentation, bindings, and toolchain
  updates are outside this port's scope.

Merge library changes before updating dependent applications, and pin the tested
commit in their builds. Core fixes belong here; registry and installation policy
belong in consumers such as `ttrek`.
