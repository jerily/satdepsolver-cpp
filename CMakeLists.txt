cmake_minimum_required(VERSION 3.22.1)
project(satdepsolver-cpp VERSION 0.5.0 LANGUAGES C CXX)

set(TARGET ${PROJECT_NAME})
set(CMAKE_C_STANDARD   11)
set(CMAKE_C_STANDARD_REQUIRED true)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED true)
set(THREADS_PREFER_PTHREAD_FLAG ON)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_FLAGS  "-g -DTCL_THREADS -DPROJECT_VERSION=${PROJECT_VERSION} ${CMAKE_CXX_FLAGS}")

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
    set(CMAKE_C_FLAGS "-g -DDEBUG ${CMAKE_C_FLAGS}")
else()
    # NDEBUG to not generate code for assert
    add_compile_definitions(NDEBUG)
endif ()

add_executable(test_resolvo
        tests/test_resolvo.cc
        tests/solver.h
        src/internal/Arena.h
        src/internal/ArenaId.h
        src/internal/NameId.h
        src/internal/SolvableId.h
        src/internal/StringId.h
        src/internal/VersionSetId.h
        src/Pool.h
        src/Solvable.h
        src/internal/ClauseId.h
        src/solver/Solver.h
        src/internal/LearntClauseId.h
        src/internal/CandidatesId.h
        src/internal/DependenciesId.h
        src/solver/Clause.h
        src/Problem.h
        src/solver/WatchMap.h
        src/solver/DecisionTracker.h
        src/solver/Decision.h
        src/solver/DecisionMap.h
        src/solver/PropagationError.h
        src/internal/FrozenCopyMap.h
        src/solver/SolverCache.h
        src/Common.h
        src/Range.h
        src/DisplayClause.h
        src/DisplayName.h
        src/DisplayDecisionMap.h
        src/internal/Mapping.h
        src/DisplayUnsat.h
        src/DisplayMergedSolvable.h
)

set(CMAKE_CTEST_ARGUMENTS "--verbose")
set(CMAKE_CTEST_ARGUMENTS "--output-on-failure")
enable_testing()
add_test(NAME AllUnitTests COMMAND ./test_resolvo)

install(
        DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include/satdepsolver-cpp
        FILES_MATCHING PATTERN "*.h"
)
