cmake_minimum_required(VERSION 3.22.1)
project(satdepsolver VERSION 0.6.0 LANGUAGES CXX)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
find_package(Threads REQUIRED)

# The solver is templated on the consumer's version set and provider.
add_library(satdepsolver INTERFACE)
add_library(satdepsolver::satdepsolver ALIAS satdepsolver)
target_compile_features(satdepsolver INTERFACE cxx_std_17)
target_include_directories(satdepsolver INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/satdepsolver-cpp>)
target_link_libraries(satdepsolver INTERFACE Threads::Threads)

option(SATDEPSOLVER_BUILD_TESTS "Build the solver regression tests" ${PROJECT_IS_TOP_LEVEL})
if(SATDEPSOLVER_BUILD_TESTS)
    enable_testing()
    add_executable(test_satdepsolver tests/test_resolvo.cc)
    target_link_libraries(test_satdepsolver PRIVATE satdepsolver::satdepsolver)
    # Assertions are the test harness; keep them active in Release too.
    if(MSVC)
        target_compile_options(test_satdepsolver PRIVATE /UNDEBUG)
    else()
        target_compile_options(test_satdepsolver PRIVATE -UNDEBUG)
    endif()
    add_test(NAME solver COMMAND test_satdepsolver)
    add_executable(test_upstream_regressions tests/upstream_regressions.cc)
    target_link_libraries(test_upstream_regressions PRIVATE satdepsolver::satdepsolver)
    if(MSVC)
        target_compile_options(test_upstream_regressions PRIVATE /UNDEBUG)
    else()
        target_compile_options(test_upstream_regressions PRIVATE -UNDEBUG)
    endif()
    add_test(NAME upstream_regressions COMMAND test_upstream_regressions)
    add_executable(test_public_headers tests/public_headers.cc tests/public_headers_other.cc)
    target_link_libraries(test_public_headers PRIVATE satdepsolver::satdepsolver)
    add_test(NAME public_headers COMMAND test_public_headers)
    add_executable(example_basic examples/basic.cc)
    target_link_libraries(example_basic PRIVATE satdepsolver::satdepsolver)
    add_test(NAME example_basic COMMAND example_basic)
    set_tests_properties(example_basic PROPERTIES PASS_REGULAR_EXPRESSION "app=1[\r\n]+library=3")
endif()

install(FILES LICENSE DESTINATION ${CMAKE_INSTALL_DATADIR}/licenses/satdepsolver)
install(TARGETS satdepsolver EXPORT satdepsolverTargets)
install(DIRECTORY src/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/satdepsolver-cpp
    FILES_MATCHING PATTERN "*.h")
install(EXPORT satdepsolverTargets NAMESPACE satdepsolver::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/satdepsolver)
configure_package_config_file(cmake/satdepsolverConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/satdepsolverConfig.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/satdepsolver)
write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/satdepsolverConfigVersion.cmake
    VERSION ${PROJECT_VERSION} COMPATIBILITY SameMinorVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/satdepsolverConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/satdepsolverConfigVersion.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/satdepsolver)
