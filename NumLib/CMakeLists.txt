cmake_minimum_required(VERSION 2.8.12)
enable_language(Fortran)
project(C2)
find_package(MPI REQUIRED)
include_directories(${MPI_Fortran_INCLUDE_PATH})
set(C2_SRCS ${C2_SRCS}
    src/Utils/mpi_manager.f90
    src/Utils/utilities.f90
    src/Utils/strings.f90
    src/Utils/read_mps.f90
    src/INTEGRAL/cells.f90
    src/INTEGRAL/gaussi.f90
    src/INTEGRAL/montekarlo.f90
    src/INTEGRAL/simpson.f90
    src/INTEGRAL/squares.f90
    src/INV/invgj.f90
    src/INV/invslau.f90
    src/INV/shultz.f90
    src/LA/cannon.f90
    src/LA/cblock.f90
    src/LA/classic.f90
    src/LA/fox.f90
    src/LA/matvecutil.f90
    src/LA/strassen.f90
    src/LP/simplex.f90
    src/ODU/eiler.f90
    src/ODU/rk.f90
    src/ODU/A5.f90
    src/SLAU/determinant.f90
    src/SLAU/gauss.f90
    src/SLAU/jacobi.f90
#   src/SLAU/kramer.f90
    src/SLAU/lud.f90
    src/SLAU/tda.f90
    src/SLAU/zeidel.f90
    src/ULA/newton.f90
    src/ELEMENTARY/sort_merge.f90
    src/ELEMENTARY/psum.f90
    src/ELEMENTARY/numdiff.f90
    src/ELEMENTARY/norm.f90
)
set(C2_TST_SRC ${C2_TST_SRC}
    tst/test_mpi_cannon.f90
    tst/test_cblock.f90
    tst/test_mpi_cblock.f90
    tst/test_cells.f90
    tst/test_mpi_cells.f90
    tst/test_classic.f90
    tst/test_mpi_classic.f90
    tst/test_custom.f90
    tst/test_determinant.f90
    tst/test_mpi_determinant.f90
    tst/test_mpi_fox.f90
    tst/test_diff_gradient.f90
#
    tst/test_different.f90
    tst/test_gauss.f90
    tst/test_mpi_gauss.f90
    tst/test_gaussi.f90
    tst/test_mpi_gaussi.f90
    tst/test_invgj.f90
    tst/test_mpi_invgj.f90
    tst/test_invslau.f90
    tst/test_mpi_invslau.f90
    tst/test_jacobi_matrix.f90
    #
    tst/test_jacobi.f90
    tst/test_mpi_jacobi.f90
    #tst/test_kramer.f90
    tst/test_lu.f90
    tst/test_mpi_lu.f90
    tst/test_ludec.f90
    tst/test_mpi_ludec.f90
    tst/test_matrix_vector.f90
    tst/test_mpi_matrix_vector.f90
    tst/test_montekarlo.f90
    tst/test_mpi_montekarlo.f90
    tst/test_mps.f90
    tst/test_newton.f90
    tst/test_mpi_newton.f90
    tst/test_odu.f90
    tst/test_mpi_odu.f90
    tst/test_scalar_mult.f90
    tst/test_mpi_scalar_mult.f90
    tst/test_shultz.f90
    tst/test_mpi_shultz.f90
    tst/test_simplex.f90
    tst/test_simpson.f90
    tst/test_mpi_simpson.f90
    tst/test_sort_merge.f90
    tst/test_mpi_sort_merge.f90
    tst/test_squares.f90
    tst/test_mpi_squares.f90
    tst/test_tda_gen.f90
    tst/test_tda.f90
    tst/test_mpi_tda.f90
    tst/test_strassen.f90
    tst/test_mpi_strassen.f90
    tst/test_zeidel.f90
    tst/test_mpi_zeidel.f90

    bnch/bench_mpi_cannon.f90
    bnch/bench_cblock.f90
    bnch/bench_mpi_cblock.f90
    bnch/bench_cells.f90
    bnch/bench_mpi_cells.f90
    bnch/bench_classic.f90
    bnch/bench_mpi_classic.f90
    #bnch/bench_custom.f90
    bnch/bench_determinant.f90
    bnch/bench_mpi_determinant.f90
    bnch/bench_mpi_fox.f90
    bnch/bench_diff.f90
    bnch/bench_mpi_diff.f90
    bnch/bench_gauss.f90
    bnch/bench_mpi_gauss.f90
    bnch/bench_gaussi.f90
    bnch/bench_mpi_gaussi.f90
    bnch/bench_invgj.f90
    bnch/bench_mpi_invgj.f90
    bnch/bench_invslau.f90
    bnch/bench_mpi_invslau.f90
    bnch/bench_jacobi.f90
    bnch/bench_mpi_jacobi.f90
    #bnch/bench_kramer.f90
    bnch/bench_lu.f90
    bnch/bench_mpi_lu.f90
    bnch/bench_montekarlo.f90
    bnch/bench_mpi_montekarlo.f90
    bnch/bench_newton.f90
    bnch/bench_mpi_newton.f90
    bnch/bench_odu.f90
    bnch/bench_mpi_odu.f90
    bnch/bench_scalar_mult.f90
    bnch/bench_mpi_scalar_mult.f90
    bnch/bench_shultz.f90
    bnch/bench_mpi_shultz.f90
    bnch/bench_simpson.f90
    bnch/bench_mpi_simpson.f90
    bnch/bench_sort_merge.f90
    bnch/bench_mpi_sort_merge.f90
    bnch/bench_squares.f90
    bnch/bench_mpi_squares.f90
    bnch/bench_strassen.f90
    bnch/bench_mpi_strassen.f90
    bnch/bench_tda.f90
    bnch/bench_mpi_tda.f90
    bnch/bench_zeidel.f90
    bnch/bench_mpi_zeidel.f90
    bnch/bench_mymult_vs_matmul.f90
)
set(C2_FLAGS "-fopenmp -Wall -Wextra -Wuninitialized -Wmaybe-uninitialized -Wcharacter-truncation -Warray-bounds")
set(C2_FLAGS_DEBUG "-fbacktrace -fcheck=all")
set(C2_FLAGS_RELASE "-O3")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/inc)
set(include_directories "${include_directories}${CMAKE_CURRENT_BINARY_DIR}/inc")

add_library(C2 STATIC ${C2_SRCS})
set_target_properties(
    C2 PROPERTIES
    COMPILE_FLAGS 
        "${MPI_Fortran_COMPILE_FLAGS} ${C2_FLAGS}"
    COMPILE_FLAGS_DEBUG
        "${MPI_Fortran_COMPILE_FLAGS} ${C2_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG} ${C2_FLAGS_DEBUG}"
    COMPILE_FLAGS_RELEASE
        "${MPI_Fortran_COMPILE_FLAGS} ${C2_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE} ${C2_FLAGS_RELASE}"
)
set_target_properties(C2 PROPERTIES LINK_FLAGS "${MPI_Fortran_LINK_FLAGS} ${C2_FLAGS}")
target_link_libraries(C2 ${MPI_Fortran_LIBRARIES})

foreach(test_source_file ${C2_TST_SRC})
    string(REPLACE ".f90" "" test_target_name ${test_source_file})
    string(REPLACE "tst/" "" test_target_name ${test_target_name})
    string(REPLACE "bnch/" "" test_target_name ${test_target_name})
    add_executable(${test_target_name} ${test_source_file})
    set_target_properties(
        ${test_target_name} PROPERTIES 
        COMPILE_FLAGS 
            "${MPI_Fortran_COMPILE_FLAGS} ${C2_FLAGS}"
        COMPILE_FLAGS_DEBUG
            "${MPI_Fortran_COMPILE_FLAGS} ${C2_FLAGS} ${CMAKE_Fortran_FLAGS_DEBUG} ${C2_FLAGS_DEBUG}"
        COMPILE_FLAGS_RELEASE
            "${MPI_Fortran_COMPILE_FLAGS} ${C2_FLAGS} ${CMAKE_Fortran_FLAGS_RELEASE} ${C2_FLAGS_RELASE}"
    )
    string(FIND "${test_target_name}" "mpi" temp)
    IF(${temp} GREATER 0)
        set_target_properties(${test_target_name} PROPERTIES SUFFIX ".mpi")
    else()
        set_target_properties(${test_target_name} PROPERTIES SUFFIX ".gce")
    endif()
    set_target_properties(${test_target_name} PROPERTIES LINK_FLAGS "${MPI_Fortran_LINK_FLAGS} ${C2_FLAGS}")
    target_link_libraries(${test_target_name} ${MPI_Fortran_LIBRARIES} C2)
endforeach(test_source_file ${C2_TST_SRC})