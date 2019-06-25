program test_mpi_tda
    use tda_lib
    use gauss_lib
    use utilities
    use mpi_manager
    implicit none
    integer, parameter :: n = 16
    real, dimension(n) :: b, bg, res
    real, dimension(n,n) :: a, ag
    call mpi_launch
    call mpi_setup
    call tda_matrix_vector_create_spec(n, a, b)
    if(is_master) call output_amatrix(a)
    if(is_master) print *, b
    ag = a
    bg = b
    res = tda_m_mpi(a, b, n)
    if(is_master) print *, res
    res = gauss(ag, bg, n)
    if(is_master) print *, "gauss:", res
    call mpi_finish
end program test_mpi_tda