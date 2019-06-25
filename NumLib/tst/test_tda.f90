program test_tda
    use tda_lib
    use gauss_lib
    use utilities
    implicit none
    integer, parameter :: n = 16
    real, dimension(n) :: b, bg, res
    real, dimension(n,n) :: a, ag
    call tda_matrix_vector_create_spec(n, a, b)
    call output_amatrix(a)
    print *, b
    ag = a
    bg = b
    res = tda_m(a, b, n)
    print *, "SEQ:", res
    a = ag
    b = bg
    res = tda_m_omp(a, b, n)
    print *, "OMP:", res
    print *, "gauss:", gauss(ag, bg, n)
end program test_tda