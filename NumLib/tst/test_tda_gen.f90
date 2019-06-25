program test_tda_generation
    use utilities, only : tda_matrix_vector_create, output_matrix
    implicit none
    integer, parameter :: n = 10
    real, dimension(n) :: b
    real, dimension(n,n) :: a
    call tda_matrix_vector_create(n, a, b)
    call output_matrix(a)
    print *, b
end program test_tda_generation