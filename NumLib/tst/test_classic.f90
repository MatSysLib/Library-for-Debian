program test_classic
    use classic_lib, only : classic, classic_omp
    use utilities, only : output_matrix
    implicit none
    integer, parameter :: n = 10
    real, dimension(n,n) :: A
    A = 1
    print *, "A:"
    call output_matrix(a)
    !A^2 = n
    print *, "A^2:"
    call output_matrix(classic(A, A, n))
    print *, "A^2(OMP):"
    call output_matrix(classic_omp(A, A, n))
end program test_classic