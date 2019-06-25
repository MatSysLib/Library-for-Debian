program test_cblock
    use cblock_lib
    use utilities, only : output_matrix
    implicit none
    integer, parameter :: n = 10    
    real, dimension(n,n) :: A, res
    A = 1
    print *, "A:"
    call output_matrix(a)
    !A^2 = n
    print *, "A^2:"
    res = cblock(A, A, n, 4)
    call output_matrix(res)
    A = 1
    print *, "A^2(OMP):"
    res = cblock_omp(A, A, n, 4)
    call output_matrix(res)
end program test_cblock