program test_scalar_mult
    use classic_lib, only : scalar_mult, scalar_mult_omp
    implicit none
    real, allocatable :: A(:), b(:)
    integer :: i, n
    n = 10
    allocate(A(n), b(n))
    do i=1,n
        a(i) = i
        b(i) = i
    enddo
    print *, "A:"
    print *, a
    print *, "B:"
    print *, b
    print *, "A*B:"
    print *, scalar_mult(A, b)
    print *, "A*B(OMP):"
    print *, scalar_mult_omp(A, b)
    deallocate(a, b)
end program test_scalar_mult