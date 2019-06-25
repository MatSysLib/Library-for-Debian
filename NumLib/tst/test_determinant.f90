program test_determinant 
    use determinant_lib, only : determinant, determinant_omp
    use utilities, only : output_qmatrix, slau_matrix_create
    implicit none
    real, allocatable :: A(:,:), res
    integer :: n
    n = 12
    allocate(A(n,n))
    A = slau_matrix_create(n)
    print *,"A:"
    call output_qmatrix(a,n)
    res = determinant(A, n)
    print *,"SEQ:"
    print *,res
    A = slau_matrix_create(n)
    res = determinant_omp(A, n)
    print *, "OMP:"
    print *, res
    deallocate(A)
end program test_determinant