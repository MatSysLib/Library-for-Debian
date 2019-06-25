program test_invslau  
    use invslau_lib, only : invslau, invslau_omp
    use utilities, only : output_qmatrix, slau_matrix_create
    implicit none
    real, allocatable :: A(:,:), res(:,:)
    integer :: n
    n = 10
    allocate(A(n,n), res(n,n))
    A = slau_matrix_create(n)
    print *,"A:"
    call output_qmatrix(a, n)
    res = invslau(A, n)
    print *,"SEQ:"
    call output_qmatrix(res, n)
    A = slau_matrix_create(n)
    res = 0
    res = invslau_omp(A, n)
    print *,"OMP:"
    call output_qmatrix(res, n)
    deallocate(A, res)
end program test_invslau