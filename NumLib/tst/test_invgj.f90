program test_invgj  
    use invgj_lib, only : invgj, invgj_omp
    use utilities, only : output_qmatrix, slau_matrix_create
    implicit none
    real, allocatable :: A(:,:), res(:,:)
    integer :: n
    n = 10
    allocate(A(n,n), res(n,n))
    A = slau_matrix_create(n)
    call output_qmatrix(a,n)
    res = invgj(A, n)
    call output_qmatrix(res,n)
    res = 0
    A = slau_matrix_create(n)
    res = invgj_omp(A, n)
    call output_qmatrix(res,n)
    deallocate(A, res)
end program test_invgj