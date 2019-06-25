program test_jacobi
    use jacobi_lib, only : jacobi, jacobi_omp
    use utilities, only : slau_matrix_create, slau_vector_create, output_qmatrix
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    real :: eps = 0.0001
    integer :: n
    n = 10
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    call output_qmatrix(a, n)
    print *, b
    res = jacobi(A, b, n, eps)
    print *, "SEQ"
    print *, res
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    res = 0
    res = jacobi_omp(A, b, n, eps)
    print *, "OMP"
    print *, res
    deallocate(A, b, res)
end program test_jacobi