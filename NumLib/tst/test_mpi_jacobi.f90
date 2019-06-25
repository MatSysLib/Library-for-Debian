program mpi_test_jacobi
    use jacobi_lib, only : jacobi_mpi
    use utilities, only : output_qmatrix, slau_matrix_create, slau_vector_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    real :: eps
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    eps = 0.0001
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    res = jacobi_mpi(A, b, n, eps)
    if(is_master) then
        print *,"A:"
        call output_qmatrix(A, n)
        print *,"B:"
        print *, b
        print *,"RES:"
        print *, res
    endif
    deallocate(A, b, res)
    call mpi_finish
end program mpi_test_jacobi