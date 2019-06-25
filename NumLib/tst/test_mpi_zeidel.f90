program mpi_test_zeidel
    use zeidel_lib, only : zeidel_mpi
    use utilities, only : output_qmatrix, slau_matrix_create, slau_vector_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    real :: eps
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    eps = 0.001
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    res = zeidel_mpi(A, b, n, eps)
    if(is_master.EQV..TRUE.) then
        print *,"A:"
        call output_qmatrix(A, n)
        print *,"B:"
        print *, b
        print *,"RES:"
        print *, res
    endif
    deallocate(A, b, res)
    call mpi_finish
end program mpi_test_zeidel