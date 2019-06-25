program mpi_test_shultz
    use shultz_lib, only : shultz_mpi
    use utilities, only : output_qmatrix, slau_matrix_create, slau_vector_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:),  res(:,:)
    real :: eps, alpha 
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    eps = 0.001
    alpha = 0.001
    allocate(A(n,n), res(n,n))

    A = slau_matrix_create(n)
    print *,"Ok"
    res = shultz_mpi(A, n, alpha, eps)
    if(is_master.EQV..TRUE.) then
        print *,"A:"
        call output_qmatrix(A, n)
        print *,"RES:"
        call output_qmatrix(res, n)
    endif
    deallocate(A, res)
    call mpi_finish
end program mpi_test_shultz