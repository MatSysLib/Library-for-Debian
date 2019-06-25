program mpi_test_gauss
    use invslau_lib, only : invslau_mpi
    use utilities, only : output_qmatrix, slau_matrix_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:), res(:,:)
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n,n), res(n,n))
    A = slau_matrix_create(n)
    res = invslau_mpi(A, n)
    if(is_master.EQV..TRUE.) then
        print *,"A:"
        call output_qmatrix(A, n)
        print *,"RES:"
        call output_qmatrix(res, n)
    endif
    deallocate(A, res)
    call mpi_finish
end program mpi_test_gauss