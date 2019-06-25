program mpi_test_gauss
    use gauss_lib, only : gauss_mpi
    use utilities, only : output_qmatrix, slau_matrix_create, slau_vector_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    res = gauss_mpi(A, b, n)
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
end program mpi_test_gauss