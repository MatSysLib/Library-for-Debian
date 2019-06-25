program mpi_test_determinant
    use determinant_lib, only : determinant_mpi
    use utilities, only : output_qmatrix, slau_matrix_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:)
    real :: res
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n,n))
    A = slau_matrix_create(n)
    if(is_master.EQV..TRUE.) then
        print *,"A:"
        call output_qmatrix(A, n)
    endif
    res = determinant_mpi(A, n)
    if(is_master.EQV..TRUE.) then
        print *,"RES:"
        print *,res
    endif
    deallocate(A)
    call mpi_finish
end program mpi_test_determinant