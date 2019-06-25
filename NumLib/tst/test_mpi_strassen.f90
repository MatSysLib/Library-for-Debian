program mpi_test_strassen   
    use strassen_lib, only : strassen_mpi
    use utilities, only : output_qmatrix
    use mpi_manager
    use mpi
    implicit none
    real, allocatable :: A(:,:), res(:,:)
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n,n), res(n,n))
    A = 1
    ! n^2 on all positions
    res = strassen_mpi(A, A, n)
    if(is_master.EQV..TRUE.) then
        print *,"A"
        call output_qmatrix(A, n)
        print *,"RES"
        call output_qmatrix(Res, n)
    endif
    deallocate(A, res)
    call mpi_finish
end program mpi_test_strassen