program test_fox
    use mpi_manager
    use fox_lib, only : fox
    implicit none
    integer, parameter :: n = 100
    real, dimension(n,n) :: A, res
    call mpi_launch
    call mpi_setup
    print *, process_rank, " - Start!"
    A = 1
    if(process_rank==0) print *, "A:", A(1,1), A(n/2,n/2), a(n,n)
    res = fox(A, A, n)
    if(process_rank==0) print *, "A^2:", res(1,1), res(n/2,n/2), res(n,n)
    print *, process_rank, " - End!"
    call mpi_finish
end program test_fox