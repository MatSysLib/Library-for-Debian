program mpi_test_invgj
    use invgj_lib, only : timing_mpi
    use utilities, only : slau_matrix_create
    use mpi_manager
    implicit none
    integer :: n, repeats
    real, dimension(:,:), allocatable :: A
    double precision, dimension(:), allocatable :: time
    character(len=8) :: temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
        repeats = 10
    endif
    if(command_argument_count()>=2) then
        call get_command_argument(2, temporary)
        read(temporary, *) repeats
    endif
    allocate(time(repeats), A(n,n))
    A = slau_matrix_create(n)
    time = timing_mpi(A, n, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:", sum(time)/dble(repeats)
    deallocate(A)
    call mpi_finish
end program mpi_test_invgj