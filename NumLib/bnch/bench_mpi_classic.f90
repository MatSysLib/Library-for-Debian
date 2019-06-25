program mpi_bench_classic   
    use classic_lib, only : timing_mpi
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
    A = 1
    time = timing_mpi(A, A, n, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:", sum(time)/dble(repeats)
    deallocate(A, time)
    call mpi_finish
end program mpi_bench_classic