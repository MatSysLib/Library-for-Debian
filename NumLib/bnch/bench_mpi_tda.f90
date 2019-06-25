program bench_mpi_tda
    use tda_lib, only : timing_mpi
    use mpi_manager
    implicit none
    integer :: n, repeats
    real, dimension(:), allocatable :: undg, diag, updg, rvec
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
    allocate(time(repeats), undg(n), diag(n), updg(n), rvec(n))
    undg = -1
    updg = -1
    diag =  4
    rvec =  2
    updg(n) = 0
    undg(1) = 0
    rvec(1) = 3
    rvec(n) = 3
    time = timing_mpi(undg, diag, updg, rvec, n, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:", sum(time)/dble(repeats)
    deallocate(undg, diag, updg, rvec, time)
    call mpi_finish
end program bench_mpi_tda