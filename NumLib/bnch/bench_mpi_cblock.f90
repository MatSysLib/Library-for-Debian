program bench_mpi_cblock
    use cblock_lib, only : timing_mpi
    use mpi_manager
    implicit none
    integer :: n, repeats, ichunk
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
    ichunk = 16
    do while(ichunk <= 512)
        A = 1
        time = timing_mpi(A, A, n, ichunk, repeats)
        if(is_master) write(*,"(A11,I4,A7,F20.10)") "Chunk Size:", ichunk, "MPIAVG:", sum(time)/dble(repeats)
        ichunk = ichunk * 2
    enddo
    deallocate(A, time)
    call mpi_finish
end program bench_mpi_cblock