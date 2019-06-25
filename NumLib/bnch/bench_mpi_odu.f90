program mpi_test_odu
    use eiler_lib, only : eiler_timing_mpi
    use rk_lib, only : rk_timing_mpi
    use adams5_lib, only : adams5_timing_mpi
    use mpi_manager
    implicit none
    integer :: n, repeats
    real :: a, b, h
    double precision, allocatable :: time(:)
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
    allocate(time(repeats))
    a = 0
    b = 1
    h = 0.1
    time = eiler_timing_mpi(a, b, h, n, repeats)
    if(is_master) write(*,"(A8,F20.10)") "EMPIAVG:", sum(time)/dble(repeats)
    time = rk_timing_mpi(a, b, h, n, repeats)
    if(is_master) write(*,"(A9,F20.10)") "RKMPIAVG:", sum(time)/dble(repeats)
    time = adams5_timing_mpi(a, b, h, n, repeats)
    if(is_master) write(*,"(A8,F20.10)") "AMPIAVG:", sum(time)/dble(repeats)
    deallocate(time)
    call mpi_finish
end program mpi_test_odu