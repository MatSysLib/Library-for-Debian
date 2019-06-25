program mpi_test_shultz
    use shultz_lib, only : timing_mpi
    use utilities, only : slau_matrix_create
    use mpi_manager
    implicit none
    integer :: n, repeats
    real :: eps = 0.001, alpha = 0.000001
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
    allocate(A(n,n), time(repeats))
    A = slau_matrix_create(n)
    time = timing_mpi(A, n, alpha, eps, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:",sum(time)/dble(repeats)
    deallocate(A, time)
    call mpi_finish
end program mpi_test_shultz