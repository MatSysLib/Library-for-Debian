program mpi_test_zeidel
    use zeidel_lib, only : timing_mpi
    use utilities, only : slau_matrix_create, slau_vector_create
    use mpi_manager
    implicit none
    integer :: n, repeats
    real :: eps = 0.0001
    real, dimension(:), allocatable :: b
    real, dimension(:,:), allocatable :: a
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
    allocate(time(repeats))
    if(process_rank==0) then
        allocate(A(n,n), b(n))
        A = slau_matrix_create(n)
        b = slau_vector_create(n)
    else
        allocate(A(0,0), b(0))
    endif
    time = timing_mpi(A, b, n, eps, repeats)
    if(is_master) write(*,"(A7,F20.10)") "MPIAVG:", sum(time)/dble(repeats)
    deallocate(A, b, time)
    call mpi_finish
end program mpi_test_zeidel