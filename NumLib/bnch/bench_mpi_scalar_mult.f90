program benchmark_mpi_scalar_mult
    use classic_lib, only : scalar_mult_mpi, get_scalar_time
    use utilities, only : random
    use mpi_manager
    implicit none
    integer :: n, i
    real :: res
    real, dimension(:), allocatable :: a, b
    character(len=8) :: temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
    endif
    allocate(a(n), b(n))
    do i=1,n
        a(i) = random(-1., 1.)
        b(i) = random(-1., 1.)
    enddo
    res = scalar_mult_mpi(a, b)
    if(is_master) WRITE(*,"(F16.10)") get_scalar_time()
    deallocate(A, B)
    call mpi_finish
end program benchmark_mpi_scalar_mult