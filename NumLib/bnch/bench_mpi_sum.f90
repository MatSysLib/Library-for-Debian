program bench_mpi_sum
    use psum_lib, only : psum_mpi, get_time
    use utilities, only : random
    use mpi_manager
    implicit none
    integer :: n, i, points
    real :: res
    real, dimension(:), allocatable :: A
    character(len=8) :: temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) points
    endif
    n = points
    allocate(a(n))
    do i = 1,n
        a(i) = random(-1., 1.)
    enddo
    res = psum_mpi(a, n)
    if(is_master) WRITE(*,"(F16.10)") get_time()
    deallocate(A)
    call mpi_finish
end program bench_mpi_sum