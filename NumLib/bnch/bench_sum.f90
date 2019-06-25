program bench_sum
    use psum_lib, only : psum, psum_omp, get_time
    use utilities, only : random
    implicit none
    integer :: n, i
    real, dimension(:), allocatable :: a
    real :: res
    character(len=8) :: temporary
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
    endif
    allocate(a(n))
    do i = 1,n
        a(i) = random(-1., 1.)
    enddo
    res = psum(a, n)
    WRITE(*,"(F16.10)") get_time()
    res = psum_omp(a, n)
    WRITE(*,"(F16.10)") get_time()
    deallocate(a)
end program bench_sum