function fc(x) result(res)
    implicit none
    real, dimension(:), intent(in) :: x
    real, dimension(size(x)) :: res
    integer :: i, n
    n = size(x)
    res(1) = (3 + 2 * x(1)) * x(1) - 2 * x(2) - 3
    do i = 2,n-1
        res(i) = (3 + 2 * x(i)) * x(i) - x(i-1) - 2 * x(i+1) - 2
    enddo
    res(n) = (3 + 2 * x(n)) * x(n) - x(n-1) - 4
end function fc
program bench_newton
    use mpi_manager
    use newton_lib, only : timing, timing_omp
    use num_diff_lib, only : efunc
    implicit none
    integer :: n, repeats
    real, parameter :: eps = 0.0001
    real, dimension(:), allocatable :: xinput1
    double precision, dimension(:), allocatable :: time
    character(len=8) :: temporary
    procedure(efunc) :: fc
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
        repeats = 10
    endif
    if(command_argument_count()>=2) then
        call get_command_argument(2, temporary)
        read(temporary, *) repeats
    endif
    allocate(time(repeats), xinput1(n))
    xinput1 = 0.5
    time = timing(fc, xinput1, eps, n, repeats)
    write(*,"(A7,F20.10)") "SEQAVG:",sum(time)/dble(repeats)
    xinput1 = 0.5
    time = timing_omp(fc, xinput1, eps, n, repeats)
    write(*,"(A7,F20.10)") "OMPAVG:",sum(time)/dble(repeats)
    deallocate(time, xinput1)
end program bench_newton