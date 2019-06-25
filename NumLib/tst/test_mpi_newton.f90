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

program test_mpi_newton
    use mpi_manager
    use newton_lib
    use num_diff_lib
    implicit none
    integer, parameter :: n = 10
    real, parameter :: eps = 0.0001
    real, dimension(n) :: xinput1, vtemp
    procedure(efunc):: fc
    call mpi_launch
    call mpi_setup
    xinput1 = 0.5
    vtemp = newton_func_system_mpi(fc, xinput1, eps, n)
    if(is_master) print *, "resMPI:", vtemp
    call mpi_finish
end program test_mpi_newton