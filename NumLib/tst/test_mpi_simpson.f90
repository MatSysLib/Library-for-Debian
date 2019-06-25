real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program test_simpson
    use simpson_lib, only : simpson_mpi
    use mpi_manager
    implicit none
    integer :: n
    real :: a, b, eps, res
    real, external :: f
    call mpi_launch
    call mpi_setup
    n = 10
    a = 1
    b = 2
    eps = 0.001
    res = simpson_mpi(a, b, n, eps, f)
    if(is_master) print *,res
    call mpi_finish
end program test_simpson