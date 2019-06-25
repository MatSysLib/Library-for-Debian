real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program test_gaussi
    use gaussi_lib, only : gauss_integrate_mpi
    use mpi_manager
    implicit none
    integer :: n, p
    real :: a, b, eps, res
    real, external :: f
    call mpi_launch
    call mpi_setup
    n = 100
    a = 1
    b = 2
    p = 7
    eps = 0.001
    res = gauss_integrate_mpi(a, b, n, p, eps, f)
    if(is_master) print *,res
    call mpi_finish
end program test_gaussi