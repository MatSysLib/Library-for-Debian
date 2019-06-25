real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program test_simpson
    use simpson_lib, only : simpson, simpson_omp
    implicit none
    integer :: n
    real :: a, b, eps, res
    real, external :: f
    a = 1
    b = 2
    n = 10
    eps = 0.001
    res = simpson(a, b, n, eps, f)
    print *, "SEQ:"
    print *,res
    n = 10
    res = simpson_omp(a, b, n, eps, f)
    print *, "OMP:"
    print *,res
end program test_simpson