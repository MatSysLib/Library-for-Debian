real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program test_squares
    use squares_lib
    implicit none
    integer :: n
    real :: a, b, eps, res
    real, external :: f
    a = 1
    b = 2
    n = 100
    eps = 0.001
    print *,"SEQ"
    res = left_square(a, b, n, eps, f)
    print *,res
    n = 100
    res = central_square(a, b, n, eps, f)
    print *,res
    n = 100
    res = right_square(a, b, n, eps, f)
    print *,res
    print *,"OMP"
    res = left_square_omp(a, b, n, eps, f)
    print *,res
    n = 100
    res = central_square_omp(a, b, n, eps, f)
    print *,res
    n = 100
    res = right_square_omp(a, b, n, eps, f)
    print *,res
end program test_squares