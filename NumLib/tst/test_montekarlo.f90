real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

real function f2(x,y) result(res)
    implicit none
    real, intent(in) :: x, y
    res = 1 - x + y**2 !6.45833
end function f2

program test_montekarlo
    use montekarlo_lib, only : montekarlo_one, montekarlo_one_omp, montekarlo_two, montekarlo_two_omp
    implicit none
    integer :: counter
    real :: res, ax, ay, bx, by, eps, a, b
    real, external :: f, f2
    counter = 100
    a = 1
    b = 2
    eps = 0.001
    res = montekarlo_one(counter, a, b, eps, f)
    print *, res
    counter = 100
    a = 1
    b = 2
    eps = 0.001
    res = montekarlo_one_omp(counter, a, b, eps, f)
    print *, res
    counter = 100
    ax = 0
    bx = 1
    ay = 0
    by = 1
    eps = 0.001
    res = montekarlo_two(counter, ax, bx, ay, by, eps, f2)
    print *, res
    counter = 100
    ax = 0
    bx = 1
    ay = 0
    by = 1
    eps = 0.001
    res = montekarlo_two_omp(counter, ax, bx, ay, by, eps, f2)
    print *, res
    counter = 100
end program test_montekarlo