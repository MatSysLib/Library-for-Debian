real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

real function f2(x,y) result(res)
    implicit none
    real, intent(in) :: x, y
    res = 1 - x + y**2
end function f2

program mpi_test_montekarlo
    use montekarlo_lib, only : montekarlo_one_mpi, montekarlo_two_mpi
    use mpi_manager
    implicit none
    integer :: counter
    real :: res, ax, ay, bx, by, eps, a, b
    real, external :: f, f2
    call mpi_launch
    call mpi_setup
    counter = 100000
    a = 1
    b = 2
    eps = 0.001
    res = montekarlo_one_mpi(counter, a, b, eps, f)
    if(is_master.eqv..true.) print *, res
    counter = 100
    ax = 0
    bx = 1
    ay = 0
    by = 1
    eps = 0.001
    res = montekarlo_two_mpi(counter, ax, bx, ay, by, eps, f2)
    if(is_master) print *, res
    call mpi_finish
end program mpi_test_montekarlo