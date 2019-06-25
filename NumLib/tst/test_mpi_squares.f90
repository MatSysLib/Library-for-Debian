real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = 2*x**2 !1->2 = 4 2/3
end function f

program mpi_test_squares
    use squares_lib
    use mpi_manager
    implicit none
    integer :: n
    real :: a, b, eps, res
    real, external :: f
    call mpi_launch
    call mpi_setup
    n = 100
    a = 1
    b = 2
    eps = 0.001
    res = left_square_mpi(a, b, n, eps, f)
    if(is_master.eqv..true.) print *,res
    n = 100
    res = central_square_mpi(a, b, n, eps, f)
    if(is_master.eqv..true.) print *,res
    n = 100
    res = right_square_mpi(a, b, n, eps, f)
    if(is_master.eqv..true.) print *,res
    n = 100
    call mpi_finish
end program mpi_test_squares