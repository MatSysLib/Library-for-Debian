real function test1(x) result(res)
    implicit none
    real, intent(in) :: x
    res = x**2 - 2
end function test1

function test2(x) result(res)
    implicit none
    real, dimension(:), intent(in) :: x
    real, dimension(size(x)) :: res
    integer :: i
    do i=1,size(x)
        res(i) = cos(x(i)) - 1
    enddo
end function test2
!Решение - 1
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
program test_newton
    use newton_lib
    use num_diff_lib
    implicit none
    real :: temp
    real, dimension(2) :: xinput
    real, dimension(10) :: xinput1
    real, dimension(:), allocatable :: vtemp
    real, parameter :: eps = 0.0001
    real, external :: test1
    procedure(efunc):: test2, fc
    xinput(1) = -2
    xinput(2) = 2
    temp = newton_func_onev(test1, 0., eps)
    print *, "№1 ", temp
    vtemp = newton_func_onev_all_ione(test1, 0., eps, 2)
    print *, "№2 ", vtemp
    vtemp = newton_func_onev_imany(test1, xinput, eps, 2)
    print *, "№3 ", vtemp
    vtemp = newton_func_onev_imany_omp(test1, xinput, eps, 2)
    print *, "№4 ", vtemp
    vtemp = newton_func_onev_all_imany(test1, xinput, eps, 2)
    print *, "№5 ", vtemp
    xinput1 = 1
    print *, "func test: ",fc(xinput1)
    xinput1 = 0.5
    vtemp = newton_func_system(fc, xinput1, eps, 10)
    print *, "res:", vtemp
    xinput1 = 0.5
    vtemp = newton_func_system_omp(fc, xinput1, eps, 10)
    print *, "resOMP:", vtemp
end program test_newton