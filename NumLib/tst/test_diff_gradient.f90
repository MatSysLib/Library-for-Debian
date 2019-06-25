function f(x) result(res)
    implicit none
    real, dimension(:), intent(in) :: x
    real, dimension(size(x)) :: res
    integer :: i
    do i=1,size(x)
        res(i) = cos(x(i)) - 1
    enddo
end function f

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
    res(n) = (3 + 2 * x(n)) * x(n) - 2 * x(n-1) - 4
end function fc

function f2(x) result(res)
    implicit none
    real, dimension(:), intent(in) :: x
    real, dimension(size(x)) :: res
    res(1) = 4 * x(1) ** 2 * x(2) 
    res(2) = x(1) - x(2) ** 2
end function f2

program test_differentiation
    use num_diff_lib
    use utilities
    implicit none
    procedure(efunc) :: f, fc, f2
    integer :: i
    integer, parameter :: n = 10, np = 2
    real, dimension(n) :: x
    real, dimension(n,n) :: a
    real, dimension(np) :: xp
    real, dimension(np,np) :: ap
    do i = 1,n
        x(i) = i * 0.1
    enddo
    xp = 1
    !Calculate jacobi matrix
    do i = 1,n
        a(:,i) = gradient(x, 0.0001, f, n, i, d1x_2p_fd_mult)
    enddo
    call output_amatrix(a)
    do i = 1,n
        a(:,i) = gradient(x, 0.0001, fc, n, i, d1x_2p_fd_mult)
    enddo
    call output_amatrix(a)
    do i = 1,np
        ap(:,i) = gradient(xp, 0.0001, f2, np, i, d1x_2p_fd_mult)
    enddo
    call output_amatrix(ap)
!TODO: OPENMP
end program test_differentiation