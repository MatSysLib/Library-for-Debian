function f(x) result(res)
    implicit none
    real, dimension(:), intent(in) :: x
    real, dimension(size(x)) :: res
    integer :: i
    do i=1,size(x)
        res(i) = cos(x(i)) - 1
    enddo
end function f

function f2(x) result(res)
    implicit none
    real, dimension(:), intent(in) :: x
    real, dimension(size(x)) :: res
    res(1) = 4 * x(1) ** 2 * x(2) 
    res(2) = x(1) - x(2) ** 2
end function f2
program test_jacobi_matrix
    use num_diff_lib
    use utilities
    implicit none
    procedure(efunc) :: f, f2
    integer, parameter :: n = 10, np = 2
    real, dimension(n) :: x
    real, dimension(n,n) :: a
    real, dimension(np) :: xp
    real, dimension(np,np) :: ap
    x = 0.5
    xp = 1
    a = jacobi_matrix(f, x, 0.0001, n, d1x_2p_fd_mult)
    call output_amatrix(a)
    ap = jacobi_matrix(f2, xp, 0.0001, np, d1x_2p_fd_mult)
    call output_amatrix(ap)
    a = jacobi_matrix_omp(f, x, 0.0001, n, d1x_2p_fd_mult)
    call output_amatrix(a)
    ap = jacobi_matrix_omp(f2, xp, 0.0001, np, d1x_2p_fd_mult)
    call output_amatrix(ap)
!TODO: OPENMP
end program test_jacobi_matrix