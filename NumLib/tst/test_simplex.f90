!Robert Israel. Simplex Method Phase I Artificial Variable
program test_simplex
    use simplex_lib, only : simplex_method
    implicit none
    integer :: n, c
    integer, dimension(:), allocatable :: eq_t
    real, dimension(:), allocatable :: f_v, r_v
    real, dimension(:,:), allocatable :: c_m
    n = 3
    c = 4
    allocate(f_v(n), c_m(n+1, c), eq_t(c))
    f_v(1) = -5
    f_v(2) = 1
    f_v(3) = 1
    c_m = 0
    c_m(1,1) = 3
    c_m(2,1) = -1
    c_m(3,1) = -1
    c_m(4,1) = -1
    c_m(1,2) = 1
    c_m(2,2) = 2
    c_m(3,2) = -1
    c_m(4,2) = -2
    c_m(1,3) = 2
    c_m(2,3) = 1

    c_m(4,3) = 2
    c_m(1,4) = 1
    c_m(2,4) = 1
    
    c_m(4,4) = 1
    eq_t(1) = 1
    eq_t(2) = 1
    eq_t(3) = 1
    eq_t(4) = 0
    r_v = simplex_method(f_v, c_m, eq_t, n, c)
    print *, " "
    print *, r_v
    deallocate(f_v, c_m, r_v)
end program test_simplex