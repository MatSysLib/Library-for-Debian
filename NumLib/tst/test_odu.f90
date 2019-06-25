program test_odu
    use eiler_lib, only : eiler_system_solve_test, eiler_system_solve_omp_test
    use rk_lib, only : rk_system_solve_test, rk_system_solve_omp_test
    use adams5_lib, only : adams5_system_solve_test, adams5_system_solve_omp_test
    implicit none
    integer :: n
    real :: a, b, h
    real, allocatable :: res(:)
    n = 10
    allocate(res(n))
    a = 0
    b = 1
    h = 0.1
    print *,"ESEQ:"
    res = eiler_system_solve_test(a,b,h,n)
    print *, res
    print *,"EOMP:"
    res = eiler_system_solve_omp_test(a,b,h,n)
    print *, res
    print *,"RK4SEQ:"
    res = rk_system_solve_test(a,b,h,n)
    print *, res
    print *,"RK4OMP:"
    res = rk_system_solve_omp_test(a,b,h,n)
    print *, res
    print *,"A5SEQ:"
    res = adams5_system_solve_test(a,b,h,n)
    print *, res
    print *,"A5OMP:"
    res = adams5_system_solve_omp_test(a,b,h,n)
    print *, res
    deallocate(res)
end program test_odu