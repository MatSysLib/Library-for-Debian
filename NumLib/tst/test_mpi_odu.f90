program test_mpi_odu
    use eiler_lib
    use rk_lib
    use adams5_lib
    use mpi_manager
    implicit none
    integer, parameter :: n = 10
    real :: a, b, h
    real, dimension(n) :: res
    call mpi_launch
    call mpi_setup
    res = 0
    a = 0
    b = 1
    h = 0.1
    res = eiler_system_solve_mpi_test(a, b, h, n)
    if(is_master) then
        print *,"RES:"
        print *, res
    endif
    res = rk_system_solve_mpi_test(a, b, h, n)
    if(is_master) then
        print *,"RES:"
        print *, res
    endif
    res = adams5_system_solve_mpi_test(a, b, h, n)
    if(is_master) then
        print *,"RES:"
        print *, res
    endif
    call mpi_finish
end program test_mpi_odu