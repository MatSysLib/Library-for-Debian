program mpi_test_scalar_mult
    use classic_lib, only : scalar_mult_mpi
    use mpi_manager
    implicit none
    real :: res
    real, allocatable :: A(:), b(:)
    integer :: i, n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n), b(n))
    do i = 1,n
        a(i) = i
        b(i) = i
    enddo
    res = scalar_mult_mpi(A, b)
    if(is_master.EQV..TRUE.) then
        print *,"A"
        print *, a
        print *,"b"
        print *, b
        print *,"RES"
        print *, res
    endif
    deallocate(A, b)
    call mpi_finish
end program mpi_test_scalar_mult