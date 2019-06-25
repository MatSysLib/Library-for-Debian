program test_mpi_cblock
    use cblock_lib, only : cblock_mpi
    use utilities, only : output_matrix
    use mpi_manager
    implicit none
    integer, parameter :: n = 10
    real, dimension(n,n) :: A, res
    call mpi_launch
    call mpi_setup
    A = 1
    !A^2=n
    res = cblock_mpi(A, A, n, 4)
    if(is_master) then
        print *,"A"
        call output_matrix(A)
        print *,"RES"
        call output_matrix(Res)
    endif
    call mpi_finish
end program test_mpi_cblock