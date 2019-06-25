program test_mpi_classic   
    use classic_lib, only : classic_mpi
    use utilities, only : output_matrix
    use mpi_manager
    implicit none
    integer, parameter :: n = 10
    real, dimension(n,n) :: A, res
    call mpi_launch
    call mpi_setup
    A = 1
    !A^2=n
    res = classic_mpi(A, A, n)
    if(is_master) then
        print *,"A"
        call output_matrix(A)
        print *,"RES"
        call output_matrix(res)
    endif
    call mpi_finish
end program test_mpi_classic