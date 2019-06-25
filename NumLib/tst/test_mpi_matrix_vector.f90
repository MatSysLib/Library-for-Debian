program mpi_test_matrix_vector
    use classic_lib, only : matrixvector_mpi
    use utilities, only : output_qmatrix
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    integer :: i, j, n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n,n), b(n), res(n))
    do i = 1,n
        do j=1,n
            A(j,i) = j+(i-1)*n
        enddo
        b(i) = i
    enddo
    res = matrixvector_mpi(A, b, n)
    if(is_master.EQV..TRUE.) then
        print *,"A"
        call output_qmatrix(A, n)
        print *,"b"
        print *, b
        print *,"RES"
        print *, res
    endif
    deallocate(A, b, res)
    call mpi_finish
end program mpi_test_matrix_vector