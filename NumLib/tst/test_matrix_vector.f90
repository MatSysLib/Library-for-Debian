program test_matrix_vector
    use classic_lib, only : matrixvector, matrixvector_omp
    use utilities, only : output_qmatrix
    implicit none
    real, allocatable :: A(:,:), b(:)
    integer :: i, j, n
    n = 10
    allocate(A(n,n), b(n))
    do i=1,n
        do j=1,n
            A(j,i) = j+(i-1)*n
        enddo
        b(i) = i
    enddo
    print *, "A:"
    call output_qmatrix(a,n)
    print *, "B:"
    print *, b
    print *, "A*b:"
    print *, matrixvector(A, b, n)
    print *, "A*b(OMP):"
    print *, matrixvector_omp(A, b, n)
    !print *, "MATMUL:"
    !print *, matmul(A, b)
    deallocate(a, b)
end program test_matrix_vector