program test_strassen   
    use strassen_lib, only : strassen, strassen_omp
    use utilities, only : output_qmatrix
    implicit none
    real, allocatable :: A(:,:), res(:,:)
    integer :: n
    n = 10
    allocate(A(n,n), res(n,n))
    A = 1
    print *,"A"
    call output_qmatrix(a,n)
    res = strassen(A, A, n)
    print *,"SEQ:"
    call output_qmatrix(res,n)
    A = 1
    res = 0
    res = strassen_omp(A, A, n)
    print*,"OMP:"
    call output_qmatrix(res,n)
    deallocate(A, res)
end program test_strassen