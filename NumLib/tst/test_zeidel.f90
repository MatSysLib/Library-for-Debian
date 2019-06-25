program test_zeidel
    use zeidel_lib
    use utilities
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    real :: eps = 0.0001
    integer :: n
    n = 10
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    print *, "A:"
    call output_matrix(a)
    print *, "B:"
    print *, b
    res = zeidel(A, b, n, eps)
    print *, "SEQ:"
    print *,res
    res = 0
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    !res = zeidel_omp(A, b, n, eps)
    !print *, "OMP:"
    !print *,res
    deallocate(A, b, res)
end program test_zeidel