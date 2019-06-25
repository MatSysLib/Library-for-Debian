program lu_test
    use lud_lib, only : lud, lud_omp
    use utilities
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    integer :: n
    n = 10
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    call output_qmatrix(a,n)
    print *, b
    res = lud(A, b, n)
    print *,res
    res = 0
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    res = lud_omp(A, b, n)
    print *,res
end program lu_test