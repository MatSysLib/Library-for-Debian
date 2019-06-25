program test_kramer
    use kramer_lib
    use utilities
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    integer :: n
    n = 10
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    call output_qmatrix(a,n)
    print *,b
    res = kramer(A, b, n)
    print *,res
end program test_kramer