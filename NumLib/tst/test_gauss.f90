program test_gauss
    use gauss_lib, only : gauss, gauss_omp
    use utilities, only : output_qmatrix, slau_matrix_create, slau_vector_create
    implicit none
    real, allocatable :: A(:,:), b(:), res(:)
    integer :: n
    n = 10
    allocate(A(n,n), b(n), res(n))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    print *,"A, b:"
    call output_qmatrix(a,n)
    print *,b
    print *," "
    res = gauss(A, b, n)
    print *,"SEQ Result:"
    print *,res
    res = 0
    A = slau_matrix_create(n)
    res = gauss_omp(A, b, n)
    print *,"OMP Result:"
    print *,res
end program test_gauss