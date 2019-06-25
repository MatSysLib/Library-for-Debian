program test_shultz  
    use shultz_lib, only : shultz, shultz_omp
    use utilities, only : output_qmatrix, slau_matrix_create
    implicit none
    real, allocatable :: A(:,:), res(:,:)
    real :: eps = 0.0001, alpha = 0.5
    integer :: n
    n = 10
    allocate(A(n,n), res(n,n))
    A = slau_matrix_create(n)
    print *,"A:"
    call output_qmatrix(a,n)
    res = shultz(A, n, alpha, eps)
    print *,"RESSEQ:"
    call output_qmatrix(res,n)
    A = slau_matrix_create(n)
    res = 0
    !res = shultz_omp(A, n, alpha, eps)
    !print *,"OMP:"
    !call output_qmatrix(res,n)
end program test_shultz