program ludec_test
    use lud_lib, only : lu_decomposition, lu_decomposition_omp
    use utilities
    implicit none
    real, dimension(:,:), allocatable :: A, L, U
    integer :: n
    n = 10
    allocate(A(n,n), L(n,n), U(n,n))
    A = slau_matrix_create(n)
    call output_matrix(a)
    call lu_decomposition(A, n, L, U)
    print *, "SEQ:"
    print *, "L:"
    call output_matrix(L)
    print *, "U:"
    call output_matrix(U)
    call output_matrix(A)
    L = 0
    U = 0
    A = slau_matrix_create(n)
    call lu_decomposition_omp(A, n, L, U)
    print *, "OMP:"
    print *, "L:"
    call output_matrix(L)
    print *, "U:"
    call output_matrix(U)
end program ludec_test