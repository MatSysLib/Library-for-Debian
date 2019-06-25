program mpi_test_ludec
    use lud_lib, only : lu_decomposition_mpi
    use utilities, only : output_matrix, slau_matrix_create
    use mpi_manager
    implicit none
    real, allocatable :: A(:,:), L(:,:), U(:,:)
    integer :: n
    call mpi_launch
    call mpi_setup
    n = 10
    allocate(A(n,n), L(n,n), U(n,n))
    A = slau_matrix_create(n)
    if(is_master.EQV..TRUE.) then
        print *,"A:"
        call output_matrix(A)
    endif
    call lu_decomposition_mpi(a, n, l, u)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    if(is_master.EQV..TRUE.) then
        call output_matrix(L)
        call output_matrix(U)
    endif
    deallocate(A, L, U)
    call mpi_finish
end program mpi_test_ludec