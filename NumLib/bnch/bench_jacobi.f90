program bench_jacobi
    use jacobi_lib, only : timing, timing_omp
    use utilities, only : slau_matrix_create, slau_vector_create
    implicit none
    integer :: n, repeats
    real, dimension(:), allocatable :: b
    real, dimension(:,:), allocatable :: A
    real, parameter :: eps = 0.001
    double precision, dimension(:), allocatable :: time
    character(len=8) :: temporary
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
        repeats = 10
    endif
    if(command_argument_count()>=2) then
        call get_command_argument(2, temporary)
        read(temporary, *) repeats
    endif
    allocate(A(n,n), b(n), time(repeats))
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    time = timing(A, b, n, eps, repeats)
    write(*,"(A7,F20.10)") "SEQAVG:", sum(time)/dble(repeats)
    A = slau_matrix_create(n)
    b = slau_vector_create(n)
    time = timing_omp(A, b, n, eps, repeats)
    write(*,"(A7,F20.10)") "OMPAVG:", sum(time)/dble(repeats)
    deallocate(A, b, time)
end program bench_jacobi