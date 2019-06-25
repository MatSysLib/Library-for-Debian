program bench_shultz  
    use invgj_lib, only : invgj
    use shultz_lib, only : timing, timing_omp
    use utilities, only : output_qmatrix, slau_matrix_create
    implicit none
    integer :: n, repeats
    real :: eps = 0.001, alpha = 0.01
    real, dimension(:,:), allocatable :: A, res
    double precision, dimension(:), allocatable :: time
    character(len=8)::temporary
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
        repeats = 10
    endif
    if(command_argument_count()>=2) then
        call get_command_argument(2, temporary)
        read(temporary, *) repeats
    endif
    allocate(A(n,n), time(repeats), res(n,n))
    A = slau_matrix_create(n)
    time = timing(A, n, alpha, eps, repeats)
    write(*,"(A7,F20.10)") "SEQAVG:",sum(time)/dble(repeats)
    A = slau_matrix_create(n)
    res = invgj(A, n)
    print *, res(1,1), res(2,1)
    !A = slau_matrix_create(n)
    !time = timing_omp(A, n, alpha, eps, repeats)
    !write(*,"(A7,F20.10)") "OMPAVG:",sum(time)/dble(repeats)
    deallocate(A, time, res)
end program bench_shultz