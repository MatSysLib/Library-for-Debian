program bench_strassen   
    use strassen_lib, only : timing, timing_omp
    implicit none
    integer :: n, repeats = 10
    real, allocatable :: A(:,:)
    double precision, allocatable :: time(:)
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

    allocate(A(n,n), time(repeats))
    A = 1

    time = timing(A, A, n, repeats)
    write(*,"(A7,F20.10)") "SEQAVG:",sum(time)/dble(repeats)

    time = timing_omp(A, A, n, repeats)
    write(*,"(A7,F20.10)") "OMPAVG:",sum(time)/dble(repeats)

    deallocate(A, time)
end program bench_strassen