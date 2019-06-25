program bench_mymult_vs_matmul
    use classic_lib, only : timing_classic => timing, timing_classic_omp => timing_omp
    use strassen_lib, only : timing_strassen => timing, timing_strassen_omp => timing_omp, timing_recursive, timing_recursive_omp
    use cblock_lib, only : timing_cblock => timing, timing_cblock_omp => timing_omp
    implicit none
    integer :: n, repeats
    integer, parameter :: ichunk = 512
    real, allocatable :: A(:,:), TEMP(:,:)
    double precision, allocatable :: time(:)
    integer(kind=8) :: LT1, LT2, r
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
    allocate(time(repeats), A(n,n), TEMP(n,n))
    A = 1

    time = timing_classic(A, A, n, repeats)
    write(*,"(A10,F20.10)") "CLSSEQAVG:",sum(time)/dble(repeats)

    time = timing_classic_omp(A, A, n, repeats)
    write(*,"(A10,F20.10)") "CLSOMPAVG:",sum(time)/dble(repeats)

    time = timing_strassen(A, A, n, repeats)
    write(*,"(A10,F20.10)") "STRSEQAVG:",sum(time)/dble(repeats)

    time = timing_recursive(A, A, n, repeats)
    write(*,"(A10,F20.10)") "STRRECAVG:",sum(time)/dble(repeats)

    time = timing_strassen_omp(A, A, n, repeats)
    write(*,"(A10,F20.10)") "STROMPAVG:",sum(time)/dble(repeats)

    time = timing_recursive_omp(A, A, n, repeats)
    write(*,"(A10,F20.10)") "STREOMAVG:",sum(time)/dble(repeats)

    time = timing_cblock(A, A, n, ichunk, repeats)
    write(*,"(A10,F20.10)") "BLKSEQAVG:",sum(time)/dble(repeats)

    time = timing_cblock_omp(A, A, n, ichunk, repeats)
    write(*,"(A10,F20.10)") "BLKOMPAVG:",sum(time)/dble(repeats)

    CALL system_clock(count_rate=r)
    call SYSTEM_CLOCK(LT1)
    TEMP = MATMUL(A, A)
    call SYSTEM_CLOCK(LT2)
    write(*,"(A7,F20.10)") "MATMUL:",dble((LT2-LT1))/dble(r)
    deallocate(A, time)
end program bench_mymult_vs_matmul