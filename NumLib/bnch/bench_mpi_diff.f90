real function func(x) result(res)
    implicit none
    real, intent(in) :: x
    res = (sin(x**2)*cos(x**2)+exp(sin(x**2)*cos(x**2)+x**2)) / (x**2)
end function func
program f_mpi_bench_diff
    use num_diff_lib
    use mpi_manager
    implicit none
    integer :: n, i
    real :: start, end, h, diffh
    real, dimension(:), allocatable :: array, res
    real, external :: func
    double precision :: T1, T2
    character(len=8) :: temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
    endif
    start = 0
    end = 10 * n
    h = 0.01
    diffh = 0.00000000001
    n = ceiling((end - start) / h)
    if(state_mpi==0) call planner(n, .FALSE.)
    allocate(array(arr_length), res(n))
    array = 0
    T1 = MPI_Wtime()
    do i = arr_start,arr_end
        array(i-arr_start+1) = d1x_5p((i-1)*h, diffh, func)
    enddo
    if(.NOT.is_master) then
        call MPI_Send(array, arr_length, MPI_REAL, 0, process_rank, MPI_COMM_WORLD, ierr)
    else
        if(is_worker) res(1:arr_length) = array(:)
        do i = 1,process_size-1
            call MPI_Recv(res(all_arr_start(i+1):all_arr_end(i+1)), all_arr_len(i+1), MPI_REAL, i, i, &
                                                                                            MPI_COMM_WORLD, status, ierr)
        enddo
    endif
    T2 = MPI_Wtime()
    call MPI_Bcast(res, n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
    if(is_master.eqv..true.) write(*,"(F20.10)") T2 - T1
    deallocate(array, res)
    call mpi_finish
end program f_mpi_bench_diff