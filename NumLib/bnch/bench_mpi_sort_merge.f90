program benchmark_mpi_sort_merge
    use sort_merge, only : merge_sort_mpi, get_time_par
    use mpi_manager
    implicit none
    integer :: n, i
    real, dimension(:), allocatable :: A, res
    character(len=8) :: temporary
    call mpi_launch
    call mpi_setup
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) n
    endif
    allocate(A(n), res(n))
    forall(i=1:n) a(i) = n - i
    call mpi_barrier(MPI_COMM_WORLD, ierr)
    res = merge_sort_mpi(a, n)
    if(is_master) write(*,"(F20.10)") get_time_par()
    deallocate(A, res)
    call mpi_finish
end program benchmark_mpi_sort_merge