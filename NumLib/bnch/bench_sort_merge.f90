program bench_sort_merge
    use sort_merge, only : merge_sort, merge_sort_omp, get_time, get_time_par
    implicit none
    integer :: n, i, points
    real, dimension(:), allocatable :: A, res
    logical :: error
    character(len=8) :: temporary
    if(command_argument_count()>=1) then
        call get_command_argument(1, temporary)
        read(temporary, *) points
    endif
    error = .false.
    n = points
    allocate(A(n), res(n))
    forall(i=1:n) a(i) = n - i
    res = merge_sort(a, n)
    do i = 1,n-1
        if(res(i)>res(i+1)) then 
            error = .TRUE.
            print *, i, res(i), res(i+1)
            exit
        endif
    enddo
    if(error) then
        print *, "Something wrong"
    else
        write(*,"(A4,F20.10)") "SEQ:", get_time()
    endif 
    error = .false.
    n = points
    forall(i=1:n) a(i) = n - i
    res = merge_sort_omp(a,n)
    do i = 1,n-1
        if(res(i)>res(i+1)) then 
            error = .TRUE.
            print *, i, res(i), res(i+1)
            exit
        endif
    enddo
    if(error) then
        print *, "Something wrong"
    else
        write(*,"(A4,F20.10)") "OMP:", get_time_par()
    endif 
    deallocate(A, res)
end program bench_sort_merge