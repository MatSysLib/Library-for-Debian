module sort_merge
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    function merge_sort(array, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(:), intent(in) :: array
        real, dimension(n) :: res
        real, dimension((n + 1)/2) :: temp
        call system_clock(T1)
        call sort(array, temp, n)
        call system_clock(T2)
        res = array
    end function merge_sort

    recursive subroutine sort(a, t, n)
        implicit none
        integer, intent(in) :: n
        integer :: na, nb
        real :: temp
        real, dimension(n) :: a
        real, dimension((n + 1)/2) :: t
        if(n < 2) return
        if(n == 2) then
            if(a(1) > a(2)) then
                temp = a(1)
                a(1) = a(2)
                a(2) = temp
            endif
            return
        endif
        na = (n + 1) / 2
        nb = n - na
        call sort(a, t, na)
        call sort(a(na+1), t, nb)
        if(a(na) > a(na + 1)) then
            t(:na) = a(:na)
            call merge(t, a(na+1:), a, na, nb)
        endif
        return
    end subroutine sort

    subroutine merge(fhalf, shalf, mergeres, nfh, nsh)
        integer, intent(in) :: nfh, nsh
        integer :: ai, si, bi
        real, dimension(:) :: fhalf, shalf, mergeres
        ai = 1
        si = 1
        bi = 1
        do while((ai <= nfh) .AND. (bi <= nsh))
            if(fhalf(ai) <= shalf(bi)) then
                mergeres(si) = fhalf(ai)
                ai = ai + 1
            else
                mergeres(si) = shalf(bi)
                bi = bi + 1
            endif
            si = si + 1
        enddo
        if(ai >= nfh) then
            do while(bi <= nsh)
                mergeres(si) = shalf(bi)
                bi = bi + 1
                si = si + 1
            enddo
        endif        
        if(bi >= nsh) then
            do while(ai <= nfh)
                mergeres(si) = fhalf(ai)
                ai = ai + 1
                si = si + 1
            enddo
        endif
    end subroutine merge

    function merge_sort_omp(array, n) result(res)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: array
        integer :: max_threads
        real, dimension(n) :: res, temp
!$OMP   parallel
        max_threads = omp_get_max_threads()
!$OMP   end parallel
        call omp_set_nested(.TRUE.)
        RT1 = omp_get_wtime()
        call sort_omp(array, temp, n, max_threads)
        RT2 = omp_get_wtime()
        res = array
    end function merge_sort_omp

    recursive subroutine sort_omp(a, t, n, threads)
        use omp_lib
        implicit none
        integer, intent(in) :: n, threads
        integer :: na, nb
        real :: temp
        real, dimension(n) :: a, t
        if(n < 2) return
        if(n == 2) then
            if(a(1) > a(2)) then
                temp = a(1)
                a(1) = a(2)
                a(2) = temp
            endif
            return
        endif
        na = (n + 1) / 2
        nb = n - na
        if(threads>1) then
            !$omp parallel sections shared(a)
            !$omp section
            call sort_omp(a, t, na, threads/2)
            !$omp section
            call sort_omp(a(na + 1), t(na+1), nb, threads - threads/2)
            !$omp end parallel sections
        else
            call sort(a, t, na)
            call sort(a(na + 1), t, nb)
        endif
        if(a(na) > a(na + 1)) then
            t(:na) = a(:na)
            call merge(t, a(na+1:), a, na, nb)
        endif
    end subroutine sort_omp

    function merge_sort_mpi(array, n) result(res)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, dimension(:), intent(in) :: array
        integer :: i
        real, dimension(n) :: res, t, a
        real, dimension(:), allocatable :: temp, sub
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(temp(arr_length), sub(arr_length))
        sub = array(arr_start:arr_end)
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime()
        call sort(sub, temp, arr_length)
        if(.NOT.is_master) then
            call MPI_Send(sub, arr_length, MPI_REAL, 0, process_rank, MPI_COMM_WORLD, ierr)
        else
            if(is_worker) res(arr_start:arr_end) = sub
            do i = 1,process_size-1
                call MPI_Recv(t(1:all_arr_len(i+1)), all_arr_len(i+1), MPI_REAL, i, i, MPI_COMM_WORLD, status, ierr)
                a(1:all_arr_start(i+1)-1) = res(1:all_arr_start(i+1)-1)
                call merge(a(1:all_arr_start(i+1)-1), t(1:all_arr_len(i+1)), res, all_arr_start(i+1)-1, all_arr_len(i+1))
            enddo
        endif
        RT2 = MPI_Wtime()
        deallocate(temp, sub)
    end function merge_sort_mpi

    double precision function get_time() result(res)
        implicit none
        integer(kind=8) :: rate      
        CALL system_clock(count_rate=rate)
        res = dble(T2 - T1) / dble(rate)
    end function get_time

    double precision function get_time_par() result(res)
        implicit none
        res = RT2 - RT1
    end function get_time_par
end module sort_merge