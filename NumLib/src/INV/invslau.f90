module invslau_lib
    implicit none
    integer(kind=8), private :: T1,T2
    double precision, private :: RT1, RT2
    contains
    function invslau(A, n) result(inv)
        use gauss_lib
        use lud_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A
        integer :: i
        real, dimension(n) :: b
        real, dimension(n,n) :: inv, l, u, uorig
        inv = 0
        call SYSTEM_CLOCK(T1)
        call lu_decomposition(a, n, l, uorig)
        do i = 1,n
            b = 0
            b(i) = 1
            u = uorig
            call set_triangle_type('d', 'l', n)
            call null_triangle_gauss_b(L, b)
            call lu_transistion(u, b)
            call set_triangle_type('u', 'r', n)
            call null_triangle_gauss_b(U, b)
            inv(:,i) = b
        enddo
        call SYSTEM_CLOCK(T2)
    end function invslau

    function invslau_omp(A, n) result(inv)
        use gauss_lib
        use lud_lib
        use omp_lib
        use utilities
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A
        integer :: i, tid, tnum, ts, tl, te
        real, dimension(n) :: b
        real, dimension(n,n) :: inv, l, u, uorig
        inv = 0
        RT1 = omp_get_wtime()
        call lu_decomposition_omp(a, n, l, uorig)
!$OMP   parallel private(i, b, u, tid, tl, ts, te) firstprivate(n, l) shared(uorig, inv, tnum)
        tnum = omp_get_num_threads()
        tid = omp_get_thread_num()
        tl = n / tnum
        ts = tid * tl +1
        te = (tid+1) * tl
        if(tid==tnum-1) then
            tl = tl + mod(n,tnum)
            te = n
        endif
        do i = ts,te
            b = 0
            b(i) = 1
            u = uorig
            call set_triangle_type('d', 'l', n)
            if(tid==0) call output_matrix(l)
            call null_triangle_gauss_b(L, b)
            if(tid==0) call output_matrix(l)
            call lu_transistion(u, b)
            call set_triangle_type('u', 'r', n)
            call null_triangle_gauss_b(U, b)
            inv(:,i) = b
        enddo
!$OMP   end parallel
        RT2 = omp_get_wtime()
    end function invslau_omp

    function invslau_mpi(A, n) result(inv)
        use gauss_lib
        use lud_lib
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A
        integer :: i
        integer, dimension(process_size) :: naas
        real, dimension(n) :: b
        real, dimension(n,n) :: inv, l, u, uorig
        real, dimension(:,:), allocatable :: mbuf
        if (state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(mbuf(n,arr_length))
        naas(1) = 0
        do i = 2, process_size
            naas(i) = naas(i - 1) + n*all_arr_len(i - 1)
        enddo
        inv = 0
        RT1 = MPI_Wtime()
        call lu_decomposition_mpi(a, n, l, uorig)
        do i = 1,arr_length
            b = 0
            b(i + arr_start - 1) = 1
            u = uorig
            call set_triangle_type('d', 'l', n)
            call null_triangle_gauss_b(L, b)
            call lu_transistion(u, b)
            call set_triangle_type('u', 'r', n)
            call null_triangle_gauss_b(U, b)
            mbuf(:,i) = b
        enddo
        call MPI_Allgatherv(mbuf, n*arr_length, MPI_REAL, inv, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        RT2 = MPI_Wtime()
        deallocate(mbuf)
    end function invslau_mpi

    function timing(a, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n,n), intent(in) :: a
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = invslau(a, n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = invslau_omp(a, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = invslau_mpi(a, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module invslau_lib