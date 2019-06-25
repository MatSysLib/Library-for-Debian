module tda_lib
    implicit none
    interface tda
        module procedure tda_v, tda_m
    end interface
    interface tda_omp
        module procedure tda_v_omp, tda_m_omp
    end interface
    interface tda_mpi
        module procedure tda_v_mpi, tda_m_mpi
    end interface
    integer(kind=8) :: T1, T2
    double precision :: RT1, RT2
    contains
    real function rrlog2(x)
        implicit none
        real, intent(in) :: x
        rrlog2 = log(x) / log(2.)
    end function

    integer function iilog2(x)
        implicit none
        integer, intent(in) :: x
        iilog2 = int(log(real(x)) / log(2.))
    end function

    function tda_m(a, b, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n) :: res, diagonal, updiagonal, undiagonal
        do i=1,n
            diagonal(i) = a(i,i)
        enddo
        updiagonal = 0
        undiagonal = 0
        do i = 1,n-1
            updiagonal(i) = a(i+1,i)
        enddo
        do i = 2,n
            undiagonal(i) = a(i-1,i)
        enddo
        res = tda_v(diagonal, undiagonal, updiagonal, b, n)
    end function tda_m

    function tda_v(c, a, b, d, n) result(res)
        integer, intent(in) :: n
        real, dimension(n) :: a, b, c, d, res
        integer :: i
        call system_clock(T1)
        b(1) = b(1) / c(1)
        d(1) = d(1) / c(1)
        do i = 2,n
            b(i) = b(i) / (c(i) - a(i) * b(i-1))
            d(i) = (d(i) - a(i) * d(i-1)) / (c(i) - a(i) * b(i-1))
        enddo
        do i = n-1,1,-1
            d(i) = d(i) - b(i) * d(i+1)
        enddo
        call system_clock(T2)
        res = d
    end function tda_v

    function tda_m_omp(a, b, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n) :: res, diagonal, updiagonal, undiagonal
        do i=1,n
            diagonal(i) = a(i,i)
        enddo
        updiagonal = 0
        undiagonal = 0
        do i = 1,n-1
            updiagonal(i) = a(i+1,i)
        enddo
        do i = 2,n
            undiagonal(i) = a(i-1,i)
        enddo
        res = tda_v_omp(diagonal, undiagonal, updiagonal, b, n)
    end function tda_m_omp

    function tda_v_omp(c, a, b, d, n) result(res)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        integer :: i, tnum, tid, ts, te, tl
        real, dimension(:), allocatable :: fa, fb, fc, fd, fr
        real, dimension(n) :: a, b, c, d, res
        !$omp parallel 
        tnum = omp_get_num_threads()
        tid = omp_get_thread_num()
        !$omp end parallel
        allocate(fa(2*tnum), fb(2*tnum), fc(2*tnum), fd(2*tnum), fr(2*tnum))
        fa = 0
        fb = 0
        fc = 0
        fd = 0
        res = 0
        b(n) = b(n-1)
        !FWD
        RT1 = omp_get_wtime()
!$OMP   parallel shared(fa, fb, fc, fd, a, b, c, d, tnum) private(tid, tl, ts, te)
        tid = omp_get_thread_num()
        tl = n / tnum
        ts = tid * tl +1
        te = (tid+1) * tl
        if(tid==tnum-1) then
            tl = tl + mod(n,tnum)
            te = n
        endif
        do i = ts+1,te
            a(i) = a(i) / c(i-1)
            c(i) = c(i) - b(i-1) * a(i)
            d(i) = d(i) - d(i-1) * a(i)
        enddo
        !BWD
        do i = te-1,ts,-1
            b(i) = b(te) / c(i+1)
            a(i) = a(i) - a(i+1) * b(i)
            d(i) = d(i) - d(i+1) * b(i)
        enddo
        if(tid/=0) then
            fa(2*tid+1) = a(ts)
            fa(2*tid+2) = a(te)
        endif
        fb(2*tid+1) = b(ts)
        fb(2*tid+2) = b(te)
        fc(2*tid+1) = c(ts)
        fc(2*tid+2) = c(te)
        fd(2*tid+1) = d(ts)
        fd(2*tid+2) = d(te)
!$OMP   end parallel
        fr = tda_v(fc, fa, fb, fd, 2*tnum)
!$OMP   parallel shared(res) private(tid, tl, ts, te)
        tid = omp_get_thread_num()
        tl = n / tnum
        ts = tid * tl +1
        te = (tid+1) * tl
        if(tid==tnum-1) then
            tl = tl + mod(n,tnum)
            te = n
        endif
        res(ts) = fr(2*tid+1)
        res(te) = fr(2*tid+2)
        do i = te-1,ts+1,-1
            res(i) = ((d(i)-a(i))-(d(te)-c(te)-a(te))*b(i)/b(te))/c(i)
        enddo
!$OMP   end parallel
        RT2 = omp_get_wtime()
        deallocate(fa, fb, fc, fd, fr)
    end function tda_v_omp

    function tda_m_mpi(a, b, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n) :: res, diagonal, updiagonal, undiagonal
        do i=1,n
            diagonal(i) = a(i,i)
        enddo
        updiagonal = 0
        undiagonal = 0
        do i = 1,n-1
            updiagonal(i) = a(i+1,i)
        enddo
        do i = 2,n
            undiagonal(i) = a(i-1,i)
        enddo
        res = tda_v_mpi(diagonal, undiagonal, updiagonal, b, n)
    end function tda_m_mpi

    function tda_v_mpi(c, a, b, d, n) result(res)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        integer :: i
        real, dimension(2) :: pres
        real, dimension(8) :: package
        real, dimension(process_size * 2) :: fa, fb, fc, fd, fr
        real, dimension(:), allocatable :: la, lb, lc, ld, lres
        real, dimension(n) :: a, b, c, d, res
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(la(arr_length), lb(arr_length), lc(arr_length), ld(arr_length), lres(arr_length))
        la = a(arr_start:arr_end)
        lb = b(arr_start:arr_end)
        lc = c(arr_start:arr_end)
        ld = d(arr_start:arr_end)
        fa = 0
        fb = 0
        fc = 0
        fd = 0
        lres = 0
        if(process_rank==process_size-1) lb(arr_length) = lb(arr_length-1)
        !FWD
        RT1 = MPI_Wtime()
        do i = 2,arr_length
            la(i) = la(i) / lc(i-1)
            lc(i) = lc(i) - lb(i-1) * la(i)
            ld(i) = ld(i) - ld(i-1) * la(i)
        enddo
        !BWD
        do i = arr_length-1,1,-1
            lb(i) = lb(arr_length) / lc(i+1)
            la(i) = la(i) - la(i+1) * lb(i)
            ld(i) = ld(i) - ld(i+1) * lb(i)
        enddo
        !Assuming master on zero node
        if(is_master) then
            fc(1) = lc(arr_start)
            fc(2) = lc(arr_end)
            fb(1) = lb(arr_start)
            fb(2) = lb(arr_end)
            fd(1) = ld(arr_start)
            fd(2) = ld(arr_end)
            do i = 2,process_size
                call MPI_Recv(package, 8, MPI_REAL, i-1, 0, MPI_COMM_WORLD, status, ierr)
                fa(2*i-1) = package(1)
                fa(2*i) = package(2)
                fc(2*i-1) = package(3)
                fc(2*i) = package(4)
                fb(2*i-1) = package(5)
                fb(2*i) = package(6)
                fd(2*i-1) = package(7)
                fd(2*i) = package(8)
            enddo
            fr = tda_v(fc, fa, fb, fd, 2*process_size)
            do i = 2,process_size
                call MPI_Send([fr(2*i-1), fr(2*i)], 2, MPI_REAL, i-1, 1, MPI_COMM_WORLD, ierr)
            enddo
            lres(1) = fr(1)
            lres(arr_length) = fr(2)
            la = 0
        else
            if(process_rank==process_size-1) lb = 0
            call MPI_Send([la(1), la(arr_length),  &
                           lc(1), lc(arr_length),  &
                           lb(1), lb(arr_length),  &
                           ld(1), ld(arr_length)], &
                           8, MPI_REAL, 0, 0, MPI_COMM_WORLD, ierr)
            call MPI_Recv(pres, 2, MPI_REAL, 0, 1, MPI_COMM_WORLD, status, ierr)
            if(process_rank==process_size-1) then 
                lb = la
                la = 0
            endif
            lres(1) = pres(1)
            lres(arr_length) = pres(2)
        endif
        do i = arr_length-1,2,-1
            lres(i) = ((d(i)-a(i))-(d(arr_length)-c(arr_length)-a(arr_length))*b(i)/b(arr_length))/c(i)
        enddo
        RT2 = MPI_Wtime()
        call MPI_Allgatherv(lres, arr_length, MPI_REAL, res, all_arr_len, all_arr_start - 1, MPI_REAL, MPI_COMM_WORLD, ierr)
        deallocate(la, lb, lc, ld, lres)
    end function tda_v_mpi

    function timing(undg, diag, updg, rvec, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(:), intent(in) :: undg, diag, updg, rvec
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1, repeats
            t = tda_v(diag, undg, updg, rvec, n)
            res(i) = dble(T2 - T1)/dble(rate)
        enddo
    end function timing

    function timing_omp(undg, diag, updg, rvec, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(:), intent(in) :: undg, diag, updg, rvec
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res(repeats)
        do i = 1, repeats
            t = tda_v_omp(diag, undg, updg, rvec, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(undg, diag, updg, rvec, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(:), intent(in) :: undg, diag, updg, rvec
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res(repeats)
        do i = 1, repeats
            t = tda_v_mpi(diag, undg, updg, rvec, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module tda_lib