module lud_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    function lud(A, b, n) result(res)
        use gauss_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n) :: res
        real, dimension(n,n) :: A, L, U
        res = b
        call SYSTEM_CLOCK(T1)
        call lu_decomposition(a, n, l, u)
        call set_triangle_type('d', 'l', n)
        call null_triangle_gauss_b(L, res)
        call lu_transistion(U, res)
        call set_triangle_type('u', 'r', n)
        call null_triangle_gauss_b(U, res)
        call SYSTEM_CLOCK(T2)
    end function lud

    function lud_omp(A,b,n) result(res)
        use gauss_lib
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n) :: res
        real, dimension(n,n) :: A, L, U
        res = b
        RT1 = omp_get_wtime()
        call lu_decomposition_omp(a, n, l, u)
        call set_triangle_type('d', 'l', n)
        call null_triangle_gauss_b_omp(L, res)
        call lu_transistion_omp(U, res)
        call set_triangle_type('u', 'r', n)
        call null_triangle_gauss_b_omp(U, res)
        RT2 = omp_get_wtime()
    end function lud_omp

    function lud_mpi(A,b,n) result(res)
        use gauss_lib
        use mpi
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n) :: res
        real, dimension(n,n) :: A, L, U
        res = b
        RT1 = MPI_Wtime()
        call lu_decomposition_mpi(a, n, l, u)
        call set_triangle_type('d', 'l', n)
        call null_triangle_gauss_b_mpi(L, res, n)
        call lu_transistion_mpi(u, res)
        call set_triangle_type('u', 'r', n)
        call null_triangle_gauss_b_mpi(U, res, n)
        RT2 = MPI_Wtime()
    end function lud_mpi

    function timing(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = lud(a, b, n)
            res(i) = dble(T2-T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = lud_omp(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = lud_mpi(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi

    subroutine lu_transistion(u, b)
        implicit none
        real, dimension(:), intent(inout) :: b
        real, dimension(size(b),size(b)), intent(inout) :: u
        integer :: n, i
        n = size(b)
        do i=1,n
            b(i) = b(i) / u(i,i)
            u(i:,i) = u(i:,i) / u(i,i)
        enddo
    end subroutine lu_transistion

    subroutine lu_transistion_omp(u, b)
        implicit none
        real, dimension(:), intent(inout) :: b
        real, dimension(size(b),size(b)), intent(inout) :: u
        integer :: n, i, j
        real :: temp
        n = size(b)
        !$omp parallel do private(temp)
        do i=1,n
            temp = u(i,i)
            b(i) = b(i) / temp
            do j=i,n
                u(j,i) = u(j,i) / temp
            enddo
        enddo
        !$omp end parallel do
    end subroutine lu_transistion_omp

    subroutine lu_transistion_mpi(u, b)
        use mpi_manager
        implicit none
        real, dimension(:), intent(inout) :: b
        real, dimension(size(b),size(b)), intent(inout) :: u
        real, dimension(:), allocatable :: vbuf
        real, dimension(:,:), allocatable :: mbuf
        integer :: n, i, c_pos
        integer, dimension(process_size) :: naas
        n = size(b)
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(vbuf(n), mbuf(n, arr_length))
        mbuf = u(:, arr_start:arr_end)
        vbuf = b(arr_start:arr_end)
        naas(1) = 0
        do i = 2,process_size
            naas(i) = naas(i-1) + n * all_arr_len(i-1)
        enddo
        c_pos = arr_start
        do i = 1,arr_length
            vbuf(i) = vbuf(i) / mbuf(c_pos,i)
            mbuf(:,i) = mbuf(:,i)/ mbuf(c_pos,i)
            c_pos = c_pos + 1
        enddo
        call MPI_Allgatherv(vbuf, arr_length, MPI_REAL, b, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
        call MPI_Allgatherv(mbuf, n*arr_length, MPI_REAL, u, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        deallocate(vbuf, mbuf)
    end subroutine lu_transistion_mpi

    subroutine lu_decomposition(a, n, l, u)
        integer, intent(in) :: n
        real, dimension(n,n) :: a
        real, dimension(n,n), intent(out) :: l, u
        integer :: i, j
        real :: temp
        u = 0
        l = 0
        do i=1,n      
            L(i,i) = 1
        enddo
        do i = 1,n-1
            do j = i+1,n
                temp = A(j,i)/A(j,j)
                A(i+1:n,j) = A(i+1:n,j) - temp * A(i+1:n,i)
                A(i,j) = temp
            enddo
        enddo
        do i = 1,n
            L(:i-1,i) = A(:i-1,i)
            U(i:,i) = A(i:,i)
        enddo
    end subroutine lu_decomposition

    subroutine lu_decomposition_omp(a, n, l, u)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n) :: a
        real, dimension(n,n), intent(out) :: l, u
        integer :: i, j, k
        real :: temp
        u = 0
        l = 0
        do i=1,n      
            L(i,i) = 1
        enddo
        do i = 1,n-1
            !$omp parallel do private(temp,j,k)
            do j = i+1,n
                temp = A(j,i) / A(j,j)
                do k = i+1,n
                    A(k,j) = A(k,j) - temp * A(k,i)
                enddo
                A(i,j) = temp
            enddo
            !$omp end parallel do
        enddo
        do i = 1,n
            L(:i-1,i) = A(:i-1,i)
            U(i:,i) = A(i:,i)
        enddo
    end subroutine lu_decomposition_omp

    subroutine lu_decomposition_mpi(a, n, l, u)
        use mpi_manager
        implicit none
        real :: a(:,:), p_t(n), v_temp(n), vt
        real, dimension(:,:), allocatable :: sendbuf, vtbuf
        real, dimension(n,n), intent(out) :: l, u
        integer :: n, i, j, p, c_now
        integer, dimension(process_size) :: naas
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(sendbuf(n, arr_length), vtbuf(n, arr_length))
        naas(1) = 0
        do i = 2,process_size
            naas(i) = naas(i-1) + n * all_arr_len(i-1)
        enddo
        sendbuf = a(:, arr_start:arr_end)
        u = 0
        c_now = 1
        do p = 0,process_size-1
            do i = 1,all_arr_len(p+1)
                if(process_rank == p) then
                    vt = sendbuf(arr_start + i - 1, i)
                    vtbuf(arr_start + i - 1, i) = vt
                    sendbuf(:,i) = sendbuf(:,i) / vt
                    call MPI_Bcast(sendbuf(c_now:n, i), n - c_now + 1, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    do j = i+1,all_arr_len(p+1)
                        vt = sendbuf(all_arr_start(p+1)+i-1,j)
                        vtbuf(all_arr_start(p+1)+i-1,j) = vt
                        sendbuf(:,j) = sendbuf(:,j) - vt * sendbuf(:,i)
                    enddo
                elseif(process_rank>p) then
                    p_t = 0
                    call MPI_Bcast(p_t(abs(c_now-1)+1), n - c_now + 1, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    v_temp = p_t(1:n)
                    do j = 1,arr_length
                        vt = sendbuf(all_arr_start(p+1)+i-1,j)
                        vtbuf(all_arr_start(p+1)+i-1,j) = vt
                        sendbuf(:,j) = sendbuf(:,j) - vt * v_temp(:)
                    enddo
                else
                    !TODO:Unneeded info transfer
                    call MPI_Bcast(p_t(abs(c_now-1)+1), n - c_now + 1, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                endif
                c_now = c_now + 1
            enddo
        enddo
        if(is_serial) then
            a = sendbuf
        else
            call MPI_Allgatherv(sendbuf, n*arr_length, MPI_REAL, l, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
            call MPI_Allgatherv(vtbuf, n*arr_length, MPI_REAL, u, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        l = transpose(l)
        u = transpose(u)
        deallocate(sendbuf, vtbuf)
    end subroutine lu_decomposition_mpi
end module lud_lib