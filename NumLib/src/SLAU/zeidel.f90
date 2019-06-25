module zeidel_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    subroutine normalization_iteration(A, b, n)
        implicit none
        real :: temp
        real, dimension(n) :: b
        real, dimension(n,n) :: A
        integer :: n, i
        do i = 1,n
            temp = A(i,i)
            A(i,:) = -1 * A(i,:) / temp
            A(i,i) = 0
            b(i) = b(i) / temp
        enddo
    end subroutine normalization_iteration

    function zeidel(i_A, i_b, n, eps) result(res)
        use classic_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: i_b
        real, dimension(n,n), intent(in) :: i_A
        integer :: k, i
        real :: error
        real, dimension(n) :: res, x, b
        real, dimension(n,n) :: ah, a
        x = 0
        error = 1
        res = b
        k = 0
        a = i_a
        b = i_b
        call normalization_iteration(A, b, n)
        ah = a
        do i = 2,n
            ah(i,:i-1) = 0
        end do
        ah = transpose(ah)
        call SYSTEM_CLOCK(T1)
        do while(error > eps)
            k = k + 1
            res = matrixvector(ah, x, n)
            do i=1,n
                res(i) = res(i) + b(i) + scalar_mult(a(i,:i-1), res(:i-1))
            end do
            error = maxval(ABS(x - res))
            x = res
        enddo
        call SYSTEM_CLOCK(T2)
    end function zeidel

    function zeidel_omp(i_A, i_b, n, eps) result(res)
        use omp_lib
        use classic_lib
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: i_b
        real, dimension(n,n), intent(in) :: i_a
        integer :: k, p, nr, i
        integer, dimension(:), allocatable :: larr_l, larr_s, larr_e
        real :: error
        real, dimension(n) :: b
        real, dimension(n,n) :: a
        real, dimension(:), allocatable :: x, res
        real, dimension(:,:), allocatable :: ah
        a = i_a
        b = i_b
        call normalization_iteration(A, b, n)
        !$omp parallel
        p = omp_get_num_threads()
        !$omp end parallel
        allocate(ah(n,n), x(n), res(n),larr_l(p), larr_s(p), larr_e(p))
        ah = a
        do i=2,n
            ah(i,:i-1) = 0
        end do
        x = b
        error = 1
        k = 0
        nr=n/p
        larr_l = nr
        larr_s(1) = 1
        do i = 1,n-nr*p
            larr_l(i) = larr_l(i) + 1
        end do
        do i=2,p
            larr_s(i) = larr_s(i-1) + larr_l(i-1)
            larr_e(i-1) = larr_s(i) - 1
        end do
        larr_e(p) = larr_s(p) + larr_l(p) - 1
        RT1 = omp_get_wtime()
        do while(error>=eps)
            !$omp parallel firstprivate(larr_s, larr_e) shared(x, res, ah, b)
            i = omp_get_thread_num() + 1
            res(larr_s(i):larr_e(i)) = matmul(ah(larr_s(i):larr_e(i), :), x(:))
            !$omp end parallel
            res(1)=res(1)+b(1)
            do i=2,n
                res(i) = res(i) + b(i) + scalar_mult(a(i,:i-1), res(:i-1))
            end do
            error = MAXVAL(ABS(res-x))
            k = k+1
            x = res
        end do
        RT2 = omp_get_wtime()
        deallocate(larr_s, larr_e, larr_l, x, ah)
    end function zeidel_omp

    function zeidel_mpi(i_A, i_b, n, eps) result(res)
        use classic_lib
        use mpi_manager
        use utilities
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(:), intent(in) :: i_b
        real, dimension(:,:), intent(in) :: i_a
        integer :: i
        integer, dimension(process_size) :: naas
        real :: error
        real, dimension(n) :: res, x, b
        real, dimension(:), allocatable :: buf
        real, dimension(:,:), allocatable :: mbuf, a
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(mbuf(n,arr_length), buf(arr_length))
        error = 1
        naas(1) = 0
        do i = 2, process_size
            naas(i) = naas(i - 1) + n*all_arr_len(i - 1)
        enddo
        if(is_master) then
            allocate(a(n,n))
            A = i_a
            b = i_b
            call normalization_iteration(A, b, n)
            do i = 2,n
                a(i,:i-1) = 0
            end do
            a = transpose(a)
            call MPI_Bcast(b, n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        else
            allocate(a(0,0))
            call MPI_Bcast(b, n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        endif
        call MPI_Scatterv(A, n*all_arr_len, naas, MPI_REAL, mbuf, n*arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime() 
        do while(error > eps)
            buf = 0
            do i = 1,arr_length
                buf(i) = buf(i) + scalar_mult(mbuf(:,i),x)
            enddo
            call MPI_Allgatherv(buf, arr_length, MPI_REAL, res, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
            if(is_master) then
                do i = 1,n
                    res(i) = res(i) + b(i) + scalar_mult(a(i,:i-1), res(:i-1))
                end do
            endif
            call MPI_Bcast(res, n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
            error = maxval(abs(x - res))
            x = res
        enddo
        RT2 = MPI_Wtime()
        deallocate(mbuf, buf, a)
    end function zeidel_mpi

    function timing(A, b, n, eps, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: A
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = zeidel(A, b, n, eps)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing    

    function timing_omp(A, b, n, eps, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: A
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = zeidel_omp(A, b, n, eps)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp
    
    function timing_mpi(A, b, n, eps, repeats) result(res)
        integer, intent(in) :: n, repeats
        real, intent(in) :: eps
        real, dimension(:), intent(in) :: b
        real, dimension(:,:), intent(in) :: A
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = zeidel_mpi(A, b, n, eps)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module zeidel_lib