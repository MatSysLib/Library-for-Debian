module classic_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    double precision, private :: scalar_T1, scalar_T2
    contains
    function classic(A, B, n) result(C)
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A, B
        integer :: j, k
        real, dimension(n,n) :: C
        C = 0
        call SYSTEM_CLOCK(T1)
        do j = 1,n
            do k = 1,n
                C(:,j) = C(:,j) + A(:,k) * B(k,j)
            end do
        end do 
        call SYSTEM_CLOCK(T2)
    end function classic

    function classic_omp(A, B, n) result(C)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A, B
        integer :: j, k
        real, dimension(n,n) :: C
        C = 0
        RT1 = omp_get_wtime()
        !$omp parallel do shared(a, b, c) private(j, k)
        do j = 1,n
            do k = 1,n
                C(:,j) = C(:,j) + A(:,k) * B(k,j)
            end do
        end do
        !$omp end parallel do
        RT2 = omp_get_wtime()
    end function classic_omp

    function classic_mpi(A, B, n) result(C)
        use mpi_manager 
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A, B
        integer :: j, k
        integer, dimension(process_size) :: naas
        real, dimension(n,n) :: C
        real, dimension(:,:), allocatable :: T, BT
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(T(n, arr_length), BT(n, arr_length))
        T = 0
        BT = -1
        naas(1) = 0
        do j=2,process_size
            naas(j) = naas(j-1)+n*all_arr_len(j-1)
        enddo
        call MPI_Scatterv(B, n*all_arr_len, naas, MPI_REAL, BT, n*arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime()
        do j = 1,arr_length
            do k = 1,n
                T(:,j) = T(:,j) + A(:,k) * BT(k,j)
            end do
        end do
        if(is_serial) then
            C = T
        else
            call MPI_Gatherv(T, n*arr_length, MPI_REAL, C, n*all_arr_len, naas, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        endif
        RT2 = MPI_Wtime()
        call MPI_Bcast(C, n*n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        deallocate(T, BT)
    end function classic_mpi

    function matrixvector(a, b, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i, j
        real, dimension(n) :: res
        res = 0
        do i = 1,n
            do j = 1,n
                res(i) = res(i) + a(j,i) * b(j)
            enddo
        enddo
    end function matrixvector

    function matrixvector_omp(a, b, n) result(res)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i, j
        real, dimension(n) :: res
        res = 0
        RT1 = omp_get_wtime()
        !$omp parallel do reduction(+:res)
        do i = 1,n
            do j = 1,n
                res(i) = res(i) + a(j,i) * b(j)
            enddo
        enddo
        !$omp end parallel do
        RT2 = omp_get_wtime()
    end function matrixvector_omp

    function matrixvector_mpi(a, b, n) result(res)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: a
        integer :: i, j
        integer, dimension(process_size) :: naas
        real, dimension(n) :: res
        real, dimension(:), allocatable :: temp
        real, dimension(:,:), allocatable :: AT
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        naas(1) = 0
        do i = 2,process_size
            naas(i) = naas(i-1)+n*all_arr_len(i-1)
        enddo   
        allocate(AT(n,arr_length), temp(arr_length))
        temp = 0
        call MPI_Scatterv(A, n*all_arr_len, naas, MPI_REAL, AT, n*arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime()
        do i = 1,arr_length
            do j = 1,n
                temp(i) = temp(i) + AT(j,i) * b(j)
            enddo
        enddo
        if(is_serial) then
            res = temp
        else
            call MPI_Allgatherv(temp, arr_length, MPI_REAL, res, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        RT2 = MPI_Wtime()
        deallocate(AT)
    end function matrixvector_mpi

    real function scalar_mult(a, b) result(res)
        implicit none
        real, intent(in), dimension(:) :: a, b
        integer :: i
        res = 0
        call SYSTEM_CLOCK(T1)
        do i = 1,size(a)
            res = res + a(i) * b(i)
        enddo
        call SYSTEM_CLOCK(T2)
    end function scalar_mult

    real function scalar_mult_omp(a, b) result(res)
        use omp_lib        
        implicit none
        real, intent(in), dimension(:) :: a, b
        integer :: i
        res = 0
        scalar_T1 = omp_get_wtime()
        !$omp parallel do reduction(+:res)
        do i = 1,size(a)
            res = res + a(i) * b(i)
        enddo
        !$omp end parallel do
        scalar_T2 = omp_get_wtime()
    end function scalar_mult_omp

    real function scalar_mult_mpi(a, b) result(res)
        use mpi_manager
        implicit none
        real, intent(in), dimension(:) :: a, b
        integer :: n, i
        real :: lres
        n = size(a)
        res = 0
        lres = 0
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        scalar_T1 = MPI_Wtime()
        do i = arr_start,arr_end
            lres = lres + a(i) * b(i)
        enddo
        call MPI_Reduce(lres, res, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
        scalar_T2 = MPI_Wtime()
        call MPI_Bcast(res, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
    end function scalar_mult_mpi

    double precision function get_scalar_time() result(res)
        implicit none
        res = scalar_T2 - scalar_T1
    end function get_scalar_time

    function timing(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, dimension(n,n), intent(in) :: a, b
        integer :: i
        integer(kind=8) :: rate 
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = classic(a, b, n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, dimension(n,n), intent(in) :: a, b
        integer :: i
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = classic_omp(a,b,n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, dimension(n,n), intent(in) :: a, b
        integer :: i
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = classic_mpi(a,b,n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module classic_lib