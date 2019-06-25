module cblock_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    double precision, private :: scalar_T1, scalar_T2
    contains
    function cblock(A, B, n, ichunk) result(C)
        implicit none
        integer :: i, j, k, ii, jj, kk
        integer, intent(in) :: n, ichunk
        real :: C(n,n)
        real, intent(in) :: A(:,:), B(:,:)
        C = 0
        call SYSTEM_CLOCK(T1)
        do ii = 1,n,ichunk
            i = min(ii+ichunk-1,n)
            do jj = 1,n,ichunk
                do kk = 1,n,ichunk
        do j = jj,min(jj+ichunk-1,n)
            do k = kk,min(kk+ichunk-1,n)
                C(ii:i,j) = C(ii:i,j) + A(ii:i,k) * B(k,j)
            enddo
        enddo
            enddo
                enddo
                    enddo
        call SYSTEM_CLOCK(T2)
    end function cblock

    function cblock_omp(A, B, n, ichunk) result(C)
        use omp_lib
        implicit none
        real, intent(in) :: A(:,:), B(:,:)
        real :: C(n,n)
        integer, intent(in) :: n, ichunk
        integer :: i, j, k, ii, jj, kk
        C = 0
        RT1 = omp_get_wtime()
        !$OMP parallel do private(i,j,k,ii,jj,kk) shared(A, B, C)
        do ii=1,n,ichunk
            i = min(ii+ichunk-1,n)
            do jj=1,n,ichunk
                do kk=1,n,ichunk
        do j=jj,min(jj+ichunk-1,n)
            do k=kk,min(kk+ichunk-1,n)
                C(ii:i,j)=C(ii:i,j) + A(ii:i,k)*B(k,j)
            enddo
        enddo
            enddo
                enddo
                    enddo
        !$OMP end parallel do
        RT2 = omp_get_wtime()
    end function cblock_omp

    function cblock_mpi(A, B, n, ichunk) result(C)
        use mpi_manager 
        implicit none
        integer, intent(in) :: n, ichunk
        real, dimension(n,n), intent(in) :: A, B
        integer :: i, j, k, ii, jj, kk
        integer, dimension(process_size) :: naas
        real, dimension(n,n) :: C
        real, dimension(:,:), allocatable :: T, BT
        RT1 = MPI_Wtime()
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(T(n, arr_length), BT(n, arr_length))
        T = 0
        BT = -1
        naas(1) = 0
        do i=2,process_size
            naas(i) = naas(i-1)+n*all_arr_len(i-1)
        enddo
        call MPI_Scatterv(B, n*all_arr_len, naas, MPI_REAL, BT, n*arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        do ii=1,n,ichunk
            i = min(ii+ichunk-1,n)
            do jj=1,arr_length,ichunk
                do kk=1,n,ichunk
        do j=jj,min(jj+ichunk-1,arr_length)
            do k=kk,min(kk+ichunk-1,n)
                T(ii:i,j) = T(ii:i,j) + A(ii:i,k) * B(k,j)
            end do
        enddo
            enddo
                enddo
                    enddo
        if(is_serial) then
            C = T
        else
            call MPI_Allgatherv(T, n*arr_length, MPI_REAL, C, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        deallocate(T, BT)
        RT2 = MPI_Wtime()
    end function cblock_mpi

    double precision function get_scalar_time() result(res)
        implicit none
        res = scalar_T2 - scalar_T1
    end function get_scalar_time

    function timing(a, b, n, ichunk, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats, ichunk
        integer(kind=8) :: rate 
        real, intent(in) :: a(:,:),b(:,:)
        real :: t(n,n)
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = cblock(a, b, n, ichunk)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, b, n, ichunk, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats, ichunk
        real, intent(in) :: a(:,:),b(:,:)
        real :: t(n,n)
        double precision :: res(repeats)
        do i = 1,repeats
            t = cblock_omp(a,b,n, ichunk)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, b, n, ichunk, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats, ichunk
        real, intent(in) :: a(:,:),b(:,:)
        real :: t(n,n)
        double precision :: res(repeats)
        do i = 1,repeats
            t = cblock_mpi(a, b ,n, ichunk)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module cblock_lib