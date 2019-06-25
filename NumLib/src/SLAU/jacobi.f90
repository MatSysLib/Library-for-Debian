module jacobi_lib
    implicit none
    integer(kind=8), private :: T1,T2
    double precision, private :: RT1, RT2
    contains
    function jacobi(i_a, i_b, n, eps) result(res)
        use classic_lib, only : matrixvector
        use zeidel_lib, only : normalization_iteration
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(:), intent(in) :: i_b
        real, dimension(:,:), intent(in) :: i_a 
        real :: error
        real, dimension(n) :: res, x, b
        real, dimension(n,n) :: a
        a = i_a
        b = i_b
        call normalization_iteration(A, b, n)
        error = 1
        res = b
        call SYSTEM_CLOCK(T1)
        do while(error > eps)
            x = res
            res = b + matrixvector(a, res, n)
            error = maxval(abs(x-res))
        enddo
        call SYSTEM_CLOCK(T2)
    end function jacobi

    function jacobi_omp(i_a, i_b, n, eps) result(res)
        use classic_lib, only : matrixvector_omp
        use zeidel_lib, only : normalization_iteration
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(:), intent(in) :: i_b
        real, dimension(:,:), intent(in) :: i_a 
        integer :: i, j
        real :: error
        real, dimension(n) :: res, x, b
        real, dimension(n,n) :: a
        a = i_a
        b = i_b
        call normalization_iteration(A, b, n)
        error = 1
        res = b
        RT1 = omp_get_wtime()
        do while(error > eps)
            x = res
            res = 0
            !$OMP parallel do reduction(+:res) collapse(2)
            do i = 1,n
                do j = 1,n
                    res(i) = res(i) + a(j,i) * x(j)
                enddo
                res(i) = res(i) + b(i)
            enddo
            !$OMP end parallel do
            error = maxval(abs(x-res))
        enddo
        RT2 = omp_get_wtime()
    end function jacobi_omp

    function jacobi_mpi(i_a, i_b, n, eps) result(res)
        use classic_lib
        use zeidel_lib, only : normalization_iteration
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(:), intent(in) :: i_b
        real, dimension(:,:), intent(in) :: i_a
        integer :: i
        integer, dimension(process_size) :: naas
        real :: error
        real, dimension(n) :: res, x
        real, dimension(:), allocatable :: temp, borig, b
        real, dimension(:,:), allocatable :: mbuf, a
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        naas(1) = 0
        do i = 2,process_size
            naas(i) = naas(i-1)+n*all_arr_len(i-1)
        enddo 
        if(is_master) then
            allocate(a(n,n), b(n))
            a = i_a
            b = i_b
            call normalization_iteration(A, b, n)
            call MPI_Bcast(b, n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
            res = b
        else
            allocate(A(0,0), b(0))
            call MPI_Bcast(res, n, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        endif
        allocate(mbuf(n, arr_length), temp(arr_length), borig(arr_length))
        error = 1
        RT1 = MPI_Wtime()
        call MPI_Scatterv(A, n*all_arr_len, naas, MPI_REAL, mbuf, n*arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        call MPI_Scatterv(B, all_arr_len, all_arr_start-1, MPI_REAL, borig, arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        do while(error > eps)
            x = res
            temp = 0
            RT1 = MPI_Wtime()
            do i = 1,arr_length
                temp(i) = temp(i) + scalar_mult(mbuf(:,i), res)
            enddo
            temp = borig + temp
            call MPI_Allgatherv(temp, arr_length, MPI_REAL, res, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
            error = maxval(abs(x - res))
        enddo
        RT2 = MPI_Wtime()
        deallocate(a, b, mbuf, temp, borig)
    end function jacobi_mpi

    function timing(A, b, n, eps, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: eps
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n) :: t, b
        real, dimension(n,n) :: A
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = jacobi(A, b, n, eps)
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
            t = jacobi_omp(A, b, n, eps)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp
    
    function timing_mpi(A, b, n, eps, repeats) result(res)
        integer, intent(in) :: n, repeats
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: b
        real, dimension(n,n), intent(in) :: A
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = jacobi_mpi(A, b, n, eps)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module jacobi_lib