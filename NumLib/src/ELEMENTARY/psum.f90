module psum_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    real function psum(a) result(res)
        implicit none
        real, dimension(:), intent(in) :: a
        integer :: n, i
        n = size(a)
        res = 0
        call SYSTEM_CLOCK(T1)
        do i = 1,n
            res = res + a(i)
        enddo
        call SYSTEM_CLOCK(T2)
    end function psum

    real function psum_omp(a) result(res)
        use omp_lib
        implicit none
        real, dimension(:), intent(in) :: a
        integer :: n, i
        n = size(a)
        res = 0
        RT1 = omp_get_wtime()
        !$omp parallel do reduction(+:res)
        do i = 1,n
            res = res + a(i)
        enddo
        !$omp end parallel do
        RT2 = omp_get_wtime()
    end function psum_omp

    real function psum_mpi(a) result(res)
        use mpi_manager
        implicit none
        real, dimension(:), intent(in) :: a
        integer :: i, n
        real :: lres
        n = size(a)
        res = 0
        lres = 0
        if(state_mpi==0) call planner(n, .FALSE.)
        RT1 = MPI_Wtime()
        do i = arr_start,arr_end
            lres = lres + a(i)
        enddo
        call MPI_Reduce(lres, res, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
        RT2 = MPI_Wtime()
        call MPI_Bcast(res, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
    end function psum_mpi

    double precision function get_time() result(res)
        implicit none
        res = RT2 - RT1
    end function get_time
end module psum_lib