module norm_lib
    implicit none
    contains
    !Square root from sum of square of all elements
    real pure function v_norm2(a, b, n)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: a, b
        integer :: i
        real :: ta, tb
        ta = 0
        tb = 0
        do i = 1,n
            ta = ta + a(i)**2
            tb = tb + b(i)**2
        enddo
        v_norm2 = sqrt(tb - ta)
    end function v_norm2

    real function v_norm2_omp(a, b, n)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: a, b
        integer :: i
        real :: ta, tb
        ta = 0
        tb = 0
!$OMP   parallel do
        do i = 1,n
            ta = ta + a(i)**2
            tb = tb + b(i)**2
        enddo
!$OMP   end parallel do
        v_norm2_omp = sqrt(tb - ta)
    end function v_norm2_omp

    real function v_norm2_mpi(a, b, n)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: a, b
        integer :: i
        real :: ta, tb
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        ta = 0
        tb = 0
        do i = arr_start,arr_end
            ta = ta + a(i)**2
            tb = tb + b(i)**2
        enddo
        call MPI_Allreduce(MPI_IN_PLACE, ta, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
        call MPI_Allreduce(MPI_IN_PLACE, tb, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
        v_norm2_mpi = sqrt(tb - ta)
    end function v_norm2_mpi

    real pure function m_norm4(A, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(:,:), intent(in) :: A
        integer :: i, j
        res = 0
        do i = 1,n
            do j = 1,n
                res = res + A(j,i)**2
            enddo
        enddo
        res = sqrt(res)
    end function m_norm4

    real function m_norm4_omp(A, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(:,:), intent(in) :: A
        integer :: i, j
        res = 0
        !$omp parallel do reduction(+:res)
        do i = 1,n
            do j = 1,n
                res = res + A(j,i)**2
            enddo
        enddo
        !$omp end parallel do
        res = sqrt(res)
    end function m_norm4_omp

    real function m_norm4_mpi(A, n) result(res)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, dimension(:,:), intent(in) :: A
        integer :: i, j
        real, dimension(:,:), allocatable :: Ta
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(Ta(n, arr_length))
        call MPI_Scatterv(A, n*all_arr_len, all_arr_start-1, MPI_REAL, Ta, n*arr_length, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        res = 0
        do i = 1,n
            do j = 1,n
                res = res + A(j,i)**2
            enddo
        enddo
        call MPI_Allreduce(MPI_IN_PLACE, res, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
        res = sqrt(res)
    end function m_norm4_mpi
end module norm_lib