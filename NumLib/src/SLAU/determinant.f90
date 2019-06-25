module determinant_lib
    implicit none
    integer(kind = 8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    function determinant(a, n) result(res)
        use gauss_lib, only : set_triangle_type, null_triangle_matrix_get_normalise_mult
        implicit none
        real, intent(in) :: a(:,:)
        integer, intent(in) :: n
        real :: res
        call SYSTEM_CLOCK(T1)
        call set_triangle_type('u', 'r', n)
        res = null_triangle_matrix_get_normalise_mult(a)
        call SYSTEM_CLOCK(T2)
    end function determinant

    function determinant_omp(a, n) result(res)
        use gauss_lib, only : set_triangle_type, null_triangle_matrix_get_normalise_mult_omp
        use omp_lib
        implicit none
        real, intent(in) :: a(:,:)
        integer, intent(in) :: n
        real :: res
        RT1 = omp_get_wtime()
        call set_triangle_type('u', 'r', n)
        res = null_triangle_matrix_get_normalise_mult_omp(a)
        RT2 = omp_get_wtime()
    end function determinant_omp

    function determinant_mpi(a, n) result(res)
        use gauss_lib, only : set_triangle_type, null_triangle_matrix_get_normalise_mult_mpi
        use mpi, only : MPI_Wtime
        implicit none
        real, intent(in) :: a(:,:)
        integer, intent(in) :: n
        real :: res
        RT1 = MPI_Wtime()
        call set_triangle_type('u', 'r', n)
        res = null_triangle_matrix_get_normalise_mult_mpi(a, n)
        RT2 = MPI_Wtime()
    end function determinant_mpi

    double precision function get_time() result(res)
        implicit none
        res = RT2 - RT1
    end function get_time

    function timing(a, n, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats
        integer(kind=8) :: rate 
        real, intent(in) :: a(:,:)
        real :: t(n,n)
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = determinant(a,n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, n, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats
        real, intent(in) :: a(:,:)
        real :: t(n,n)
        double precision :: res(repeats)
        do i = 1,repeats
            t = determinant_omp(a, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, n, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats
        real, intent(in) :: a(:,:)
        real :: t(n,n)
        double precision :: res(repeats)
        do i = 1,repeats
            t = determinant_mpi(a,n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module determinant_lib