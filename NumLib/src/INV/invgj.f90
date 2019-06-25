module invgj_lib
    implicit none
    integer(kind = 8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    function invgj(A, n) result(Inv)
        use gauss_lib, only : set_triangle_type, null_triangle_matrix_mirror
        implicit none
        integer, intent(in) :: n
        integer :: i
        real, dimension(n, n) :: A, Inv
        Inv = 0
        forall(i=1:n) Inv(i,i) = 1 
        call SYSTEM_CLOCK(T1)       
        call set_triangle_type('d', 'l', n)
        call null_triangle_matrix_mirror(A, Inv)
        call set_triangle_type('u', 'r', n)
        call null_triangle_matrix_mirror(A, Inv)
        call SYSTEM_CLOCK(T2)
    end function invgj

    function invgj_omp(A, n) result(Inv)
        use gauss_lib, only : set_triangle_type, null_triangle_matrix_mirror_omp
        use omp_lib
        implicit none
        integer, intent(in) :: n
        integer :: i
        real, dimension(n, n) :: A, Inv
        Inv = 0
        forall(i=1:n) Inv(i,i) = 1 
        RT1 = omp_get_wtime()
        call set_triangle_type('d', 'l', n)
        call null_triangle_matrix_mirror_omp(A, Inv)
        call set_triangle_type('u', 'r', n)
        call null_triangle_matrix_mirror_omp(A, Inv)
        RT2 = omp_get_wtime()
    end function invgj_omp

    function invgj_mpi(a, n) result(inv)
        use gauss_lib, only : set_triangle_type, null_triangle_matrix_mirror_mpi
        use mpi
        implicit none
        integer, intent(in) :: n
        integer :: i
        real, dimension(n, n) :: A, Inv
        Inv = 0
        forall(i=1:n) Inv(i,i) = 1 
        RT1 = MPI_Wtime()
        call set_triangle_type('d', 'l', n)
        call null_triangle_matrix_mirror_mpi(A, Inv, n)
        call set_triangle_type('u', 'r', n)
        call null_triangle_matrix_mirror_mpi(A, Inv, n)
        RT2 = MPI_Wtime()
    end function invgj_mpi

    function timing(a, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(n,n), intent(in) :: a
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i=1,repeats
            t = invgj(a,n)
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
            t = invgj_omp(a, n)
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
            t = invgj_mpi(a, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module invgj_lib