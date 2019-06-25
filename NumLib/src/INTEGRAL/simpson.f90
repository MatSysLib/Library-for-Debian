module simpson_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    real function simpson(a, b, n, eps, f) result(res)
        implicit none
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: n, i
        real :: resold, step
        res = 0
        resold = 1
        call SYSTEM_CLOCK(T1)
        !do while(abs(resold-res)>eps)
            resold = res
            step = (b - a) / real(n)
            res = 0
            do i = 1,n
                res = res + (f(a+(i-1)*step) + 4*f(a+i*step) + f(a+(i+1)*step)) * step / 6.
            enddo
            n = n * 2
        !enddo
        call SYSTEM_CLOCK(T2)
        n = n / 2
    end function simpson

    real function simpson_omp(a, b, n, eps, f) result(res)
        use omp_lib
        implicit none
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: n, i
        real :: resold, step
        res = 0
        resold = 1
        RT1 = omp_get_wtime()
        !do while(abs(resold-res)>eps)
            resold = res
            step = (b-a)/real(n)
            res = 0
            !$omp parallel do reduction(+:res)
            do i = 1,n
                res = res + (f(a+(i-1)*step) + 4*f(a+i*step)+f(a+(i+1)*step)) * step / 6.
            enddo
            !$omp end parallel do
            res = res 
            n = n * 2
        !enddo
        RT2 = omp_get_wtime()
        n = n / 2
    end function simpson_omp

    real function simpson_mpi(a, b, n, eps, f) result(res)
        use mpi_manager
        implicit none
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: n, i
        real :: resold, step
        call no_plan
        res = 0
        resold = 1
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        RT1 = MPI_Wtime()
        !do while(abs(resold-res)>eps)
            resold = res
            step = (b-a)/real(n)
            res = 0
            do i = process_rank*n/process_size+1,min(n,(process_rank+1)*n/process_size)
                res = res + (f(a+(i-1)*step) + 4*f(a+i*step)+f(a+(i+1)*step)) * step / 6.
            enddo
            call MPI_Allreduce(MPI_IN_PLACE,res,1,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
            n = n * 2
        !enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        n = n / 2
    end function simpson_mpi

    function timing(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        integer(kind=8) :: rate
        real :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = simpson(a, b, n, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = simpson_omp(a, b, n, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = simpson_mpi(a, b, n, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module simpson_lib