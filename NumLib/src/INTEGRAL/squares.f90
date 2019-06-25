module squares_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    real function left_square(a, b, n, eps, f) result(res)
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
            step = (b-a)/real(n)
            res = 0
            do i=0,n-1
                res = res + f(a+i*step)*step
            enddo
            n = n*2
        !enddo
        call SYSTEM_CLOCK(T2)
        n = n/2
    end function left_square

    real function central_square(a, b, n, eps, f) result(res)
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
            step = (b-a)/real(n)
            res = 0
            do i=0,n-1           
                res = res + f(((a+i*step)+(a+(i+1)*step))/2.)*step
            enddo
            n = n*2
        !enddo
        call SYSTEM_CLOCK(T2)
        n = n/2
    end function central_square

    real function right_square(a, b, n, eps, f) result(res)
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
            step = (b-a)/real(n)
            res = 0
            do i=0,n-1
                res = res + f(a+i*step)*step
            enddo
            n = n*2
        !enddo
        call SYSTEM_CLOCK(T2)
        n = n/2
    end function right_square
    
    real function left_square_omp(a, b, n, eps, f) result(res)
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
            do i=0,n-1
                res = res + f(a+i*step)*step
            enddo
            !$omp end parallel do
            n = n*2
        !enddo
        RT2 = omp_get_wtime()
        n = n/2
    end function left_square_omp

    real function central_square_omp(a, b, n, eps, f) result(res)
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
            do i=0,n-1           
                res = res + f(((a+i*step)+(a+(i+1)*step))/2.)*step
            enddo
            !$omp end parallel do
            n = n*2
        !enddo
        RT2 = omp_get_wtime()
        n = n/2
    end function central_square_omp

    real function right_square_omp(a, b, n, eps, f) result(res)
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
            do i=0,n-1
                res = res + f(a+i*step)*step
            enddo
            !$omp end parallel do
            n = n*2
        !enddo
        RT2 = omp_get_wtime()
        call SYSTEM_CLOCK(T2)
        n = n/2
    end function right_square_omp

    real function left_square_mpi(a, b, n, eps, f) result(res)
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
        do while(abs(resold-res)>eps)
            resold = res
            step = (b-a)/real(n)
            res = 0
            do i = process_rank*n/process_size+1,min(n,(process_rank+1)*n/process_size)
                res = res + f(a+i*step)*step
            enddo
            call MPI_Allreduce(MPI_IN_PLACE,res,1,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
            n = n*2
        enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        n = n/2
    end function left_square_mpi

    real function central_square_mpi(a, b, n, eps, f) result(res)
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
                res = res + f(((a+i*step)+(a+(i+1)*step))/2.)*step
            enddo
            call MPI_Allreduce(MPI_IN_PLACE,res,1,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
            n = n*2
        !enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        n = n/2
    end function central_square_mpi

    real function right_square_mpi(a, b, n, eps, f) result(res)
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
                res = res + f(a+i*step)*step
            enddo
            call MPI_Allreduce(MPI_IN_PLACE,res,1,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
            n = n*2
        !enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        n = n/2
    end function right_square_mpi

    function timing_ls(a, b, n, eps, f, repeats) result(res)
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
            t = left_square(a,b,n,eps,f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing_ls

    function timing_cs(a, b, n, eps, f, repeats) result(res)
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
            t = central_square(a, b, n, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing_cs

    function timing_rs(a, b, n, eps, f, repeats) result(res)
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
            t = right_square(a, b, n, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing_rs

    function timing(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        double precision, dimension(repeats, 3) :: res
        res(:,1) = timing_ls(a, b, n, eps, f, repeats)
        res(:,2) = timing_cs(a, b, n, eps, f, repeats)
        res(:,3) = timing_rs(a, b, n, eps, f, repeats)
    end function timing

    function timing_ls_omp(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = left_square_omp(a,b,n,eps,f)
            res(i) = RT2 - RT1
        enddo
    end function timing_ls_omp

    function timing_cs_omp(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = central_square_omp(a,b,n,eps,f)
            res(i) = RT2 - RT1
        enddo
    end function timing_cs_omp

    function timing_rs_omp(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a,b,eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = right_square_omp(a,b,n,eps,f)
            res(i) = RT2 - RT1
        enddo
    end function timing_rs_omp

    function timing_omp(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        double precision, dimension(repeats, 3) :: res
        res(:,1) = timing_ls_omp(a, b, n, eps, f, repeats)
        res(:,2) = timing_cs_omp(a, b, n, eps, f, repeats)
        res(:,3) = timing_rs_omp(a, b, n, eps, f, repeats)
    end function timing_omp

    function timing_ls_mpi(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i=1,repeats
            t = left_square_mpi(a, b, n, eps, f)
            res(i) = RT2-RT1
        enddo
    end function timing_ls_mpi

    function timing_cs_mpi(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i=1,repeats
            t = central_square_mpi(a,b,n,eps,f)
            res(i) = RT2-RT1
        enddo
    end function timing_cs_mpi

    function timing_rs_mpi(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a,b,eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i=1,repeats
            t = right_square_mpi(a,b,n,eps,f)
            res(i) = RT2-RT1
        enddo
    end function timing_rs_mpi

    function timing_mpi(a, b, n, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        double precision, dimension(repeats, 3) :: res
        res(:,1) = timing_ls_mpi(a, b, n, eps, f, repeats)
        res(:,2) = timing_cs_mpi(a, b, n, eps, f, repeats)
        res(:,3) = timing_rs_mpi(a, b, n, eps, f, repeats)
    end function timing_mpi
end module squares_lib