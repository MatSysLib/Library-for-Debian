module montekarlo_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    interface montekarlo
        module procedure montekarlo_one, montekarlo_two
    end interface montekarlo
    interface montekarlo_omp
        module procedure montekarlo_one_omp, montekarlo_two_omp
    end interface montekarlo_omp
    interface montekarlo_mpi
        module procedure montekarlo_one_mpi, montekarlo_two_mpi
    end interface montekarlo_mpi
    contains
    real function montekarlo_one(counter, a, b, eps, f) result(res)
        use utilities, only : random
        implicit none
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: counter, i
        real :: resold
        res = 0
        resold = 1
        !if(counter>=10000000) counter = 2500000
        call SYSTEM_CLOCK(T1)
        !do while(abs(resold - res) > eps .AND. counter < 10000000)
            resold = res
            res = 0
            do i = 1,counter
                res = res + f(random(a, b)) / real(counter)
            enddo
            res = res * (b - a)
            counter = counter * 2
        !enddo
        call SYSTEM_CLOCK(T2)
        counter = counter / 2
    end function montekarlo_one

    real function montekarlo_two(counter, ax, bx, ay, by, eps, f) result(res)
        use utilities, only : random
        implicit none
        real, intent(in) :: ax, bx, ay, by, eps
        real, external :: f
        integer :: counter, i
        real :: resold
        resold = 1
        res = 0
        !if(counter>=10000000) counter = 2500000
        call SYSTEM_CLOCK(T1)
        !do while(abs(resold - res) > eps .AND. counter < 10000000)
            resold = res
            res = 0
            do i = 1,counter
                res = res + (f(random(ax, bx),random(ay, by)) - res) / real(i)
            enddo
            res = res * (bx - ax) * (by - ay)
            counter = counter * 2
        !enddo
        call SYSTEM_CLOCK(T2)
        counter = counter / 2
    end function montekarlo_two

    function timing_one(counter, a, b, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: counter, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        integer(kind=8) :: rate
        real :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = montekarlo_one(counter, a, b, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing_one

    function timing_two(counter, ax, bx, ay, by, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: counter, repeats
        real, intent(in) :: ax, bx, ay, by, eps
        real, external :: f
        integer :: i
        integer(kind=8) :: rate
        real :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = montekarlo_two(counter, ax, bx, ay, by, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing_two

    real function montekarlo_one_omp(counter, a, b, eps, f) result(res)
        use utilities, only : parallel_random
        use omp_lib
        implicit none
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: counter, i, tid, tnum
        real :: resold, t
!$OMP   parallel
        tnum = omp_get_num_threads()
!$OMP   end parallel
        res = 0
        resold = 1
        !if(counter>=10000000) counter = 2500000
        RT1 = omp_get_wtime()
        !do while(abs(resold - res) > eps .AND. counter < 10000000)
            resold = res
            res = 0
!$OMP       parallel
            tid = omp_get_thread_num() 
!$OMP           do
            do i = 1,counter/tnum
                t = t + f(parallel_random(a, b)) / real(counter)
            enddo
!$OMP           end do
!$OMP           critical
            res = res + t
!$OMP           end critical
!$OMP       end parallel
            res = res * (b - a) / tnum
            counter = counter * 2
        !enddo
        RT2 = omp_get_wtime()
        counter = counter / 2
    end function montekarlo_one_omp

    real function montekarlo_two_omp(counter, ax, bx, ay, by, eps, f) result(res)
        use utilities, only : parallel_random
        use omp_lib
        implicit none
        real, intent(in) :: ax, bx, ay, by, eps
        real, external :: f
        integer :: counter, i, tid, tnum
        real :: x, y, resold, t
!$OMP   parallel
        tnum = omp_get_num_threads()
!$OMP   end parallel
        resold = 1
        res = 0
        !if(counter>=10000000) counter = 2500000
        RT1 = omp_get_wtime()
        !do while(abs(resold - res) > eps .AND. counter < 10000000)
            resold = res
            res = 0
            t = 0
!$OMP       parallel
            tid = omp_get_thread_num() 
!$OMP           do
            do i = 1,counter/tnum
                x = parallel_random(ax, bx)
                y = parallel_random(ay, by)
                t = t + (f(x, y) - t) / real(i)
            enddo
!$OMP           end do
!$OMP           critical
            res = res + t
!$OMP           end critical
!$OMP       end parallel
            res = res * (bx - ax) * (by - ay) / tnum
            counter = counter * 2
        !enddo
        RT2 = omp_get_wtime()
        counter = counter / 2
    end function montekarlo_two_omp

    function timing_one_omp(counter, a, b, eps, f, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: counter, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = montekarlo_one_omp(counter, a, b, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_one_omp

    function timing_two_omp(counter, ax, bx, ay, by, eps, f, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: counter, repeats
        real, intent(in) :: ax, bx, ay, by, eps
        real, external :: f
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = montekarlo_two_omp(counter, ax, bx, ay, by, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_two_omp

    real function montekarlo_one_mpi(counter, a, b, eps, f) result(res)
        use utilities, only : parallel_random
        use mpi_manager
        implicit none
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: counter, i
        real :: resold
        call no_plan
        resold = 1
        res = 0
        if(counter < process_size) counter = process_size
        !if(counter >= 10000000 * process_size) counter = 2500000 * process_size
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime()
        !do while(abs(resold - res) > eps .AND. counter < 10000000 * process_size)
            resold = res
            res = 0
            do i = 1,counter/process_size
                res = res + f(parallel_random(a, b)) / real(counter)
            enddo
            res = res * (b - a)
            call MPI_Allreduce(MPI_IN_PLACE, res, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
            counter = counter * 2
        !enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        counter = counter / 2
    end function montekarlo_one_mpi

    real function montekarlo_two_mpi(counter, ax, bx, ay, by, eps, f) result(res)
        use utilities, only : parallel_random
        use mpi_manager
        implicit none
        real, intent(in) :: ax, bx, ay, by, eps
        real, external :: f
        integer :: counter, i, hit
        real :: resold
        call no_plan
        hit = 0
        resold = 1
        res = 0
        if(counter < process_size) counter = process_size
        !if(counter >= 10000000 * process_size) counter = 2500000 * process_size
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime()
        !do while(abs(resold - res) > eps .AND. counter < 10000000 * process_size)
            resold = res
            res = 0
            do i = 1,counter/process_size
                res = res + (f(parallel_random(ax, bx), parallel_random(ay, by)) - res) / real(i)
            enddo
            res = res * (bx - ax) * (by - ay) / process_size
            call MPI_Allreduce(MPI_IN_PLACE, res, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
            counter = counter * 2
        !enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        counter = counter / 2
    end function montekarlo_two_mpi
    
    function timing_one_mpi(counter, a, b, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: counter, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = montekarlo_one_mpi(counter, a, b, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_one_mpi

    function timing_two_mpi(counter, ax, bx, ay, by, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: counter, repeats
        real, intent(in) :: ax, bx, ay, by, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = montekarlo_two_mpi(counter, ax, bx, ay, by, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_two_mpi

    real function get_time() result(res)
        implicit none
        integer :: rate
        CALL system_clock(count_rate=rate)
        res = real(T2 - T1) / real(rate)
    end function get_time

    double precision function get_par_time() result(res)
        implicit none
        res = RT2 - RT1
    end function get_par_time
end module montekarlo_lib