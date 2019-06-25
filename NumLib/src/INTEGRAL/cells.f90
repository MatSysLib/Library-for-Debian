module cells_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains  
    real function cells(nx, ny, ax, bx, ay, by, eps, f) result(res)
        implicit none
        real, intent(in) :: ax, ay, bx, by, eps
        real, external :: f
        integer :: nx, ny, i, j
        real :: resold, stepx, stepy
        res = 0
        resold = 1
        call SYSTEM_CLOCK(T1)
        !do while((abs(res - resold) > eps).AND.(nx+ny<10000000))
            resold = res
            stepx = (bx - ax) / real(nx)
            stepy = (by - ay) / real(ny)
            res = 0
            do i = 1,nx
                do j = 1,ny
                    res = res + f(((ax+(i-1)*stepx)+(ax+i*stepx))/2., ((ay+(j-1)*stepy)+(ay+j*stepy))/2.) * stepx * stepy
                enddo
            enddo
            nx = nx * 2
            ny = ny * 2
        !enddo
        call SYSTEM_CLOCK(T2)
        nx = nx / 2
        ny = ny / 2
    end function cells

    real function cells_omp(nx, ny, ax, bx, ay, by, eps, f) result(res)
        use omp_lib
        implicit none
        real, intent(in) :: ax, ay, bx, by, eps
        real, external :: f
        integer :: nx, ny, i, j
        real :: resold, stepx, stepy
        res = 0
        resold = 1
        RT1 = omp_get_wtime()
        !do while((abs(res - resold) > eps).AND.(nx+ny<10000000))
            resold = res
            stepx = (bx - ax) / real(nx)
            stepy = (by - ay) / real(ny)
            res = 0
            !$omp parallel do private(i,j,nx,ny) reduction(+:res) collapse(2)
            do i = 1, nx
                do j = 1, ny
                    res = res + f(((ax+(i-1)*stepx)+(ax+i*stepx))/2., ((ay+(j-1)*stepy)+(ay+j*stepy))/2.) * stepx * stepy
                enddo
            enddo
            !$omp end parallel do
            nx = nx * 2
            ny = ny * 2
        !enddo
        RT2 = omp_get_wtime()
        nx = nx / 2
        ny = ny / 2
    end function cells_omp

    real function cells_mpi(nx, ny, ax, bx, ay, by, eps, f) result(res)
        use mpi_manager
        implicit none
        real, intent(in) :: ax, ay, bx, by, eps
        real, external :: f
        integer :: nx, ny, i, j
        real :: resold, stepx, stepy
        call no_plan
        if(nx<process_size) nx = process_size
        if(ny<process_size) ny = process_size
        res = 0
        resold = 1
        RT1 = MPI_Wtime()
        !do while((abs(res - resold) > eps).AND.(nx+ny<10000000))
            resold = res
            stepx = (bx - ax) / real(nx)
            stepy = (by - ay) / real(ny)
            res = 0
            do i = process_rank*nx/process_size+1,min(nx,(process_rank+1)*nx/process_size)
                do j = 1,ny
                    res = res + f(((ax+(i-1)*stepx)+(ax+i*stepx))/2., ((ay+(j-1)*stepy)+(ay+j*stepy))/2.) * stepx * stepy
                enddo
            enddo
            call MPI_Allreduce(MPI_IN_PLACE, res, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
            nx = nx * 2
            ny = ny * 2
        !enddo
        RT2 = MPI_Wtime()
        nx = nx / 2
        ny = ny / 2
    end function cells_mpi 

    function timing(nx, ny, ax, bx, ay, by, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: nx, ny, repeats
        real, intent(in) :: ax, ay, bx, by, eps
        real, external :: f
        integer :: i
        integer(kind=8) :: rate
        real :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = cells(nx, ny, ax, bx, ay, by, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(nx, ny, ax, bx, ay, by, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: nx, ny, repeats
        real, intent(in) :: ax, ay, bx, by, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = cells_omp(nx, ny, ax, bx, ay, by, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp
    
    function timing_mpi(nx, ny, ax, bx, ay, by, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: nx, ny, repeats 
        real, intent(in) :: ax, ay, bx, by, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = cells_mpi(nx, ny, ax, bx, ay, by, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module cells_lib