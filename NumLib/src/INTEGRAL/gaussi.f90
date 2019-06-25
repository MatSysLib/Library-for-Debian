module gaussi_lib
    implicit none
    integer(kind=8), private :: T1, T2
    integer :: number = 0
    double precision, private :: RT1, RT2
    real, private, allocatable :: points(:), weights(:)
    contains
    real function gauss_integrate(a, b, n, p, eps, f) result(res)
        implicit none
        integer,intent(in) :: p
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: n, i, j
        real :: resold, step
        if(number /= p) call initialise(p)
        res = 0
        resold = 1
        call SYSTEM_CLOCK(T1)
        !do while(abs(res - resold) > eps)
            resold = res
            step = (b - a) / real(n)
            res = 0
            do i = 1,n
                do j = 1,number
                    res = res + weights(j) * f(step * points(j) / 2. + (2. * (a + i * step) - step) / 2.)
                enddo
            enddo
            res = res * step / 2.
            n = n*2
        !enddo
        call SYSTEM_CLOCK(T2)
        n = n / 2
    end function gauss_integrate

    real function gauss_integrate_omp(a, b, n, p, eps, f) result(res)
        use omp_lib
        implicit none
        integer,intent(in) :: p
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: n, i, j
        real :: resold, step
        if(number /= p) call initialise(p)
        res = 0
        resold = 1
        RT1 = omp_get_wtime()
        !do while(abs(res - resold) > eps)
            resold = res
            step = (b - a) / real(n)
            res = 0
            !$omp parallel do reduction(+:res)
            do i = 1,n
                do j = 1,number
                    res = res + weights(j) * f(step * points(j) / 2. + (2. * (a + i * step) - step) / 2.)
                enddo
            enddo
            !$omp end parallel do
            res = res * step / 2.
            n = n * 2
        !enddo
        RT2 = omp_get_wtime()
        n = n / 2
    end function gauss_integrate_omp

    real function gauss_integrate_mpi(a, b, n, p, eps, f) result(res)
        use mpi_manager
        implicit none
        integer,intent(in) :: p
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: n, i, j
        real :: resold, step
        call no_plan
        if(number < process_size) number = process_size
        if(number /= p) call initialise(p)
        res = 0
        resold = 1
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        RT1 = MPI_Wtime()
        !do while(abs(res - resold) > eps)
            resold = res
            step = (b - a) / real(n)
            res = 0
            do i = process_rank*n/process_size+1,min(n,(process_rank+1)*n/process_size)
                do j = 1,number
                    res = res + weights(j) * f(step * points(j) / 2. + (2. * (a + i * step) - step) / 2.)
                enddo
            enddo
            call MPI_Allreduce(MPI_IN_PLACE, res, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
            res = res * step / 2.
            n = n * 2
        !enddo
        RT2 = MPI_Wtime()
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        n = n / 2
    end function gauss_integrate_mpi

    function timing(a, b, n, p, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, p, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        integer(kind=8) :: rate
        real :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = gauss_integrate(a, b, n, p, eps, f)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_omp(a, b, n, p, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, p, repeats
        real, intent(in) :: a, b, eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i=1,repeats
            t = gauss_integrate_omp(a, b, n, p, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, b, n, p, eps, f, repeats) result(res)
        implicit none
        integer, intent(in) :: n, p, repeats
        real, intent(in) :: a,b,eps
        real, external :: f
        integer :: i
        real :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = gauss_integrate_mpi(a, b, n, p, eps, f)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi

    real function Pn(a,n,m,x) result(res)
        implicit none
        integer, intent(in) :: n, m
        real, intent(in) :: a(0:n), x
        integer :: i
        res = 0
        if(m==0) then
            do i = 0,n,2
                !if(x == 0) continue
                if(abs(x) < 0.00001) continue
                res = res + a(i) * x**i
            enddo
        else 
            do i = 1,n,2
                res = res + a(i) * x**i
            enddo
        endif
    end function Pn 

    real function Dn(a,n,m,x) result(res)
        implicit none
        integer, intent(in) :: n, m
        real, intent(in) :: a(0:n), x
        integer :: i
        res = 0
        if(m==0) then
            do i = 0,n,2
                !if(x == 0) continue
                if(abs(x) < 0.00001) continue
                res = res + i * a(i) * x**(i-1)
            enddo
        else 
            do i=1,n,2
                res = res + i * a(i) * x**(i-1)
            enddo
        endif
    end function Dn

    real function factorial(n) result(res)
        implicit none
        integer, intent(in) :: n
        integer :: i
        res = 1
        do i=2,n
            res = res * i
        enddo
    end function factorial

    subroutine initialise(n)
        implicit none
        integer, intent(in) :: n
        integer :: m, np, i
        real :: a(0:n), z(0:n), y(0:n-1), w(0:n-1), l, v, s
        if(allocated(points)) deallocate(points, weights)
        allocate(points(n), weights(n))
        m = MOD(n,2)
        if(m == 0) then
            np = n / 2
        else 
            np = (n - 1) / 2
        endif
        do i = 0,np
            a(n - 2 * i) = ((-1 * mod(i,2) * 2 + 1) * factorial(2 * n - 2 * i)) /&
                            ((2**n) * factorial(i) * factorial(n - i) * factorial(n - 2 * i))
        enddo
        do i = 0,n-1
            z(i) = cos(4. * ATAN(1.) * (i + 0.75) / (n + 0.5))
            l = z(i)
            v = l - 1
            do while(abs(l - v)>0.000001)
                s = l - pn(a, n, m, l) / dn(a, n, m, l)
                v = l
                l = s
            enddo
            y(i) = l
            w(i) = 2. / ((1 - l**2) * (dn(a, n, m ,l)**2))
        enddo
        points(1:n) = y(0:n-1)
        weights(1:n) = w(0:n-1)
        number = n
    end subroutine initialise
    !https://deepanshubhatti.blogspot.com/2013/10/to-evaluate-definite-integral-by-gauss.html
end module gaussi_lib