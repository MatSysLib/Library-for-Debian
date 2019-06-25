module shultz_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
        !
        !    Input, integer ( kind = 4 ) N, the order of the matrix.
        !    Input, real ( kind = 8 ) A(N,N), the matrix.
        !    Input/output, real ( kind = 8 ) Y(N), the estimate for the eigenvector.
        !    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations.
        !    1 <= IT_MAX.
        !    Input, real ( kind = 8 ) TOL, an error tolerance.
        !    Output, real ( kind = 8 ) LAMBDA, the estimate for the eigenvalue.
        !    Output, integer ( kind = 4 ) IT_NUM, the number of iterations taken.
    function shultz(A, n, alpha, eps) result(Inv)
        use classic_lib, only : classic
        use norm_lib, only : m_norm4
        use utilities, only : output_qmatrix
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: A(:,:), alpha, eps
        !integer, optional, intent(in) :: iprecision, imres
        !real, optional, intent(in) :: ispeed
        integer :: i, j, k, ktotal, resets, calc_precision, max_resets
        real :: error, error_old, ia, div_speed, temp
        real, dimension(n,n) :: Inv, Psi, E
        logical :: first = .true.
        calc_precision = 1
        max_resets = 10000
        div_speed = 2.
        !if(present(iprecision)) calc_precision = iprecision
        !if(present(imres)) max_resets = imres
        !if(present(ispeed)) div_speed = ispeed
        k = 0
        ktotal = 0
        E = 0
        resets = 0
        do i = 1,n
            E(i,i) = 1
        enddo
        temp = 0
        do i = 1,n
            do j=1,n
                temp = temp + a(j,i)**2
            enddo
        enddo
        print *, temp, 1./temp
        Inv = alpha * TRANSPOSE(A)
        ia = min(alpha, 1./temp)
        error_old = huge(0.)
        error = huge(0.)
        call SYSTEM_CLOCK(T1)
        do while(error>eps)
            k = k + 1
            ktotal = ktotal + 1
            Inv = 2*Inv - classic(classic(Inv, A, n), Inv, n)
            error = m_norm4(E - classic(A, Inv, n),n)
            print *, k, ktotal, error
            if(resets>=max_resets) then
                print *, error, "Resets limit exceeded, retry with increased method precision or allowed resets or division speed"
                print *, "Returning last inversion attempt"
                error = 0
            endif
            if(first) then
                error_old = error + 1
                first = .false.
            endif
            if(error>error_old) then
                print *, "Old:", ia, error, error_old
                k = 0
                ia = ia / div_speed
                Inv = ia * TRANSPOSE(A)
                error_old = huge(0.)
                error = huge(0.)
                print *, "New:", ia
                resets = resets + 1
            endif
            error_old = error
        enddo
        call SYSTEM_CLOCK(T2)
    end function shultz

    function shultz_omp(A, n, alpha, eps) result(Inv)
        use classic_lib, only : classic_omp
        use omp_lib
        implicit none
        real, intent(in) :: A(:,:), alpha, eps
        real :: Inv(n,n),E(n,n), error
        integer, intent(in) :: n
        integer :: i
        E=0
        do i = 1,n
            E(i,i) = 1
        enddo
        Inv = alpha * TRANSPOSE(A)
        error = 1000  
        RT1 = omp_get_wtime()
        do while(error > eps)
            Inv = 2*Inv - classic_omp(classic_omp(Inv, A, n), Inv, n)
            error = SUM(ABS(E - classic_omp(A, Inv, n)))
        enddo
        RT2 = omp_get_wtime()
    end function shultz_omp

    function shultz_mpi(A,n,alpha,eps) result(Inv)
        use classic_lib, only : classic_mpi
        use mpi
        implicit none
        integer, intent(in) :: n
        integer :: i
        real, intent(in) :: A(:,:), alpha, eps
        real :: Inv(n,n), E(n,n), error
        E=0
        do i = 1,n
            E(i,i) = 1
        enddo
        Inv = alpha * TRANSPOSE(A)
        error = 1000
        RT1 = MPI_Wtime()
        do while(error > eps)
            Inv = 2*Inv - classic_mpi(classic_mpi(Inv, A, n), Inv, n)
            error = SUM(ABS(E - classic_mpi(A, Inv, n)))
        enddo
        RT2 = MPI_Wtime()
    end function shultz_mpi

    subroutine power_method ( n, a, it_max, tol, lambda)
        !
        !    Input, integer ( kind = 4 ) N, the order of the matrix.
        !    Input, real ( kind = 8 ) A(N,N), the matrix.
        !    Input/output, real ( kind = 8 ) Y(N), the estimate for the eigenvector.
        !    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations.
        !    1 <= IT_MAX.
        !    Input, real ( kind = 8 ) TOL, an error tolerance.
        !    Output, real ( kind = 8 ) LAMBDA, the estimate for the eigenvalue.
        !    Output, integer ( kind = 4 ) IT_NUM, the number of iterations taken.
        implicit none
        integer :: n
        real a(n,n)
        real ay(n)
        real cos_y1y2
        integer :: it_max
        integer :: it_num
        real lambda
        real lambda_old
        real sin_y1y2
        real tol
        real val_dif
        real y(n)
        real y_old(n)
        y = 1
        it_num = 0
        y_old(1:n) = y(1:n)
        !  Compute AY = A*Y.
        ay(1:n) = matmul ( a(1:n,1:n), y(1:n) )
        !  Estimate LAMBDA = (AY,Y)/(Y,Y).
        lambda = dot_product ( y(1:n), ay(1:n) )
        !  Force AY to have unit norm.
        !  Replace Y by AY.
        y(1:n) = ay(1:n) / sqrt ( sum ( ay(1:n)**2 ) )
        !  The sign of Y is optional.  If LAMBDA is probably negative,
        !  switch sign of new Y to match old one.
        if ( lambda < 0.0D+00 ) then
            y(1:n) = - y(1:n)
        end if
        
        val_dif = 0.0D+00
        cos_y1y2 = dot_product ( y(1:n), y_old(1:n) )
        sin_y1y2 = sqrt ( ( 1.0D+00 - cos_y1y2 ) * ( 1.0D+00 + cos_y1y2 ) )
        !  Now repeat these steps in an iteration.
        do it_num = 1, it_max
            lambda_old = lambda
            y_old(1:n) = y(1:n)
            ay(1:n) = matmul ( a(1:n,1:n), y(1:n) )
            lambda = dot_product ( y(1:n), ay(1:n) )
            y(1:n) = ay(1:n) / sqrt ( sum ( ay(1:n)**2 ) )
            if ( lambda < 0.0D+00 ) then
                y(1:n) = - y(1:n)
            end if
            val_dif = abs ( lambda - lambda_old )
            cos_y1y2 = dot_product ( y(1:n), y_old(1:n) )
            sin_y1y2 = sqrt ( ( 1.0D+00 - cos_y1y2 ) * ( 1.0D+00 + cos_y1y2 ) )
            if ( val_dif <= tol ) then
                exit
            end if
        end do
        y(1:n) = ay(1:n) / lambda
        return
    end subroutine

    real function pa(a, n, eps) result(res)
        use classic_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        integer :: k
        real :: error, resold
        real, dimension(n,n), intent(in) :: a
        real, dimension(n) :: x, xold
        error = 10
        k = 1
        x = 0.1
        resold = sum(x) / real(n)
        x = matrixvector(A, x, n)
        res = sum(x) / real(n) / resold
        print *, res, resold
        do while(error>eps)
            k = k + 1
            resold = res
            x = matrixvector(A, x, n)
            res = sum(x) / real(n) / resold
            error = abs(res - resold)
            print *, res, resold, error
            if(k==6) then
                x = x / maxval(x)
                k = 1
            endif
        enddo
        res = abs(res)
    end function pa

    function timing(a, n, alpha, eps, repeats) result(res)
        implicit none
        integer :: i
        integer(kind=8) :: rate
        integer, intent(in) ::repeats, n
        real :: t(n,n)
        real, intent(in) :: a(:,:), alpha, eps
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            !print *,"Check:", A(1,1), A(2,1), alpha, eps
            t = shultz(A, n, alpha, eps)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
        print *,"RESCHECK:", t(1,1), t(2,1)
    end function timing

    function timing_omp(a,n,alpha,eps,repeats) result(res)
        implicit none
        real, intent(in) :: a(:,:), alpha, eps
        real :: t(n,n)
        double precision :: res(repeats)
        integer :: i
        integer, intent(in) ::repeats, n
        do i = 1,repeats
            t = shultz_omp(A, n, alpha, eps)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp
    
    function timing_mpi(a,n,alpha,eps,repeats) result(res)
        implicit none
        real, intent(in) :: a(n,n),alpha,eps
        real :: t(n,n)
        double precision :: res(repeats)
        integer :: i
        integer, intent(in) ::repeats, n
        do i = 1,repeats
            t = shultz_mpi(A, n, alpha, eps)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module shultz_lib