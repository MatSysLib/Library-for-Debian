module adams5_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    real, private, dimension(5) :: adams_koeffs = (/ 1901./720., 1387./360., 109./30., 637./360., 251./720. /)
    contains
    !Смысла параллелизовать нет.
    real function adams5_function_solve(a, b, h, f) result(y)
        implicit none
        real, intent(in) :: a, b, h
        real, external :: f
        integer :: i, n
        real :: k1, k2, k3, k4
        real, dimension(4) :: sold
        n = ceiling((b - a) / h)
        y = f(a)
        call SYSTEM_CLOCK(T1)
        do i = 2,5
            k1 = f(a + h*(i-1))
            sold(6-i) = k1
            k2 = f(a + h*(i-1) + h / 2. * k1)
            k3 = f(a + h*(i-1) + h / 2. * k2)
            k4 = f(a + h*(i-1) + h * k3)
            y = y + h / 6. * (k1 + 2*k2 + 2*k3 + k4)
        enddo
        do i = 6,n
            k1 = f(a + h*(i-1))
            y = y + h * (k1 * adams_koeffs(1) - sold(1) * adams_koeffs(2) + sold(2) * adams_koeffs(3) - &
                sold(3) * adams_koeffs(4) + sold(4) * adams_koeffs(5))
            sold(2:4) = sold(1:3)
            sold(1) = k1
        enddo
        call SYSTEM_CLOCK(T2)
    end function adams5_function_solve

    function adams5_function_solve_vector(a, b, h, n, vf, vs) result(y)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        real, dimension(n), intent(in) :: vs
        real, external :: vf
        integer :: i, s
        real, dimension(n) :: k1, k2, k3, k4, y
        real, dimension(n, 4) :: sold
        s = ceiling((b - a) / h)
        y = vf(a, vs)
        call SYSTEM_CLOCK(T1)
        do i = 2,5
            k1 = vf(a + h*(i-1)         , y)
            k2 = vf(a + h*(i-1) + h / 2., y + h / 2. * k1)
            k3 = vf(a + h*(i-1) + h / 2., y + h / 2. * k2)
            k4 = vf(a + h*(i-1) + h,      y + h      * k3)
            sold(:,6-i) = k1
            y = y + h / 6. * (k1 + 2 * k2 + 2 * k3 + k4)
        enddo
        do i = 6,s
            k1 = vf(a + h*(i-1), y)
            y = y + h * (k1 * adams_koeffs(1) - sold(:,1) * adams_koeffs(2) + sold(:,2) * adams_koeffs(3) - &
                sold(:,3) * adams_koeffs(4) + sold(:,4) * adams_koeffs(5))
            sold(:,2:4) = sold(:,1:3)
            sold(:,1) = k1
        enddo
        call SYSTEM_CLOCK(T2)
    end function adams5_function_solve_vector

    real function integral_function(x, number) result(res)
        implicit none
        integer, intent(in) :: number
        real, intent(in) :: x
        res = exp(-1 * (x - number)**2)
    end function integral_function 

    real function integral_result(x, number) result(integral)
        implicit none
        integer, intent(in) :: number
        real, intent(in) :: x
        integer :: i        
        real, dimension(7) :: gauss_x = (/ real(-1.), -1 * sqrt(5. / 11. + 2. / 11.*sqrt(5. / 3.)),&
            -1 * sqrt(5. / 11. - 2. / 11.*sqrt(5. / 3.)), real(0), sqrt(5. / 11. - 2. / 11.*sqrt(5. / 3.)),&
            sqrt(5. / 11. + 2. / 11.*sqrt(5. / 3.)), real(1) /), &
        gauss_weight = (/ 1./21., (124. - 7.*sqrt(15.)) / 350.,&
            (124. + 7.*sqrt(15.)) / 350., 256. / 525., (124. + 7.*sqrt(15.)) / 350., &
            (124. - 7.*sqrt(15.)) / 350.,1. / 21. /)
        integral = 0
        do i = 1,7
            integral = integral + gauss_weight(i) * integral_function(x / 2. * gauss_x(i) + x / 2., number)
        enddo
        integral = integral * x / 2.
    end function integral_result

    function adams5_system_solve_test(a,b,h,n) result(ynow)
        use utilities, only : odu_matrix_create
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        integer :: i, j, k, steps
        real :: k1, k2, k3, k4
        real, dimension(n) :: s, ynow, ythen
        real, dimension(n,n) :: koefficients_matrix(n,n) 
        real, dimension(4,n) :: sold
        steps = ceiling((b-a)/h)
        koefficients_matrix = odu_matrix_create(n)
        ynow = 1
        ythen = 1
        call SYSTEM_CLOCK(T1)
        do i = 2,5
            do j = 1,n
                k1 = integral_result((i-1)*h, j)
                do k = 1,n
                    k1 = k1 + ythen(k) * koefficients_matrix(k,j)
                end do
                sold(6-i,j) = k1
                if((j==1).OR.(j==n)) then
                    k2 = -3
                    k3 = -3
                    k4 = -3
                else
                    k2 = -2
                    k3 = -2
                    k4 = -2
                endif
                k2 = k1 + h / 2. * k1 * k2
                k3 = k1 + h / 2. * k2 * k3
                k4 = k1 + h      * k3 * k4
                ynow(j) = ythen(j) + h/6.*(k1+2*k2+2*k3+k4)
            enddo
            ythen = ynow
        enddo
        do i = 6,steps+1
            do j = 1,n
                s(j) = integral_result((i-1)*h, j)
                do k = 1,n
                    s(j) = s(j) + ythen(k) * koefficients_matrix(k,j)
                end do
                ynow(j) = ythen(j) + h*(s(j) * adams_koeffs(1) - sold(1,j) * adams_koeffs(2) +&
                    sold(2,j) * adams_koeffs(3) - sold(3,j) * adams_koeffs(4) + sold(4,j) * adams_koeffs(5))
            end do
            sold(2:4,:) = sold(1:3,:) 
            sold(1,:) = s
            ythen = ynow
        end do
        call SYSTEM_CLOCK(T2)
    end function adams5_system_solve_test

    function adams5_system_solve_omp_test(a,b,h,n) result(ynow)
        use utilities, only : odu_matrix_create
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        integer :: i, j, k, steps
        real :: k1, k2, k3, k4
        real, dimension(n) :: s, ynow, ythen
        real, dimension(4,n) :: sold
        real, dimension(n,n) :: koefficients_matrix
        steps = ceiling((b-a)/h)
        koefficients_matrix = odu_matrix_create(n)
        ynow = 1
        ythen = 1
        RT1 = omp_get_wtime()
        do i = 2,5
!$OMP       PARALLEL DO private(j, k1, k2, k3, k4)
            do j = 1,n
                k1 = integral_result((i-1)*h, j)
                do k = 1,n
                    k1 = k1 + ythen(k) * koefficients_matrix(k,j)
                end do
                sold(6-i,j) = k1
                if((j==1).OR.(j==n)) then
                    k2 = -3
                    k3 = -3
                    k4 = -3
                else
                    k2 = -2
                    k3 = -2
                    k4 = -2
                endif
                k2 = k1 + h / 2. * k1 * k2
                k3 = k1 + h / 2. * k2 * k3
                k4 = k1 + h      * k3 * k4
                ynow(j) = ythen(j) + h/6.*(k1+2*k2+2*k3+k4)
            enddo
!$OMP       END PARALLEL DO
            ythen = ynow
        enddo
        do i = 6,steps+1
!$omp       parallel do private(j, k1)
            do j = 1,n
                k1 = integral_result((i-1)*h, j)
                do k = 1,n
                    k1 = k1 + ythen(k)*koefficients_matrix(k,j)
                end do
                ynow(j) = ythen(j) + h*(k1 * adams_koeffs(1) - sold(1,j) * adams_koeffs(2) +&
                    sold(2,j) * adams_koeffs(3) - sold(3,j) * adams_koeffs(4) + sold(4,j) * adams_koeffs(5))
                s(j) = k1
            end do
!$omp       end parallel do
            sold(2:4,:) = sold(:3,:)
            sold(1,:) = s
            ythen = ynow
        end do
        RT2 = omp_get_wtime()
    end function adams5_system_solve_omp_test 

    function adams5_system_solve_mpi_test(a,b,h,n) result(ythen)
        use utilities, only : odu_matrix_create
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        integer :: i, j, k, c_pos, steps
        real :: k1, k2, k3, k4, ss
        real, dimension(4,n) :: sold
        real, dimension(n,n) :: koefficients_matrix
        real, dimension(:), allocatable :: ythen, ynow, s, sall
        steps = ceiling((b-a)/h)
        koefficients_matrix = odu_matrix_create(n)
        if(state_mpi == 0) call planner(n, .TRUE.)
        allocate(ythen(n))
        if(is_master) then
            allocate(ynow(n))
            allocate(s(n))
            allocate(sall(n))
            sall = 0
        else
            allocate(ynow(arr_length))
            allocate(s(arr_length))
            allocate(sall(0))
        endif
        ynow = 1
        ythen = 1    
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)    
        RT1 = MPI_WTIME()    
        do i = 2,5
            do j = 1,arr_length
                c_pos = j + arr_start - 1
                k1 = integral_result((i-1)*h, c_pos)
                do k = 1,n
                    k1 = k1 + ythen(k)*koefficients_matrix(k,c_pos)
                enddo
                if((c_pos==1).OR.(c_pos==n)) then
                    k2 = -3
                    k3 = -3
                    k4 = -3
                else
                    k2 = -2
                    k3 = -2
                    k4 = -2
                endif
                s(j) = k1
                k2 = k1 + h / 2. * k1 * k2
                k3 = k1 + h / 2. * k2 * k3
                k4 = k1 + h      * k3 * k4
                ynow(j) = ythen(c_pos) + h/6.*(k1+2*k2+2*k3+k4)
            enddo
            if(is_serial) then
                ythen = ynow
                sold(6-i,:) = s
            else
                call MPI_Allgatherv(ynow, arr_length, MPI_REAL, ythen, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
                call MPI_Gatherv(s, arr_length, MPI_REAL, sall, all_arr_len, all_arr_start-1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
                if(is_master) sold(6-i,:) = sall
            endif
        enddo
        call MPI_BCAST(sold, n*4, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        do i = 6,STEPS+1
            if(is_worker) then
                do j = 1,arr_length
                    c_pos = j + arr_start - 1
                    ss = integral_result((i-1)*h, c_pos)
                    do k = 1,n
                        ss = ss + ythen(k)*koefficients_matrix(k,c_pos)
                    end do
                    ynow(j) = ythen(c_pos) + h*(ss * adams_koeffs(1) - sold(1,c_pos) * adams_koeffs(2) +&
                        sold(2,c_pos) * adams_koeffs(3) - sold(3,c_pos) * adams_koeffs(4) + sold(4,c_pos) * adams_koeffs(5))
                    sold(2:4,c_pos) = sold(1:3,c_pos)
                    sold(1,c_pos) = ss
                end do
            endif
            if(is_serial) then
                ythen = ynow
            else
                call MPI_Allgatherv(ynow, arr_length, MPI_REAL, ythen, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
            endif
        enddo
        RT2 = MPI_WTIME() 
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        deallocate(s, ynow, sall)
    end function adams5_system_solve_mpi_test

    function adams5_system_solve_matrix(h, n, matrix, eps) result(ynow)
        use norm_lib, only : v_norm2
        use classic_lib, only : scalar_mult
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h, eps
        real, dimension(n,n), intent(in) :: matrix
        integer :: i, j
        real :: error, errorold,k1, k2, k3, k4
        real, dimension(n) :: s, ynow, ythen
        real, dimension(4,n) :: sold
        ynow = 1
        ythen = 1
        error = 10
        errorold = 9
        call SYSTEM_CLOCK(T1)
        do i = 2,5
            do j = 1,n
                k1 = scalar_mult(ythen, matrix(:,i))
                k2 = h / 2. * k1 * k1
                k3 = h / 2. * k2 * k1
                k4 = h      * k3 * k1
                sold(6-i,j) = k1
                ynow(i) = ythen(j) + h/6.*(k1 + 2 * k2 + 2 * k3 + k4)
            enddo
            ythen = ynow
        enddo
        do while(abs(error-errorold)>eps)
            errorold = error
            do i = 1,n
                s(i) = scalar_mult(ythen, matrix(:,i))
                ynow(i) = ythen(i) + h*(s(i) * adams_koeffs(1) - sold(1,i) * adams_koeffs(2) +&
                    sold(2,i) * adams_koeffs(3) - sold(3,i) * adams_koeffs(4) + sold(4,i) * adams_koeffs(5))
            end do
            sold(2:4,:) = sold(:3,:)
            sold(1,:) = s
            error = v_norm2(ynow, ythen, n)
            ythen = ynow
        end do
        call SYSTEM_CLOCK(T2)
    end function adams5_system_solve_matrix

    function adams5_system_solve_matrix_omp(h, n, matrix, eps) result(ynow)
        use norm_lib, only : v_norm2_omp
        use classic_lib, only : scalar_mult_omp
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h, eps
        real, dimension(n,n), intent(in) :: matrix
        integer :: i, j
        real :: error, errorold, k1, k2, k3, k4
        real, dimension(n) :: s, ynow, ythen
        real, dimension(4,n) :: sold
        k1 = 0
        ynow = 1
        ythen = 1
        error = 10
        errorold = 9
        RT1 = omp_get_wtime()
        do i = 2,5
            do j = 1,n
                k1 = scalar_mult_omp(ythen, matrix(:,i))
                k2 = h / 2. * k1 * k1
                k3 = h / 2. * k2 * k1
                k4 = h      * k3 * k1
                sold(6-i,j) = k1
                ynow(i) = ythen(j) + h/6.*(k1 + 2 * k2 + 2 * k3 + k4)
            enddo
            ythen = ynow
        enddo
        do while(abs(error-errorold)>eps)
            errorold = error
            do i = 1,n
                s(i) = scalar_mult_omp(ythen, matrix(:,i))
                ynow(i) = ythen(i) + h*(s(i) * adams_koeffs(1) - sold(1,i) * adams_koeffs(2) +&
                    sold(2,i) * adams_koeffs(3) - sold(3,i) * adams_koeffs(4) + sold(4,i) * adams_koeffs(5))
            end do
!$OMP       workshare
            sold(2:4,:) = sold(:3,:)
            sold(1,:) = s
!$OMP       end workshare
            error = v_norm2_omp(ynow, ythen, n)
            ythen = ynow
        end do
        RT2 = omp_get_wtime()
    end function adams5_system_solve_matrix_omp

    function adams5_system_solve_matrix_mpi(h, n, matrix, eps) result(ythen)
        use classic_lib
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h, eps
        integer :: i, j, c_pos
        real, dimension(n,n), intent(in) :: matrix
        real :: k1, k2, k3, k4, ss, error, errorold
        real, dimension(:), allocatable :: ythen, ynow, s, sall
        real, dimension(4,n) :: sold
        if(state_mpi == 0) call planner(n, .FALSE.)
        allocate(ythen(n))
        if(is_master.EQV..TRUE.) then
            allocate(ynow(n))
            allocate(s(n))
            allocate(sall(n))
            sall = 0
        else
            allocate(ynow(arr_length))
            allocate(s(arr_length))
        endif
        ynow = 1
        ythen = 1   
        error = 10
        errorold = 9 
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)    
        RT1 = MPI_WTIME()    
        do i = 2,5
            do j = 1,arr_length
                c_pos = j + arr_start - 1
                k1 = scalar_mult(ythen, matrix(:,c_pos))
                k2 = h/2. * k1 * k1
                k3 = h/2. * k2 * k1
                k4 =    h * k3 * k1
                s(j) = k1
                ynow(j) = ythen(c_pos) + h/6.*(k1+2*k2+2*k3+k4)
            enddo
            if(is_serial) then
                ythen = ynow
                sold(6-i,:) = s
            else
                call MPI_Allgatherv(ynow, arr_length, MPI_REAL, ythen, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
                call MPI_Gatherv(s, arr_length, MPI_REAL, sall, all_arr_len, all_arr_start-1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
                if(is_master) sold(6-i,:) = sall
            endif
        enddo
        call MPI_BCAST(sold, n*4, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        do while(abs(error-errorold)>eps)
            do j = 1,arr_length
                c_pos = j + arr_start - 1
                ss = scalar_mult(ythen, matrix(:,c_pos))
                ynow(j) = ythen(c_pos) + h*(ss * adams_koeffs(1) - sold(1,c_pos) * adams_koeffs(2) +&
                    sold(2,c_pos) * adams_koeffs(3) - sold(3,c_pos) * adams_koeffs(4) + sold(4,c_pos) * adams_koeffs(5))
                sold(2:4,c_pos) = sold(1:3,c_pos)
                sold(1,c_pos) = ss
            end do
            if(is_serial) then
                ythen = ynow
            else
                k1 = 0
                k2 = 0
                do i = 1,arr_length
                    k1 = ynow(i) ** 2
                    k2 = ythen(i+arr_start-1) ** 2
                enddo
                k1 = abs(k2 - k1)
                call MPI_Allgatherv(ynow, arr_length, MPI_REAL, ythen, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
                call MPI_Allreduce(k1, error, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
                error = sqrt(error)
            endif
        enddo
        RT2 = MPI_WTIME() 
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        deallocate(s, ynow)
        if(is_master.EQV..TRUE.) deallocate(sall)
    end function adams5_system_solve_matrix_mpi

    function adams5_timing(a, b, h, n, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats
        integer(kind=8) :: rate 
        real :: t(n)
        real, intent(in) :: a, b, h
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = adams5_system_solve_test(a, b, h, n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function adams5_timing

    function adams5_timing_omp(a, b, h, n, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats
        real :: t(n)
        real, intent(in) :: a, b, h
        double precision :: res(repeats)
        do i = 1,repeats
            t = adams5_system_solve_omp_test(a, b, h, n)
            res(i) = RT2 - RT1
        enddo
    end function adams5_timing_omp

    function adams5_timing_mpi(a, b, h, n, repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: n, repeats
        real :: t(n)
        real, intent(in) :: a, b, h
        double precision :: res(repeats)
        do i = 1,repeats
            t = adams5_system_solve_mpi_test(a, b, h, n)
            res(i) = RT2 - RT1
        enddo
    end function adams5_timing_mpi
end module adams5_lib