module eiler_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
contains
    !Смысла параллелизовывать нет.
    real function eiler_function_solve(a, b, h, f) result(y)
        implicit none
        real, intent(in) :: a, b, h
        real, external :: f
        integer :: i, n
        n = ceiling((b-a)/h)
        y = f(a)
        call SYSTEM_CLOCK(T1)
        do i = 2,n
            y = y + h * f(a+h*(i-1))
        enddo
        call SYSTEM_CLOCK(T2)
    end function eiler_function_solve

    function eiler_system_solve_function(a, b, h, n, vf, vs) result(y)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        real, dimension(n), intent(in) :: vs
        real, external :: vf
        integer :: i, s
        real, dimension(n) :: y
        s = ceiling((b-a)/h)
        y = vf(a, vs)
        call SYSTEM_CLOCK(T1)
        do i = 2,s
            y = y + h * vf(h*(i-1), y)
        enddo
        call SYSTEM_CLOCK(T2)
    end function eiler_system_solve_function

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

    function eiler_system_solve_test(a, b, h, n) result(ynow)
        use utilities, only : odu_matrix_create
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        integer :: i, j, k, steps
        real :: temp
        real, dimension(n) :: ynow, ythen
        real, dimension(n,n) :: koefficients_matrix
        steps = ceiling((b-a)/h)
        koefficients_matrix = odu_matrix_create(n)
        ynow = 1
        ythen = 1
        call SYSTEM_CLOCK(T1)
        do i = 2,steps+1
            do j = 1,n
                temp = integral_result((i-1)*h, j)
                do k=1,n
                    temp = temp + ythen(k) * koefficients_matrix(k,j)
                enddo
                ynow(j) = ythen(j) + h * temp
            enddo
            ythen = ynow
        enddo
        call SYSTEM_CLOCK(T2)
    end function eiler_system_solve_test

    function eiler_system_solve_omp_test(a, b, h, n) result(ynow)
        use utilities, only : odu_matrix_create
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        integer :: i, j, k, steps
        real :: temp
        real, dimension(n) :: ynow, ythen
        real, dimension(n,n) :: koefficients_matrix
        temp = 0
        steps = ceiling((b-a)/h)
        koefficients_matrix = odu_matrix_create(n)
        ynow = 1
        ythen = 1
        RT1 = omp_get_wtime()
        do i = 2,STEPS+1
!$OMP       parallel do reduction(+:temp)
            do j=1,n
                temp = integral_result((i-1)*h, j)
                do k=1,n
                    temp = temp + ythen(k) * koefficients_matrix(k,j)
                enddo
                ynow(j) = ythen(j) + h * temp
            enddo
!$OMP       end parallel do
            ythen = ynow
        enddo
        RT2 = omp_get_wtime()
    end function eiler_system_solve_omp_test 

    function eiler_system_solve_mpi_test(a, b, h, n) result(ythen)
        use utilities, only : odu_matrix_create
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: a, b, h
        integer :: i, j, k, c_pos, steps
        real :: temp
        real, dimension(n) :: ythen
        real, dimension(n,n) :: koefficients_matrix
        real, dimension(:), allocatable :: ynow
        steps = ceiling((b-a)/h)
        koefficients_matrix = odu_matrix_create(n)
        if(state_mpi == 0) call planner(n, .TRUE.)
        if(is_master) then
            allocate(ynow(n))
        else
            allocate(ynow(arr_length))
        endif
        ynow = 1
        ythen = 1    
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)    
        RT1 = MPI_WTIME()    
        do i = 2,steps+1
            do j = 1,arr_length
                c_pos = j + arr_start - 1
                temp = integral_result((i-1)*h, c_pos)
                do k = 1,n
                    temp = temp + ythen(k) * koefficients_matrix(k,c_pos)
                enddo
                ynow(j) = ythen(c_pos) + h * temp
            enddo
            if(is_serial) then
                ythen = ynow
            else
                call MPI_Allgatherv(ynow, arr_length, MPI_REAL, ythen, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
            endif
        enddo  
        RT2 = MPI_WTIME()
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        deallocate(ynow)
    end function eiler_system_solve_mpi_test

    function eiler_system_solve_matrix(h, n, matrix, eps) result(ynow)
        use norm_lib, only : v_norm2
        use classic_lib, only: scalar_mult
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h, eps
        real, dimension(n,n), intent(in) :: matrix
        integer :: i
        real :: error, errorold
        real, dimension(n) :: ynow, ythen
        ynow = 1
        ythen = 1
        error = 10
        errorold = 9
        call SYSTEM_CLOCK(T1)
        do while(abs(error-errorold)>eps)
            errorold = error
            do i=1,n
                ynow(i) = ythen(i) + h * scalar_mult(ythen, matrix(:,i))
            enddo
            error = v_norm2(ynow, ythen, n)
            ythen = ynow
        enddo
        call SYSTEM_CLOCK(T2)
    end function eiler_system_solve_matrix

    function eiler_system_solve_matrix_omp(h, n, matrix, eps) result(ynow)
        use norm_lib, only : v_norm2_omp
        use classic_lib, only: scalar_mult_omp
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h, eps
        real, dimension(n,n), intent(in) :: matrix
        integer :: i
        real :: error, errorold
        real, dimension(n) :: ynow, ythen
        ynow = 1
        ythen = 1
        error = 10
        errorold = 9
        RT1 = omp_get_wtime()
        do while(abs(error-errorold)>eps)
            errorold = error
!$OMP       parallel do
            do i=1,n
                ynow(i) = ythen(i) + h * scalar_mult_omp(ythen, matrix(:,i))
            enddo
!$OMP       end parallel do
            error = v_norm2_omp(ynow, ythen, n)
            ythen = ynow
        enddo
        RT2 = omp_get_wtime()
    end function eiler_system_solve_matrix_omp

    function eiler_system_solve_matrix_mpi(h, n, matrix, eps) result(ythen)
        use norm_lib, only : v_norm2
        use classic_lib
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h, eps
        real, dimension(n,n), intent(in) :: matrix
        integer :: i, c_pos
        real :: k1, k2, error, errorold
        real, dimension(:), allocatable :: ynow, ythen
        if(state_mpi == 0) call planner(n, .FALSE.)
        allocate(ythen(n))
        if(is_master) then
            allocate(ynow(n))
        else
            allocate(ynow(arr_length))
        endif
        ynow = 1
        ythen = 1
        error = 10
        errorold = 9
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)    
        RT1 = MPI_WTIME()    
        do while(abs(error-errorold)>eps)
            do i = 1,arr_length
                c_pos = i+arr_start-1
                ynow(i) = ythen(c_pos) + h * scalar_mult(ythen, matrix(:,c_pos))
            enddo
            if(is_serial) then
                error = v_norm2(ynow, ythen, n)
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
        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        deallocate(ynow, ythen)
    end function eiler_system_solve_matrix_mpi

    function eiler_timing(a, b, h, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, h
        integer :: i
        integer(kind=8) :: rate 
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = eiler_system_solve_test(a, b, h, n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function eiler_timing

    function eiler_timing_omp(a, b, h, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, h
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = eiler_system_solve_omp_test(a, b, h, n)
            res(i) = RT2 - RT1
        enddo
    end function eiler_timing_omp

    function eiler_timing_mpi(a, b, h, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, intent(in) :: a, b, h
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = eiler_system_solve_mpi_test(a, b, h, n)
            res(i) = RT2 - RT1
        enddo
    end function eiler_timing_mpi
end module eiler_lib