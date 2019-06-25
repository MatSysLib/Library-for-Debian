!Module for numerical differentiation.
!Based on finite difference method
!Be careful - symmetrical functions may return zero
module num_diff_lib
    implicit none
    abstract interface
        function efunc(x) result(result)
            !integer, intent(in) :: n
            real, dimension(:), intent(in) :: x
            real, dimension(size(x)) :: result
        end function efunc
    end interface
    !Interface for all functions from this module with multple variable derivative selection
    abstract interface
        function da_m_s(x, h, func, n, pos) result(result)
            integer, intent(in) :: n, pos
            real, intent(in) :: h
            real, dimension(n), intent(in) :: x
            real, dimension(n) :: result
            procedure(efunc) :: func
        end function da_m_s
        real function da_m_s_o(x, h, func, n, pos) result(result)
            integer, intent(in) :: n, pos
            real, intent(in) :: h
            real, dimension(n), intent(in) :: x
            procedure(efunc) :: func
        end function da_m_s_o
    end interface
    !Each is two methods:
    !Basic for function of one variable
    !Advanced for function of multiple variables with derivative variable selection (mult & select)
    interface d1x_2p_fd
        module procedure d1x_2p_fd_b, d1x_2p_fd_mult_select
    end interface
    interface d1x_2p_cd
        module procedure d1x_2p_cd_b, d1x_2p_cd_mult_select
    end interface
    interface d1x_5p
        module procedure d1x_5p_b, d1x_5p_mult_select
    end interface
    interface d2x_5p
        module procedure d2x_5p_b, d2x_5p_mult_select
    end interface
    interface d3x_5p
        module procedure d3x_5p_b, d3x_5p_mult_select
    end interface
    interface d4x_5p
        module procedure d4x_5p_b, d4x_5p_mult_select
    end interface
    contains
    real function d1x_2p_fd_b(x, h, f) result(res)
        implicit none
        real, intent(in) :: x, h
        real, external :: f
        res = (f(x + h) - f(x)) / h
    end function d1x_2p_fd_b

    function d1x_2p_fd_mult(x, h, f, n, d_var) result(res)
        implicit none
        integer, intent(in) :: n, d_var
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: hv, res
        procedure(efunc) :: f
        hv = 0
        hv(d_var) = h
        res = (f(x + hv) - f(x)) / h
    end function d1x_2p_fd_mult
    
    real function d1x_2p_fd_mult_select(x, h, f, n, pos) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: temp
        procedure(efunc) :: f
        temp = d1x_2p_fd_mult(x, h, f, n, pos)
        res = temp(pos)
    end function d1x_2p_fd_mult_select

    real function d1x_2p_cd_b(x, h, f) result(res)
        implicit none
        real, intent(in) :: x, h
        real, external :: f
        res = (f(x + h) - f(x - h)) / (2. * h)
    end function d1x_2p_cd_b

    function d1x_2p_cd_mult(x, h, f, n, d_var) result(res)
        implicit none
        integer, intent(in) :: n, d_var
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: hv, res
        procedure(efunc) :: f
        hv = 0
        hv(d_var) = h
        res = (f(x + hv) - f(x - hv)) / (2 * h)
    end function d1x_2p_cd_mult

    real function d1x_2p_cd_mult_select(x, h, f, n, pos) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: temp
        procedure(efunc) :: f
        temp = d1x_2p_cd_mult(x, h, f, n, pos)
        res = temp(pos)
    end function d1x_2p_cd_mult_select

    real function d1x_5p_b(x, h, f) result(res)
        implicit none
        real, intent(in) :: x, h
        real, external :: f
        res = (-f(x + 2 * h) + 8*f(x + h) - 8*f(x - h) + f(x - 2 * h)) / (12 * h)
    end function d1x_5p_b

    function d1x_5p_mult(x, h, f, n, d_var) result(res)
        implicit none
        integer, intent(in) :: n, d_var
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: hv, res
        procedure(efunc) :: f
        hv = 0
        hv(d_var) = h
        res = (-f(x + 2 * hv) + 8 * f(x + hv) - 8 * f(x - hv) + f(x - 2 * hv)) / (12 * h)
    end function d1x_5p_mult

    real function d1x_5p_mult_select(x, h, f, n, pos) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: temp
        procedure(efunc) :: f
        temp = d1x_5p_mult(x, h, f, n, pos)
        res = temp(pos)
    end function d1x_5p_mult_select
    
    real function d2x_5p_b(x, h, f) result(res)
        implicit none
        real, intent(in) :: x, h
        real, external :: f
        res = (-f(x + 2 * h) + 16 * f(x + h) - 30 * f(x) + 16 * f(x - h) - f(x - 2 * h)) / (12 * h**2)
    end function d2x_5p_b

    function d2x_5p_mult(x, h, f, n, d_var) result(res)
        implicit none
        integer, intent(in) :: n, d_var
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: hv, res
        procedure(efunc) :: f
        hv = 0
        hv(d_var) = h
        res = (-f(x + 2 * hv) + 16 * f(x + hv) - 30 * f(x) + 16 * f(x - hv) - f(x - 2 * hv)) / (12 * h**2)
    end function d2x_5p_mult

    real function d2x_5p_mult_select(x, h, f, n, pos) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: temp
        procedure(efunc) :: f
        temp = d2x_5p_mult(x, h, f, n, pos)
        res = temp(pos)
    end function d2x_5p_mult_select

    real function d3x_5p_b(x,h,f) result(res)
        implicit none
        real, intent(in) :: x, h
        real, external :: f
        res = (-f(x + 2 * h) - 2*f(x + h) + 2*f(x - h) - f(x - 2 * h)) / (2 * h**3)
    end function d3x_5p_b

    function d3x_5p_mult(x, h, f, n, d_var) result(res)
        implicit none
        integer, intent(in) :: n, d_var
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: hv, res
        procedure(efunc) :: f
        hv = 0
        hv(d_var) = h
        res = (-f(x + 2 * hv) - 2*f(x + hv) + 2*f(x - hv) - f(x - 2 * hv)) / (2 * h**3)
    end function d3x_5p_mult

    real function d3x_5p_mult_select(x, h, f, n, pos) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: temp
        procedure(efunc) :: f
        temp = d3x_5p_mult(x, h, f, n, pos)
        res = temp(pos)
    end function d3x_5p_mult_select

    real function d4x_5p_b(x,h,f) result(res)
        implicit none
        real, intent(in) :: x, h
        real, external :: f
        res = (f(x + 2 * h) - 4 * f(x + h) + 6 * f(x) - 4 * f(x - h) + f(x - 2 * h)) / (h**4)
    end function d4x_5p_b

    function d4x_5p_mult(x, h, f, n, d_var) result(res)
        implicit none
        integer, intent(in) :: n, d_var
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: hv, res
        procedure(efunc) :: f
        hv = 0
        hv(d_var) = h
        res = (f(x + 2 * hv) - 4 * f(x + hv) + 6 * f(x) - 4 * f(x - hv) + f(x - 2 * hv)) / (h**4)
    end function d4x_5p_mult

    real function d4x_5p_mult_select(x, h, f, n, pos) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        real, dimension(n) :: temp
        procedure(efunc) :: f
        temp = d4x_5p_mult(x, h, f, n, pos)
        res = temp(pos)
    end function d4x_5p_mult_select

    function gradient(x, h, f, n, pos, approx) result(res)
        implicit none
        integer, intent(in) :: n, pos
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        procedure(efunc) :: f
        procedure(da_m_s) :: approx
        real, dimension(n) :: res
        res = approx(x, h, f, n, pos)
    end function gradient

    function gradient_omp(x, h, exfunc, n, approx) result(res)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        procedure(efunc) :: exfunc
        procedure(da_m_s_o) :: approx
        integer :: i
        real, dimension(n) :: res
        !$omp parallel do
        do i = 1,n
            res(i) = approx(x, h, exfunc, n, i)
        enddo
        !$omp end parallel do
    end function gradient_omp

    function gradient_mpi(x, h, exfunc, n, approx) result(res)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: h
        real, dimension(n), intent(in) :: x
        procedure(efunc) :: exfunc
        procedure(da_m_s_o) :: approx
        integer :: i
        real, dimension(n) :: res
        real, dimension(:), allocatable :: temp
        call full_plan(n, .TRUE.)
        allocate(temp(arr_length))
        do i = 1,arr_length
            res(i) = approx(x, h, exfunc, n, arr_start + i - 1)
        enddo
        call MPI_Allgatherv(temp, arr_length, MPI_REAL, res, all_arr_len, all_arr_start-1, MPI_REAL, MPI_COMM_WORLD, ierr)
    end function gradient_mpi

    function jacobi_matrix(exfunc, vect, step, n, approx) result(res)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: step
        real, dimension(n), intent(in) :: vect
        integer :: i
        real, dimension(n, n) :: res
        procedure(efunc) :: exfunc
        procedure(da_m_s) :: approx
        do i = 1,n
            res(:, i) = gradient(vect, step, exfunc, n, i, approx)
        enddo
    end function jacobi_matrix

    function jacobi_matrix_omp(exfunc, vect, step, n, approx) result(res)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: step
        real, dimension(n), intent(in) :: vect
        integer :: i
        real, dimension(n, n) :: res
        procedure(efunc) :: exfunc
        procedure(da_m_s) :: approx
        !$omp parallel do
        do i = 1,n
            res(:, i) = gradient(vect, step, exfunc, n, i, approx)
        enddo
        !$omp end parallel do
    end function jacobi_matrix_omp

    function jacobi_matrix_mpi(exfunc, vect, step, n, approx) result(res)
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: step
        real, dimension(n), intent(in) :: vect
        integer :: i
        integer, dimension(process_size) :: naas
        real, dimension(n, n) :: res
        real, dimension(:,:), allocatable :: temp
        procedure(efunc) :: exfunc
        procedure(da_m_s) :: approx
        if(state_mpi == 0) call full_plan(n, .TRUE.)
        allocate(temp(n,arr_length))
        naas(1) = 0
        do i = 2,process_size
            naas(i) = naas(i-1) + n * all_arr_len(i-1)
        enddo
        do i = 1,arr_length
            temp(:, i) = gradient(vect, step, exfunc, n, i + arr_start - 1, approx)
        enddo
        call MPI_Allgatherv(temp, n*arr_length, MPI_REAL, res, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
    end function jacobi_matrix_mpi
end module num_diff_lib