module newton_lib
    implicit none
    integer(kind = 8), private :: T1, T2
    double precision, private :: RT1, RT2
    interface newton_func_onev_all
        module procedure newton_func_onev_all_ione, newton_func_onev_all_imany
    end interface
    real, parameter :: dx_step = 0.001
    integer :: iteration_counter = 0
    contains
    !Simple function with one variable
    real function newton_func_onev(func, xstart, eps) result(xnew)
        use num_diff_lib, only : dx => d1x_2p_fd
        implicit none
        real, external :: func
        real, intent(in) :: xstart, eps
        real :: xold
        iteration_counter = 1
        xold = xstart
        xnew = xold - func(xold)/dx(xold, dx_step, func)
        do while(abs(xold - xnew)>eps)
            xold = xnew
            xnew = xold - func(xold)/dx(xold, dx_step, func)
            iteration_counter = iteration_counter + 1
        enddo
    end function newton_func_onev

    !Function for one varible
    !Trying to exclude already found variables
    real function newton_func_manyv(func, xstart, eps, xfound) result(xnew)
        use num_diff_lib, only : dx => d1x_2p_fd
        implicit none
        real, external :: func
        real, intent(in) :: xstart, eps
        real, dimension(:), intent(in) :: xfound
        integer :: i, imax
        real :: xold, temp
        imax = iteration_counter / 5
        xnew = xstart
        do iteration_counter = 0, imax
            xold = xnew
            temp = func(xold)/dx(xold, dx_step, func)
            do i=1,size(xfound)
                temp = temp / (xold-xfound(i))
            enddo
            xnew = xold - temp
        enddo
        do while(abs(xold - xnew)>eps)
            xold = xnew
            xnew = xold - func(xold)/dx(xold, dx_step, func)
            iteration_counter = iteration_counter + 1
        enddo
    end function newton_func_manyv 

    !Function with one variable and several roots
    !Accepts point
    function newton_func_onev_all_ione(func, xstart, eps, n) result(xnew)
        implicit none
        integer, intent(in) :: n
        real, external :: func
        real, intent(in) :: xstart, eps
        integer :: i
        real, dimension(n) :: xnew
        real, dimension(:), allocatable :: xold
        allocate(xold(1))
        do i=1,n
            if(i==1) then
                xnew(i) = newton_func_onev(func, xstart, eps)
                xold(i) = xnew(i)
            else
                xnew(i) = newton_func_manyv(func, xstart, eps, xold)
                xold = [xold, xnew(i)]
            endif
        enddo
        deallocate(xold)
    end function newton_func_onev_all_ione

    !Function with one variable and several roots
    !Doesn't tries to find all roots - assuming that starting values close enough
    !Accepts vector
    !TODO: Try to do a two phase: first - low precision sequential
                                ! second - high precision parallel
    function newton_func_onev_imany(func, xstart, eps, n) result(xnew)
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        real, external :: func
        integer :: i
        real, dimension(n) :: xnew
        do i=1,n
            xnew(i) = newton_func_onev(func, xstart(i), eps)
        enddo
    end function newton_func_onev_imany
    
    function newton_func_onev_imany_omp(func, xstart, eps, n) result(xnew)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        real, external :: func
        integer :: i
        real, dimension(n) ::xnew
        !$omp parallel do
        do i=1,n
            xnew(i) = newton_func_onev(func, xstart(i), eps)
        enddo
        !$omp end parallel do
    end function newton_func_onev_imany_omp
    
    !Function with one variable and several roots
    !Tries to find all roots by excluding found roots
    !Accepts vector
    function newton_func_onev_all_imany(func, xstart, eps, n) result(xnew)
        implicit none
        integer, intent(in) :: n
        real, external :: func
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        integer :: i
        real, dimension(n) :: xnew
        real, dimension(:), allocatable :: xold
        allocate(xold(1))
        do i=1,n
            if(i==1) then
                xnew(i) = newton_func_onev(func, xstart(i), eps)
                xold(i) = xnew(i)
            else
                xnew(i) = newton_func_manyv(func, xstart(i), eps, xold)
                xold = [xold, xnew(i)]
            endif
        enddo
        deallocate(xold)
    end function newton_func_onev_all_imany

    !A system of nonlinear equations
    !This variant accepts function returning vector of results
    !TODO: Variant which accepts array of functions. Think saw on Intel forums, but google failed me.
    function newton_func_system(func, xstart, eps, n) result(xnew)
        use num_diff_lib, dx => d1x_2p_fd_mult
        use gauss_lib, only : gauss
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        procedure(efunc) :: func
        real, dimension(n) :: xold, xnew
        xold = xstart - 1
        xnew = xstart
        call system_clock(T1)
        do while(norm2(xold-xnew)>eps)
            xold = xnew
            xnew = xold - gauss(jacobi_matrix(func, xold, dx_step, n, dx), func(xold), n)
        enddo
        call system_clock(T2)
    end function newton_func_system

    function timing(func, xstart, eps, n, repeats) result(res)
        use num_diff_lib, only : efunc
        implicit none
        integer, intent(in) :: repeats, n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        procedure(efunc) :: func
        integer :: i
        integer(kind = 8) :: rate
        real, dimension(n) :: t
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = newton_func_system(func, xstart, eps, n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function newton_func_system_omp(func, xstart, eps, n) result(xnew)
        use num_diff_lib, dx => d1x_2p_fd_mult
        use gauss_lib, only : gauss_omp
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        procedure(efunc) :: func
        real, dimension(n) :: xold, xnew
        xold = xstart - 1
        xnew = xstart
        RT1 = omp_get_wtime()
        do while(norm2(xold-xnew)>eps)
            xold = xnew
            xnew = xold - gauss_omp(jacobi_matrix_omp(func, xold, dx_step, n, dx), func(xold), n)
        enddo
        RT2 = omp_get_wtime()
    end function newton_func_system_omp

    function timing_omp(func, xstart, eps, n, repeats) result(res)
        use num_diff_lib, only : efunc
        implicit none
        integer, intent(in) :: repeats, n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        procedure(efunc) :: func
        integer :: i
        real, dimension(n) :: t
        double precision :: res(repeats)
        do i = 1,repeats
            t = newton_func_system_omp(func, xstart, eps, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function newton_func_system_mpi(func, xstart, eps, n) result(xnew)
        use num_diff_lib, dx => d1x_2p_fd_mult
        use gauss_lib, only : gauss_mpi
        use mpi
        implicit none
        integer, intent(in) :: n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        procedure(efunc) :: func
        real, dimension(n) :: xold, xnew
        xold = xstart - 1
        xnew = xstart
        RT1 = MPI_Wtime()
        do while(norm2(xold-xnew)>eps)
            xold = xnew
            xnew = xold - gauss_mpi(jacobi_matrix_mpi(func, xold, dx_step, n, dx), func(xold), n)
        enddo
        RT2 = MPI_Wtime()
    end function newton_func_system_mpi

    function timing_mpi(func, xstart, eps, n, repeats) result(res)
        use num_diff_lib, only : efunc
        implicit none
        integer, intent(in) :: repeats, n
        real, intent(in) :: eps
        real, dimension(n), intent(in) :: xstart
        procedure(efunc) :: func
        integer :: i
        real, dimension(n) :: t
        double precision :: res(repeats)
        do i = 1,repeats
            print *, xstart
            t = newton_func_system_mpi(func, xstart, eps, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module newton_lib