module gauss_lib
    implicit none
    interface null_triangle
        module procedure null_triangle_gauss, null_triangle_matrix
    end interface null_triangle
    interface null_triangle_omp
        module procedure null_triangle_gauss_omp, null_triangle_matrix_omp
    end interface null_triangle_omp
    interface null_triangle_mpi
        module procedure null_triangle_gauss_mpi, null_triangle_matrix_mpi
    end interface null_triangle_mpi
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    integer, private :: istart, iend, jend, c_step, ps, pe, ita_s, ita_e
contains
    !Location of triangle remaining
    !In other corner will be 0
    !u or d, l or r
    subroutine set_triangle_type(vertical, horizontal, n)
        implicit none
        integer, intent(in) :: n
        character, intent(in) :: vertical, horizontal
        if ((vertical == 'd') .AND. (horizontal == 'l')) then
            istart = 1
            iend = n
            jend = n

            c_step = 1
            ps = 0
            pe = -10
            ita_s = 1
            ita_e = -10
        endif
        if ((vertical == 'u') .AND. (horizontal == 'r')) then
            istart = n
            iend = 1
            jend = 1

            c_step = -1
            ps = -10
            pe = 0
            ita_s = -10
            ita_e = 1
        endif
    end subroutine set_triangle_type

    integer function s_set(input) result(res)
        implicit none
        integer, intent(in) :: input
        res = input + c_step
    end function s_set

    function gauss(a, b, n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(:), intent(in) :: b
        real, dimension(:, :), intent(in) :: a
        real, dimension(n) :: res
        real, dimension(n, n) :: wa
        wa = a
        res = b
        call SYSTEM_CLOCK(T1)
        call set_triangle_type('d', 'l', n)
        call null_triangle(wa, res)
        call set_triangle_type('u', 'r', n)
        call null_triangle_gauss_b(wa, res)
        call SYSTEM_CLOCK(T2)
    end function gauss

    function gauss_omp(a, b, n) result(res)
        use omp_lib
        implicit none
        integer, intent(in) :: n
        real, dimension(:), intent(in) :: b
        real, dimension(:, :), intent(in) :: a
        real, dimension(n) :: res
        real, dimension(n, n) :: wa
        wa = a
        res = b
        RT1 = omp_get_wtime()
        call set_triangle_type('d', 'l', n)
        call null_triangle_omp(wa, res)
        call set_triangle_type('u', 'r', n)
        call null_triangle_gauss_b_omp(wa, res)
        RT2 = omp_get_wtime()
    end function gauss_omp

    function gauss_mpi(a, b, n) result(res)
        use mpi
        implicit none
        integer, intent(in) :: n
        real, dimension(:), intent(in) :: b
        real, dimension(:, :), intent(in) :: a
        real, dimension(n) :: res
        real, dimension(n, n) :: wa
        wa = a
        res = b
        RT1 = MPI_Wtime()
        call set_triangle_type('d', 'l', n)
        call null_triangle_gauss_mpi(wa, res, n)
        call set_triangle_type('u', 'r', n)
        call null_triangle_gauss_b_mpi(wa, res, n)
        RT2 = MPI_Wtime()
    end function gauss_mpi

    function timing(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(:), intent(in) :: b
        real, dimension(:, :), intent(in) :: a
        integer :: i
        integer(kind=8) :: rate
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1, repeats
            t = gauss(a, b, n)
            res(i) = dble(T2 - T1)/dble(rate)
        enddo
    end function timing

    function timing_omp(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(:), intent(in) :: b
        real, dimension(:, :), intent(in) :: a
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res(repeats)
        do i = 1, repeats
            t = gauss_omp(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: repeats, n
        real, dimension(:), intent(in) :: b
        real, dimension(:, :), intent(in) :: a
        integer :: i
        real, dimension(n) :: t
        double precision, dimension(repeats) :: res(repeats)
        do i = 1, repeats
            t = gauss_mpi(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi

    subroutine null_triangle_gauss(a, b)
        implicit none
        integer :: i, j
        real :: temp
        real, dimension(:) :: b
        real, dimension(:, :) :: a
        do i = istart, iend, c_step
            temp = a(i, i)
            a(:, i) = a(:, i)/temp
            b(i) = b(i)/temp
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                a(:, j) = a(:, j) - a(:, i)*temp
                b(j) = b(j) - b(i)*temp
            enddo
        enddo
    end subroutine null_triangle_gauss

    subroutine null_triangle_gauss_b(a, b)
        implicit none
        integer :: i, j
        real, dimension(:) :: b
        real, dimension(:, :) :: a
        do i = istart, iend, c_step
            b(i) = b(i)/a(i, i)
            do j = s_set(i), jend, c_step
                b(j) = b(j) - b(i)*a(i, j)
            enddo
        enddo
    end subroutine null_triangle_gauss_b

    subroutine null_triangle_matrix(a)
        implicit none
        integer :: i, j
        real :: temp
        real, dimension(:, :) :: a
        do i = istart, iend, c_step
            temp = a(i, i)
            a(:, i) = a(:, i)/temp
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                a(:, j) = a(:, j) - a(:, i)*temp
            enddo
        enddo
    end subroutine null_triangle_matrix

    real function null_triangle_matrix_get_normalise_mult(a) result(res)
        implicit none
        real :: a(:, :), temp
        integer :: i, j
        res = 1
        do i = istart, iend, c_step
            temp = a(i, i)
            a(:, i) = a(:, i)/temp
            res = res*temp
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                a(:, j) = a(:, j) - a(:, i)*temp
            enddo
        enddo
    end function null_triangle_matrix_get_normalise_mult

    subroutine null_triangle_matrix_mirror(A, mirror)
        implicit none
        integer :: i, j
        real :: temp
        real, dimension(:, :) ::  A, mirror
        do i = istart, iend, c_step
            temp = a(i, i)
            mirror(:, i) = mirror(:, i)/temp
            a(:, i) = a(:, i)/temp
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                a(:, j) = a(:, j) - a(:, i)*temp
                mirror(:, j) = mirror(:, j) - mirror(:, i)*temp
            enddo
        enddo
    end subroutine null_triangle_matrix_mirror

    !Conveer not parallelized, forced to use do loop
    subroutine null_triangle_gauss_omp(a, b)
        use omp_lib
        implicit none
        integer :: i, j, k
        real :: temp
        real, dimension(:) :: b
        real, dimension(:, :) :: a
        do i = istart, iend, c_step
            temp = a(i, i)
            a(:, i) = a(:, i)/temp
            b(i) = b(i)/temp
!$OMP       parallel do private(temp, j, k)
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                do k = istart, iend, c_step
                    a(k, j) = a(k, j) - a(k, i)*temp
                enddo
                b(j) = b(j) - b(i)*temp
            enddo
!$OMP       end parallel do
        enddo
    end subroutine null_triangle_gauss_omp

    subroutine null_triangle_gauss_b_omp(a, b)
        use omp_lib
        implicit none
        integer :: i, j
        real, dimension(:) :: b
        real, dimension(:, :) :: a
        do i = istart, iend, c_step
            b(i) = b(i)/a(i, i)
!$OMP       parallel do
            do j = s_set(i), jend, c_step
                b(j) = b(j) - b(i)*a(i, j)
            enddo
!$OMP       end parallel do
        enddo
    end subroutine null_triangle_gauss_b_omp

    subroutine null_triangle_matrix_omp(a)
        use omp_lib
        implicit none
        integer :: i, j, k
        real :: temp
        real, dimension(:, :) :: a
        do i = istart, iend, c_step
            temp = a(i, i)
            a(:, i) = a(:, i)/temp
!$OMP       parallel do private(temp, j, k)
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                do k = istart, iend, c_step
                    a(k, j) = a(k, j) - a(k, i)*temp
                enddo
            enddo
!$OMP       end parallel do
        enddo
    end subroutine null_triangle_matrix_omp

    real function null_triangle_matrix_get_normalise_mult_omp(a) result(res)
        use omp_lib
        implicit none
        integer :: i, j, k
        real :: temp
        real, dimension(:, :) :: a
        res = 1
        do i = istart, iend, c_step
            temp = a(i, i)
            a(:, i) = a(:, i)/temp
            res = res*temp
!$OMP       parallel do private(temp, j, k)
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                do k = istart, iend, c_step
                    a(k, j) = a(k, j) - a(k, i)*temp
                enddo
            enddo
!$OMP       end parallel do
        enddo
    end function null_triangle_matrix_get_normalise_mult_omp

    subroutine null_triangle_matrix_mirror_omp(A, mirror)
        use omp_lib
        implicit none
        integer :: i, j, k
        real :: temp
        real, dimension(:, :) :: A, mirror
        do i = istart, iend, c_step
            temp = a(i, i)
            mirror(:, i) = mirror(:, i)/temp
            a(:, i) = a(:, i)/temp
!$OMP       parallel do private(temp, j, k)
            do j = s_set(i), jend, c_step
                temp = a(i, j)
                do k = istart, iend, c_step
                    a(k, j) = a(k, j) - a(k, i)*temp
                    mirror(k, j) = mirror(k, j) - mirror(k, i)*temp
                enddo
            enddo
!$OMP       end parallel do
        enddo
    end subroutine null_triangle_matrix_mirror_omp

    !Size of matrix A must be divisible by processor number
    !Block distribution
    !Forward
    subroutine null_triangle_gauss_mpi(a, b, n)
        use mpi_manager
        implicit none
        real :: temp, vt
        real, dimension(:) :: b
        real, dimension(n) :: v_temp
        real, dimension(n + 1) :: p_t
        real, dimension(:, :) :: a
        real, dimension(:), allocatable :: vbuf
        real, dimension(:, :), allocatable :: sendbuf
        integer :: n, i, j, p, c_start, c_end, c_now, ias, iae, ian
        integer, dimension(process_size) :: naas
        if (state_mpi == 0) call full_plan(n, .TRUE.)
        ita_s = 1
        ita_e = 1
        ias = -1
        iae = -1
        c_start = -1
        c_end = -1
        if (c_step > 0) then
            ps = 0
            pe = process_size - 1
            c_start = 1
            c_end = n
        elseif (c_step < 0) then
            ps = process_size - 1
            pe = 0
            c_start = n
            c_end = 1
        endif
        allocate (sendbuf(n, arr_length), vbuf(arr_length))
        naas(1) = 0
        do i = 2, process_size
            naas(i) = naas(i - 1) + n*all_arr_len(i - 1)
        enddo
        sendbuf = a(:, arr_start:arr_end)
        vbuf = b(arr_start:arr_end)
        c_now = c_start

        do p = ps, pe, c_step
            if (c_step > 0) then
                ita_e = all_arr_len(p + 1)
            elseif (c_step < 0) then
                ita_s = all_arr_len(p + 1)
            endif
            do i = ita_s, ita_e, c_step
                if (process_rank == p) then
                    vt = sendbuf(arr_start + i - 1, i)
                    sendbuf(:, i) = sendbuf(:, i)/vt
                    vbuf(i) = vbuf(i)/vt
                    if (c_step > 0) then
                        ias = c_now
                        iae = c_end
                        ian = n - c_now + 1 + 1
                    elseif (c_step < 0) then
                        ias = c_end
                        iae = c_now
                        ian = c_now + 1
                    endif
                    call MPI_Bcast([sendbuf(ias:iae, i), vbuf(i)], ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    do j = s_set(i), ita_e, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*sendbuf(:, i)
                        vbuf(j) = vbuf(j) - vt*vbuf(i)
                    enddo
                elseif (((process_rank < p) .AND. (c_step < 0)) .OR. ((process_rank > p) .AND. (c_step > 0))) then
                    p_t = 0
                    if (c_step > 0) then
                        ian = n - c_now + 1 + 1
                    elseif (c_step < 0) then
                        ian = c_now + 1
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    if (c_step < 0) v_temp(1:c_now) = p_t(abs(c_now - c_start) + 1:n)
                    if (c_step > 0) v_temp = p_t(1:c_end)
                    temp = p_t(n + 1)
                    if (c_step > 0) then
                        ias = ita_s
                        iae = arr_length
                    elseif (c_step < 0) then
                        ias = arr_length
                        iae = ita_e
                    endif
                    do j = ias, iae, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*v_temp(:)
                        vbuf(j) = vbuf(j) - vt*temp
                    enddo
                else
                    if (c_step > 0) then
                        ian = n - c_now + 1 + 1
                    elseif (c_step < 0) then
                        ian = c_now + 1
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                endif
                c_now = c_now + c_step
            enddo
        enddo
        if (is_serial) then
            a = sendbuf
            b = vbuf
        else
            call MPI_Allgatherv(sendbuf, n*arr_length, MPI_REAL, a, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
            call MPI_Allgatherv(vbuf, arr_length, MPI_REAL, b, all_arr_len, all_arr_start - 1, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        deallocate (sendbuf, vbuf)
    end subroutine null_triangle_gauss_mpi

    !Size of matrix A must be divisible by processor number
    !Block distribution
    !Forward
    subroutine null_triangle_gauss_b_mpi(a, b, n)
        use mpi_manager
        implicit none
        real :: temp
        real, dimension(:) :: b
        real, dimension(:, :) :: a
        real, dimension(:), allocatable :: vbuf
        real, dimension(:, :), allocatable :: sendbuf
        integer :: n, i, p, c_start, c_end
        if (state_mpi == 0) call full_plan(n, .TRUE.)
        c_start = -1
        c_end = -1
        ita_e = 1
        ita_s = 1
        if (c_step > 0) then
            pe = process_size - 1
            c_start = 1
            c_end = -1
        elseif (c_step < 0) then
            ps = process_size - 1
            c_start = 1
            c_end = arr_length
        endif
        allocate (sendbuf(n, arr_length), vbuf(arr_length))
        sendbuf = a(:, arr_start:arr_end)
        vbuf = b(arr_start:arr_end)

        do p = ps, pe, c_step
            if (c_step > 0) then
                ita_e = all_arr_len(p + 1)
            elseif (c_step < 0) then
                ita_s = all_arr_len(p + 1)
            endif
            do i = ita_s, ita_e, c_step
                if (process_rank == p) then
                    call MPI_Bcast(vbuf(i), 1, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    if (c_step > 0) then
                        c_start = s_set(i)
                        c_end = arr_length
                    elseif (c_step < 0) then
                        c_start = 1
                        c_end = s_set(i)
                    endif
                    vbuf(c_start:c_end) = vbuf(c_start:c_end) - vbuf(i)*sendbuf(arr_start + i - 1, c_start:c_end)
                elseif (((process_rank < p) .AND. (c_step < 0)) .OR. ((process_rank > p) .AND. (c_step > 0))) then
                    call MPI_Bcast(temp, 1, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    vbuf(:) = vbuf(:) - temp*sendbuf(all_arr_start(p + 1) + i - 1, :)
                else
                    call MPI_Bcast(temp, 1, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                endif
            enddo
        enddo
        if (is_serial) then
            b = vbuf
        else
            call MPI_Allgatherv(vbuf, arr_length, MPI_REAL, b, all_arr_len, all_arr_start - 1, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        deallocate (sendbuf, vbuf)
    end subroutine null_triangle_gauss_b_mpi

    subroutine null_triangle_matrix_mpi(a, n)
        use mpi_manager
        implicit none
        real :: vt
        real, dimension(n) :: p_t, v_temp
        real, dimension(:, :) :: a
        real, dimension(:, :), allocatable :: sendbuf
        integer :: n, i, j, p, c_start, c_end, c_now, ias, iae, ian
        integer, dimension(process_size) :: naas
        if (state_mpi == 0) call full_plan(n, .TRUE.)
        ita_s = 1
        ita_e = 1
        ias = -1
        iae = -1
        c_start = -1
        c_end = -1
        if (c_step > 0) then
            ps = 0
            pe = process_size - 1
            c_start = 1
            c_end = n
        elseif (c_step < 0) then
            ps = process_size - 1
            pe = 0
            c_start = n
            c_end = 1
        endif
        allocate (sendbuf(n, arr_length))
        naas(1) = 0
        do i = 2, process_size
            naas(i) = naas(i - 1) + n*all_arr_len(i - 1)
        enddo
        sendbuf = a(:, arr_start:arr_end)
        c_now = c_start

        do p = ps, pe, c_step
            if (c_step > 0) then
                ita_e = all_arr_len(p + 1)
            elseif (c_step < 0) then
                ita_s = all_arr_len(p + 1)
            endif
            do i = ita_s, ita_e, c_step
                if (process_rank == p) then
                    vt = sendbuf(arr_start + i - 1, i)
                    sendbuf(:, i) = sendbuf(:, i)/vt
                    if (c_step > 0) then
                        ias = c_now
                        iae = c_end
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ias = c_end
                        iae = c_now
                        ian = c_now
                    endif
                    call MPI_Bcast(sendbuf(ias:iae, i), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    do j = s_set(i), ita_e, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*sendbuf(:, i)
                    enddo
                elseif (((process_rank < p) .AND. (c_step < 0)) .OR. ((process_rank > p) .AND. (c_step > 0))) then
                    p_t = 0
                    if (c_step > 0) then
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ian = c_now
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    if (c_step < 0) v_temp(1:c_now) = p_t(abs(c_now - c_start) + 1:n)
                    if (c_step > 0) v_temp = p_t(1:c_end)
                    if (c_step > 0) then
                        ias = ita_s
                        iae = arr_length
                    elseif (c_step < 0) then
                        ias = arr_length
                        iae = ita_e
                    endif
                    do j = ias, iae, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*v_temp(:)
                    enddo
                else
                    if (c_step > 0) then
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ian = c_now
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                endif
                c_now = c_now + c_step
            enddo
        enddo
        if (is_serial) then
            a = sendbuf
        else
            call MPI_Allgatherv(sendbuf, n*arr_length, MPI_REAL, a, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        deallocate (sendbuf)
    end subroutine null_triangle_matrix_mpi

    real function null_triangle_matrix_get_normalise_mult_mpi(a, n) result(res)
        use mpi_manager
        implicit none
        real :: vt, mult
        real, dimension(n) :: p_t, v_temp
        real, dimension(:, :) :: a
        real, dimension(:, :), allocatable :: sendbuf(:, :)
        integer :: n, i, j, p, c_start, c_end, c_now, ias, iae, ian
        integer, dimension(process_size) :: naas
        if (state_mpi == 0) call full_plan(n, .TRUE.)
        ita_s = 1
        ita_e = 1
        ias = -1
        iae = -1
        c_start = -1
        c_end = -1
        if (c_step > 0) then
            ps = 0
            pe = process_size - 1
            c_start = 1
            c_end = n
        elseif (c_step < 0) then
            ps = process_size - 1
            pe = 0
            c_start = n
            c_end = 1
        endif
        allocate (sendbuf(n, arr_length))
        naas(1) = 0
        do i = 2, process_size
            naas(i) = naas(i - 1) + n*all_arr_len(i - 1)
        enddo
        sendbuf = a(:, arr_start:arr_end)
        c_now = c_start
        mult = 1

        do p = ps, pe, c_step
            if (c_step > 0) then
                ita_e = all_arr_len(p + 1)
            elseif (c_step < 0) then
                ita_s = all_arr_len(p + 1)
            endif
            do i = ita_s, ita_e, c_step
                if (process_rank == p) then
                    vt = sendbuf(arr_start + i - 1, i)
                    mult = mult*vt
                    sendbuf(:, i) = sendbuf(:, i)/vt
                    if (c_step > 0) then
                        ias = c_now
                        iae = c_end
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ias = c_end
                        iae = c_now
                        ian = c_now
                    endif
                    call MPI_Bcast(sendbuf(ias:iae, i), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    do j = s_set(i), ita_e, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*sendbuf(:, i)
                    enddo
                elseif (((process_rank < p) .AND. (c_step < 0)) .OR. ((process_rank > p) .AND. (c_step > 0))) then
                    p_t = 0
                    if (c_step > 0) then
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ian = c_now
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    if (c_step < 0) v_temp(1:c_now) = p_t(abs(c_now - c_start) + 1:n)
                    if (c_step > 0) v_temp = p_t(1:c_end)
                    if (c_step > 0) then
                        ias = ita_s
                        iae = arr_length
                    elseif (c_step < 0) then
                        ias = arr_length
                        iae = ita_e
                    endif
                    do j = ias, iae, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*v_temp(:)
                    enddo
                else
                    if (c_step > 0) then
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ian = c_now
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                endif
                c_now = c_now + c_step
            enddo
        enddo
        if (is_serial .eqv. .true.) then
            a = sendbuf
        else
            call MPI_Allgatherv(sendbuf, n*arr_length, MPI_REAL, a, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        deallocate (sendbuf)
        call MPI_Allreduce(mult, res, 1, MPI_REAL, MPI_PROD, MPI_COMM_WORLD, ierr)
    end function null_triangle_matrix_get_normalise_mult_mpi

    subroutine null_triangle_matrix_mirror_mpi(A, mirror, n)
        use mpi_manager
        implicit none
        real :: vt
        real, dimension(n) :: v_temp, m_temp, p_t
        real, dimension(:, :) :: a, mirror
        real, dimension(:, :), allocatable :: sendbuf, mirbuf
        integer :: n, i, j, p, c_start, c_end, c_now, ias, iae, ian
        integer, dimension(process_size) :: naas
        if (state_mpi == 0) call full_plan(n, .TRUE.)
        ias = -1
        iae = -1
        c_start = -1
        c_end = -1
        ita_s = 1
        ita_e = 1
        if (c_step > 0) then
            ps = 0
            pe = process_size - 1
            c_start = 1
            c_end = n
        elseif (c_step < 0) then
            ps = process_size - 1
            pe = 0
            c_start = n
            c_end = 1
        endif
        allocate (sendbuf(n, arr_length), mirbuf(n, arr_length))
        naas(1) = 0
        do i = 2, process_size
            naas(i) = naas(i - 1) + n*all_arr_len(i - 1)
        enddo
        sendbuf = a(:, arr_start:arr_end)
        mirbuf = mirror(:, arr_start:arr_end)
        c_now = c_start

        do p = ps, pe, c_step
            if (c_step > 0) then
                ita_e = all_arr_len(p + 1)
            elseif (c_step < 0) then
                ita_s = all_arr_len(p + 1)
            endif
            do i = ita_s, ita_e, c_step
                if (process_rank == p) then
                    vt = sendbuf(arr_start + i - 1, i)
                    sendbuf(:, i) = sendbuf(:, i)/vt
                    mirbuf(:, i) = mirbuf(:, i)/vt
                    if (c_step > 0) then
                        ias = c_now
                        iae = c_end
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ias = c_end
                        iae = c_now
                        ian = c_now
                    endif
                    call MPI_Bcast(sendbuf(ias:iae, i), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    call MPI_Bcast(mirbuf(:, i), n, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    do j = s_set(i), ita_e, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*sendbuf(:, i)
                        mirbuf(:, j) = mirbuf(:, j) - vt*mirbuf(:, i)
                    enddo
                elseif (((process_rank < p) .AND. (c_step < 0)) .OR. ((process_rank > p) .AND. (c_step > 0))) then
                    p_t = 0
                    if (c_step > 0) then
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ian = c_now
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    call MPI_Bcast(m_temp, n, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    if (c_step < 0) v_temp(1:c_now) = p_t(abs(c_now - c_start) + 1:n)
                    if (c_step > 0) v_temp = p_t(1:c_end)
                    if (c_step > 0) then
                        ias = ita_s
                        iae = arr_length
                    elseif (c_step < 0) then
                        ias = arr_length
                        iae = ita_e
                    endif
                    do j = ias, iae, c_step
                        vt = sendbuf(all_arr_start(p + 1) + i - 1, j)
                        sendbuf(:, j) = sendbuf(:, j) - vt*v_temp(:)
                        mirbuf(:, j) = mirbuf(:, j) - vt*m_temp(:)
                    enddo
                else
                    if (c_step > 0) then
                        ian = n - c_now + 1
                    elseif (c_step < 0) then
                        ian = c_now
                    endif
                    call MPI_Bcast(p_t(abs(c_now - c_start) + 1), ian, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                    call MPI_Bcast(m_temp, n, MPI_REAL, p, MPI_COMM_WORLD, ierr)
                endif
                c_now = c_now + c_step
            enddo
        enddo
        if (is_serial) then
            a = sendbuf
            mirror = mirbuf
        else
            call MPI_Allgatherv(sendbuf, n*arr_length, MPI_REAL, a, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
            call MPI_Allgatherv(mirbuf, n*arr_length, MPI_REAL, mirror, n*all_arr_len, naas, MPI_REAL, MPI_COMM_WORLD, ierr)
        endif
        deallocate (sendbuf, mirbuf)
    end subroutine null_triangle_matrix_mirror_mpi
end module gauss_lib