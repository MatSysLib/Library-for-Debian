module fox_lib
    implicit none
    double precision :: RT1, RT2
    contains
    function fox(A, B, i_n) result(res)
        use mpi_manager
        use classic_lib
        implicit none
        integer, intent(in) :: i_n
        real, dimension(i_n,i_n), intent(in) :: A, B
        integer :: grid_slen, comm_grid, comm_row, comm_col, n, nblock, col, row, srowel, erowel, scolel, ecolel, i, j, rtob, rfromb
        integer, dimension(2) :: grid_size, grid_pos
        logical, dimension(2) :: grid_subd, period
        real, dimension(i_n,i_n) :: res
        real, dimension(:,:), allocatable :: C, Ar, Br, Cr, Tr
        is_master = .FALSE.
        if(process_rank == 0) is_master = .TRUE.
        grid_slen = int(sqrt(real(process_size)))
        if(grid_slen**2/=process_size) then
            if(is_master) print *,"You need call this routine on square number of processes"
            return
        endif
        grid_size = grid_slen
        period = .FALSE.
        call MPI_Cart_create(MPI_COMM_WORLD, 2, grid_size, period, .TRUE., comm_grid, ierr)
        call MPI_Cart_coords(comm_grid, process_rank, 2, grid_pos, ierr)
        grid_subd(1) = .FALSE.
        grid_subd(2) = .TRUE.
        call MPI_Cart_sub(comm_grid, grid_subd, comm_row, ierr)
        grid_subd(1) = .TRUE.
        grid_subd(2) = .FALSE.
        call MPI_Cart_sub(comm_grid, grid_subd, comm_col, ierr)
        if(is_master) then
            if(mod(i_n, grid_slen)/=0) then
                n = i_n + grid_slen - mod(i_n, grid_slen)
            else
                n = i_n
            endif
            nblock = n / grid_slen
            allocate(C(n,n))
            C = 0
        endif
        call MPI_Bcast(nblock, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
        allocate(Ar(nblock,nblock), Br(nblock,nblock), Cr(nblock,nblock), Tr(nblock,nblock))
        Ar = 0
        Br = 0
        Cr = 0
        if(is_master) then
            Ar = A(:nblock,:nblock)
            Br = B(:nblock,:nblock)
            do i = 1,process_size-1
                col = mod(i, grid_slen)
                row = i / grid_slen
                srowel = row * grid_slen + 1
                erowel = srowel + nblock - 1
                scolel = col * grid_slen + 1
                ecolel = scolel + nblock - 1
                call MPI_Send(A(srowel:erowel,scolel:ecolel), nblock**2, MPI_REAL, i, 0, MPI_COMM_WORLD, ierr)
                call MPI_Send(B(srowel:erowel,scolel:ecolel), nblock**2, MPI_REAL, i, 0, MPI_COMM_WORLD, ierr)            
            enddo
        else
            call MPI_Recv(Ar, nblock**2, MPI_REAL, 0, 0, MPI_COMM_WORLD, status, ierr)
            call MPI_Recv(Br, nblock**2, MPI_REAL, 0, 0, MPI_COMM_WORLD, status, ierr)
        endif
        col = mod(process_rank, grid_slen)
        row = process_rank / grid_slen
        RT1 = MPI_Wtime()
        do i = 0,grid_slen-1
            j = mod((row + i), grid_slen)
            call MPI_Bcast(Ar, nblock**2, MPI_REAL, j, comm_row, ierr)
            Cr = Cr + classic(Ar, Br, nblock)
            if(row == 0) then
                rtob = col + grid_slen * (grid_slen - 1)
            else
                rtob = process_rank - grid_slen
            endif
            if(row == grid_slen - 1) then
                rfromb = col
            else
                rfromb = process_rank + grid_slen
            endif
            call MPI_Sendrecv_replace(Br, nblock**2, MPI_REAL, rtob, 0, rfromb, 0, MPI_COMM_WORLD, status, ierr)
        enddo
        if(is_master) then
            C(:nblock,:nblock) = Cr
            do i = 1,process_size-1
                col = mod(i, grid_slen)
                row = i / grid_slen
                srowel = row * nblock + 1
                erowel = srowel + nblock - 1
                scolel = col * nblock + 1
                ecolel = scolel + nblock - 1
                call MPI_Recv(C(srowel:erowel, scolel:ecolel), nblock**2, MPI_REAL, i, 0, MPI_COMM_WORLD, status, ierr)
            enddo
            res = C(:i_n,:i_n)
        else
            call MPI_Send(Cr, nblock**2, MPI_REAL, 0, 0, MPI_COMM_WORLD, ierr)
        endif
        RT2 = MPI_Wtime()
        call MPI_Bcast(res, i_n**2, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
    end function fox

    function timing_mpi(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, dimension(n,n), intent(in) :: a, b
        integer :: i
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = fox(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module fox_lib