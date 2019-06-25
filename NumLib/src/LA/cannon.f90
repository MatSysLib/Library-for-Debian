module cannon_lib
    implicit none
    double precision :: RT1, RT2
    contains
    function cannon(i_A, i_B, i_n) result(res)
        use mpi_manager
        use classic_lib
        implicit none
        integer, intent(in) :: i_n
        real, dimension(i_n,i_n), intent(in) :: i_A, i_B
        integer :: n, nblock, grid_n, col, row, shift, i, j, srowel, erowel, scolel, ecolel, rtoa, rtob, rfroma, rfromb
        real, dimension(i_n,i_n) :: res
        real, dimension(:,:), allocatable :: A, B, C, Ar, Br, Cr
        is_master = .FALSE.
        if(process_rank == 0) is_master = .TRUE.
        grid_n = int(sqrt(real(process_size)))
        if(grid_n**2/=process_size) then
            if(is_master) print *,"You need call this routine on square number of processes"
            return
        endif
        if(is_master) then
            if(mod(i_n, grid_n).ne.0) then
                n = i_n + grid_n - mod(i_n, grid_n)
            else
                n = i_n
            endif
            allocate(A(n,n), B(n,n), C(n,n))
            A = 0
            B = 0
            C = 0
            A(:i_n,:i_n) = i_A(:i_n,:i_n)
            B(:i_n,:i_n) = i_B(:i_n,:i_n)
            nblock = n / grid_n
        endif
        call MPI_Bcast(nblock, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
        allocate(Ar(nblock,nblock), Br(nblock,nblock), Cr(nblock,nblock))
        if(is_master) then
            Ar = A(:nblock,:nblock)
            Br = B(:nblock,:nblock)
            do i = 1,process_size-1
                col = mod(i,grid_n)
                row = i / grid_n
                shift = col - row
                if(shift<0) then
                    j = grid_n * (row + 1) + shift
                else
                    j = i - row
                endif
                srowel = row * nblock + 1
                erowel = srowel + nblock - 1
                scolel = mod(i, grid_n) * nblock + 1
                ecolel = scolel + nblock - 1
                call MPI_Send(A(srowel:erowel, scolel:ecolel), nblock**2, MPI_REAL, j, 0, MPI_COMM_WORLD, ierr)
            enddo
            do i = 1,process_size-1
                col = mod(i,grid_n)
                row = i / grid_n
                srowel = row * nblock + 1
                erowel = srowel + nblock - 1
                scolel = mod(i, grid_n) * nblock + 1
                ecolel = scolel + nblock - 1
                shift = row - col
                if(shift<0) then
                    j = (process_size - (grid_n - col)) + (shift + 1) * grid_n
                else
                    j = i - col * grid_n
                endif
                call MPI_Send(B(srowel:erowel, scolel:ecolel), nblock**2, MPI_REAL, j, 1, MPI_COMM_WORLD, ierr)
            enddo
        else
            call MPI_Recv(Ar, nblock**2, MPI_REAL, 0, 0, MPI_COMM_WORLD, status, ierr)
            call MPI_Recv(Br, nblock**2, MPI_REAL, 0, 1, MPI_COMM_WORLD, status, ierr)
        endif
        if(mod(process_rank, grid_n) == 0) then
            rtoa = process_rank + grid_n - 1
        else
            rtoa = process_rank - 1
        endif
        if(process_rank / grid_n == 0) then
            rtob = process_rank + grid_n * (grid_n - 1)
        else 
            rtob = process_rank - grid_n
        endif
        if(mod(process_rank, grid_n) + 1 == grid_n) then
            rfroma = process_rank / grid_n * grid_n
        else
            rfroma = process_rank + 1
        endif
        if(process_rank / grid_n + 1 == grid_n) then
            rfromb = mod(process_rank, grid_n)
        else
            rfromb = process_rank + grid_n
        endif
        RT1 = MPI_Wtime()
        Cr = Cr + classic(Ar, Br, nblock)
        do i = 0,grid_n-2
            call MPI_Sendrecv_replace(Ar, nblock**2, MPI_REAL, rtoa, 0, rfroma, 0, MPI_COMM_WORLD, status, ierr)
            call MPI_Sendrecv_replace(Br, nblock**2, MPI_REAL, rtob, 1, rfromb, 1, MPI_COMM_WORLD, status, ierr)
            Cr = Cr + classic(Ar, Br, nblock)
        enddo
        if(is_master) then
            C(:nblock,:nblock) = Cr
            do i = 1,process_size-1
                col = mod(i, grid_n)
                row = i / grid_n
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
    end function cannon

    function timing_mpi(a, b, n, repeats) result(res)
        implicit none
        integer, intent(in) :: n, repeats
        real, dimension(n,n), intent(in) :: a, b
        integer :: i
        real, dimension(n,n) :: t
        double precision, dimension(repeats) :: res
        do i = 1,repeats
            t = cannon(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module cannon_lib