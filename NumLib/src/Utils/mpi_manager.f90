!Оптимизировано под фортрановские массивы.
!Предполагается, что массивы имеют размеры 1:n
module mpi_manager
    use mpi
    implicit none
        interface full_plan
            module procedure full_plan_def, full_plan_main
        end interface full_plan
        logical, public, save :: is_master = .false., is_worker = .false., is_serial = .false.
        !mpi_map(iterations) - sets process for each iteration
        integer, public, allocatable, save :: all_arr_len(:), all_arr_start(:), all_arr_end(:), mpi_map(:)
        integer, public, allocatable, save :: merge_map_send(:,:), merge_map_recv(:,:)
        integer, public, save :: arr_start, arr_end, arr_length
        integer, public, save :: state_mpi = 0, merge_steps
        integer, public :: status(MPI_STATUS_SIZE), process_size, process_rank
        integer, public :: ierr
        integer, private :: Number
    contains
    !Create map for merging arrays.
    !-1 - wait and merge sent
    !-2 - exit
    !otherwise sent to process with rank corresponding to number
    !This subroutine doesn't take into consideration whether the ranks lies on one processor
    subroutine create_simple_merge_map()
        implicit none
        integer :: i, j, k, power, t_size
        integer, allocatable :: free_processes(:), temp(:)
        t_size = process_size / 2
        allocate(free_processes(process_size), temp(t_size))
        power = 0
        i = 1
        do while(i<process_size)
           i = i * 2
           power = power + 1
        enddo
        allocate(merge_map_send(process_size, power), merge_map_recv(process_size, power))
        merge_steps = power
        do i = 1,process_size
           free_processes(i) = i-1
        enddo
        do i=2,process_size,2
           merge_map_send(i-1,1) = -1
           merge_map_send(i,1) = i-2 
           free_processes(i) = -1
        enddo
        merge_map_send(1,:) = -1
        if(merge_steps>1) then
           do i = 2,merge_steps
              j = 1
              do while(j < process_size)
                 do k = 1,2**i
                    if(j == 1) then
                       merge_map_send(j,i) = -1
                    else if(k==1) then
                       merge_map_send(j,i) = -1
                    else if((merge_map_send(j,i-1)>=0).OR.(merge_map_send(j,i-1)==-2)) then
                       merge_map_send(j,i) = -2
                    else if(merge_map_send(j,i-1)==-1) then
                       merge_map_send(j,i) = j - k
                    endif
                    j = j+1
                 enddo
              enddo
           enddo
        endif
        do i = 1,merge_steps
           do j = process_size,1,-1
              if(merge_map_send(j,i)>=0) then 
                 k = merge_map_send(j,i) + 1
                 merge_map_recv(k,i) = j-1
                 merge_map_recv(j,i) = -1
              else if(merge_map_send(j,i) == -2) then
                 merge_map_recv(j,i) = -2
              endif
           enddo
        enddo
    end subroutine create_simple_merge_map

    !Schedule preset functions for mpi_map array.
    !arr_start, arr_end and arr_length have consequential static scheduling.
    !These functions generate various load presets for mpi_map.
    !
    !   
    !Default schedule mapped to array.
    subroutine schedule_preset_static(n)
        implicit none
        integer, intent(in) :: n
        integer :: i
        if(allocated(mpi_map)) deallocate(mpi_map)
        allocate(mpi_map(n))
        do i = 1,process_size
            mpi_map(all_arr_start(i):all_arr_end(i)) = i-1
        enddo
    end subroutine schedule_preset_static

    !Preset following this pattern:
    !0, 1, 2, 3, ... , last_process, 0, 1, 2, 3, ...
    subroutine schedule_preset_roundrobin(n)
        implicit none
        integer, intent(in) :: n
        integer :: i
        if(allocated(mpi_map)) deallocate(mpi_map)
        allocate(mpi_map(n))
        do i = 1,n
            mpi_map(i) = mod(i-1,process_size)
        enddo
    end subroutine schedule_preset_roundrobin

    subroutine schedule_preset_roundrobin_limit(n)
        implicit none
        integer, intent(in) :: n
        integer :: i, j, k
        integer, dimension(:), allocatable :: laal
        allocate(laal(size(all_arr_len)))
        laal = all_arr_len
        if(allocated(mpi_map)) deallocate(mpi_map)
        allocate(mpi_map(n))
        do i = 1,n
            j = mod(i-1,process_size)
            if(laal(j+1)>0) then
                mpi_map(i) = j
                laal(j+1) = laal(j+1) - 1
            else
                k=2
                do while(laal(j+k)<=0)
                    k=k+1
                enddo
                mpi_map(i) = j+k-1
                laal(j+k) = laal(j+k)-1
            endif 
        enddo
        deallocate(laal)
    end subroutine schedule_preset_roundrobin_limit

    function get_aal() result(aal)
        implicit none
        real, allocatable :: aal(:)
        allocate(aal(0:process_size))
    end function get_aal

    subroutine no_plan
        implicit none
        call mpi_setup
        call planner_no_array
        state_mpi = 1
    end subroutine no_plan

    !Master not working
    subroutine full_plan_def(i_n)
        implicit none
        integer, intent(in) :: i_n
        call full_plan_main(i_n, .FALSE.)
    end subroutine full_plan_def

    !Size for which to do calculations and if the master node should work.
    !Most algorithms assume that master work or at least coordinate
    subroutine full_plan_main(i_n, worker_override)
        implicit none
        integer, intent(in) :: i_n
        logical, intent(in) :: worker_override
        call mpi_setup
        call planner(i_n, worker_override)
    end subroutine full_plan_main

    subroutine planner(i_n, worker_override)
        implicit none      
        integer, intent(in) :: i_n
        logical, intent(in) :: worker_override
        state_mpi = 2
        Number = i_n 
        if(process_rank==0) is_master = .TRUE.
        if(process_size==1) then
            allocate(all_arr_len(1))
            is_serial = .TRUE.
            is_worker = .TRUE.
            arr_start = 1
            arr_end = Number
            arr_length = Number
            all_arr_len(1) = Number
        else 
            if((is_run_on_one_node().EQV..TRUE.).OR.worker_override.EQV..TRUE.) then
                call master_slave_setup(0)
                is_worker = .TRUE.
            else
                call master_slave_setup(1)
            endif
        endif 
    end subroutine planner
    
    subroutine planner_no_array
        implicit none    
        if(process_rank==0) is_master = .TRUE.
        if(process_size==1) then
            is_serial = .TRUE.
            is_worker = .TRUE.
        else 
            if(is_run_on_one_node().EQV..TRUE.) then
                if(process_rank/=0) is_worker = .TRUE.
            endif
        endif 
    end subroutine planner_no_array

    function is_run_on_one_node() result(res)
        implicit none
        logical :: res
        integer :: actual_len,l, i
        character(len = MPI_MAX_PROCESSOR_NAME) :: processor_name, other_processor
        res = .TRUE.
        call MPI_Get_processor_name(processor_name, actual_len, ierr)
        if(process_rank == 0) then
            processor_name = trim(processor_name)
            do i=1,process_size-1
                call MPI_RECV(l,1,MPI_INT,i,i,MPI_COMM_WORLD,status,ierr)
                call MPI_RECV(other_processor,l,MPI_CHAR,i,i,MPI_COMM_WORLD,status,ierr)
                other_processor = trim(other_processor)
                if(processor_name/=other_processor) then
                    res = .FALSE.
                endif
            enddo
        else
            call MPI_Send(len(processor_name), 1, MPI_INT, 0, process_rank, MPI_COMM_WORLD, ierr)
            call MPI_Send(processor_name, len(processor_name), MPI_CHAR, 0, process_rank, MPI_COMM_WORLD, ierr)
        endif
    end function is_run_on_one_node

    subroutine master_slave_setup(master_not_worker)
        implicit none
        integer, intent(in) :: master_not_worker
        integer :: i, diff, buf(3)
        arr_length = Number / (process_size - master_not_worker)
        diff = mod(Number, (process_size - master_not_worker))
        allocate(all_arr_len(process_size))
        allocate(all_arr_start(process_size))
        allocate(all_arr_end(process_size))
        if(process_rank == 0) then
            all_arr_start = 1
            all_arr_len = arr_length
            all_arr_end = 1
            if(master_not_worker == 0) then
                all_arr_end(1) = arr_length
            else              
                all_arr_end(1) = 1 - arr_length
            endif
            do i = 2,process_size
                all_arr_start(i) = all_arr_end(i-1) + 1
                all_arr_end(i) = all_arr_start(i) + arr_length - 1
            enddo
            all_arr_end(1) = arr_length
            do i = 1,diff
                all_arr_len(process_size+1-i) = all_arr_len(process_size+1-i)+1
            enddo
            do i = process_size-diff+1,process_size
                all_arr_start(i) = all_arr_end(i-1) + 1
                all_arr_end(i) = all_arr_start(i) + arr_length
            enddo
            arr_start = all_arr_start(1)
            arr_end = all_arr_end(1)
            arr_length = all_arr_len(1)
            do i=2,process_size
                buf(1) = all_arr_start(i)
                buf(2) = all_arr_end(i)
                buf(3) = all_arr_len(i)
                call MPI_Send(buf,3,MPI_INT,i-1,i-1,MPI_COMM_WORLD,ierr)
            enddo
        else
            call MPI_Recv(buf,3,MPI_INT,0,process_rank,MPI_COMM_WORLD,status,ierr)
            arr_start = buf(1)
            arr_end = buf(2)
            arr_length = buf(3)
            is_worker = .TRUE.
        endif
        call MPI_Bcast(all_arr_start, process_size, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(all_arr_end, process_size, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(all_arr_len, process_size, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
        !call mpi_manager_info
    end subroutine master_slave_setup

    subroutine mpi_manager_info
        implicit none
        integer :: i
        print *, process_rank, arr_start, arr_end, arr_length, is_worker, is_master
        if(is_master.EQV..TRUE.) then
            do i=1,process_size
                print *, i-1, all_arr_start(i), all_arr_end(i), all_arr_len(i)
            enddo
        endif
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
    end subroutine mpi_manager_info

    subroutine mpi_launch
        call MPI_INIT(ierr)
    end subroutine mpi_launch

    subroutine mpi_setup
        call MPI_COMM_SIZE(MPI_COMM_WORLD, process_size, ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD, process_rank, ierr)
    end subroutine mpi_setup

    subroutine mpi_finish
        call MPI_FINALIZE(ierr)
    end subroutine mpi_finish

    integer function mpi_n_eq(n) result(res)
        implicit none
        integer, intent(in) :: n
        res = n / process_size
        if(res * process_size /= n) then
            res = n / process_size + 1
        endif
        res = res * process_size
    end function mpi_n_eq

    subroutine world_square_array_output(matrix,n)
        use utilities, only : output_qmatrix
        implicit none
        real, intent(in) :: matrix(:,:)
        integer, intent(in) :: n
        integer :: r
        r = 0
        do while(r<process_size)
            if(process_rank == r) then
                print *,"Now process ", process_rank
                call output_qmatrix(matrix,n)
                print*," "
            endif
            r = r + 1
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
        enddo
    end subroutine world_square_array_output

    subroutine master_square_array_output(matrix, n)
        use utilities, only : output_qmatrix
        implicit none
        real, intent(in) :: matrix(:,:)
        integer, intent(in) :: n
        if(is_master.EQV..TRUE.) call output_qmatrix(matrix, n)
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
    end subroutine master_square_array_output

    subroutine world_any_array_output_mpi(array,n,m)
        implicit none
        real, intent(in) :: array(:,:)
        integer, intent(in) :: n,m
        integer :: r, i,j
        r = 0
        do while(r<process_size)
            if(process_rank == r) then
                print *,"Now process ", process_rank
                do i=1,n
                    do j=1,m
                        write(*,"(F10.5)", advance="no") array(i,j)
                    enddo
                    write(*,*) " "
                enddo
                print*," "
            endif
            r = r + 1
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
        enddo
    end subroutine world_any_array_output_mpi
end module mpi_manager