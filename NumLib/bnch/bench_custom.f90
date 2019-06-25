program test_custom
   implicit none
   integer, parameter :: process_size = 8
   integer :: i, j, k, l, power, t_size, merge_steps
   integer, allocatable :: free_processes(:), temp(:), merge_map_send(:,:), merge_map_recv(:,:)
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
               print *,j
               j = j+1
            enddo
         enddo
      enddo
   endif
   do i = 1,merge_steps
      do j = process_size,1,-1
         if(merge_map_send(j,i)>=0) then 
            l = merge_map_send(j,i) + 1
            merge_map_recv(l,i) = j-1
            merge_map_recv(j,i) = -1
         else if(merge_map_send(j,i) == -2) then
            merge_map_recv(j,i) = -2
         endif
      enddo
   enddo
   print *,merge_steps
   print *,"Send"
   do i = 1,merge_steps
      print *,merge_map_send(:,i)
   enddo
   print *,"Recv"
   do i = 1,merge_steps
      print *,merge_map_recv(:,i)
   enddo
end program test_custom