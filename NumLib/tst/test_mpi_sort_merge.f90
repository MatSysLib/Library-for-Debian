program test_mpi_sort_merge
    use mpi_manager
    use sort_merge, only : merge_sort_mpi
    implicit none
    integer :: i
    integer, parameter :: n = 16
    real, dimension(n) :: A, res
    call mpi_launch
    call mpi_setup
    a = (/(i,i=n,1,-1)/)
    res = merge_sort_mpi(a,n)
    if(is_master) then
        print *,"Before:"
        print *,a
        print *,"After:"
        print *,res
    endif
    call mpi_finish
end program test_mpi_sort_merge