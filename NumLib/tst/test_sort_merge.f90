program test_sort_merge
    use sort_merge, only : merge_sort, merge_sort_omp
    implicit none
    integer :: i
    integer, parameter :: n = 32
    real, dimension(n) :: A
    a = (/(i,i=n,1,-1)/)
    print *,"Before:"
    print *, a
    print *,"After:"
    a = merge_sort(a,n)
    print *, a
    a = (/(i,i=n,1,-1)/)
    print *,"Before:"
    print *, a
    print *,"After:"
    a = merge_sort_omp(a,n)
    print *, a
end program test_sort_merge