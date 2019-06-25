real function f(x,y) result(res)
    implicit none
    real, intent(in) :: x, y
    res = 1 - x + y**2
end function f

program test_cells
    use cells_lib, only : cells_mpi
    use mpi_manager
    implicit none
    integer :: nx, ny
    real :: ax, bx, ay, by, eps, res
    real, external :: f
    call mpi_launch
    call mpi_setup
    nx = 10
    ny = 10
    ax = -1
    bx = 1
    ay = -1
    by = 1
    eps = 0.001
    res = cells_mpi(nx, ny, ax, bx, ay, by, eps, f)
    if(is_master) print *,res
    call mpi_finish
end program test_cells