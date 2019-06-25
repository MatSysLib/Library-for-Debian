program main
	USE Plotter
	implicit none
	integer :: i
	real :: x,y,phi,r,PI=DACOS(-1.D0)
	real,dimension(:,:),allocatable :: matrix
	phi = 0
	x = 0
	y = 0
	r = 0
	allocate(matrix(3,8400))
	do i=1,8400
		phi = phi+PI/4096.
		r=(1+sin(phi))*(1-0.9*abs(sin(4*phi)))*(0.9+0.05*cos(phi))
		matrix(1,i) = cos(phi)*r*5
		matrix(2,i) = sin(phi)*r*5
		matrix(3,i) = 0.
	end do
	!Передаём размеры окна, массив точек и их количество
	call DrawGraph(960,720,matrix,8400)
end
