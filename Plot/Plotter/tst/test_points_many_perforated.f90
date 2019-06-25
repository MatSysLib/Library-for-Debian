program main
	USE Plotter
	implicit none
	integer :: i
	real :: x,y,phi,r,PI=DACOS(-1.D0)
	real,dimension(:,:,:),allocatable :: matrix
	logical,dimension(:,:),allocatable :: perf
	phi = 0
	x = 0
	y = 0
	r = 0
	allocate(matrix(3,8400,2))
	allocate(perf(8400,2))
	do i=1,8400
		phi = phi+PI/4096.
		r=(1+sin(phi))*(1-0.9*abs(sin(4*phi)))*(0.9+0.05*cos(phi))
		matrix(1,i,1) = cos(phi)*r*5
		matrix(2,i,1) = sin(phi)*r*5
		matrix(3,i,1) = 0.
		matrix(1,i,2) = cos(phi)*r*5
		matrix(2,i,2) = -sin(phi)*r*5
		matrix(3,i,2) = 0.
		perf(i,1) = .FALSE.
		perf(i,2) = .FALSE.
		if(i>1000 .AND. i<2000) then
			perf(i,1) = .TRUE.
		end if
		if(i>4000 .AND. i<5000) then
			perf(i,2) = .TRUE.
		end if
	end do
	!Передаём размеры окна, массив точек и их количество
	call DrawGraph(960,720,matrix,8400,2,perf)
end
