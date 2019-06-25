program main
	USE Plotter
	implicit none
	!Массив точек исключения
	logical,dimension(:),allocatable :: perf
	!Матрица точек графика
	real,dimension(:,:),allocatable :: matrix
	integer :: i
	real :: phi,r,PI=DACOS(-1.D0)
	phi = 0
	r = 0
	!3 оси - X,Y,Z и 7 точек
	allocate(matrix(3,8400))
	allocate (perf(8400))
	do i=1,8400
		phi = phi+PI/4096.
		r=(1+sin(phi))*(1-0.9*abs(sin(4*phi)))*(0.9+0.05*cos(phi))
		matrix(1,i) = cos(phi)*r*5
		matrix(2,i) = sin(phi)*r*5
		matrix(3,i) = 0.
		if(i>2000 .AND. i<4000) then
			perf(i) = .TRUE.
		else 
			perf(i) = .FALSE.
		end if
	end do
	!Передаём размеры окна, массив точек, число точек, массив исключения
	call DrawGraph(960,720,matrix,8400,perf)
end
