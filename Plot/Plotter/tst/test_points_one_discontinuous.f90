program main
	USE Plotter
	implicit none
	!Для массива
	integer :: i
	!Массив интервалов исключения
	real,dimension(:,:),allocatable :: restr
	real :: phi,r,PI=DACOS(-1.D0)
	!Массив точек
	real,dimension(:,:),allocatable :: matrix
	phi = 0
	r = 0
	!Три оси - X,Y,Z и 11 точек
	allocate(matrix(3,8400))
	do i=1,8400
		phi = phi+PI/4096.
		r=(1+sin(phi))*(1-0.9*abs(sin(4*phi)))*(0.9+0.05*cos(phi))
		matrix(1,i) = cos(phi)*r*5
		matrix(2,i) = sin(phi)*r*5
		matrix(3,i) = 0.
	end do
	!3 пары координат и два ограничения
	allocate(restr(6,2))
	!По Х
	restr(1,1) = -0.5
	restr(2,1) = 0.5
	!По У - Одинаковые - игнорируются
	restr(3,1) = 0
	restr(4,1) = 0
	!По Z
	restr(5,1) = 0
	restr(6,1) = 0
	restr(1,2) = 0
	restr(2,2) = 0
	restr(3,2) = 0.75
	restr(4,2) = 1.5
	restr(5,2) = 0
	restr(6,2) = 0
	!Передаём размеры окна, массив точек, число точек, массив интервалов исключения, размер массива
	call DrawGraph(960,720,matrix,8400,restr,2)
end
