include 'test_functions.f90'
program main
	USE Plotter
	implicit none
	real, external :: fourth_function, first_function, afourth_function, third_function
	!Куб ограничений
	real,dimension(:,:,:),allocatable :: restr
	!Обёртка над указателем - тогда их можно в массиве расположить
	type(functions),dimension(:),allocatable::test_function
	!Две функции
	allocate(test_function(2))
	!Указатели на функцию для каждой из них
	test_function(1)%function_y=>fourth_function
	test_function(1)%function_z=>first_function
	test_function(2)%function_y=>afourth_function
	test_function(2)%function_z=>third_function
	!Пара точек, в которых три кооржинаты, 2 ограничения и 2набора
	allocate(restr(6,2,2))
	!X
	restr(1,1,1) = -1
	restr(2,1,1) = 1
	!Y
	!Одинаковые - не учитываются
	restr(3,1,1) = 0
	restr(4,1,1) = 0
	restr(5,1,1) = 0
	restr(6,1,1) = 0
	!Следующий набор
	restr(3,1,2) = -3
	restr(4,1,2) = -2
	call Set_Step_X(0.01)
	!Если не вылетает - значит заполнять до конца не надо
	!Передаём размер окна, интервал, функции, число функций,массив ограничения и число ограничений
	call DrawGraph(960,720,-5.,5.,test_function,2,restr,1)
end
