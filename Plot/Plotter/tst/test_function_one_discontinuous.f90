include 'test_functions.f90'
program main
	USE Plotter
	implicit none
	real, external :: second_function, fifth_function
	!Массив интервалов исключения
	real,dimension(:,:),allocatable :: restr
	!Функция, которую отрисовываем
	type(functions)::test_function
	!Пара точек с тремя координатами, 2 интервала
	allocate(restr(6,2))
	!Х
	restr(1,1) = 0
	restr(2,1) = 0
	!У
	!Одинаковые - не учитываются
	restr(3,1) = 1
	restr(4,1) = 3
	!Z
	restr(5,1) = 0
	restr(6,1) = 0
	!Цепляем указатели на функции для y и z
	test_function%function_y=>fifth_function
	test_function%function_z=>second_function
	call Set_Step_X(0.01)
	!Передаём размеры окна, интервал вычисления и отображения, функцию, интервалы исключений, число интервалов 
	call DrawGraph(960,720,-5.,5.,test_function,restr,1)
end
