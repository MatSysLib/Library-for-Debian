include 'test_functions.f90'
program main
	USE Plotter
	implicit none
!	Массив, в котором хранятся указатели.
!	Тип нужен потому что Фортран не умеет делать массивы указателей,
!	А вот указатели на массивы - умеет
	type(functions),dimension(:),allocatable::multiple_graphic
	real, external :: first_surface, second_surface
!	Мы собираемся отправить 2 функции поверхности
	allocate(multiple_graphic(2))
	multiple_graphic(1)%function_surface=>first_surface
	multiple_graphic(2)%function_surface=>second_surface
!       Шаг по осям
        call Set_Step_X(1.)
        call Set_Step_Y(1.)
!       Передаём в функцию Размер окна, ограничения по осям Х и У и указатели с их количеством
	call DrawGraph(960,720,-5.,5.,-5.,5.,multiple_graphic,2)
end
