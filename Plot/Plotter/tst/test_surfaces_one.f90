!Функция поверхности
function sedlo(a,b) result(c)
	implicit none
	real,intent(in) :: a,b
	real :: c
	c = (a*a-b*b)/4
end function sedlo

program main
!       Используем графическую библиотеку
	USE Plotter
	implicit none
!	Тип, хранящий указатель
	type(functions):: one_surface
!       Указываем что мы используем функцию с выводом типа real
	real, external :: sedlo
!	Мы собираемся отправить функцию поверхности
	one_surface%function_surface=>sedlo
!       Шаг по осям
		call Set_Step_X(1.)
		call Set_Step_Y(1.)
!       Передаём в функцию Размер окна, ограничения по осям Х и У и указатели с их количеством
	call DrawGraph(960,720,-5.,5.,-5.,5.,one_surface)
end
