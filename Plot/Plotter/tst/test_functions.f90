function wave(x) result (y)
	use Plotter
	implicit none
	real, intent(in) :: x
	real :: y
	y=sin(x) + sin(x)/3. + sin(x)/5. + sin(x)/7.
end function wave

function first_function(first_input) result (first_output)
	implicit none
	real, intent(in) :: first_input
	real :: first_output
	first_output=sin(first_input)
end function first_function

function second_function(second_input) result (second_output)
	implicit none
	real, intent(in) :: second_input
	real :: second_output
	second_output = cos(second_input)
end function second_function

function third_function(third_input) result(third_output)
	implicit none
	real,intent(in) :: third_input
	real :: third_output
	third_output=third_input
end function third_function

function fourth_function(fourth_input) result(fourth_output)
	implicit none
	real,intent(in) :: fourth_input
	real :: fourth_output
	fourth_output = fourth_input**2
end function fourth_function

function afourth_function(fourth_input) result(fourth_output)
	implicit none
	real,intent(in) :: fourth_input
	real :: fourth_output
	fourth_output = -fourth_input**2
end function afourth_function

function fifth_function(fifth_input) result (fifth_output)
	implicit none
	real, intent(in) :: fifth_input
	real :: fifth_output
	fifth_output=1/fifth_input
end function fifth_function

function first_surface(a,b) result(c)
	implicit none
	real,intent(in) :: a,b
	real :: c
	c = a*a+b*b
end function first_surface

function second_surface(a,b) result(c)
	implicit none
	real,intent(in) :: a,b
	real :: c
	c = (a*a-b*b)/4
end function second_surface