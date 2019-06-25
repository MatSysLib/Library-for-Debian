    function function_name(function_input) result (function_output)
    real, intent(in) :: function_input
    real :: function_output
    function_output=1/function_input
    end function function_name

    program main
    USE Plotter
    type(functions):: dfunction
    real,external :: function_name
    dfunction%function_y=>function_name
    call Set_Step_X(0.01)
    call Set_Axis_Text_Size(0.01)
    call DrawGraph(960,720,-10.,10.,dfunction)
    write( *, * ) 'If you can see this before you close graphic window - this is asynchronous' 
    write( *, * ) 'Press Enter to continue'
    read( *, * ) 
    end
