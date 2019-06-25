    module Plotter
    USE opengl_gl
    USE opengl_glu
    USE opengl_glut
    implicit none
    public
    !Список допустимых функций для указателей
    !Достаточно, чтобы функция имела один из видов:
    interface
    !а) принимает и возвращает одно значение real - график y = f(x)
    function graphic(x) result (y)
    real,intent(in):: x
    real :: y
    end function graphic
    !б) принимает два значения, возвращает одно - график z=f(x,y)
    function surface(x,y) result(z)
    real,intent(in) :: x,y
    real :: z
    end function surface
    end interface
    !Здесь хранятся указатели на функции.
    type functions
        !Сделать массив указателей нельзя. Придётся делать массив типов, хранящих указатели
        procedure (graphic), pointer, nopass :: function_y => null()
        !Можно указать, что мы рисуем график в 3Д
        procedure (graphic), pointer, nopass :: function_z => null()
        !Поверхность
        procedure (surface), pointer, nopass :: function_surface => null()
    end type functions
    !Хранение координат
    type point
        real :: coordinate_x
        real :: coordinate_y
        real :: coordinate_z
        !Необходимо ли скрыть точку?
        logical :: hide = .FALSE.
    end type point
    !Хранение функций
    type(functions),dimension(:),allocatable :: f_to_d

    !Хранение данных о поверхности
    type(point), dimension(:,:,:), allocatable :: surface_tesseract
    !Хранение данных о графиках
    real, dimension(:,:,:), allocatable :: points_cube
    !Хранение данных о интервалах исключения
    real, dimension(:,:,:),allocatable :: exclusion_intervals
    !Хранение данных о типе точки
    !False - отображаемая
    !True - скрытая
    logical,dimension(:,:), allocatable :: point_status
    !Для ускорения
    
    
    interface DrawGraph
    module procedure drawgraph_function_one_basic
    module procedure drawgraph_function_one_discontinuous
    module procedure drawgraph_function_many_basic
    module procedure drawgraph_function_many_discontinuous
    module procedure drawgraph_points_one_basic
    module procedure drawgraph_points_one_perforated
    module procedure drawgraph_points_one_discontinuous
    module procedure drawgraph_points_many_basic
    module procedure drawgraph_points_many_perforated
    module procedure drawgraph_points_many_discontinuous
    module procedure drawgraph_surface_one_basic
    module procedure drawgraph_surface_many_basic
    end interface DrawGraph

    !Параметры, на которые пользователь не может влиять напрямую.
    !Размеры по умолчанию
    !Текст
    real :: def_text_size_big = 0.005,def_text_size_normal = 0.003, def_text_size_small = 0.0015
    !Точка
    real :: def_point_size_big = 2.,def_point_size_normal = 1.,def_point_size_small = 0.5
    !Толщина линии
    real :: def_line_thick = 2.0,def_line_normal = 1.0, def_line_thin = 0.5
    !Цвета по умолчанию
    !Компоненты RGB осей и текста
    real :: color_axis(3), color_text(3)
    !Временное хранение компонент RGB
    real :: color_array(3)
    !Список цветов по умолчанию. Просто Look-up таблицы
    integer,dimension(:), allocatable :: color_table
    real,dimension(:),allocatable :: surface_colormap
    !Параметры изображения
    !Отношение сторон
    real :: aspect
    !Пределы изображения по осям
    integer :: add_x_canvas = 0,add_y_canvas = 0,add_z_canvas = 0
    !Ширина, высота, глубина
    real :: diapason_width,diapason_height,diapason_depth
    !Начало и конец диапазонов отображения
    real :: diapason_x_start,diapason_x_end,diapason_y_start,diapason_y_end,diapason_z_start,diapason_z_end
    !Настоящие пределы изображения
    real :: tx_start,tx_end,ty_start,ty_end,tz_start,tz_end
    !Здесь хранятся информация о осях для каждой поверхности - для корректной закраски
    real,dimension(:,:),allocatable :: surface_start_end

    !Вычислемые параметры, которые вычисляются исходя из данных пользователя
    !Настройки вычисления - количество точек
    integer :: point_number_x,point_number_y
    !Рисуем - поверхность?
    logical :: is_surface = .FALSE.

    !Параметры, которые даёт пользователь.
    !Экран
    !Ширина и высота - передаются при инициализации
    integer :: window_width,window_height
    !Настройки вычисления
    !Число наборов объектов, интервалов - передаются при инициализации в разных видах
    integer :: number_of_sets, number_of_intervals=0
    !Шаг по осям
    real :: step_x = 1,step_y=1
    !Размер линий
    !Ось
    real :: axis_line_size = 2.0
    !Засечки
    real :: axis_mark_line_size = 1.0
    !Сетка
    real :: net_line_size = 0.5
    !График
    real :: graphic_line_size = 3.0
    !Сеть поверхности
    real :: surface_line_size = 5.0
    !Размер точек
    !Осевые
    real :: axis_point_size = 5.
    !График
    real :: graphic_point_size = 0.5
    !Поверхность
    real :: surface_point_size = 10.
    !Толщина поверхности
    real :: surface_triangle_volume = 0.5
    !Толщина линий текста
    real :: text_size = 2
    !Надписи
    !TODO Посмотреть почему не принимаются статические строки - связано с объявлением?
    character(:),allocatable :: axis_ox_text,axis_oy_text,axis_oz_text
    !Название скриншотов
    !TODO Add changename function
    character(:),allocatable :: screenshot_file_name
    !Название окна
    !TODO
    character(:),allocatable :: glut_window_name
    !Размер надписей
    real :: axis_text_size=0.002, marks_text_size=0.002
    !По умолчанию
    real :: def_axis_text_size = 0.001, def_marks_text_size=0.001!0.01 - One cell on default
    !Относительное позиционирование подписей осей
    real :: x_x_position,y_y_position
    !Размер точек текста
    real :: text_point_size = 0.05
    !Надписи легенды.
    character(:),dimension(:),allocatable :: object_name
    !С какой позиции идёт легенда - от -1 до 1
    real :: zero_x = 0.66
    !Параметры, которые пользователь может менять во время выполнения программы
    !Управление камерой
    !Смещение по осям
    integer :: x_shift = 0, y_shift = 0, z_shift = 0
    !Приближение/Удаление от точки
    real :: factor = 1,new_factor = 1
    !Вращение
    real :: x_degree = 0, y_degree = 0, z_degree = 0
    !Тип поверхности
    !1 - точки
    !2 - линии и точки
    !3 - линии
    !4 - поверхность
    integer :: surface_mode = 1
    !Тип графика линий
    !1 - точечный
    !2 - линии
    !3 - линии с точками
    integer :: line_type = 2
    !Тип окрашивания поверхности
    logical :: color_range = .TRUE.
    !Параметры изображения
    !Только оси
    logical :: simplify3d = .FALSE.
    !Строить засечки для оси Z?
    logical :: build_z_marks = .FALSE.
    !Легенду надо?
    logical :: legend_need = .FALSE.
    !Отображать сетку
    logical :: net_need = .TRUE.
    !Изображать все наборы
    logical :: all_sets = .FALSE.
    !Текущий отображаемый набор
    integer :: current_set = 1
    !Сглаживание
    logical :: antialiasing = .FALSE.
    !Отображать подписи осей
    logical :: axis_text_need = .TRUE.
    !TODO Просто умножение не сработает
    !Отношение размера подписей к размеру отрисовываемой области
    real :: q_size_ratio = 1
    !TODO Lower\/
    !Фиксированное количество отметок и линий сетки
    logical :: limit_lining = .FALSE.
    !Количество отметок при ограничении
    integer :: limit_lining_number = 10
    !Вынос отметок на границу экрана
    logical :: border_as_axis = .FALSE.
    
    !Перегрузка
    interface line_settings
    module procedure line_settings_explicit_color
    module procedure line_settings_implicit_color
    end interface line_settings
    
    save
    contains

    subroutine CheckOpenGLError
    interface ! strlen is a standard C function from <string.h>
      ! int strlen(char *string)
        function strlen(string) result(length) bind(C,name='strlen')
   	        use iso_c_binding
   	        type(c_ptr), value :: string ! a C pointer
            integer :: length
        end function
    end interface  
    integer :: error, length
    type(C_PTR) :: c_pointer
    character(len = 64,kind=C_CHAR), pointer :: f_pointer
    character(len = 64) :: errorstring      
    error = glGetError() !1280
    if (error /= 0) then
        c_pointer = gluErrorString(error)
        length = strlen(c_pointer)
        call c_f_pointer(c_pointer, f_pointer)
        errorstring = f_pointer(1:length)
        write(*,*) error,errorstring
    end if
    end subroutine CheckOpenGLError
    
    !Общая инициализация, передаются размеры окна
    subroutine initialization_general(i_window_width,i_window_height)
    implicit none
    integer, intent(in) :: i_window_width,i_window_height
    integer :: i
    window_width = i_window_width
    window_height = i_window_height
    call color_select('Maroon')
    !Цвет текста
    color_text(1) = color_array(1)
    color_text(2) = color_array(2)
    color_text(3) = color_array(3)
    !Цвет осей
    call color_select('Black')
    color_axis(1) = color_array(1)
    color_axis(2) = color_array(2)
    color_axis(3) = color_array(3)
    !Таблица цвета графиков по умолчанию
    allocate(color_table(10))
    color_table(1) = 5 !Red
    color_table(2) = 6 !Lime
    color_table(3) = 15!Navy
    color_table(4) = 16!Magenta
    color_table(5) = 11!Cyan
    color_table(6) = 7!Green
    color_table(7) = 0!Gold
    color_table(8) = 1!Yellow
    color_table(9) = 10!Deep Sky Blue
    color_table(10) = 12!Blue
    if(.NOT. allocated(glut_window_name)) then
        glut_window_name = trim("Fortran Plotter")
    end if
    if(.NOT. allocated(axis_ox_text)) then
        axis_ox_text = trim('ox')
    end if
    if(.NOT. allocated(axis_oy_text)) then
        axis_oy_text = trim('oy')
    end if
    if(.NOT. allocated(axis_oz_text)) then
        axis_oz_text = trim('oz')
    end if
    if(.NOT. allocated(screenshot_file_name)) then
        screenshot_file_name = trim('screenshot')
    end if
    if(.NOT. allocated(object_name)) then
        allocate(character(10) :: object_name(25))
        do i=1,25
            write(object_name(i),'(A,I2)') 'Object N',i
        end do
    end if
    end subroutine initialization_general

    !Инициализация для функций
    subroutine initialization_function(i_diapason_x_start,i_diapason_x_end,i_functions,i_functions_number)
    implicit none
    integer, intent(in) :: i_functions_number
    real, intent(in) :: i_diapason_x_start,i_diapason_x_end
    type(functions),dimension(:),allocatable,intent(in) :: i_functions

    point_number_x = (i_diapason_x_end-i_diapason_x_start)/step_x
    number_of_sets = i_functions_number
    allocate(f_to_d(i_functions_number))
    f_to_d = i_functions

    diapason_x_start = i_diapason_x_start
    diapason_x_end = i_diapason_x_end
    tx_start = i_diapason_x_start
    tx_end = i_diapason_x_end
    call compute_function

    call set_f_bounds
    end subroutine initialization_function

    !Инициализация для поверхностей
    subroutine initialization_surface(i_diapason_x_start,i_diapason_x_end,i_diapason_y_start,i_diapason_y_end,&
        i_surface_functions,i_surface_number)
    implicit none
    integer,intent(in) :: i_surface_number
    real,intent(in) :: i_diapason_x_start,i_diapason_x_end,i_diapason_y_start,i_diapason_y_end
    type(functions),dimension(:),allocatable,intent(in) :: i_surface_functions

    point_number_x = (i_diapason_x_end-i_diapason_x_start) / step_x
    point_number_y = (i_diapason_y_end-i_diapason_y_start) / step_y
    number_of_sets = i_surface_number
    allocate(f_to_d(i_surface_number))
    allocate(surface_start_end(i_surface_number,2))
    f_to_d = i_surface_functions

    diapason_x_start = i_diapason_x_start
    diapason_x_end = i_diapason_x_end
    x_x_position = diapason_x_end
    diapason_y_start = i_diapason_y_start
    diapason_y_end = i_diapason_y_end
    y_y_position = diapason_y_end
    tx_start = i_diapason_x_start
    tx_end = i_diapason_x_end
    ty_start = i_diapason_y_start
    ty_end = i_diapason_y_end

    call compute_surface
    end subroutine initialization_surface

    !Инициализация интервала исключения
    subroutine initialization_exclusion(i_exclude_intervals,i_intervals_number)
    implicit none
    integer,intent(in) :: i_intervals_number
    real, dimension(:,:,:), allocatable, intent(in) :: i_exclude_intervals
    number_of_intervals = i_intervals_number
    exclusion_intervals = i_exclude_intervals
    end subroutine initialization_exclusion
    !Расчёт точек поверхности
    subroutine compute_surface
    implicit none
    integer :: i,j,k
    real :: temp,min_v,max_v
    point_number_x = point_number_x + 1
    point_number_y = point_number_y + 1
    allocate(surface_tesseract(point_number_y,point_number_x,number_of_sets))
    do j=1,number_of_sets
        min_v=HUGE(0.0)
        max_v=-1*HUGE(0.0)
        do i=1,point_number_x
            do k=1,point_number_y
                surface_tesseract(k,i,j)%coordinate_x = diapason_x_start+step_x*(k-1)
                surface_tesseract(k,i,j)%coordinate_y = diapason_y_start+step_y*(i-1)
                temp = f_to_d(j)%function_surface(surface_tesseract(k,i,j)%coordinate_x,surface_tesseract(k,i,j)%coordinate_y)
                !Проверка на inf/nan
                if(infinity_check(temp) .OR. nan_check(temp)) then
                    surface_tesseract(k,i,j)%coordinate_z = 0
                    surface_tesseract(k,i,j)%hide = .TRUE.
                else
                    surface_tesseract(k,i,j)%coordinate_z = -1.*temp
                    if(temp>max_v) then
                        max_v = temp
                    end if
                    if(temp<min_v) then
                        min_v = temp
                    end if
                end if
            end do
        end do
        surface_start_end(j,1) = min_v
        surface_start_end(j,2) = max_v
        diapason_z_start = min(min_v,diapason_z_start)
        diapason_z_start = floor(diapason_z_start)
        diapason_z_end = max(max_v,diapason_z_end)
        diapason_z_end = ceiling(diapason_z_end)
    end do
    !write(*,*) surface_start_end
    end subroutine compute_surface

    !Расчёт точек функции
    subroutine compute_function
    implicit none
    integer :: i,j,k
    real :: temp
    logical :: skip
    point_number_x = point_number_x+1
    allocate (points_cube(3,point_number_x,number_of_sets))
    allocate (point_status(point_number_x,number_of_sets))
    do j=1,number_of_sets
        do i=1,point_number_x
            point_status(i,j) = .FALSE.
            !Дальше можно не вычислять
            skip = .FALSE.
            points_cube(1,i,j) = 0
            points_cube(2,i,j) = 0
            points_cube(3,i,j) = 0

            temp = diapason_x_start+step_x*(i-1)
            do k=1,number_of_intervals
                if(bounds_check(exclusion_intervals(1,k,j),exclusion_intervals(2,k,j),temp)) then
                    skip = .TRUE.
                    EXIT
                end if
            end do
            if(infinity_check(temp) .OR. nan_check(temp)) then
                skip = .TRUE.
            end if

            if(skip .EQV..TRUE.) then
                point_status(i,j) = .TRUE.
                CYCLE
            end if

            points_cube(1,i,j) = temp
            temp = f_to_d(j)%function_y(points_cube(1,i,j))
            do k=1,number_of_intervals
                if(bounds_check(exclusion_intervals(3,k,j),exclusion_intervals(4,k,j),temp)) then
                    skip = .TRUE.
                end if
            end do
            if(infinity_check(temp) .OR. nan_check(temp)) then
                skip = .TRUE.
            end if

            if(skip .EQV..TRUE.) then
                point_status(i,j) = .TRUE.
                CYCLE
            end if

            points_cube(2,i,j) = temp
            !Есть такая функция?
            if(associated(f_to_d(j)%function_z).EQV..TRUE.) THEN
                temp = f_to_d(j)%function_z(diapason_x_start+step_x*(i-1))
                do k=1,number_of_intervals
                    if(bounds_check(exclusion_intervals(5,k,j),exclusion_intervals(6,k,j),temp)) then
                        skip = .TRUE.
                    end if
                end do
                if(infinity_check(temp) .OR. nan_check(temp)) then
                    skip = .TRUE.
                end if
                points_cube(3,i,j) = temp
            end if

            if(skip .EQV..TRUE.) then
                point_status(i,j) = .TRUE.
                CYCLE
            end if

            point_status(i,j) = .FALSE.

        end do
    end do
    end subroutine compute_function

    !Проверка: находится ли число между диапазонами.
    !Возвращает TRUE если evalue лежит между left_bound и right_bound
    function bounds_check(left_bound,right_bound,evalue) result(unbounded)
    implicit none
    real,intent(in) :: left_bound,right_bound,evalue
    logical :: unbounded
    if(left_bound == right_bound) then
        unbounded = .FALSE.
    else if(left_bound<evalue .AND. evalue<right_bound) then
        unbounded = .TRUE.
    else
        unbounded = .FALSE.
    end if
    end function bounds_check

    !Проверка на бесконечность
    function infinity_check(evalue) result (infinite)
    implicit none
    real,intent(in) :: evalue
    logical :: infinite
    !Больше даже максимума для типа?
    if (evalue > HUGE(0.0)) then
        infinite = .TRUE.
    else
        infinite = .FALSE.
    end if
    end function infinity_check

    !Проверка nan
    function nan_check(evalue) result (nan)
    implicit none
    real,intent(in) :: evalue
    logical :: nan
    if(evalue/=evalue) then
        nan = .TRUE.
    else
        nan = .FALSE.
    end if
    end function nan_check

    !Инициализация наборов точек
    subroutine initialization_p_many(i_points_xyz,i_point_number_x,i_sets_number)
    implicit none
    integer :: i,j,arr_size
    integer,intent(in) :: i_point_number_x,i_sets_number
    real,dimension(:,:,:),allocatable :: i_points_xyz

    point_number_x = i_point_number_x
    number_of_sets = i_sets_number
    arr_size = size(i_points_xyz,1)
    allocate (points_cube(3,point_number_x,number_of_sets))
    allocate (point_status(point_number_x,number_of_sets))

    do i=1,number_of_sets
        do j=1,point_number_x
            point_status(j,i) = .FALSE.
            points_cube(1,j,i) = i_points_xyz(1,j,i)
            points_cube(2,j,i) = i_points_xyz(2,j,i)
            if(arr_size==3) then
                points_cube(3,j,i) = i_points_xyz(3,j,i)
            else
                points_cube(3,j,i) = 0
            end if
        end do
    end do

    call initialization_bounds
    call set_bounds
    end subroutine initialization_p_many

    !Исключение точек
    subroutine initialization_p_exclude
    implicit none
    integer i,j,k
    do i=1,number_of_sets
        do j=1,point_number_x
            do k=1,number_of_intervals
                !Точка лежит хоть в одном из интервалов?
                if(bounds_check(exclusion_intervals(1,k,i),exclusion_intervals(2,k,i),points_cube(1,j,i)) .OR. &
                    bounds_check(exclusion_intervals(3,k,i),exclusion_intervals(4,k,i),points_cube(2,j,i)) .OR. &
                    bounds_check(exclusion_intervals(5,k,i),exclusion_intervals(6,k,i),points_cube(3,j,i))) then
                point_status(j,i) = .TRUE.
                end if
            end do
        end do
    end do
    end subroutine initialization_p_exclude

    !Задание размеров изображения для точечного графика
    subroutine initialization_bounds
    implicit none
    real :: extremum_by_set(3,number_of_sets),extremum_by_axis(number_of_sets,3),extremum_all(3)

    extremum_by_set = MINVAL(points_cube,2)
    extremum_by_axis = TRANSPOSE(extremum_by_set)
    extremum_all = MINVAL(extremum_by_axis,1)
    diapason_x_start = floor(extremum_all(1))
    diapason_y_start = floor(extremum_all(2))
    diapason_z_start = floor(extremum_all(3))-1

    extremum_by_set = MAXVAL(points_cube,2)
    extremum_by_axis = TRANSPOSE(extremum_by_set)
    extremum_all = MAXVAL(extremum_by_axis,1)
    diapason_x_end = ceiling(extremum_all(1))
    diapason_y_end = ceiling(extremum_all(2))
    diapason_z_end = ceiling(extremum_all(3))+1
    tx_start = diapason_x_start
    tx_end = diapason_x_end
    ty_start = diapason_y_start
    ty_end = diapason_y_end
    tz_start = diapason_z_start
    tz_end = diapason_z_end
    end subroutine initialization_bounds

    !Передача информации о исключённых точках
    subroutine initialization_perfocard(i_point_number_x,i_set_number,i_perfocards)
    implicit none
    integer :: i,j
    integer,intent(in) :: i_point_number_x,i_set_number
    logical,dimension(:,:),allocatable,intent(in) :: i_perfocards
    do j=1,i_set_number
        do i=1,i_point_number_x
            point_status(i,j) = i_perfocards(i,j)
        end do
    end do
    end subroutine initialization_perfocard

    !Задание размеров изображение после вычисления
    subroutine set_bounds
    implicit none
    real:: min_q, max_q
    min_q = min(min(diapason_x_start,diapason_x_end),min(diapason_y_start,diapason_y_end))
    diapason_x_start=min_q
    diapason_y_start=min_q
    max_q = max(max(diapason_x_start,diapason_x_end),max(diapason_y_start,diapason_y_end))
    x_x_position = diapason_x_end
    y_y_position = diapason_y_end
    diapason_x_end=max_q
    diapason_y_end=max_q
    if(y_y_position>diapason_y_end) then
        y_y_position = max_q
    end if
    if(x_x_position>diapason_x_end) then
        x_x_position = max_q
    end if
    end subroutine set_bounds

    !Задание размеров изображение после вычисления функции
    subroutine set_f_bounds
    implicit none
    real:: min_q, max_q
    !For additional info
    real :: extremum_by_set(3,number_of_sets),extremum_by_axis(number_of_sets,3),extremum_all(3)
    min_q = min(diapason_x_start,diapason_x_end)
    diapason_x_start=min_q
    diapason_y_start=min_q
    max_q = max(diapason_x_start,diapason_x_end)
    diapason_x_end=max_q
    diapason_y_end=max_q
    x_x_position = diapason_x_end
    y_y_position = diapason_y_end
    if(y_y_position>diapason_y_end) then
        y_y_position = max_q
    end if
    if(x_x_position>diapason_x_end) then
        x_x_position = max_q
    end if
    
    extremum_by_set = MINVAL(points_cube,2)
    extremum_by_axis = TRANSPOSE(extremum_by_set)
    extremum_all = MINVAL(extremum_by_axis,1)
    tx_start = floor(extremum_all(1))
    ty_start = floor(extremum_all(2))
    tz_start = floor(extremum_all(3))-1

    extremum_by_set = MAXVAL(points_cube,2)
    extremum_by_axis = TRANSPOSE(extremum_by_set)
    extremum_all = MAXVAL(extremum_by_axis,1)
    tx_end = ceiling(extremum_all(1))
    ty_end = ceiling(extremum_all(2))
    tz_end = ceiling(extremum_all(3))+1
    end subroutine set_f_bounds
    
    !Начальные настройки графики
    subroutine gfxinit
    implicit none
    real :: w,h
    w = window_width
    h = window_height
    aspect = w/h
    call factorization
    call glEnable(GL_POINT_SMOOTH)
    call glHint(GL_POINT_SMOOTH_HINT,GL_NICEST)

    call glShadeModel(GL_SMOOTH)

    call glEnable(GL_DEPTH_TEST)
    call glDepthFunc(GL_LEQUAL)
    call glDepthMask(GL_TRUE)
    call glClearColor(1.0, 1.0, 1.0, 0.0)

    call glEnable(GL_BLEND)
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    if(is_surface .EQV..TRUE.) then
        call glEnable(GL_POLYGON_SMOOTH)
        call glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST)
        call glShadeModel(GL_POLYGON_SMOOTH)
    end if
    call background
    end

    !Отрисовка кадра
    subroutine display() bind(c)
    implicit none
    ! Применение вращения, смешения и масштабирования
    call factorization
    ! Фон
    call background 
    !Сеть, оси и засечки.
    !В упрощённом режиме только оси.
    if(simplify3d .EQV. .FALSE.) then
        if(net_need.EQV..TRUE.) then
            call draw_net
        end if
        call draw_axis
        call draw_axis_marks
    else
        call draw_axis
    end if
    !отрисовка
    call draw
    !GUI
    if(legend_need .EQV. .TRUE.) then
        call draw_legend
    end if
    call draw_view_border 

    !вывод
    call glutSwapBuffers
    end

    !Границы вывода
    subroutine draw_view_border         
    call glPushMatrix
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity
    call glViewport(0, 0, window_width, window_height)!0.1-0.9=0.8
    call glOrtho(REAL(0.,8), REAL(window_width,8), real(0.,8), REAL(window_height,8), real(0.,8), real(1.,8))
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity
    call line_settings(2.,'Black')
    call draw_line(window_width*0.075, window_height*0.075,0., window_width*0.075, window_height*0.9,0.)
    call draw_line(window_width*0.075, window_height*0.9,0., window_width*0.9, window_height*0.9,0.)
    call draw_line(window_width*0.9, window_height*0.9,0., window_width*0.9, window_height*0.075,0.)
    call draw_line(window_width*0.9, window_height*0.075,0., window_width*0.075, window_height*0.075,0.)
    call glpopmatrix
    end subroutine draw_view_border
    
    !Вращение, смещение, факторизация
    subroutine factorization
    implicit none
    real :: w,h
    w = window_width
    h = window_height
    aspect = w/h
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity
    !TODO Динамический размер
    call glViewport(INT(window_width*0.075,4), INT(window_height*0.075,4), INT(window_width*0.825,4), INT(window_height*0.825,4))!0.1-0.9=0.8
    if(aspect > 1) then
        call glOrtho(real(diapason_x_start,8)*aspect, real(diapason_x_end,8)*aspect, real(diapason_y_start,8),&
        real(diapason_y_end,8),real(-100,8),real(100,8))!real(diapason_z_start,8),real(diapason_z_end,8))
    else
         call glOrtho(real(diapason_x_start,8), real(diapason_x_end,8), real(diapason_y_start,8)/aspect,&
         real(diapason_y_end,8)/aspect,real(-100,8),real(100,8))
    end if
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity
    call glScalef(1.,1.,-1.)
    call glRotatef(x_degree,1.,0.,0.)
    call glRotatef(y_degree,0.,1.,0.)
    call glRotatef(z_degree,0.,0.,1.)
    call glTranslatef(-x_shift/new_factor,-y_shift/new_factor,-z_shift/new_factor)
    call glScalef(1/new_factor,1/new_factor,1/new_factor)
    end subroutine factorization

    !Фон
    subroutine background
    call glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)
    end subroutine background

    !Легенда
    subroutine draw_legend
    implicit none
    real :: one,zero_y,numbers
    integer :: from_set,to_set,temp,s
    if(all_sets .EQV..FALSE.) then
        from_set = current_set
        to_set = current_set
    else
        from_set = 1
        to_set = number_of_sets
    end if
    call glPushMatrix
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity
    call glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
    call line_settings(5.,'Light Grey')
    one = 1
    numbers = 0.06*(to_set-from_set+1)
    zero_y = one-numbers
    call glBegin(GL_QUADS)		        	!from -1,-1 to 1,1
    call glVertex3f(zero_x,zero_y,0.)!0,0	0,0	-->	1,0
    call glVertex3f(one,zero_y,0.)!1,0	    / \		 |
    call glVertex3f(one,one,0.)!1,1		     |		\ /
    call glVertex3f(zero_x,one,0.)!0,1   	0,1 <--	1,1
    call glEnd()
    do s=from_set,to_set
        temp = color_table(s)
        call color_select(color_map(temp))
        call line_settings(1.,color_array(1),color_array(2),color_array(3))
        call glPushMatrix
        call glMatrixMode(GL_MODELVIEW)
        call glLoadIdentity
        call glBegin(GL_QUADS)		        	!from -1,-1 to 1,1
        call glVertex3f(zero_x+0.01,one-0.015-0.05*(s-from_set),0.)!0,0	0,0	->	1,0
        call glVertex3f(zero_x+0.04,one-0.015-0.05*(s-from_set),0.)!1,0	/ \		 |
        call glVertex3f(zero_x+0.04,one-0.055-0.05*(s-from_set),0.)!1,1		 |		\ /
        call glVertex3f(zero_x+0.01,one-0.055-0.05*(s-from_set),0.)!0,1	0,1 <--	1,1
        call glEnd()
        call glTranslatef(zero_x+0.05,one-0.05*(s-from_set+1),0.)
        call glScalef(0.0003,0.0003,0.0003)
        call line_settings(1.,'Black')
        call draw_string_stroke(object_name(s))
        call glPopMatrix
    end do
    call glPopMatrix
    end subroutine draw_legend

    !Отрисовка сети
    subroutine draw_net
    implicit none
    real :: i, factor_x,factor_y,lining_step = 1
    integer :: start_point,end_point,full_shift
    factor_x=1
    factor_y=1
    if(aspect>0) then
        factor_x = aspect
    else
        factor_y = aspect
    end if
    !Полное смещение в плоскости
    full_shift = abs(x_shift) + abs(y_shift)

    !Горизонтальная линия
    end_point = (diapason_y_end - diapason_y_start+add_y_canvas+full_shift)*2/factor_y*factor_x
    start_point = -end_point

    call line_settings(net_line_size,'Dark Grey')
    if(limit_lining) then
        end_point = ty_end
        start_point = ty_start
        lining_step = (end_point-start_point)/limit_lining_number
    endif
    do i = start_point,end_point,lining_step
        call draw_line(real(start_point),real(i),0.,real(end_point),real(i),0.)
    end do

    !Вертикальные линии
    end_point = (diapason_x_end - diapason_x_start + add_x_canvas+full_shift)*2/factor_y*factor_x
    start_point = -end_point
    if(limit_lining) then 
        end_point = tx_end
        start_point = tx_start
        lining_step = (end_point-start_point)/limit_lining_number
    endif
    do i = start_point,end_point,lining_step
        call draw_line(real(i),real(start_point),0.,real(i),real(end_point),0.)
    end do
    end subroutine draw_net

    !Отрисовка подписей осей
    subroutine draw_axis_text
    implicit none
    integer :: i
    real :: max_len, text_size_koefficient, x_text_w, y_text_w
    x_text_w = 0.
    y_text_w = 0.
    max_len = x_x_position - 1
    text_size_koefficient = 0.125*axis_text_size/def_axis_text_size/3.
    do i = 1,len(axis_ox_text)
        x_text_w = x_text_w + glutStrokeWidth( GLUT_STROKE_ROMAN, ICHAR(axis_ox_text(i:i)))*axis_text_size/3.
    enddo
    do i = 1,len(axis_oy_text)
        y_text_w = y_text_w + glutStrokeWidth( GLUT_STROKE_ROMAN, ICHAR(axis_oy_text(i:i)))*axis_text_size/3.
    enddo
    
    call line_settings(text_size, color_text(1),color_text(2),color_text(3))
    call draw_string(axis_ox_text,x_x_position-x_text_w,-0.75*text_size_koefficient,axis_text_size/3.)
    call draw_string(axis_oy_text,-0.25-y_text_w,y_y_position-0.75*text_size_koefficient,axis_text_size/3.)

    end subroutine draw_axis_text

    !Отрисовка осей
    subroutine draw_axis
    implicit none
    real::full_shift,x_p,y_p,z_p,factor_x,factor_y
    factor_x=1
    factor_y=1
    full_shift = abs(x_shift) + abs(y_shift)

    x_p = (abs(diapason_y_start)+abs(diapason_y_end + 50)+real(add_y_canvas)+full_shift)/factor_y*factor_x
    y_p = (abs(diapason_x_start)+abs(diapason_x_end + 50)+real(add_x_canvas)+full_shift)/factor_y*factor_x
    z_p = abs(diapason_z_start)+abs(diapason_z_end)+real(add_z_canvas)

    call line_settings(axis_line_size, color_axis(1),color_axis(2),color_axis(3))
    call draw_line(0.,-x_p,0.,0.,x_p, 0.)
    call draw_line(-y_p,0.,0.,y_p,0.,0.)
    call draw_line(0.,0.,-5.*z_p,0.,0.,5.*z_p)

    if(axis_text_need .EQV..TRUE.) then
        call draw_axis_text
    end if
    end subroutine draw_axis

    !Отрисовка засечек
    subroutine draw_axis_marks
    implicit none
    character(len =:),allocatable :: minus
    integer :: i, string_size,net_points = 50,start_point,end_point,full_shift
    real :: factor_x, factor_y, lining_step=1
    factor_x=1
    factor_y=1
    if(aspect>0) then
        factor_x = aspect
    else
        factor_y = aspect
    end if
    full_shift = abs(x_shift) + abs(y_shift)
    !Порядок числа - сколько символов надо
    string_size = GetMagnitudeOfInteger(real(net_points))
    allocate(character(len=string_size+1) :: minus)
!TODO Явный цвет
    call line_settings(text_size,color_text(1), color_text(2), color_text(3))
    call draw_string('0', 0.1, 0.1,marks_text_size)

    call color_select('Maroon')
    !end_point = (diapason_y_end - diapason_y_start+add_y_canvas+full_shift)/factor_y*factor_x
    !start_point = -end_point
    start_point = diapason_x_start+x_shift
    end_point = diapason_x_end+x_shift
    if(limit_lining) then
        lining_step = (diapason_y_end - diapason_y_start)/limit_lining_number
    endif
    !OX
    do i = start_point,end_point,lining_step
        call line_settings(axis_mark_line_size, color_axis(1),color_axis(2),color_axis(3))
        call draw_line(real(i),0.2,0.,real(i),-0.2,0.)

        call point_settings(axis_point_size/factor,color_array(1),color_array(2),color_array(3))
        call draw_point(real(i),0.,0.)

        call line_settings(text_size, color_text(1), color_text(2), color_text(3))
        if(i /= 0) then
            write(minus,'(G0)') int(i,8)
            call draw_string(minus,i-0.0,0.1,marks_text_size)
        end if
    end do

    !end_point = (diapason_x_end - diapason_x_start + add_x_canvas+full_shift)/factor_y*factor_x
    !start_point = -end_point
    start_point = diapason_y_start+y_shift
    end_point = diapason_y_end+y_shift
    if(limit_lining) then 
        lining_step = (diapason_x_end - diapason_x_start)/limit_lining_number
    endif
    do i=start_point,end_point,lining_step
        call line_settings(axis_mark_line_size, color_axis(1),color_axis(2),color_axis(3))
        call draw_line(0.2,real(i),0.,-0.2,real(i),0.)

        call point_settings(axis_point_size/factor,color_array(1),color_array(2),color_array(3))
        call draw_point(0.,real(i),0.)

        call line_settings(text_size, color_text(1), color_text(2), color_text(3))
        if(i /= 0) then
            write(minus,'(G0)') int(i,8)
            call draw_string(minus,0.3,real(i),marks_text_size)
        end if
    end do

    if(build_z_marks .EQV..TRUE.) then
        end_point = (diapason_z_end - diapason_z_start + abs(z_shift))
        start_point = -end_point
        do i = start_point,end_point
            call line_settings(axis_mark_line_size, color_axis(1),color_axis(2),color_axis(3))
            call draw_line(-0.2,0.,real(i),0.2,0.,real(i))

            call point_settings(axis_point_size/factor,color_array(1),color_array(2),color_array(3))
            call draw_point(0.,0.,real(i))

            call line_settings(text_size, color_text(1), color_text(2), color_text(3))
            if(i /= 0) then
                write(minus,'(G0)') int(i,8)
                call draw_string_3d(minus,0.2,0.1,real(i),marks_text_size)
            end if
        end do
    end if

    end subroutine draw_axis_marks

    !Отрисовка графика
    subroutine draw
    implicit none
    if(is_surface.EQV..TRUE.) then
        !Точки
        if(surface_mode == 1 .OR. surface_mode == 2 ) then
            call draw_surface(1)
        end if
        !Линии
        if(surface_mode == 2 .OR. surface_mode == 3) then
            call glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
            call draw_surface(2)
        end if
        !Заливка
        if(surface_mode == 4 ) then
            call glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
            call draw_surface(3)
        end if
    else
        call draw_graphic
    end if
    end subroutine draw

    subroutine draw_surface(t)
    implicit none
    integer, intent(in) :: t
    integer :: i,j,s,from_set,to_set,temp
    real :: t_size
    t_size = surface_line_size
    !Выбор наборов для отрисовки
    if(all_sets .EQV..FALSE.) then
        from_set = current_set
        to_set = current_set
    else
        from_set = 1
        to_set = number_of_sets
    end if

    if(t == 1) then
        call glPolygonMode( GL_FRONT_AND_BACK, GL_POINT )
        t_size = surface_point_size
    else if (t == 2) then
        call glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
        t_size = surface_line_size
    else if (t == 3) then
        call glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
        t_size = surface_triangle_volume
    end if
    t_size = t_size / factor
    do s=from_set,to_set
        temp = color_table(s)
        call color_select(color_map(temp))
        if(t==2 .OR. t==3) then
            call line_settings(t_size,color_array(1),color_array(2),color_array(3))
        else
            call point_settings(t_size,color_array(1),color_array(2),color_array(3))
        end if
        do i=2,point_number_x
            do j=2,point_number_y
                !Проверка на скрытость точек
                if(surface_tesseract(j,i,s)%hide.NEQV..TRUE..AND.surface_tesseract(j,i-1,s)%hide.NEQV..TRUE..AND.&
                    surface_tesseract(j-1,i-1,s)%hide.NEQV..TRUE.) then
                call draw_triangle(surface_tesseract(j,i,s),surface_tesseract(j,i-1,s),surface_tesseract(j-1,i-1,s),t_size)
                end if
                if(surface_tesseract(j,i,s)%hide.NEQV..TRUE..AND.surface_tesseract(j-1,i,s)%hide.NEQV..TRUE..AND.&
                    surface_tesseract(j-1,i-1,s)%hide.NEQV..TRUE.) then
                call draw_triangle(surface_tesseract(j,i,s),surface_tesseract(j-1,i,s),surface_tesseract(j-1,i-1,s),t_size)
                end if
            end do
        end do
    end do

    end subroutine draw_surface

    subroutine draw_graphic
    implicit none
    integer :: i,s,from_set,to_set,temp
    if(all_sets .EQV..FALSE.) then
        from_set = current_set
        to_set = current_set
    else
        from_set = 1
        to_set = number_of_sets
    end if
    do s=from_set,to_set
        temp = color_table(s)
        call color_select(color_map(temp))
        do i = 2,point_number_x
            if(point_status(i-1,s).NEQV..TRUE.) then
                if(line_type /= 2) then
                    call point_settings(graphic_point_size/factor,color_array(1),color_array(2),color_array(3))
                    call draw_point(points_cube(1,i-1,s),points_cube(2,i-1,s),-points_cube(3,i-1,s))
                end if
            end if
            if(((point_status(i-1,s).NEQV..TRUE.).EQV..TRUE.).AND.((point_status(i,s) .NEQV..TRUE.).EQV..TRUE.)) then
                if(line_type /=1) then
                    call line_settings(graphic_line_size, color_array(1),color_array(2),color_array(3))
                    call draw_line(points_cube(1,i-1,s),points_cube(2,i-1,s),-points_cube(3,i-1,s),points_cube(1,i,s),&
                        points_cube(2,i,s),-points_cube(3,i,s))
                end if
            end if
        end do
        if(point_status(point_number_x,s).NEQV..TRUE.) then
            if(line_type/=2) then
                call point_settings(graphic_point_size/factor, color_array(1),color_array(2),color_array(3))
                call draw_point(points_cube(1,point_number_x,s),points_cube(2,point_number_x,s),-points_cube(3,point_number_x,s))
            end if
        end if
    end do
    end subroutine draw_graphic

    !Отрисовка треугольника
    subroutine draw_triangle(first_point,second_point,third_point,t_size)
    implicit none
    type(point), intent(in) :: first_point,second_point,third_point
    real, intent(in) :: t_size
    call glBegin(GL_TRIANGLES)
    if(all_sets .EQV..FALSE.) then
        call surface_coloring(surface_start_end(current_set,1),surface_start_end(current_set,2),first_point%coordinate_z)
        call line_settings(t_size,color_array(1),color_array(2),color_array(3))
    end if
    call glVertex3f(first_point%coordinate_x,first_point%coordinate_y,first_point%coordinate_z)

    if(all_sets .EQV..FALSE.) then
        call surface_coloring(surface_start_end(current_set,1),surface_start_end(current_set,2),second_point%coordinate_z)
        call line_settings(t_size,color_array(1),color_array(2),color_array(3))
    end if
    call glVertex3f(second_point%coordinate_x,second_point%coordinate_y,second_point%coordinate_z)

    if(all_sets.EQV..FALSE.) then
        call surface_coloring(surface_start_end(current_set,1),surface_start_end(current_set,2),third_point%coordinate_z)
        call line_settings(t_size,color_array(1),color_array(2),color_array(3))
    end if
    call glVertex3f(third_point%coordinate_x,third_point%coordinate_y,third_point%coordinate_z)
    call glEnd
    end subroutine draw_triangle

    !Параметры линий
    subroutine line_settings_explicit_color(line_width,color_r,color_b,color_g)
    real, intent(in) :: line_width,color_r,color_b,color_g
    call glLineWidth(line_width)
    call glColor4f(color_r,color_b,color_g,1.)
    end subroutine line_settings_explicit_color
    
    subroutine line_settings_implicit_color(line_width,color_name)
    real, intent(in) :: line_width
    character(*), intent(in) :: color_name
    call glLineWidth(line_width)
    call color_select(color_name)
    call glColor4f(color_array(1),color_array(2),color_array(3),1.)
    end subroutine line_settings_implicit_color

    !Параметры точек
    subroutine point_settings(point_size,color_r,color_b,color_g)
    real,intent(in) :: point_size,color_r,color_b,color_g
    call glPointSize(point_size)
    call glColor4f(color_r,color_b,color_g,1.)
    end subroutine point_settings

    !Отрисовка точки
    subroutine draw_point(x_pos,y_pos,z_pos)
    real,intent(in) :: x_pos,y_pos,z_pos
    call glBegin(GL_POINTS)
    call glVertex3f(x_pos,y_pos,z_pos)
    call glEnd
    end subroutine draw_point

    !Отрисовка линии
    subroutine draw_line(x_start,y_start,z_start,x_end,y_end,z_end)
    implicit none
    real, intent(in) :: x_start,y_start,z_start,x_end,y_end,z_end
    call glBegin(GL_LINES)
    call glVertex3f(x_start,y_start,z_start)
    call glVertex3f(x_end,y_end,z_end)
    call glEnd()
    end subroutine draw_line

    !Получение порядка числа
    function GetMagnitudeOfInteger(x) result (y)
    implicit none
    real, intent(in) :: x
    real :: temp
    integer :: y
    y=0
    temp=x
    do while(temp>0)
        temp=temp/10
        y=y+1
    end do
    end function GetMagnitudeOfInteger

    !Отрисовка текста - подготовка
    subroutine draw_text_preparation(coord_x,coord_y,coord_z,size_coeff)
    implicit none
    real, intent(in) :: coord_x,coord_y,coord_z,size_coeff
    real :: w,h
    w = window_width
    h = window_height
    aspect = w/h
    call glPushMatrix
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity
    if(aspect > 1) then
        call glOrtho(real(diapason_x_start,8)*aspect, real(diapason_x_end,8)*aspect, real(diapason_y_start,8),&
        real(diapason_y_end,8),real(-100,8),real(100,8))!real(diapason_z_start,8),real(diapason_z_end,8))
    else
         call glOrtho(real(diapason_x_start,8), real(diapason_x_end,8), real(diapason_y_start,8)/aspect,&
         real(diapason_y_end,8)/aspect,real(-100,8),real(100,8))
    end if
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity
    call glScalef(1.,1.,-1.)
    call glRotatef(x_degree,1.,0.,0.)
    call glRotatef(y_degree,0.,1.,0.)
    call glRotatef(z_degree,0.,0.,1.)
    call glTranslatef((coord_x-x_shift)/new_factor,(coord_y-y_shift)/new_factor,(coord_z-z_shift)/new_factor)
    call glScalef(size_coeff/new_factor,size_coeff/new_factor,size_coeff/new_factor)
    end subroutine draw_text_preparation

    !Пишем текст
    subroutine draw_string_stroke(input)
    implicit none
    character(len = *), intent(in) :: input
    integer :: i
    call glPointSize(text_point_size)
    do i=1,len(input)
        call glutStrokeCharacter(GLUT_STROKE_ROMAN,ICHAR(input(i:i)))
    end do
    end subroutine draw_string_stroke

    !Полный текст
    subroutine draw_string(input,coord_x,coord_y,size_coeff)
    implicit none
    real, intent(in) :: coord_x,coord_y,size_coeff
    character(len = *), intent(in) :: input
    call draw_string_3d(input,coord_x,coord_y,0.0,size_coeff)
    end subroutine draw_string

    subroutine draw_string_3d(input,coord_x,coord_y,coord_z,size_coeff)
    implicit none
    real, intent(in) :: coord_x,coord_y,coord_z,size_coeff
    character(len = *), intent(in) :: input
    call draw_text_preparation(coord_x,coord_y,coord_z,size_coeff)
    call draw_string_stroke(input)
    call glPopMatrix
    end subroutine draw_string_3d

    function file_exists(filename) result(res)
    implicit none
    character(len=*),intent(in) :: filename
    logical :: res
    inquire( file=trim(filename), exist=res )
    end function

    !TODO add overwrite option
    subroutine save_image
    implicit none
    integer :: file_number
    integer(KIND = 1), allocatable, dimension(:), TARGET :: image_data
    integer(KIND = 2), dimension(9) :: TGAHeader
    character(len=3) :: number
    allocate(image_data(window_height*window_width*3))
    image_data = 0
    TGAHeader(1) = 0
    TGAHeader(2) = 2
    TGAHeader(3) = 0
    TGAHeader(4) = 0
    TGAHeader(5) = 0
    TGAHeader(6) = 0
    TGAHeader(7) = window_width
    TGAHeader(8) = window_height
    TGAHeader(9) = 24

    file_number = 1

    call glReadBuffer(GL_FRONT)
    call glReadPixels(0, 0, window_width, window_height, GL_BGR, GL_UNSIGNED_BYTE, C_LOC(image_data))

    write(number,"(I0.3)") file_number
    do while (file_exists(TRIM(TRIM(screenshot_file_name)//TRIM(number)//TRIM('.tga'))).EQV..TRUE.)
        file_number = file_number+1
        write(number,"(I0.3)") file_number
    end do

    open(unit = 1, file = TRIM(TRIM(screenshot_file_name)//TRIM(number)//TRIM('.tga')), FORM = 'UNFORMATTED', ACCESS = 'STREAM')!Recordtype->ACCESS
    write(1) TGAHeader
    write(1) image_data
    close(1)
    end subroutine save_image

    !Вызов отрисовки
    subroutine OpenGL_Draw
    implicit none
    integer :: i
    interface OpenGL_Bindings
    subroutine display() bind(c)
    end subroutine display

    subroutine keyboard(key,x,y) bind(c)
    use opengl_gl
    integer(GLbyte), value :: key
    integer(GLint), value  :: x, y
    end subroutine keyboard

    subroutine OnRedraw(w,h) bind(c)
    use opengl_gl
    use opengl_glu
    use opengl_glut
    integer(GLint), value :: w,h
    end subroutine OnRedraw
    end interface OpenGL_Bindings
    call glutinit
    call glutinitdisplaymode(GLUT_DOUBLE+GLUT_RGBA+GLUT_DEPTH)
    call glutInitWindowSize(window_width, window_height)
    call glutInitWindowPosition(0, 0)
    i = glutcreatewindow(glut_window_name)
    call gfxinit
    call glutDisplayFunc(display)
    call glutKeyboardFunc(keyboard)
    call glutReshapeFunc( OnRedraw )
    call glutmainloop
    end subroutine OpenGL_Draw

    !Изменение размера
    subroutine OnRedraw(w,h) bind(c)
    use opengl_gl
    use opengl_glu
    use opengl_glut
    integer(GLint),  value :: w,h
    window_width  = w
    window_height = h
    !call factorization
    end subroutine OnRedraw

    !Получить цвет по номеру
    function color_map(inumber) result(color)
    implicit none
    integer, parameter :: number_of_colors = 22+1
    integer, intent(in) :: inumber
    integer :: numer
    character(:),allocatable :: color
    !TODO Check looping
    numer = modulo(inumber,number_of_colors)
    select case(numer)
    case(0)
        color = 'Gold'
    case(1)
        color = 'Yellow'
    case(2)
        color = 'Maroon'
    case(3)
        color = 'Crimson'
    case(4)
        color = 'Dark Red'
    case(5)
        color = 'Red'
    case(6)
        color = 'Lime'
    case(7)
        color = 'Green'
    case(8)
        color = 'Spring Green'
    case(9)
        color = 'Dark Green'
    case(10)
        color = 'Deep Sky Blue'
    case(11)
        color = 'Cyan'
    case(12)
        color = 'Blue'
    case(13)
        color = 'Medium Blue'
    case(14)
        color = 'Dark Blue'
    case(15)
        color = 'Navy'
    case(16)
        color = 'Magenta'
    case(17)
        color = 'Black'
    case(18)
        color = 'Silver'
    case(19)
        color = 'Light Grey'
    case(20)
        color = 'Dark Grey'
    case(21)
        color = 'Grey'
    case default
        color = 'Red'
    end select
    end function color_map

    !Получение цвета по названию
    subroutine color_select(color_name)
    implicit none
    character(len=*),intent(in) :: color_name
    color_array(1)=0.
    color_array(2)=0.
    color_array(3)=0.
    select case(color_name)
    case('Gold')
        color_array(1) = 1
        color_array(2) = 0.843
    case('Yellow')
        color_array(1) = 1
        color_array(2) = 1
    case('Maroon')
        color_array(1) = 0.502
    case('Crimson')
        color_array(1) = 0.863
        color_array(2) = 0.078
        color_array(3) = 0.235
    case('Dark Red')
        color_array(1) = 0.545
    case('Red')
        color_array(1) = 1
    case('Lime')
        color_array(2) = 1
    case('Green')
        color_array(2) = 0.502
    case('Spring Green')
        color_array(2) = 1
        color_array(3) = 0.498
    case('Dark Green')
        color_array(2) = 0.392
    case('Deep Sky Blue')
        color_array(2) = 0.749
        color_array(3) = 1
    case('Cyan')
        color_array(2) = 1
        color_array(3) = 1
    case('Blue')
        color_array(3) = 1
    case('Medium Blue')
        color_array(3) = 0.804
    case('Dark Blue')
        color_array(3) = 0.545
    case('Navy')
        color_array(3) = 0.502
    case('Magenta')
        color_array(1) = 1
        color_array(3) = 1
    case('Black')
        color_array(1) = 0
        color_array(2) = 0
        color_array(3) = 0
    case('Silver')
        color_array(1) = 0.753
        color_array(2) = 0.753
        color_array(3) = 0.753
    case('Light Grey')
        color_array(1) = 0.827
        color_array(2) = 0.827
        color_array(3) = 0.827
    case('Dark Grey')
        color_array(1) = 0.663
        color_array(2) = 0.663
        color_array(3) = 0.663
    case('Grey')
        color_array(1) = 0.502
        color_array(2) = 0.502
        color_array(3) = 0.502
        case default
        color_array(1) = 1
    end select
    end subroutine color_select

    !Цвет поверхности
    !Преобразуем точку y в точку между 0 и 1
    !На основе этого вычисляем цвет
    subroutine surface_coloring(axis_start,axis_end,current_axis)
    implicit none
    real,intent(in) :: axis_start,axis_end,current_axis
    real :: length,c_axis
    length = axis_end - axis_start
    c_axis = (abs(current_axis - axis_start)/length)
    if(c_axis<= current_axis .AND. c_axis<0.2) then
        call color_select('Magenta')
        color_array(1) = 1-(c_axis*5)
    else if(c_axis<=0.2 .AND. c_axis<0.4) then
        call color_select('Blue')
        color_array(2) = (c_axis-0.2)*5
    else if(c_axis<=0.4 .AND. c_axis<0.6) then
        call color_select('Cyan')
        color_array(3) = 1-(c_axis-0.4)*5
    else if(c_axis<=0.6 .AND. c_axis<0.8) then
        call color_select('Green')
        color_array(1) = (c_axis-0.6)*5
    else if(c_axis<=0.8 .AND. c_axis<1) then
        call color_select('Yellow')
        color_array(2) = 1-(c_axis-0.8)*5
    else
        call color_select('Red')
    end if
    end subroutine surface_coloring

    !Конвертер отрисовки одной поверхности
    subroutine drawgraph_surface_one_basic(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,&
        i_diapason_y_start,i_diapason_y_end,i_surface_function)
    implicit none
    integer, intent(in) :: i_window_width,i_window_height
    real, intent(in) :: i_diapason_x_start,i_diapason_x_end,i_diapason_y_start,i_diapason_y_end
    type(functions),intent(in) :: i_surface_function
    type(functions),dimension(:),allocatable :: c_surface_function
    allocate(c_surface_function(1))
    !write(*,*) 'drawgraph_surface_one_basic'
    !Конвертируем в массив функций
    c_surface_function(1)%function_surface=>i_surface_function%function_surface
    call drawgraph_surface_many_basic(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,&
        i_diapason_y_start,i_diapason_y_end,c_surface_function,1)
    end subroutine drawgraph_surface_one_basic

    !Инициализация отрисовки многих поверхностей
    subroutine drawgraph_surface_many_basic(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,&
        i_diapason_y_start,i_diapason_y_end,i_surface_functions,i_surface_number)
    implicit none
    integer, intent(in) :: i_window_width,i_window_height,i_surface_number
    real, intent(in) :: i_diapason_x_start,i_diapason_x_end,i_diapason_y_start,i_diapason_y_end
    type(functions),dimension(:),allocatable,intent(in) :: i_surface_functions
    !write(*,*) 'drawgraph_surface_many_basic'
    call initialization_general(i_window_width,i_window_height)
    is_surface = .TRUE.
    call initialization_surface(i_diapason_x_start,i_diapason_x_end,i_diapason_y_start,i_diapason_y_end,&
        i_surface_functions,i_surface_number)
    call OpenGL_Draw
    end subroutine drawgraph_surface_many_basic

    !Конвертер отрисовки одной функции
    subroutine drawgraph_function_one_basic(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,i_function)
    implicit none
    integer, intent(in) :: i_window_width,i_window_height
    real, intent(in) :: i_diapason_x_start,i_diapason_x_end
    type(functions),intent(in) :: i_function
    type(functions),dimension(:),allocatable :: c_function
    allocate(c_function(1))
    !write(*,*) 'drawgraph_function_one_basic'
    c_function(1)%function_y=>i_function%function_y
    c_function(1)%function_z=>i_function%function_z
    call drawgraph_function_many_basic(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,c_function,1)
    end subroutine drawgraph_function_one_basic

    !Инициализация отрисовки многих функций
    subroutine drawgraph_function_many_basic(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,&
        i_functions,i_functions_number)
    implicit none
    integer, intent(in) :: i_window_width,i_window_height,i_functions_number
    real, intent(in) :: i_diapason_x_start,i_diapason_x_end
    type(functions),dimension(:),allocatable,intent(in)::i_functions
    !write(*,*) 'drawgraph_function_many_basic'
    call initialization_general(i_window_width,i_window_height)
    call initialization_function(i_diapason_x_start,i_diapason_x_end,i_functions,i_functions_number)
    call OpenGL_Draw
    end subroutine drawgraph_function_many_basic

    !Конвертер отрисовки одной функии с ограничениями
    subroutine drawgraph_function_one_discontinuous(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,&
        i_function,i_exclude_intervals,i_exclude_pairs_number)
    implicit none
    integer :: i,j
    integer, intent(in) :: i_window_width,i_window_height,i_exclude_pairs_number
    real,intent(in) :: i_diapason_x_start,i_diapason_x_end
    real, dimension(:,:), allocatable, intent(in) :: i_exclude_intervals
    type(functions),intent(in)::i_function
    real, dimension(:,:,:),allocatable :: c_exclude_intervals
    type(functions),dimension(:),allocatable :: c_function
    !write(*,*) 'drawgraph_function_one_discontinuous'
    allocate(c_function(1))
    c_function(1)%function_y=>i_function%function_y
    c_function(1)%function_z=>i_function%function_z
    allocate(c_exclude_intervals(6,i_exclude_pairs_number,1))
    do i=1,i_exclude_pairs_number
        do j=1,6
            c_exclude_intervals(j,i,1)=i_exclude_intervals(j,i)
        end do
    end do
    call drawgraph_function_many_discontinuous(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,c_function,1,&
        c_exclude_intervals,i_exclude_pairs_number)
    end subroutine drawgraph_function_one_discontinuous

    !Инициализация отрисовки многих функций с ограничениями
    subroutine drawgraph_function_many_discontinuous(i_window_width,i_window_height,i_diapason_x_start,i_diapason_x_end,&
    i_functions,i_functions_number,i_exclude_intervals,i_exclude_pairs_number)
    implicit none
    integer, intent(in) :: i_window_width,i_window_height,i_functions_number,i_exclude_pairs_number
    real, intent(in) :: i_diapason_x_start,i_diapason_x_end
    real, dimension(:,:,:), allocatable, intent(in) :: i_exclude_intervals
    type(functions),dimension(:),allocatable,intent(in)::i_functions
    !write(*,*) 'drawgraph_function_many_discontinuous'
    call initialization_general(i_window_width,i_window_height)
    call initialization_exclusion(i_exclude_intervals,i_exclude_pairs_number)
    call initialization_function(i_diapason_x_start,i_diapason_x_end,i_functions,i_functions_number)
    call OpenGL_Draw
    end subroutine drawgraph_function_many_discontinuous

    !Конвертер отрисовки одного точечного графика
    subroutine drawgraph_points_one_basic(i_window_width,i_window_height,i_points_xyz,i_point_number_x)
    implicit none
    integer :: i,arr_size
    integer,intent(in) :: i_window_width,i_window_height,i_point_number_x
    real,dimension(:,:),allocatable,intent(in) :: i_points_xyz
    real,dimension(:,:,:),allocatable :: c_points_xyz
    !write(*,*) 'drawgraph_points_one_basic'
    arr_size = size(i_points_xyz,1)
    allocate(c_points_xyz(arr_size,i_point_number_x,1))
    do i=1,i_point_number_x
        c_points_xyz(1,i,1) = i_points_xyz(1,i)
        c_points_xyz(2,i,1) = i_points_xyz(2,i)
        if(arr_size == 3) then
            c_points_xyz(3,i,1) = i_points_xyz(3,i)
        end if
    end do
    call drawgraph_points_many_basic(i_window_width,i_window_height,c_points_xyz,i_point_number_x,1)
    end subroutine drawgraph_points_one_basic

    !Конвертер отрисовки одного точечного графика с точками исключения
    subroutine drawgraph_points_one_perforated(i_window_width,i_window_height,i_points_xyz,i_point_number_x,i_perfocard)
    implicit none
    integer :: i,arr_size
    integer,intent(in) :: i_window_width,i_window_height,i_point_number_x
    real,dimension(:,:),allocatable,intent(in) :: i_points_xyz
    real,dimension(:,:,:),allocatable :: c_points_xyz
    logical,dimension(:), allocatable,intent(in) :: i_perfocard
    logical,dimension(:,:),allocatable :: c_perfocard
    arr_size = size(i_points_xyz,1)
    allocate(c_points_xyz(arr_size,i_point_number_x,1))
    !write (*,*) 'drawgraph_points_one_perforated'
    allocate(c_perfocard(i_point_number_x,1))
    do i=1,i_point_number_x
        c_points_xyz(1,i,1) = i_points_xyz(1,i)
        c_points_xyz(2,i,1) = i_points_xyz(2,i)
        if(arr_size == 3) then
            c_points_xyz(3,i,1) = i_points_xyz(3,i)
        end if
        c_perfocard(i,1) = i_perfocard(i)
    end do
    call drawgraph_points_many_perforated(i_window_width,i_window_height,c_points_xyz,i_point_number_x,1,c_perfocard)
    end subroutine drawgraph_points_one_perforated

    !Конвертер отрисовки одного точечного графика с интервалами исключения
    subroutine drawgraph_points_one_discontinuous(i_window_width,i_window_height,i_points_xyz,i_point_number_x,&
        i_exclude_intervals,i_exclude_pairs_number)
    implicit none
    integer :: i,j,arr_size
    integer,intent(in) :: i_window_width,i_window_height,i_point_number_x,i_exclude_pairs_number
    real,dimension(:,:),allocatable,intent(in):: i_points_xyz,i_exclude_intervals
    real,dimension(:,:,:),allocatable :: c_points_xyz,c_exclude_intervals
    !write (*,*) 'drawgraph_points_one_discontinuous'
    arr_size = size(i_points_xyz,1)
    allocate(c_points_xyz(arr_size,i_point_number_x,1))
    allocate(c_exclude_intervals(6,i_exclude_pairs_number,1))
    do i=1,i_point_number_x
        c_points_xyz(1,i,1) = i_points_xyz(1,i)
        c_points_xyz(2,i,1) = i_points_xyz(2,i)
        if(arr_size == 3) then
            c_points_xyz(3,i,1) = i_points_xyz(3,i)
        end if
    end do
    do i=1,i_exclude_pairs_number
        do j=1,6
            c_exclude_intervals(j,i,1)=i_exclude_intervals(j,i)
        end do
    end do
    call drawgraph_points_many_discontinuous(i_window_width,i_window_height,c_points_xyz,i_point_number_x,1,&
        c_exclude_intervals,i_exclude_pairs_number)
    end subroutine drawgraph_points_one_discontinuous

    !Инициализация отрисовки нескольких точечных графиков
    subroutine drawgraph_points_many_basic(i_window_width,i_window_height,i_points_xyz,i_point_number_x,i_set_number)
    implicit none
    integer,intent(in) :: i_window_width,i_window_height,i_point_number_x,i_set_number
    real,dimension(:,:,:),allocatable,intent(in):: i_points_xyz
    !write(*,*) 'drawgraph_points_many_basic'
    call initialization_general(i_window_width,i_window_height)
    call initialization_p_many(i_points_xyz,i_point_number_x,i_set_number)
    call OpenGL_Draw
    end subroutine drawgraph_points_many_basic

    !Инициализация отрисовки нескольких точечных графиков с исключёнными точками
    subroutine drawgraph_points_many_perforated(i_window_width,i_window_height,i_points_xyz,&
        i_point_number_x,i_set_number,i_perfocards)
    implicit none
    integer,intent(in) :: i_window_width,i_window_height,i_point_number_x,i_set_number
    real,dimension(:,:,:),allocatable,intent(in):: i_points_xyz
    logical,dimension(:,:),allocatable,intent(in):: i_perfocards
    !write(*,*) 'drawgraph_points_many_perforated'
    call initialization_general(i_window_width,i_window_height)
    call initialization_p_many(i_points_xyz,i_point_number_x,i_set_number)
    call initialization_perfocard(i_point_number_x,i_set_number,i_perfocards)
    call OpenGL_Draw
    end subroutine drawgraph_points_many_perforated

    !Инициализация отрисовки нескольких точечных графиков с интервалами исключения
    subroutine drawgraph_points_many_discontinuous(i_window_width,i_window_height,i_points_xyz,i_point_number_x,i_set_number,&
        i_exclude_intervals,i_exclude_pairs_number)
    implicit none
    integer,intent(in) :: i_window_width,i_window_height,i_point_number_x,i_set_number,i_exclude_pairs_number
    real,dimension(:,:,:),allocatable,intent(in):: i_points_xyz,i_exclude_intervals
    !write(*,*) 'drawgraph_points_many_discontinuous'
    call initialization_general(i_window_width,i_window_height)
    call initialization_p_many(i_points_xyz,i_point_number_x,i_set_number)
    call initialization_exclusion(i_exclude_intervals,i_exclude_pairs_number)
    call initialization_p_exclude
    call OpenGL_Draw
    end subroutine drawgraph_points_many_discontinuous

    !Замыкание круга
    function degree_circling (x) result (y)
    real, intent(in) :: x
    real :: y
    if(x>=360) then
        y=x-360
    else
        y=x
    end if
    end function degree_circling

    !Обработчик клавиатуры.
    subroutine keyboard(key,x,y) bind(c)
    use opengl_gl
    use opengl_glu
    use opengl_glut
    integer(GLbyte),value :: key
    integer(GLint), value  :: x, y
    select case(key)
        !Сглаживание
    case(ichar('u'))
        if(antialiasing .EQV. .FALSE.) then
            call glEnable(GL_LINE_SMOOTH)
            call glHint(GL_LINE_SMOOTH_HINT,GL_NICEST)
        else
            call glDisable(GL_LINE_SMOOTH)
        end if
        antialiasing = .NOT.antialiasing
        !Следующий набор
    case(ichar('o'))
        if(current_set<number_of_sets) then
            current_set = current_set +1
        else
            current_set = 1
        end if
        !Все наборы
    case(ichar('i'))
        all_sets = .NOT.all_sets
        !Переключение режима изображения
    case(ichar('t'))
        if(is_surface.EQV..FALSE.) then
            if(line_type < 3) then
                line_type = line_type+1
            else
                line_type =1
            end if
        else
            if(surface_mode < 4 ) then
                surface_mode = surface_mode + 1
            else
                surface_mode = 1
            end if
        end if
        !Повороты по осям.
    case(ichar('x'))
        x_degree = x_degree + 1
    case(ichar('X'))
        x_degree = x_degree + 359
    case(ichar('y'))
        y_degree = y_degree + 1
    case(ichar('Y'))
        y_degree = y_degree + 359
    case(ichar('z'))
        z_degree = z_degree + 1
    case(ichar('Z'))
        z_degree = z_degree + 359
    case(ichar('-'))!Масштаб
        factor = new_factor
        new_factor = factor + 0.01
        add_x_canvas = add_x_canvas+1
        add_y_canvas = add_y_canvas+1
    case(ichar('+'))
        if(factor>0.05) then
            factor = new_factor
            new_factor = factor - 0.01
        end if
        if(add_x_canvas>0) then
            add_x_canvas = add_x_canvas - 1
        end if
        if(add_y_canvas>0) then
            add_y_canvas = add_y_canvas - 1
        end if
        !Смещение по осям
    case(ichar('w'))
        if(y_shift<ty_end) then
            y_shift=y_shift+1
        endif
    case(ichar('s'))
        if(y_shift>ty_start) then
            y_shift=y_shift-1
        endif
    case(ichar('a'))
        if(x_shift<tx_end) then
            x_shift=x_shift-1
        endif
    case(ichar('d'))
        if(x_shift>tx_start) then
            x_shift=x_shift+1
        endif
    case(ichar('q'))
        if(z_shift<tz_end) then
            z_shift=z_shift+1
        endif
    case(ichar('e'))
        if(z_shift>tz_start) then
            z_shift = z_shift-1
        endif
    case(ichar('l'))
        legend_need = .NOT.legend_need
    case(ichar('n'))
        net_need = .NOT.net_need
    case(ichar('N'))
        simplify3d = .NOT.simplify3d
    case(ichar('A'))
        axis_text_need = .NOT.axis_text_need
    case(ichar('S'))
        call save_image
    end select
    !Обработка замыкания круга
    x_degree = degree_circling(x_degree)
    y_degree = degree_circling(y_degree)
    z_degree = degree_circling(z_degree)
    call display()
    end subroutine keyboard
    !Параметры, которые может изменять пользователь
!TODO Поделить функции на 2 типа - Absolute(Текущий вариант, вводится абсолютная величина) и Relative(Вводится коэффициент, на который умножается величина)
    !Documented
    subroutine Set_Step_X(new_step)
    implicit none
    real,intent(in) :: new_step
    step_x = new_step
    
    !Documented
    end subroutine Set_Step_X
    subroutine Set_Step_Y(new_step)
    implicit none
    real,intent(in) :: new_step
    step_y = new_step
    end subroutine Set_Step_Y
    
    !Documented
    subroutine Set_Axis_Line_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    axis_line_size = new_size
    end subroutine Set_Axis_Line_Size

    !Documented
    subroutine Set_Marks_Line_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    axis_mark_line_size = new_size
    end subroutine Set_Marks_Line_Size

    !Documented
    subroutine Set_Net_Line_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    net_line_size = new_size
    end subroutine Set_Net_Line_Size

    !Documented
    subroutine Set_Graphic_Line_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    graphic_line_size = new_size
    end subroutine Set_Graphic_Line_Size

    !Documented
    subroutine Set_Surface_Line_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    surface_line_size = new_size
    end subroutine Set_Surface_Line_Size

    !Documented
    subroutine Set_Axis_Point_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    axis_point_size = new_size
    end subroutine Set_Axis_Point_Size

    !Documented
    subroutine Set_Graphic_Point_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    graphic_point_size = new_size
    end subroutine Set_Graphic_Point_Size

    !Documented
    subroutine Set_Surface_Point_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    surface_point_size = new_size
    end subroutine Set_Surface_Point_Size

    !Documented
    subroutine Set_Surface_Width(new_size)
    implicit none
    real,intent(in) :: new_size
    surface_triangle_volume = new_size
    end subroutine Set_Surface_Width

    !Documented
    subroutine Set_Text_Line_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    text_size = new_size
    end subroutine Set_Text_Line_Size

    !Documented
    subroutine Set_Axis_X_Text(new_text)
    implicit none
    character(:),allocatable, intent(in) :: new_text
    if(allocated(axis_ox_text)) then
        deallocate(axis_ox_text)
    end if
    axis_ox_text = trim(new_text)
    end subroutine Set_Axis_X_Text

    !Documented
    subroutine Set_Axis_Y_Text(new_text)
    implicit none
    character(:),allocatable, intent(in) :: new_text
    print*,new_text
    if(allocated(axis_oy_text)) then
        deallocate(axis_oy_text)
    end if
    axis_oy_text = trim(new_text)
    end subroutine Set_Axis_Y_Text

    !Documented
    subroutine Set_Axis_Z_Text(new_text)
    implicit none
    character(:),allocatable, intent(in) :: new_text
    if(allocated(axis_oz_text)) then
        deallocate(axis_oz_text)
    end if
    axis_oz_text = trim(new_text)
    end subroutine Set_Axis_Z_Text
    
    !Documented
    subroutine Set_Window_Text(new_text)
    implicit none
    character(:),allocatable, intent(in) :: new_text
    if(allocated(glut_window_name)) then
        deallocate(glut_window_name)
    end if
    glut_window_name = trim(new_text)
    end subroutine Set_Window_Text

    !Documented
    subroutine Set_Axis_Text_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    axis_text_size = new_size
    end subroutine Set_Axis_Text_Size

    !Documented
    subroutine Set_Marks_Text_Size(new_size)
    implicit none
    real,intent(in) :: new_size
    marks_text_size = new_size
    end subroutine Set_Marks_Text_Size

    !Documented
    subroutine Show_Z_Marks(boolean)
    implicit none
    logical,intent(in) :: boolean
    build_z_marks = boolean
    end subroutine Show_Z_Marks

	!Internal compiler error gimplify.c 2085 gimplify_var_or_parm_decl
    !Documented
    subroutine Set_Legend_Text(new_text)
    implicit none
    character(:),dimension(:),allocatable,intent(in) :: new_text
    object_name = new_text
    end subroutine Set_Legend_Text

    !Documented
    subroutine Set_Legend_Start_Pos_X(posit)
    implicit none
    real,intent(in) :: posit
    zero_x=posit
    end subroutine Set_Legend_Start_Pos_X

    !Documented
    subroutine Show_Legend(flag)
    implicit none
    logical,intent(in) :: flag
    legend_need = flag
    end subroutine Show_Legend

    !Documented
    subroutine Set_Net(flag)
    implicit none
    logical,intent(in) :: flag
    net_need = flag
    end subroutine Set_Net
    
    !EXPERIMENTAL
    subroutine Set_Limit_Lining(flag, number)
    implicit none
    logical,intent(in) :: flag
    integer, optional, value :: number
    if(PRESENT(number)) limit_lining_number = number
    limit_lining = flag
    end subroutine Set_Limit_Lining
    
    !EXPERIMENTAL
    subroutine Set_Border_As_Axis(flag)
    implicit none
    logical,intent(in) :: flag
    border_as_axis = flag
    end subroutine Set_Border_As_Axis

    end module Plotter