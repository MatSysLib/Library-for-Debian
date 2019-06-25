module mps_reader
    use string_util
    implicit none
    contains
!func_vec - functional vector, a vector of var_num sequential multipliers of variables for function accounting
!constraints - list of vectors in following format:
!   var_num sequential variable multipliers, 
!   right side value
!eq_type - vector of integer containing equations type in following format:
!   equation type:  >= = -1
!                   <= =  1
!                   =  =  0
!   don't forget that fortran is column-major order
!var_num - total number of variables
!constraint_num - total number of constraints
    !Based on format description from ILOG CPLEX
    subroutine mps_read(abs_file_path, ifunc_vec, iconstraints, ieq_type, ivar_num, iconstraint_num)
        implicit none
        character(len=255), intent(in) :: abs_file_path
        integer, intent(out) :: ivar_num, iconstraint_num
        real, allocatable, dimension(:), intent(out) :: ifunc_vec, ieq_type
        real, allocatable, dimension(:,:), intent(out) :: iconstraints
        real, allocatable, dimension(:) :: rhs
        real, dimension(:), allocatable :: switch_vec
        character(len=255), dimension(:), allocatable :: rowsdesignations, columndesignations
        allocate(ieq_type(0), rowsdesignations(0), switch_vec(0), columndesignations(0))
        rowsdesignations = get_rows(abs_file_path)
        columndesignations = get_cols(abs_file_path)
        allocate(iconstraints(size(rowsdesignations)+1, size(columndesignations)))
        ieq_type = get_equations_types(abs_file_path)
        iconstraints = get_matrix(abs_file_path, rowsdesignations, columndesignations)
        rhs = get_rhs(abs_file_path, rowsdesignations)
        iconstraints(:,size(rowsdesignations)+1) = rhs
        allocate(ifunc_vec(size(rowsdesignations)-1))
    end subroutine mps_read

    function get_rows(abs_file_path) result(rows)
        implicit none
        character(len=255), intent(in) :: abs_file_path
        integer :: uuid, num_fields
        character(len=255) :: tstring, A, B
        character(len=255), dimension(:), allocatable :: rows
        allocate(rows(0))
        open(newunit=uuid, file=trim(adjustl(abs_file_path)), status='old', action='read')
        read(uuid, fmt="(A)") tstring
        tstring = StringNormalise(tstring)
        do while(tstring/="rows")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
        enddo
        do while(tstring/="columns")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
            if(tstring == "columns") cycle
            num_fields = fields_num(tstring)
            read(tstring, *) B, A
            if(get_index(A, rows)<0) rows = [character(len=255)::rows, A]
        enddo
        close(uuid)
    end function get_rows

    function get_equations_types(abs_file_path) result(eq_types)
        implicit none
        character(len=255), intent(in) :: abs_file_path
        integer :: uuid, num_fields, nstring
        integer, allocatable, dimension(:) :: eq_types
        character(len=255) :: tstring, A, B
        nstring = 0
        allocate(eq_types(0))
        open(newunit=uuid, file=trim(adjustl(abs_file_path)), status='old', action='read')
        read(uuid, fmt="(A)") tstring
        nstring = 1
        tstring = StringNormalise(tstring)
        do while(tstring/="rows")
            read(uuid, fmt="(A255)") tstring
            tstring = StringNormalise(tstring)
            nstring = nstring + 1
        enddo
        do while(tstring/="columns")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
            if(tstring == "columns") cycle
            nstring = nstring + 1
            num_fields = fields_num(tstring)
            read(tstring, *) A, B           
            if (A == "l") then 
                eq_types = [eq_types, 1]
            elseif (A == "g") then
                eq_types = [eq_types, -1]
            elseif (A == "e") then
                eq_types = [eq_types, 0]
            elseif (A == "n") then
                eq_types = [eq_types, 2]
            else
                print *,"Unknown type of equation at string ", nstring, " - ", A
                error stop 1
            endif
        enddo
        close(uuid)
    end function get_equations_types

    function get_cols(abs_file_path) result(columns)
        implicit none
        character(len=255), intent(in) :: abs_file_path
        integer :: uuid, num_fields
        character(len=255) :: tstring, A
        character(len=255), dimension(:), allocatable :: columns
        allocate(columns(0))
        open(newunit=uuid, file=trim(adjustl(abs_file_path)), status='old', action='read')
        read(uuid, fmt="(A)") tstring
        tstring = StringNormalise(tstring)
        do while(tstring/="columns")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
        enddo
        do while(tstring/="rhs")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
            if(tstring == "rhs") cycle
            num_fields = fields_num(tstring)
            if((num_fields==3).OR.(num_fields==5)) then
                read(tstring, *) A
                !Skip Markers
                if (A == "marker") cycle
                if(get_index(A, columns)<0) columns = [character(len=255)::columns, A]
            endif
        enddo
        close(uuid)
    end function get_cols

    !Find string in array of strings.
    !Returns index of found string
    !If string not found, returns -1
    integer function get_index(string, string_array) result(index)
        implicit none
        character(*), intent(in) :: string
        character(*), dimension(:), intent(in) :: string_array
        integer :: i
        if(size(string_array) /= 0) then 
            do i = 1,size(string_array)
                if(StringNormalise(string) == StringNormalise(string_array(i))) then 
                    index = i
                    return
                endif
            enddo
        endif
        index = -1
    end function get_index

    function get_matrix(abs_file_path, rows, cols) result(res)
        implicit none
        character(len=255), intent(in) :: abs_file_path
        character(len=255), dimension(:), intent(in) :: rows, cols
        integer :: uuid, num_fields, i, j
        real :: tval
        real, dimension(size(cols), size(rows)) :: res
        character(len=255) :: tstring, A, B, C, D, E
        res = 0
        open(newunit=uuid, file=trim(adjustl(abs_file_path)), status='old', action='read')
        read(uuid, fmt="(A)") tstring
        tstring = StringNormalise(tstring)
        do while(tstring/="columns")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
        enddo
        do while(tstring/="rhs")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
            if(tstring == "rhs") cycle
            num_fields = fields_num(tstring)
            if((num_fields==3).OR.(num_fields==5)) then
                if(num_fields == 3) read(tstring, *) A, B, C
                if(num_fields == 5) read(tstring, *) A, B, C, D, E
                if((num_fields /= 3) .AND. (num_fields /= 5)) cycle
                !Skip Markers
                B = StringNormalise(B)
                if (B == "marker") cycle
                C = StringNormalise(C)
                read(C, *) tval
                A = StringNormalise(A)
                B = StringNormalise(B)
                i = get_index(A, cols)
                j = get_index(B, rows)
                if(i<0.OR.j<0) then
                    print *,"NEGATIVE ARRAY INDEX"
                    print *, A
                    print *, B
                    error stop 1
                endif
                res(i, j) = tval
                if(num_fields == 5) then
                    read(E, *) tval
                    D = StringNormalise(D)
                    j = get_index(D, rows)                
                    if(i<0.OR.j<0) then
                        print *,"NEGATIVE ARRAY INDEX"
                        print *, D
                        error stop 1
                    endif
                    res(i, j) = tval
                endif
            endif
        enddo
        close(uuid)
    end function get_matrix

    function get_rhs(abs_file_path, cols) result(res)
        implicit none
        character(len=255), intent(in) :: abs_file_path
        character(len=255), dimension(:), intent(in) :: cols
        integer :: uuid, num_fields, tval, i, j
        real, dimension(size(cols)) :: res
        character(len=255) :: tstring, A, B, C, D, E
        res = 0
        open(newunit=uuid, file=trim(adjustl(abs_file_path)), status='old', action='read')
        read(uuid, fmt="(A)") tstring
        tstring = StringNormalise(tstring)
        do while(tstring/="rhs")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
        enddo
        do while(tstring/="bounds")
            read(uuid, fmt="(A)") tstring
            tstring = StringNormalise(tstring)
            if(tstring == "bounds") cycle
            num_fields = fields_num(tstring)
            if((num_fields==3).OR.(num_fields==5)) then
                if(num_fields == 3) read(tstring, *) A, B, C
                if(num_fields == 5) read(tstring, *) A, B, C, D, E
                if((num_fields /= 3) .OR. (num_fields /= 5)) cycle
                read(C, *) tval
                j = get_index(B, cols)
                res(j) = tval
                if(num_fields == 5) then
                    read(E, *) tval
                    j = get_index(D, cols)
                    res(j) = tval
                endif
            endif
        enddo
        close(uuid)
    end function get_rhs
end module mps_reader