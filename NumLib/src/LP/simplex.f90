module simplex_lib
    implicit none
    integer :: c_start, s_start, a_start, rhs_pos, t_row, &
               add_slack, add_art, add_slack_and_art, &
               cols, rows
    integer, dimension(:), allocatable :: var_in_basis
    real, dimension(:,:), allocatable :: tableu
    logical :: is_phase_i = .FALSE.,az_added = .FALSE.
    contains
    subroutine canonical_tableau(func_vec, iconstraints, eq_type, var_num, constraint_num)
        use utilities, only : LTOI
        implicit none
        integer, intent(in) :: var_num, constraint_num
        integer, dimension(constraint_num), intent(in) :: eq_type
        real, dimension(var_num), intent(in) :: func_vec
        real, dimension(var_num+1, constraint_num), intent(in) :: iconstraints
        real, dimension(var_num+1, constraint_num) :: constraints
        integer :: i, j
        logical, dimension(constraint_num) :: is_slack_variable, is_artificial_variable, az_location
        add_slack = 0
        add_art = 0
        add_slack_and_art = 0
        c_start = 0
        s_start = 0
        a_start = 0
        rhs_pos = 0
        cols = 0
        rows = 0
        t_row = 0
        constraints = iconstraints
        az_location = .FALSE.
        !Preparing
        do i = 1,constraint_num
            if((eq_type(i) == 0).AND.(constraints(var_num+1,i) < 0)) constraints(:,i) = -constraints(:,i)
            if(eq_type(i) < 0) constraints(:,i) = -constraints(:,i)
        enddo
        !Every constraint is now either <= or =
        !if = then rhs is >0
        do i = 1,constraint_num
            if(constraints(var_num+1,i) < 0) then
                az_location(i) = .TRUE.
                if(az_added.eqv..FALSE.) then
                    az_added = .TRUE.
                    add_art = add_art + 1
                    add_slack_and_art = add_slack_and_art + 1
                endif
            endif
            !for equality constraints slack variables treated as artificial
            if(eq_type(i) == 0) then
                add_art = add_art + 1
                add_slack_and_art = add_slack_and_art + 1
                is_slack_variable(i) = .TRUE.
                is_artificial_variable(i) = .TRUE.
                if(is_phase_i.eqv..FALSE.) is_phase_i = .TRUE.
            else
                add_slack = add_slack + 1
                add_slack_and_art = add_slack_and_art + 1
                is_slack_variable(i) = .TRUE.
            endif
        enddo
        !Construction
        !TODO: Определиться отдельно ли выделять базисные переменные или запихнуть в таблицу
        !Transpose:
        !W+Z+xi+(si+ai+a0)+rhs
        !+
        !Z
        !+
        !Number of constraints
        cols = LTOI(is_phase_i)+1+var_num+add_slack_and_art+1
        rows = LTOI(is_phase_i)+constraint_num+1
        allocate(tableu(cols,rows), var_in_basis(constraint_num))
        tableu = 0
        tableu(1,1) = 1
        if(is_phase_i) then
            tableu(2,2) = 1
            c_start = 3
            t_row = 2
        else
            c_start = 2
            t_row = 1
        endif
        s_start = c_start + var_num
        a_start = s_start + add_slack
        rhs_pos = a_start + add_art
        do j = 1,constraint_num
            do i = 1,var_num
                tableu(c_start-1+i,t_row+j) = constraints(i,j)
            enddo
            tableu(s_start-1+j,t_row+j) = 1
            if(az_added.eqv..true.) then
                if(az_location(j).eqv..true.) tableu(rhs_pos-1,t_row+j) = -1
            endif
            tableu(rhs_pos,t_row+j) = constraints(var_num+1,j)
        enddo
        do i = 1,var_num
            tableu(c_start-1+i, t_row) = func_vec(i)
        enddo
        do i = 1,add_art
            tableu(a_start-1+i, 1) = 1
        enddo
        if(add_slack_and_art - LTOI(az_added)>constraint_num) then
            print *,"More slack and artificial variables then constraints. No mention in algorithm of this situation. Exitting."
            call p_table(tableu, size(tableu,2), size(tableu,1))
            print *,add_slack_and_art - LTOI(az_added)
            print *,constraint_num
            print *,"Error: Failed to create basis of variables"
            return 1
        endif
        var_in_basis = 0
        do i = 1,add_slack_and_art - LTOI(az_added)
            var_in_basis(i) = s_start+i-1
        enddo
        !Construction Complete
    end subroutine canonical_tableau
!simplex_method(func_vec, constraints, eq_type, var_num, constraint_num) result(res_vector)
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
    function simplex_method(func_vec, constraints, eq_type, var_num, constraint_num) result(res_vector)
        implicit none
        integer, intent(in) :: var_num, constraint_num
        integer, dimension(constraint_num), intent(in) :: eq_type
        real, dimension(var_num), intent(in) :: func_vec
        real, dimension(var_num+1,constraint_num), intent(in) :: constraints
        real, dimension(:), allocatable :: res_vector
        allocate(res_vector(5))
        res_vector = 0
        call canonical_tableau(func_vec, constraints, eq_type, var_num, constraint_num)
        call p_table(tableu, size(tableu,2), size(tableu,1))
        print *,var_in_basis
        if(is_phase_i.eqv..true.) then 
            call simplex_phase_i
            call convert_tableu
        endif
        call simplex_phase_ii
    end function simplex_method

    subroutine simplex_phase_i
        use utilities, only : LTOI
        implicit none
        integer :: i, row_pos, col_pos
        real :: temp
        logical :: c_stop = .FALSE., az_override = .FALSE., az_pivot_override = .FALSE.
        row_pos = 10000
        print *, size(tableu,2), size(tableu,1)
        print *, rows, cols
        !turning artificial variables into basic
        do i = 1,add_art-LTOI(az_added)
            if(abs(tableu(a_start+1-i,t_row-1))>0.0001) then
                row_pos = get_pos_non_zero_first(tableu(a_start+1-i,2:))+1
                tableu(:,t_row-1) = tableu(:,t_row-1) - sign(1.,tableu(row_pos, t_row-1)) * tableu(:, row_pos)
            endif
        enddo
        do while(c_stop.neqv..TRUE.)
            !find variable in target row with minimal value (negative)
            col_pos = minloc(tableu(1:cols-1,1),DIM=1)
            !if there is a0 then disregard that, deal with it first
            if(az_override.eqv..false.) then
                if(az_added.eqv..true.) then 
                    col_pos = rhs_pos-1
                endif
                az_override = .true.
                az_pivot_override = .true.
            endif
            !print *,"Column to add to basis:", col_pos
            !print *, "Current RHS:"
            !print *, tableu(rhs_pos,:)
            !print *, "Current column:"
            !print *, tableu(col_pos,:)
            temp = 9999
            !We have column, find a row for initial pivot
            do i = t_row+1,rows
                !find location of minimal value in rhs
                !disregard ususal inhibitions for a0
                if(az_pivot_override) then
                    row_pos = minloc(tableu(rhs_pos,t_row+1:rows),dim=1)+t_row
                    az_pivot_override = .false.
                    exit
                elseif((tableu(rhs_pos,i)/tableu(col_pos,i)<temp).AND.(tableu(col_pos,i)>0)) then
                    !print *,"№", i, ": ", tableu(col_pos,i)," / ",tableu(rhs_pos,i), " = ", tableu(rhs_pos,i)/tableu(col_pos,i)
                    temp = tableu(rhs_pos,i) / tableu(col_pos,i)
                    row_pos = i
                endif
            enddo
            !print *,"Row and temp:", row_pos, temp
            !Enter the basis
            var_in_basis(row_pos - t_row) = col_pos
            !normalise row
            tableu(:,row_pos) = tableu(:,row_pos) / tableu(col_pos, row_pos)
            !adjust rows
            do i = 1,rows
                !print *,"Mult:", tableu(col_pos,i)
                if((i/=row_pos).AND.(abs(tableu(col_pos,i))>0.00001)) then
                    !print *, "before:", tableu(:,i)
                    !print *, "change:", tableu(:,row_pos)
                    !print *, "m:", sign(1.,tableu(col_pos,i))
                    tableu(:,i) = tableu(:,i) - tableu(col_pos,i) * tableu(:, row_pos)
                    !print *, "after:"
                endif
            enddo
            !print *,"Value of W:", tableu(rhs_pos,1)
            !check target row for stop
            if(minval(tableu(1:a_start-1,1),dim=1)>-0.0001) then
                if(tableu(rhs_pos,1)<0) then
                    print *,"W<0 - Original problem is infeasible"
                    c_stop = .TRUE.
                    return 0
                elseif(abs(tableu(rhs_pos,1))<0.00001) then
                    c_stop = .TRUE.
                endif
            endif
            print*,"Table now:"
            call p_table(tableu, size(tableu,2), size(tableu,1))
            pause
        enddo
        print *, "Final Table:"
        call p_table(tableu, size(tableu,2), size(tableu,1))
        print *, "Columns in basis:"
        print *, var_in_basis
        print *, "The end of phase I"
    end subroutine simplex_phase_i

    subroutine convert_tableu
        implicit none
        real, dimension(:,:), allocatable :: temp
        allocate(temp(cols, rows))
        temp = tableu
        deallocate(tableu)
        allocate(tableu(cols-1, rows-1))
        tableu = temp(2:,2:)
        deallocate(temp)
        cols = cols - 1
        rows = rows - 1
        t_row = t_row - 1
        c_start = c_start - 1
        s_start = s_start - 1
        a_start = a_start - 1
        rhs_pos = rhs_pos - 1
    end subroutine convert_tableu

    subroutine simplex_phase_ii
        use utilities, only : LTOI
        implicit none
        integer :: i, row_pos, col_pos
        real :: temp
        logical :: c_stop = .FALSE.
        row_pos = 10000
        do while(c_stop.neqv..TRUE.)
            col_pos = minloc(tableu(1:a_start-1,1),DIM=1)
            print *,"Column to add to basis:", col_pos
            print *, "Current RHS:"
            print *, tableu(rhs_pos,:)
            print *, "Current column:"
            print *, tableu(col_pos,:)
            temp = 9999
            do i = t_row+1,rows
                if((tableu(rhs_pos,i)/tableu(col_pos,i)<temp).AND.(tableu(col_pos,i)>0)) then
                    temp = tableu(rhs_pos,i) / tableu(col_pos,i)
                    row_pos = i
                endif
            enddo
            print *,"Row:", row_pos
            var_in_basis(row_pos - t_row) = col_pos
            tableu(:,row_pos) = tableu(:,row_pos) / tableu(col_pos, row_pos)
            do i = 1,rows
                if((i/=row_pos).AND.(abs(tableu(col_pos,i))>0.00001)) then
                    tableu(:,i) = tableu(:,i) - tableu(col_pos,i) * tableu(:, row_pos)
                endif
            enddo
            print *, minval(tableu(:a_start-1,1),dim=1)>-0.0001, tableu(:a_start-1,1)
            if(minval(tableu(:a_start-1,1),dim=1)>-0.0001) then
                if(tableu(rhs_pos,1)<0) then
                    print *,"W<0 - Original problem is infeasible"
                    c_stop = .TRUE.
                    return 1
                else
                    c_stop = .TRUE.
                endif
            endif
            !print*,"Table now:"
            !call p_table(tableu, size(tableu,2), size(tableu,1))
            !pause
        enddo
        print *, "Final Table:"
        call p_table(tableu, size(tableu,2), size(tableu,1))
        print *, "Columns in basis:"
        print *, var_in_basis
        print *, "The end of phase II"
        !print *, "SPIIEOK"
    end subroutine simplex_phase_ii

    !Returns position of first non-zero value in the vector or 1
    pure integer function get_pos_non_zero_first(vector) result(pos)
        implicit none
        real, dimension(:), intent(in) :: vector
        integer :: i
        pos = 1
        do i=1,size(vector)
            if(abs(vector(i))>0.0000001) then
                pos = i
                exit
            endif
        enddo
    end function get_pos_non_zero_first

    subroutine p_table(a, x, y)
        implicit none
        integer, intent(in) :: x, y
        real, dimension(y, x), intent(in) :: a
        integer :: i, j
        do i = 1,x
            do j = 1,y
                write(*,"(F10.5)",advance='no') a(j,i)
            enddo
            write(*,"(A1)") " "
        enddo
        write(*,"(A1)") " "
    end subroutine p_table
end module simplex_lib