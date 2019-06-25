program test_mps_read
    use utilities
    use mps_reader
    implicit none
    character(len=255) :: A, B, C, D, E, ColumnNow
    character(len=255) :: tstring
    character(len=255) :: cwd, filename
    integer :: ivar_num, iconstraint_num
    real, allocatable, dimension(:) :: ifunc_vec, ieq_type, rhs
    real, allocatable, dimension(:,:) :: iconstraints
    integer :: uuid, i, ind, iostatus, nstring, num_fields
    real :: tvalf, tvals
    real, dimension(:), allocatable :: switch_vec
    logical :: skip, found, switch, endcnd
    character(len=255), dimension(:), allocatable :: rowsdesignations, columndesignations
    filename = '/bin/gen-ip002.mps'
    call getcwd(cwd)
    cwd = trim(adjustl(cwd))//trim(adjustl(filename))
    print *,cwd//filename
    allocate(ieq_type(0), rowsdesignations(0), switch_vec(0), columndesignations(0))
    rowsdesignations = get_rows(cwd//filename)
    columndesignations = get_cols(cwd//filename)
    ieq_type = get_equations_types(cwd//filename)
    allocate(iconstraints(size(columndesignations), size(rowsdesignations)+1))
    iconstraints = get_matrix(cwd//filename, rowsdesignations, columndesignations)
    rhs = get_rhs(cwd//filename, rowsdesignations)
    !iconstraints(:,size(rowsdesignations)+1) = rhs
    allocate(ifunc_vec(size(rowsdesignations)-1))
    print *, size(rowsdesignations)
    call output_string_array(rowsdesignations)
    print *, size(columndesignations)
    call output_string_array(columndesignations)
    print *, size(ieq_type)
    print *, ieq_type
    print *, size(iconstraints, dim=1), size(iconstraints, dim=2)
    call output_matrix(iconstraints)
    print *, size(rhs)
    print *, rhs
end program test_mps_read