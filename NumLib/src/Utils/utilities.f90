module utilities
    use mps_reader
    implicit none
    interface reverse
        module procedure real_reverse
    end interface reverse
    contains
    elemental pure integer function LTOI(a)
        logical, intent(in) :: a
        if(a) then
            LTOI = 1
        else
            LTOI = 0
        endif
    end function LTOI

    FUNCTION ITOA(S) RESULT(RES)
        IMPLICIT NONE
        CHARACTER(:), ALLOCATABLE :: RES
        INTEGER, INTENT(IN) :: S
        CHARACTER(RANGE(S)+2) :: TMP
        WRITE(TMP,'(I0)') S
        RES = TRIM(TMP)
    END FUNCTION ITOA

    function real_reverse(vector) result(invector)
        implicit none
        real, intent(in) :: vector(:)
        real, dimension(size(vector)) :: invector
        invector = vector(size(vector):1:-1)
    end function real_reverse

    subroutine output_vector(vector)
        implicit none
        real, intent(in) :: vector(:)
        write(*,"("//ITOA(size(vector))//"(F10.5))") vector
    end subroutine output_vector

    subroutine output_qmatrix(matrix,n)
        implicit none
        real,intent(in) :: matrix(:,:)
        integer,intent(in) :: n
        integer :: i
        print *,"In Fortran Memory (Transformed Matrix):"
        write(*,"("//ITOA(n)//"(F12.6))") (matrix(i,:),i=1,n)
        print *," "
        print *,"In C Memory (Factual Original Matrix):"
        write(*,"("//ITOA(n)//"(F12.6))") (matrix(:,i),i=1,n)
        print *," "
    end subroutine output_qmatrix

    subroutine output_amatrix(matrix)
        implicit none
        real,intent(in) :: matrix(:,:)
        integer :: i
        print *,"In Fortran Memory (Transformed Matrix):"
        write(*,"("//ITOA(size(matrix,2))//"(F12.6))") (matrix(i,:),i=1,size(matrix,1))
        print *," "
        print *,"In C Memory (Factual Original Matrix):"
        write(*,"("//ITOA(size(matrix,1))//"(F12.6))") (matrix(:,i),i=1,size(matrix,2))
        print *," "
    end subroutine output_amatrix

    subroutine output_matrix(matrix)
        implicit none
        real, intent(in) :: matrix(:,:)
        integer :: i
        write(*,"("//ITOA(size(matrix,2))//"(F20.10))") (matrix(i,:),i=1,size(matrix,1))
        print *," "
    end subroutine output_matrix

    subroutine output_string_array(array)
        implicit none
        character(len=*), dimension(:), intent(in) :: array
        integer :: i
        do i = 1,size(array)
            print *,array(i)
        enddo
    end subroutine output_string_array

    function odu_matrix_create(n) result(matrix)
        implicit none
        integer, intent(in) :: n
        real :: matrix(n,n)
        integer :: i, j
        do i=1,n
            do j=1,n
                matrix(j,i) = koeff_matrix_setting(j,i,n)
            enddo
        enddo    
    end function odu_matrix_create

    function slau_matrix_create(n) result(matrix)
        implicit none
        integer, intent(in) :: n
        integer :: i
        real, dimension(n,n) :: matrix
        matrix = 1
        do i=1,n
            matrix(i,i) = n*2
        enddo
    end function slau_matrix_create

    function slau_vector_create(n) result(vector)
        implicit NONE
        integer, intent(in) :: n
        integer :: i
        real, dimension(n) :: vector
        do i=1,n
            vector(i) = n*(n+1)/2.+i*(2*n-1)
        enddo
    end function slau_vector_create

    subroutine tda_matrix_vector_create(n, a, b)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(out) :: b
        real, dimension(n,n), intent(out) :: a
        integer :: i
        real, parameter :: first = 1, second = 1024
        real, dimension(3) :: temp
        a = 0
        do i = 1,n
            temp = 0
            if(i==1) then
                temp(1) = NINT(random(first, second))
                temp(2) = temp(1) * 2
            else if(i==n) then
                temp(2) = NINT(random(first, second))
                temp(3) = temp(2) * 2
            else
                temp(1) = NINT(random(first, second))
                temp(2) = NINT(random(first, second))
                temp(3) = 2 * (temp(1) + temp(2))
            endif
            if(i==1) then
                a(1,1) = max(temp(1), temp(2))
                a(2,1) = min(temp(1), temp(2))
            else if(i==n) then
                a(n,n) = max(temp(2), temp(3))
                a(n-1,n) = min(temp(2), temp(3))
            else
                a(i,i) = temp(3)
                if(random(-1.,1.)>0) then
                    a(i-1,i) = max(temp(1),temp(2))
                    a(i+1,i) = min(temp(1),temp(2))
                else
                    a(i-1,i) = min(temp(1),temp(2))
                    a(i+1,i) = max(temp(1),temp(2))
                endif
            endif
        enddo
        a = transpose(a)
        do i = 1,n
            b(i) = sum(a(:,i))
        enddo
    end subroutine tda_matrix_vector_create

    subroutine tda_matrix_vector_create_spec(n, a, b)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(out) :: b
        real, dimension(n,n), intent(out) :: a
        integer :: i
        a = 0
        do i = 1,n
            a(i,i) = 4
            if(i/=n) a(i+1,i) = -1
            if(i/=1) a(i-1,i) = -1
            b(i) = sum(a(:,i))
        enddo
    end subroutine tda_matrix_vector_create_spec

    real function koeff_matrix_setting(j,i,n) result(value)
        implicit NONE
        integer, intent(in) :: i,j,n
        if(j==i) then
            value = -4
        else if(j>1.AND.(j-1==i)) then
            value = 1
        else if(j<n.AND.(j+1==i)) then
            value = 1
        else 
            value = 0
        endif
    end function koeff_matrix_setting

    !Методы получения случайного числа
    !Равновероятное распределение в промежутке [a, b]
    !В параллельных вычислениях использовать нельзя
    real function random(a, b) result(res)
        implicit none
        real, intent(in) :: a, b
        res = a + (b - a) * rand()
    end function random
    !То же самое, но для параллельных вычислений
    real function parallel_random(a, b) result(res)
        implicit none
        real, intent(in) :: a, b
        call random_number(res)
        res = a + (b - a) * res
    end function parallel_random

    subroutine output_stringvec(a, length)
        implicit none
        integer, intent(in) :: length
        character(len=length), dimension(:), allocatable, intent(in) :: a
        integer :: i
        do i=1,size(a)
            print *, adjustl(trim(a(i)))
        enddo
    end subroutine output_stringvec
end module utilities