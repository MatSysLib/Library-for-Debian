!Various complimentary methods for working with matrices and vectors
!No specific method applicable
module utilites_matrix_vector
    implicit none
    contains
    function vectortodiagonal(v) result(d)
        implicit none
        real, dimension(:), intent(in) :: v
        real, dimension(size(v), size(v)) :: d
        integer :: i
        d = 0
        do i = 1,size(v)
            d(i,i) = v(i)
        enddo
    end function vectortodiagonal

    function diagonalmatrixpower(matrix, power) result(powered)
        implicit none
        integer, intent(in) :: power
        real, dimension(:,:) :: matrix
        real, dimension(size(matrix,dim=1),size(matrix,dim=2)) :: powered 
        integer :: i
        powered = 0
        do i=1,size(matrix,dim=2)
            powered(i,i) = matrix(i,i) ** power
        enddo
    end function diagonalmatrixpower

    function genidenmat(n) result(res)
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n) :: res
        integer :: i
        res = 0
        do i = 1,n
            res(i,i) = 1
        enddo
    end function genidenmat

    function matpow(matrix, i_n, power) result(res)
        use classic_lib
        implicit none
        integer, intent(in) :: power, i_n
        real, dimension(i_n,i_n), intent(in) :: matrix
        integer :: n
        real, dimension(i_n,i_n) :: res, m_t 
        n = i_n
        res = genidenmat(i_n)
        m_t = matrix
        do while(n > 0)
            if(mod(n,2)==1) res = classic(res, m_t, i_n)
            if(n > 1) m_t = classic(m_t, m_t, i_n)
            n = n / 2
        enddo
    end function matpow

    function matsumpow(matrix, i_n, power) result(res)
        use classic_lib
        implicit none
        integer, intent(in) :: power, i_n
        real, dimension(i_n,i_n), intent(in) :: matrix
        integer :: n
        real, dimension(i_n,i_n) :: res, e
        e = genidenmat(i_n)
        res = e
        n = i_n
        do while(n>0)
            res = classic(res, (e + matpow(matrix, i_n, n/2)), i_n)
            if(mod(power,2)==1) res = res + matpow(matrix, i_n, n)
            n = n / 2
        enddo
    end function matsumpow
end module
