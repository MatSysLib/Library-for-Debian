module kramer_lib
    implicit none
    integer, private :: T1, T2
    contains
    function kramer(a, b, n) result(res)
        use determinant_lib, only: determinant
        implicit none
        real, intent(in) :: a(:,:), b(:)
        real :: c(n,n), temp, res(n)
        integer, intent(in) :: n
        integer :: i
        temp = determinant(a,n)
        c = a
        call SYSTEM_CLOCK(T1)
        do i=1,n
            c(:,i) = b
            res(i) = determinant(c,n)/temp
            c(:,i) = a(:,i)
        enddo
        call SYSTEM_CLOCK(T2)
    end function kramer

    function timing(a, b, n, repeats) result(res)
        implicit none
        real, intent(in) :: a(:,:), b(:)
        real :: res(repeats), t(n)
        integer :: i, repeats, n, rate
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = kramer(a, b, n)
            res(i) = real(T2 - T1) / real(rate)
        enddo
    end function
end module kramer_lib