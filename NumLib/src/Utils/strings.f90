module string_util
    implicit none
    contains
    function ToLower(s1) result(s2)
        character           :: ch
        character(*)        :: s1
        character(len(s1))  :: s2
        integer             :: i
        integer, parameter  :: DUC = ICHAR('A') - ICHAR('a')
        do i = 1,len(s1)
           ch = s1(i:i)
           if (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
           s2(i:i) = ch
        enddo
    END FUNCTION ToLower

    function StringNormalise(s) result(ns)
        character(*) :: s
        character(len(s)) :: ns
        ns = ToLower(trim(adjustl(s)))
    end function StringNormalise

    pure integer function fields_num(s)
        character(*), intent(in) :: s
        integer i, n
        i = 1
        n = len_trim(s)
        fields_num = 0
        do while(i <= n)
            do while(s(i:i) == ' ') 
                i = i + 1
                if (n < i) return
            enddo
            fields_num = fields_num + 1
            do
                i = i + 1
                if (n < i) return
                if (s(i:i) == ' ') exit
            enddo
        enddo
    end function fields_num
end module string_util