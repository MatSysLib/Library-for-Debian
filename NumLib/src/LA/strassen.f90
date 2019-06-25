module strassen_lib
    implicit none
    integer(kind=8), private :: T1, T2
    double precision, private :: RT1, RT2
    contains
    function strassen(A,B,n) result(C)
        use classic_lib, only : classic
        implicit none
        integer, intent(in) :: n
        integer :: nh, nhp
        real, intent(in) :: A(:,:), B(:,:)
        real :: C(n,n)
        real, dimension(n/2,n/2) :: P1, P2, P3, P4, P5, P6, P7
        nh = n / 2
        nhp = nh + 1
        call SYSTEM_CLOCK(T1)
        P1 = classic((A(:nh,:nh)+A(nhp:,nhp:)),(B(:nh,:nh)+B(nhp:,nhp:)), nh)
        P2 = classic((A(nhp:,:nh)+A(nhp:,nhp:)),B(:nh,:nh), nh)
        P3 = classic(A(:nh,:nh),(B(:nh,nhp:)-B(nhp:,nhp:)), nh)
        P4 = classic(A(nhp:,nhp:),(B(nhp:,:nh)-B(:nh,:nh)), nh)
        P5 = classic((A(:nh,:nh) + A(:nh,nhp:)),B(nhp:,nhp:), nh)
        P6 = classic((A(nhp:,:nh)-A(:nh,:nh)),(B(:nh,:nh)+B(:nh,nhp:)), nh)
        P7 = classic((A(:nh,nhp:) - A(nhp:,nhp:)),(B(nhp:,:nh)+B(nhp:,nhp:)), nh)
        C(:nh,:nh) = P1 + P4 - P5 + P7
        C(:nh,nhp:) = P3 + P5
        C(nhp:,:nh) = P2 + P4
        C(nhp:,nhp:) = P1 - P2 + P3 + P6
        call SYSTEM_CLOCK(T2)
    end function strassen

    recursive function recursive_strassen(a,b,N) result(c)
        use classic_lib, only : classic
        implicit none
        integer, intent(in) :: N
        integer :: nh, nhp
        real, intent(in) :: a(N,N),b(N,N)
        real :: c(N,N)
        real, dimension(N/2,N/2) :: a11,a21,a12,a22,b11,b21,b12,b22 
        real, dimension(N/2,N/2) :: q1,q2,q3,q4,q5,q6,q7
        nh = n/2
        nhp = nh+1
    
        if(iand(N,1) /= 0 .OR. N <= 256) then
            c = classic(a,b,n)
        else
            a11 = a(:nh,:nh)
            a21 = a(nhp:,:nh)
            a12 = a(:nh,nhp:)
            a22 = a(nhp:,nhp:)
            b11 = b(:nh,:nh)
            b21 = b(nhp:,:nh)
            b12 = b(:nh,nhp:)
            b22 = b(nhp:,nhp:)
            q1 = recursive_strassen(a11+a22,b11+b22,nh)
            q2 = recursive_strassen(a21+a22,b11,nh)
            q3 = recursive_strassen(a11,b12-b22,nh)
            q4 = recursive_strassen(a22,-b11+b21,nh)
            q5 = recursive_strassen(a11+a12,b22,nh)
            q6 = recursive_strassen(-a11+a21,b11+b12,nh)
            q7 = recursive_strassen(a12-a22,b21+b22,nh)
            c(:nh,:nh) = q1+q4-q5+q7
            c(nhp:,:nh) = q2+q4
            c(:nh,nhp:) = q3+q5
            c(nhp:,nhp:) = q1+q3-q2+q6
        end if
    end function recursive_strassen

    recursive function recursive_strassen_omp(a,b,N) result(c)
        use classic_lib, only : classic_omp
        implicit none
        integer, intent(in) :: N
        integer :: nh, nhp
        real, intent(in) :: a(N,N),b(N,N)
        real :: c(N,N)
        real, dimension(N/2,N/2) :: a11,a21,a12,a22,b11,b21,b12,b22 
        real, dimension(N/2,N/2) :: q1,q2,q3,q4,q5,q6,q7
        nh = n/2
        nhp = nh+1
    
        if(iand(N,1) /= 0 .OR. N < 128) then
            c = classic_omp(a,b,n)
        else
            a11 = a(:nh,:nh)
            a21 = a(nhp:,:nh)
            a12 = a(:nh,nhp:)
            a22 = a(nhp:,nhp:)
            b11 = b(:nh,:nh)
            b21 = b(nhp:,:nh)
            b12 = b(:nh,nhp:)
            b22 = b(nhp:,nhp:)
            q1 = recursive_strassen(a11+a22,b11+b22,nh)
            q2 = recursive_strassen(a21+a22,b11,nh)
            q3 = recursive_strassen(a11,b12-b22,nh)
            q4 = recursive_strassen(a22,-b11+b21,nh)
            q5 = recursive_strassen(a11+a12,b22,nh)
            q6 = recursive_strassen(-a11+a21,b11+b12,nh)
            q7 = recursive_strassen(a12-a22,b21+b22,nh)
            c(:nh,:nh) = q1+q4-q5+q7
            c(nhp:,:nh) = q2+q4
            c(:nh,nhp:) = q3+q5
            c(nhp:,nhp:) = q1+q3-q2+q6
        end if
    end function recursive_strassen_omp

    function strassen_omp(A,B,n) result(C)
        use classic_lib, only : classic_omp
        use omp_lib
        implicit none
        integer, intent(in) :: n
        integer :: nh, nhp
        real, intent(in) :: A(:,:), B(:,:)
        real :: C(n,n)
        real, dimension(n/2,n/2) :: P1, P2, P3, P4, P5, P6, P7
        nh = n/2
        nhp = nh+1
        RT1 = omp_get_wtime()
        P1 = classic_omp((A(1:nh,1:nh)+A(nhp:n,nhp:n)),(B(1:nh,1:nh)+B(nhp:n,nhp:n)), nh)
        P2 = classic_omp((A(nhp:n,1:nh)+A(nhp:n,nhp:n)),B(1:nh,1:nh), nh)
        P3 = classic_omp(A(1:nh,1:nh),(B(1:nh,nhp:n)-B(nhp:n,nhp:n)), nh)
        P4 = classic_omp(A(nhp:n,nhp:n),(B(nhp:n,1:nh)-B(1:nh,1:nh)), nh)
        P5 = classic_omp((A(1:nh,1:nh) + A(1:nh,nhp:n)),B(nhp:n,nhp:n), nh)
        P6 = classic_omp((A(nhp:n,1:nh)-A(1:nh,1:nh)),(B(1:nh,1:nh)+B(1:nh,nhp:n)), nh)
        P7 = classic_omp((A(1:nh,nhp:n) - A(nhp:n,nhp:n)),(B(nhp:n,1:nh)+B(nhp:n,nhp:n)), nh)
        C(1:nh,1:nh) = P1 + P4 - P5 + P7
        C(1:nh,nhp:n) = P3 + P5
        C(nhp:n,1:nh) = P2 + P4
        C(nhp:n,nhp:n) = P1 - P2 + P3 + P6
        RT2 = omp_get_wtime()
    end function strassen_omp

    function strassen_mpi(A,B,n) result(C)
        use classic_lib, only : classic_mpi
        use mpi_manager
        implicit none
        integer, intent(in) :: n
        integer :: nh, nhp
        real, intent(in) :: A(:,:),B(:,:)
        real :: C(n,n)
        real, dimension(n/2,n/2) :: P1, P2, P3, P4, P5, P6, P7
        nh = n/2
        nhp = nh+1
        RT1 = MPI_Wtime()
        P1 = classic_mpi((A(1:nh,1:nh)+A(nhp:n,nhp:n)),(B(1:nh,1:nh)+B(nhp:n,nhp:n)), nh)
        P2 = classic_mpi((A(nhp:n,1:nh)+A(nhp:n,nhp:n)),B(1:nh,1:nh), nh)
        P3 = classic_mpi(A(1:nh,1:nh),(B(1:nh,nhp:n)-B(nhp:n,nhp:n)), nh)
        P4 = classic_mpi(A(nhp:n,nhp:n),(B(nhp:n,1:nh)-B(1:nh,1:nh)), nh)
        P5 = classic_mpi((A(1:nh,1:nh) + A(1:nh,nhp:n)),B(nhp:n,nhp:n), nh)
        P6 = classic_mpi((A(nhp:n,1:nh)-A(1:nh,1:nh)),(B(1:nh,1:nh)+B(1:nh,nhp:n)), nh)
        P7 = classic_mpi((A(1:nh,nhp:n) - A(nhp:n,nhp:n)),(B(nhp:n,1:nh)+B(nhp:n,nhp:n)), nh)
        C(1:nh,1:nh) = P1 + P4 - P5 + P7
        C(1:nh,nhp:n) = P3 + P5
        C(nhp:n,1:nh) = P2 + P4
        C(nhp:n,nhp:n) = P1 - P2 + P3 + P6
        RT2 = MPI_Wtime()
    end function strassen_mpi

    function timing(a,b,n,repeats) result(res)
        implicit none
        integer :: i
        integer(kind=8) :: rate
        integer, intent(in) :: repeats, n
        real :: t(n,n)
        real, intent(in) :: a(:,:), b(:,:)
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            t = strassen(a, b, n)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing

    function timing_recursive(a,b,n,repeats) result(res)
        implicit none
        integer :: i
        integer(kind=8) :: rate
        integer, intent(in) :: repeats, n
        real :: t(n,n)
        real, intent(in) :: a(:,:), b(:,:)
        double precision :: res(repeats)
        CALL system_clock(count_rate=rate)
        do i = 1,repeats
            call SYSTEM_CLOCK(T1)
            t = recursive_strassen(a, b, n)
            call SYSTEM_CLOCK(T2)
            res(i) = dble(T2 - T1) / dble(rate)
        enddo
    end function timing_recursive

    function timing_recursive_omp(a,b,n,repeats) result(res)
        use omp_lib
        implicit none
        integer :: i
        integer, intent(in) :: repeats, n
        real :: t(n,n)
        real, intent(in) :: a(:,:), b(:,:)
        double precision :: res(repeats)
        do i = 1,repeats
            RT1 = omp_get_wtime()
            t = recursive_strassen_omp(a, b, n)
            RT2 = omp_get_wtime()
            res(i) = RT2 - RT1
        enddo
    end function timing_recursive_omp

    function timing_omp(a,b,n,repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: repeats, n
        real :: t(n,n)
        real, intent(in) :: a(:,:), b(:,:)
        double precision :: res(repeats)
        do i = 1,repeats
            t=strassen_omp(a,b,n)
            res(i) = RT2 - RT1
        enddo
    end function timing_omp

    function timing_mpi(a,b,n,repeats) result(res)
        implicit none
        integer :: i
        integer, intent(in) :: repeats, n
        real :: t(n,n)
        real, intent(in) :: a(:,:), b(:,:)
        double precision :: res(repeats)
        do i = 1,repeats
            t = strassen_mpi(a, b, n)
            res(i) = RT2 - RT1
        enddo
    end function timing_mpi
end module strassen_lib