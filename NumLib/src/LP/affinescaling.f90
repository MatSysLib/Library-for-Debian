module affine_scaling_lib
    implicit none
    contains
    function affine_scaling(A, b, c, x, gamma) result(res)
        use classic_lib
        use invgj_lib
        use utilites_matrix_vector
        implicit none
        real, intent(in) :: gamma
        real, dimension(:), intent(in) :: c
        real, dimension(:) :: b, x
        real, dimension(:,:) :: A
        integer :: i, iteration
        real, dimension(size(b)) :: vector, hx, hv
        real, dimension(size(b),size(b)) :: diagonal
        real :: alpha, tmin
        logical :: contwork = .TRUE.
        iteration = 0
        do while(contwork.eqv..true.)
            vector = b - matrixvector(A, x, size(x))
            diagonal = vectortodiagonal(vector)
            hx = matrixvector(&
                    invgj(&
                            classic(classic(A, diagonalmatrixpower(diagonal, 2),size(x)), transpose(a), size(x)), size(x)&
                        ), &
                            c, size(x))
            hv = matrixvector(A, -1 * hx, size(x))
            if(hv>=0) then
                print *, vector
                print *, hx
                print *, hv
                print *, "Failure. Problem is unbounded."
                exit
            endif
            tmin = 999999
            do i = 1,size(x)
                if(hv(i)<0) then
                    alpha = -1*vector(i)/hv(i)
                    if(alpha<tmin) tmin = alpha
                endif
            enddo
            alpha = gamma * tmin
            x = x + alpha * hx
            iteration = iteration + 1
        enddo
    end function affine_scaling
end module affine_scaling_lib