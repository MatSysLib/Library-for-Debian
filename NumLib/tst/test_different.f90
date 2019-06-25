real function f(x) result(res)
    implicit none
    real, intent(in) :: x
    res = x**6
end function f

program test_differentiation
    use num_diff_lib
    implicit none
    real, external :: f
    print *, "Starting function - x^6"
    print *, "Symbolic differential: 6*x^5"
    print *, "Precise value for second derivation: 6"
    print *, "D1 2P FORWARD: ", d1x_2p_fd(1., 0.0001, f)
    print *, "D1 2P CENTRAL: ", d1x_2p_cd(1., 0.0001, f)
    print *, "D1 5P: ", d1x_5p(1., 0.0001, f)
    print *, "D2 5P: ", d2x_5p(1., 0.0001, f)
    print *, "D3 5P: ", d3x_5p(1., 0.0001, f)
    print *, "D4 5P: ", d4x_5p(1., 0.0001, f)
!TODO: OPENMP
end program test_differentiation