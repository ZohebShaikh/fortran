program datatypes
    implicit none

    real , parameter :: PI = 3.1415
    real :: r_num1 = 0.0 , r_num2 = 0.0
    double precision :: dbl_num = 1.1111111111d+0
    integer :: i_num1 = 0, i_num2 = 0
    logical :: can_vote = .true.
    character (len = 10) :: month
    complex :: com_number = (2.0,4.0)

    print *, "Biggest Real ", huge(r_num1)
    print *, "Biggest Int ", huge(i_num1)
    print *, "Smallest Real", tiny(r_num1)
    ! print *, "Smallest Int ", tiny(i_num2)

    print *, "(a4, i1)", " Int", kind(i_num1)
    print *, "(a5, i1)", "Real", kind(r_num1)
    print *, "(a6, i1)", "Double", kind(dbl_num)
    print *, "(a7, i1)", "Logical", kind(can_vote)
end program datatypes