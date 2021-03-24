program print
    implicit none
    character (len=5) :: i_char


    print *, "A number ", 10

    ! RiW --> W is for width
    print "(3i5)", 7,6,8
    print "(i5)", 7,6,8

    ! RfW.D
    print "(2f8.5)", 3.1415, 1.234
    

    ! String
    print "(/, 2a8)", "Name", "Age"


    ! Exponential
    print "(e10.3)", 123.456

    ! 

    print "(a5, i2)" , "I am ", 23
    
    ! left justied


    write(i_char, "(i5)") 10

    print "(a,a)", "A number ", adjustl(i_char)

end program print