program largest
    implicit none
    integer :: i = 1 ,temp =0
    integer:: first_largest =  (huge(temp) )*(-1) , second_largest =  (huge(temp) )*(-1)
    integer :: array(10)

    print * , "Please enter 10 numbers"
    do while(i <= 10)
        read * , temp
        array(i) = temp
        i = i +1
    end do
    
    print * , " The array is "
    print *, array

    ! ! First Method 
    ! first_largest = maxval(array)
    ! array(maxloc(array)) = (huge(temp) )*(-1) ! INT_MIN = -2147483647
    ! second_largest = maxval(array)

    ! Second Method
    i = 1
    do while ( i <= 10)
        if ( array(i) > first_largest ) then
            second_largest = first_largest
            first_largest = array(i)
        end if
        if (array(i) > second_largest .and. array(i) /= first_largest ) then
            second_largest = array(i)
        end if
        i = i+ 1
    end do


    print * ,"First Largest ", first_largest
    print * ,"Second Largest", second_largest

end program largest