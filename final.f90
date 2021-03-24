! Name of program
! The f90 extension can be used for
! Fortran 2003 code
! Compile like this
! gfortran fortrantut.f90
program fortrantut

    ! A module we will use later compiled
    ! like this
    ! gfortran -c mult_mod.f90  fortrantut.f90
    ! Run like this
    ! gfortran mult_mod.o  fortrantut.o
    ! use mult_mod
    ! use shape
    use shape_mod
    use triangle_mod
  
    ! Forces you to declare all variables
    implicit none
  
    ! Create a variable that can hold up to
    ! 20 characters
    character*20 :: name
    
    ! Print a string to the screen
    ! * use default formatting
    print *, "What's your name : "
    
    ! Receive input up to a whitespace
    ! or newline
    read *, name
    
    ! Output the result
    print * , "Hello ", name
  
    character (len = 20) :: f_name, l_name
    print *, "Enter Name : "
    ! Read 2 values separated by a space
    read *, f_name, l_name
    ! Trim extra whitespace
    print *, "Hello ", trim(f_name), " ", trim(l_name)
  
    ! ----- VARIABLES / DATA TYPES -----
    ! Variables must start with a letter
    ! and then letters, numbers, _
    ! Variables are case insensitive
  
    ! Declare a constant that's value
    ! can't change
    real, parameter :: PI = 3.1415
    
    ! Numbers with decimals (floats)
    ! You can assign a value or leave undefined
    real :: r_num1 = 0.0, r_num2 = 0.0
    ! Doubles are accurate to 15 decimals
    double precision :: dbl_num = 1.1111111111111111d+0
    ! Numbers without decimals (whole numbers)
    integer :: i_num1 = 0, i_num2 = 0
    ! Boolean type
    logical :: can_vote = .true.
    ! Another way to declare a string
    character (len = 10) :: month
    ! Complex TYPES
    complex :: com_num = (2.0, 4.0)
    
    ! Get largest value for data types
    print *, "Biggest Real ", huge(r_num1)
    print *, "Biggest Int ", huge(i_num1)
  
    ! Get smallest value for data types
    print *, "Smallest Real ", tiny(r_num1)
    print *, "Smallest Int ", tiny(i_num1)
  
    ! Kind returns the number of bytes for each type
    print "(a4, i1)", "Int ", kind(i_num1)
    print "(a5, i1)", "Real ", kind(r_num1)
    print "(a7, i1)", "Double ", kind(dbl_num)
    print "(a8, i1)", "Logical ", kind(can_vote)
  
    ! ! ----- FORMATTED OUTPUT WITH PRINT -----
    ! character(len=5) :: i_char
    ! Integers are right justified by default
    print *, "A Number ", 10
    
    ! Integers are formatted like this RiW
    ! R : Number of times to use what follows per line
    ! W : Width to take up for each value
    print "(3i5)", 7, 6, 8
    print "(i5)", 7, 6, 8
    
    ! Floats are formatted like RfW.D
    ! R & W : Same as above
    ! D : Decimal places to show
    print "(2f8.5)", 3.1415, 1.234
    
    ! Characters & Strings are formatted RaW
    ! / Adds a newline
    print "(/, 2a8)", "Name", "Age"
    
    ! Exponential Notation ReW.D
    print "(e10.3)",123.456
    
    ! Use multiple types
    print "(a5,i2)", "I am ", 43
    
    ! Left justify Numbers
    ! Convert int 10 into a string
    write (i_char, "(i5)") 10
    
    ! Print formatted output left justified
    print "(a,a)", "A Number ", adjustl(i_char)
  
    ! ----- MATH OPERATORS -----
    real :: float_num = 1.111111111111111
    real :: float_num2 = 1.111111111111111
    double precision :: dbl_num = 1.1111111111111111d+0
    double precision :: dbl_num2 = 1.1111111111111111d+0
    real :: rand(1)
    integer :: low = 1, high = 10
    
    print "(a8,i1)", "5 + 4 = ", (5 + 4)
    print "(a8,i1)", "5 - 4 = ", (5 - 4)
    print "(a8,i2)", "5 * 4 = ", (5 * 4)
    print "(a8,i1)", "5 / 4 = ", (5 / 4)
    ! Modulus
    print "(a8,i1)", "5 % 4 = ", mod(5,4)
    ! Exponentiation
    print "(a7,i3)", "5**4 = ", (5**4)
    
    ! You get 6 digits of precision by default
    print "(f17.15)", float_num + float_num2
    
    ! Doubles are accurate to 15 digits
    print "(f18.16)", dbl_num + dbl_num2
  
    Generate random values between 1 and 10
    call random_number(rand)
    print "(i2)", low + floor((high + 1 - low)*rand)
  
    ! ----- Math Functions -----
    print "(a10,i1)", "ABS(-1) = ", ABS(-1)
    print "(a11,f3.1)", "SQRT(81) = ", SQRT(81.0)
    print "(a9,f7.5)", "EXP(1) = ", EXP(1.0)
    print "(a12,f7.5)", "LOG(2.71) = ", LOG(2.71)
    print "(a12,i1)", "INT(2.71) = ", INT(2.71)
    print "(a13,i1)", "NINT(2.71) = ", NINT(2.71)
    print "(a14,i1)", "FLOOR(2.71) = ", FLOOR(2.71)
    print "(a15,f3.1)", "MAX(2.7,3.4) = ", MAX(2.7,3.4)
    print "(a15,f3.1)", "MIN(2.7,3.4) = ", MIN(2.7,3.4)
    ! Trig functions use radians
    print "(a14,f3.1)", "SIN(1.5708) = ", SIN(1.5708)
    print "(a14,f3.1)", "COS(1.5708) = ", COS(1.5708)
    print "(a14,f3.1)", "TAN(1.5708) = ", TAN(1.5708)
    print "(a10,f3.1)", "ASIN(0) = ", ASIN(0.0)
    print "(a10,f3.1)", "ACOS(0) = ", ACOS(0.0)
    print "(a10,f3.1)", "ATAN(0) = ", ATAN(0.0)
  
    ! ----- CONDITIONALS -----
    ! Relational Operators : == /= > < >= <=
    ! Logical Operators : .and. .or. .not.
  
    ! If, else if, else
    integer :: age = 16
    if ((age >= 5) .and. (age <= 6)) then
      print *, "Kindergarten"
    else if ((age >= 7) .and. (age <= 13)) then
      print *, "Middle School"
    else if ((age >= 14) .and. (age <= 18)) then
      print *, "High School"
    else
      print *, "Stay Home"
    end if
    
    print *, .true. .or. .false.
    print *, .not. .true.
    print *, 5 /= 9
    
    ! Can be used with letters
    print *, "a" < "b"
    
    ! Select
    select case (age)
    case (5)
      print *, "Kindergarten"
    case (6:13)
      print *, "Middle School"
    case (14,15,16,17,18)
      print *, "High School"
    case default
      print *, "Stay Home"
    end select
  
    ! ----- LOOPING -----
    integer :: n = 0, m = 1
    integer :: secret_num = 7
    
    ! Start, Finish, Step
    do n = 1, 10, 2
      print "(i1)", n
    end do
    
    ! Exit & Cycle
    ! Print only evens
    do while (m < 20)
      if (MOD(m,2) == 0) then
        print "(i1)", m
        m = m + 1
        ! Jumps back to beginning of loop
        cycle
      end if
      m = m + 1
      if (m >= 10) then
        ! Exits the loop all together
        exit
      end if
    end do
    
    ! Continue looping while a condition is true
    do while (n /= secret_num)
      print *, "What's your guess "
      read *, n
    end do
    print *, "You guessed it!"
  
    ! ----- ARRAYS -----
    ! Create ARRAY
    integer, dimension(1:5) :: a1, a2, a3
    real, dimension(1:50) :: aR1
    ! Create multidimensional array (Matrix)
    integer, dimension(5,5) :: a4
    integer :: n, m, x, y
  
    Define an array thats size is determined
    at run time
    integer, dimension(:), allocatable :: a5
    integer :: num_vals = 0
    
    integer, dimension(1:9) :: a6 = (/ 1,2,3,4,5,6,7,8,9 /)
    integer, dimension(1:3,1:3) :: a7
    
    ! Assign values (Starts at index 1)
    a1(1) = 5
    ! Retrieve value
    print "(i1)", a1(1)
    
    ! Assign values with a loop
    do n = 1,5
      a1(n) = n
    end do
    do n = 1,5
      print "(i1)", a1(n)
    end do
    
    ! Get a range
    print "(3i2)", a1(1:3)
    
    ! Get a range with an increment
    print "(2i2)", a1(1:3:2)
    
    ! Assign values to a multidimensional array
    do n = 1,5
      do m = 1, 5
        a4(n,m) = n
      end do
    end do
    do n = 1,5
      do m = 1, 5
        print "(i1,a1,i1,a3,i1)", n, " ", m, " : ", a4(n,m)
      end do
    end do
    
    ! Use an implied do loop to print each row
    ! on one line
    do n = 1,5
      print "(5i1)", ( a4(n,m), m = 1,5 )
    end do
    
    ! Get size
    print "(i2)", Size(a1)
    print "(i2)", Size(a4)
    
    ! Number of dimensions
    print "(i2)", Rank(a4)
    
    ! Elements in each dimension
    print "(i2)", Shape(a4)
    
    ! Define array size at run time
    print *, "Size of array? "
    read *, num_vals
    allocate(a5(1:num_vals))
    do n = 1,num_vals
      a5(n) = n
    end do
    do n = 1,num_vals
      print "(i1)", a5(n)
    end do
    
    ! Change all values in array
    a2 = (/1,2,3,6,7/)
    
    ! Implied do loop
    print "(5i1)", ( a2(m), m = 1,5 )
    
    ! Reshape the ARRAY from 1x9 t0 3x3
    a7 = reshape(a6, (/ 3, 3 /))
    
    ! Check if values are equal across
    ! the 1 dimension
    print "(l1)", all(a1==a2, 1)
    
    ! Are any equal?
    print "(l1)", any(a1==a2, 1)
    
    ! How many are equal
    print "(i1)", count(a1==a2, 1)
    
    ! Get min and max value
    print "(i1)", maxval(a1)
    print "(i1)", minval(a1)
    
    ! Get product and sum
    print "(i3)", product(a1)
    print "(i2)", sum(a1)
  
    ! ----- FORMAT -----
    ! The format statement has a numbered
    ! label. You pass values to it that will
    ! fit into the designated formatting
    integer :: num
    integer :: cups
    real :: liters
    real :: quarts
    
    ! Print values 1 - 12 * 7
    do num = 1,12
      print 100, num, num * 7
    
      ! I designates an integer along with
      ! total space with values right justified
      100 format(I2,' * 7 = ',I3)
    end do
    
    ! / Adds a newline
    print "(/a18)", "Cups Liters Quarts"
    do cups = 1, 10
      liters = cups * .236
      quarts = cups * .208
      print 200 , cups,liters,quarts
    
      ! x defines spaces f is for floats
      200 format(' ',i3, 2x, f5.3, 2x, f5.3)
    end do
  
    ! ----- STRINGS -----
    ! Strings are character arrays
    character (len=30) :: str = "I'm a string"
    character (len=30) :: str2 = " that is longer"
    character (len=30) :: str3
    
    ! Join strings that have been trimmed of
    ! whitespace
    ! You can also trim right (adjustr) and
    ! left (adjustl)
    str3 = trim(str) // trim(str2)
    print *, str3
    
    ! Get a substring
    print *, str3(1:3)
    
    ! Find the index of a substring
    print "(a9,i1)", "Index at ", index(str, "string")
    
    ! Get size
    print *, len(str)
    
    ! Get number of items separated by a space
    print *, count_items(str)
  
    ! ----- STRUCTURES -----
    ! You can define custom types which contain
    ! multiple values of different types
    type Customer
      character (len = 40) :: name
      integer :: age
      real :: balance
    end type Customer
    
    type(Customer), dimension(5) :: customers
    
    integer :: n
    
    ! Create a customer
    type(Customer) :: cust1
    
    ! Assign values
    cust1%name = "Sally Smith"
    cust1%age = 34
    cust1%balance = 320.45
    
    ! Assign structure to array
    customers(1) = cust1
    
    ! Assign values independently
    customers(2)%name = "Tom May"
    customers(2)%age = 42
    customers(2)%balance = 229.78
    
    do n = 1, 2
      print *, customers(n)
    end do
  
    ! ----- FUNCTIONS -----
    ! Functions contain statements that return
    ! a single value
  
    integer :: ans, ans2
    real :: r_ans
    
    ans = get_sum(5,4)
    print "(a8,i1)", "5 + 4 = ", ans
    print "(a8,i1)", "5 + 4 = ", get_sum2(5,4)
    print "(a8,i1)", "5 + 4 = ", get_sum3(5)
    
    ! Use generic functions in a module that
    ! can work with ints and reals using
    ! the same function
    print "(a8,i2)", "5 * 4 = ", mult(5,4)
    r_ans = mult(5.3,4.4)
    print "(a12,f6.2)", "5.3 * 4.4 = ", r_ans
    
    ! Defines area for functions
    contains
    
      ! Return type, function, name, arguments
      integer function get_sum(n1, n2)
        implicit none
        integer :: n1, n2, sum
    
        ! The last value defined is returned
        sum = n1 + n2
      end function get_sum
    
      ! Define variable to be returned
      function get_sum2(n1, n2) result(sum)
        implicit none
    
        ! Don't allow variable values to change
        integer, intent(in) :: n1, n2
        integer :: sum
        sum = n1 + n2
      end function get_sum2
    
      ! Block functions from changing input
      ! variables with pure
      pure function get_sum3(n1, n2) result(sum)
        implicit none
        integer, intent(in) :: n1
    
        ! Arguments don't need to have a value passed
        integer, intent(in), optional :: n2
        integer :: sum
    
        if(present(n2)) then
          sum = n1 + n2
        else
          sum = n1 + 1
        end if
      end function get_sum3
  
    ! ----- RECURSIVE FUNCTIONS -----
    ! Recursive functions call themselves
    ! and must be labeled as such in Fortran
    integer :: ans
    ans = factorial(4)
    print "(a15,i3)", "Factorial(4) = ", ans
  
    ! 1st : result = 4 * factorial(3) = 4 * 6 = 24
    ! 2nd : result = 3 * factorial(2) = 3 * 2 = 6
    ! 3rd : result = 2 * factorial(1) = 2 * 1 = 2
  
  contains
  
    recursive function factorial(n) result(o)
      integer :: n, o
      if (n == 1) then
        o = 1
      else
        o = n * factorial(n - 1)
      end if
    end function
  
    ! ----- SUBROUTINES -----
    ! Subroutines can return multiple values
  
  integer :: i = 1, p1, p2
  call plus_two(i, p1, p2)
  print "(i1,/,i1,/,i1)", i, p1, p2
  
  contains
  
    subroutine plus_two(n, plus1, plus2)
      integer, intent(in) :: n
      integer, intent(out) :: plus1, plus2 ! Output
      plus1 = n + 1
      plus2 = n + 2
    end subroutine plus_two
  
    ! ----- POINTERS -----
    ! Declare a pointer to an integer
    integer, pointer :: ptr1, ptr2
    
    ! Pointer to an array
    integer, pointer, dimension(:) :: a_ptr1
    
    ! Declare a target whose value changes
    ! as the pointers value changes
    integer, target :: target1
    
    ! Allocate space for a pointer
    allocate(ptr1)
    ptr1 = 5
    print "(a5,i1)", "ptr1 ", ptr1
    
    ! Associate pointer with target
    ptr2 => target1
    ptr2 = 1
    
    ptr2 = ptr2 + 2
    print "(a5,i1)", "ptr1 ", ptr1
    print "(a5,i1)", "tar1 ", target1
    
    ! Disassociate pointer and target
    nullify(ptr2)
    
    ! Deallocate storage for pointer
    deallocate(ptr1)
  
    ! ----- FILE I/O -----
    character (len=100) :: str = "I'm a string"
    character (len=100) :: str2
    
    ! If set to anything other then 0 an
    ! error occurred when opening a file
    integer :: err_status
    
    ! Used to catch error messages
    CHARACTER(256) :: err_iomsg
    
    ! Open / Create a FILE
    ! The unit number must be unique for
    ! each file
    ! new (new file), old (exists),
    ! scratch (file deleted after use)
    open(10, file='data.dat', status='new', iostat = err_status, iomsg=err_iomsg)
    if(err_status /= 0) then
      write (*,*) 'Error ', trim(err_iomsg)
    
      ! Stop execution
      Stop
    end if
    
    ! Write string to file
    write (10, '(A)') str
    
    ! Close the file
    close(10)
    
    ! Open to read
    open(11, file='data.dat', status='old')
    
    ! Read from file
    read (11, '(A)') str2
    write (*, '(A)') trim(str2)
    
    ! Either KEEP or DELETE file when closed
    close(11, status="DELETE")
  
    ! ----- ANOTHER MODULE EXAMPLE -----
    ! Compile
    ! gfortran -c shape.f90  fortrantut.f90
    ! gfortran shape.o  fortrantut.o
    ! ./a.out
    call set_shape(10.5,20.5)
    call get_area()
  
    ! Compile
    ! gfortran -c shape_mod.f90 triangle_mod.f90  fortrantut.f90
    ! gfortran shape_mod.o triangle_mod.o  fortrantut.o
    type(triangle_m) :: tri
    tri%x = 10
    tri%y = 20
    print "(a3,f5.2)", "X: ", tri%x
    print "(a3,f5.2)", "Y: ", tri%y
    print "(a6,f6.2)", "Area: ", tri%get_area()
  
  ! Define the end of the program
  end program fortrantut