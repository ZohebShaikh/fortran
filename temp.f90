program calling_func

    ! real :: r
    ! real :: a
    ! a = area_of_circle(2.0) 
    ! Print *, "The area of a circle with radius 2.0 is"
    ! Print *, a
    print *,r
    call random_number(r)
    print *, r
    do while (2>1)
        ! call random_number(r)
        
        print * ,rand()
        
    end do
 end program calling_func
 
 
!  ! this function computes the area of a circle with radius r  
!  function area_of_circle (r)  
 
!  ! function result     
!  implicit none      
 
!     ! dummy arguments        
!     real :: area_of_circle   
    
!     ! local variables 
!     real :: r     
!     real :: pi
    
!     pi = 4 * atan (1.0)     
!     area_of_circle = pi * r**2  
    
!  end function area_of_circle

! program test_rand
!     integer,parameter :: seed = 86456
    
!     call srand(seed)
!     print *, rand(), rand(), rand(), rand()
!     print *, rand(seed), rand(), rand(), rand()
!   end program test_rand