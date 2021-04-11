program md
    implicit none
    real(KIND=8), dimension (216,3) :: positions
    real(KIND=8),dimension(216) :: distance
    integer :: i,j,count,index
    real :: r
    integer :: flag
    real(kind=8) :: rho , L, distance_squared,distance_temp
    rho = 0.5
    L = real((216/rho) ** real(1./3.))
    print * , L
    count = 2
    positions(1,1)=  rand() * L
    positions(1,2)=rand() * L
    positions(1,3)=rand() * L
    flag =0 
    distance(1)  = positions(1,1)**2 + positions(1,2)**2 + positions(1,3)**2
    do while (2>1)
        positions(count,1)=  rand() * L
        positions(count,2)= rand() * L
        positions(count,3)= rand() * L
        distance_temp = (positions(count,1)**2 + positions(count,2)**2 + positions(count,3)**2)
        do index = 1, count
            distance_squared = abs (distance(index)-  distance_temp)
            if ( distance_squared < 1 ) then
                flag = 10
                exit
            end if  
        end do
        if (flag == 0) then 
            distance(count) = distance_temp
            count = count  + 1
        end if 
        flag = 0
        print *, count
        if (count == 100) then
            exit
        end if 
    end do
    do index = 1, 216
        print *,index
        print *,  positions(index,1)
        print *,  positions(index,2)
        print *,  positions(index,3)
    end do
end program md

! recursive subroutine check_particles(end,positions )
! implicit none

!     integer, intent (in) :: end
!     real(KIND=8), dimension (216,3) , intent(inout) :: positions
!     real (KIND=8):: distance_squared 
!     integer :: j
!     do j = 1, end
!         distance_squared = (positions(end,1)**2 + positions(end,2)**2 + positions(end,3)**2) 
!         distance_squared = abs (distance_squared - (positions(j,1)**2 + positions(j,2)**2 + positions(j,3)**2) )
!         ! print * , distance_squared
!         if ( distance_squared < 1 ) then
!             positions(end,1)=  rand() * 7.56
!             positions(end,2)=rand() * 7.56
!             positions(end,3)=rand() * 7.56
!             call check_particles(end-1 , positions)
!         end if
!     end do
! end subroutine check_particles 