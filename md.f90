program md
    implicit none
    real(KIND=8), dimension (216,3) :: positions
    integer :: i,j
    real :: r
    real(kind=8) :: rho , L, x_temp, y_temp,z_temp, distance_squared
    
    L = 7.56
    rho = 0.5

    do i = 1, 10
        positions(i,1)=  rand() * L
        positions(i,2)=rand() * L
        positions(i,3)=rand() * L
        call check_particles(i,positions)
    end do
    print *,  positions
end program md

recursive subroutine check_particles(end,positions )
implicit none

    integer, intent (in) :: end
    real(KIND=8), dimension (216,3) , intent(inout) :: positions
    real (KIND=8):: distance_squared 
    integer :: j
    do j = 1, end
        distance_squared = (positions(end,1)**2 + positions(end,2)**2 + positions(end,3)**2) 
        distance_squared = abs (distance_squared - (positions(j,1)**2 + positions(j,2)**2 + positions(j,3)**2) )
        ! print * , distance_squared
        if ( distance_squared < 1 ) then
            positions(end,1)=  rand() * 7.56
            positions(end,2)=rand() * 7.56
            positions(end,3)=rand() * 7.56
            call check_particles(end-1 , positions)
        end if
    end do
end subroutine check_particles 