program force
    implicit none
    real (kind=8) :: Uabp,dx,dy,dz,L,rho,rij2,rij2inv,rij6,fijx,fijy,fijz,distance,T,Ke,E,temp
    real(KIND=8), dimension (216,3) :: position, f_now
    integer :: i,j,flag,index,count,Kb,x,y,z
    rho = 0.5
    L = real((216/rho) ** real(1./3.))
    T = 0.728
    Kb = 1 
    count = 2
    position(1,1)=  rand() * L
    position(1,2)=rand() * L
    position(1,3)=rand() * L
    flag =0 
    do while (2>1)
        position(count,1)=  rand() * L
        position(count,2)= rand() * L
        position(count,3)= rand() * L
        do index = 1, count-1
            distance = ((position(index,1) - position(count,1))**2 + (position(index,2) - position(count,2))**2 )
            distance = distance + (position(index,3) - position(count,3))**2
            if ( distance <= 1 ) then
                flag = 10
                exit
            end if  
        end do
        if (count == 217) then
            exit
        end if 
        if (flag == 0) then 
            count = count  + 1
        end if 
        flag = 0
    end do

    ! fn is intialized zero and will be calculated now
    do i = 1,216
        do j = 1, 3
            f_now(i,j) = 0
        end do
    end do

    
    
    !potential energy is set zero every time step
    Uabp=0.0
    do i = 1, 215
        do j = i+1,216
            dx = position(i,1)-position(j,1)
            dy = position(i,2)-position(j,2)
            dz = position(i,3)-position(j,3)

            if ( abs(dx ) > (L/2.0))then
                dx = dx - (L * ( dx / abs(dx)))
            end if
            if ( abs(dy ) > (L/2.0))then
                dy = dy - (L * ( dy / abs(dy)))
            end if
            if ( abs(dz ) > (L/2.0))then
                dz = dz - (L * ( dz / abs(dz)))
            end if

            rij2 = (dx * dx )+ (dy * dy) + (dz * dz) 

            if ( (rij2) < ((2**(1./3.))) ) then
                rij2inv=(1.0/rij2)                     ! 1/r^2
                rij6=rij2inv*rij2inv*rij2inv              !   (1/r)^6
                fijx=24.0*rij6*(2.0*rij6-1.0)*dx*rij2inv      !  Force according to LG in x direction
                fijy=24.0*rij6*(2.0*rij6-1.0)*dy*rij2inv      ! Force according to LG in y direction
                fijz=24.0*rij6*(2.0*rij6-1.0)*dz*rij2inv      ! Force according to LG in z direction
                f_now(i,1)= f_now(i,1) + fijx                              ! Force on particle i due to j
                f_now(i,2)= f_now(i,2) + fijy
                f_now(i,3)= f_now(i,3) + fijz
                f_now(j,1)= f_now(j,1) + (-fijx)                             ! Force on particles j due to i (Newton's 3 law)
                f_now(j,2)= f_now(j,2) + (-fijy)
                f_now(j,3)= f_now(j,3) + (-fijz)
                temp = (4.0 * rij6 * (rij6-1.0) ) + 1
                print * , temp
                Uabp = Uabp + temp                ! Potential Energy
            end if
        end do
    end do
    
    print *, " Potential Energy ",Uabp
    Ke = (3.0/2.0) *  T * Kb*216
    print * ," Kinetic Energy ", Ke
    print * ," Total Energy ", Ke+Uabp
end program force