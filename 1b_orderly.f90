program md
    implicit none
    real(KIND=8), dimension (216,3) :: positions
    integer :: flag,x,y,z,index
    real(kind=8) :: rho , L, distance

    rho = 0.5
    index =1 
    L = real((216/rho) ** real(1./3.))
    do x = 0 , 5
        do y = 0, 5
            do z = 0, 5
                positions(index,1) = x /( rho ** (1./3.))
                positions(index,2) = y /( rho ** (1./3.))
                positions(index,3) = z /( rho ** (1./3.))
                index = index  + 1
            end do
        end do
    end do
    do index = 1, 216
        print *,  positions(index,1)," ",positions(index,2)," ",positions(index,3)
    end do
end program md