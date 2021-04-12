{
 double f_c = 4.0 * (12.0 / pow(r_c, 13) - 6.0 / pow(r_c, 7));
 double v_c = 4.0 * (1.0 / pow(r_c, 12) - 1.0 / pow(r_c, 6)) + f_c * r_c;
 for(int i=0;i<number_particles;i++)                   //loop for number of particels
    for(int j=0;j<2;j++)                                  //loop for dimensionality
    {
    f_now[i][j]=0;                                     //fn is intialized zero and will be calculated now

    }
    Uabp=0.0;                                          //potential energy is set zero every time step
 for(int i=0;i<number_particles-1;i++)
  {
    for(int j=i+1;j<number_particles;j++)
    {
        double dx=position[i][0]-position[j][0];       //original difference in position in x direction
        double dy=position[i][1]-position[j][1];       //original difference  in position in y direction
      if (fabs(dx)>(l_x/2.0)){
        dx=dx-l_x*(dx/fabs(dx));
      }

      if (fabs(dy)>(l_x/2.0)){
        dy=dy-l_x*(dy/fabs(dy));
      }

      double rij2=dx*dx + dy*dy;                      // distance between i,j pair

      if((rij2)<(4))                            // Minimum cutoff potential condition
      {

      double rij2inv=(1.0/rij2);                     //1/r^2
      double rij6=rij2inv*rij2inv*rij2inv;               // (1/r)^6
      fijx=24.0*rij6*(2.0*rij6-1.0)*dx*rij2inv - f_c*dx*sqrt(rij2inv);     // Force according to LG in x direction
      fijy=24.0*rij6*(2.0*rij6-1.0)*dy*rij2inv - f_c*dy*sqrt(rij2inv);     //Force according to LG in y direction
      f_now[i][0]+=fijx;                              //Force on particle i due to j
      f_now[i][1]+=fijy;
      f_now[j][0]+=-fijx;                             //Force on particles j due to i (Newton's 3 law)
      f_now[j][1]+=-fijy;
      Uabp+=4.rij6(rij6-1.) + f_c*sqrt(rij2)- v_c;                //Potential Energy
    }
    }

  }