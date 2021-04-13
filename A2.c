// to calculate force and total energy

#include<stdio.h>
#include<math.h>
#include<stdlib.h>

/********** For Merssene Twister(MT) Random Number Generator (Start) ************/

/*******  Source for the Generator :- http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/emt.html ******/

#define N 624

#define M 397

#define MATRIX_A 0x9908b0dfUL   /* constant vector a */

#define UPPER_MASK 0x80000000UL /* most significant w-r bits */

#define LOWER_MASK 0x7fffffffUL /* least significant r bits */



static unsigned long mt[N]; /* the array for the state vector  */

static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */



void init_genrand(unsigned long s);

unsigned long genrand_int32(void);

double genrand_real2(void);

/********* For Merssene Twister Random Number Generator (End) ************/

// declaring variables
int n = 216;
double ro = 0.5, radius=0.5;		//ro =density, r=radius of particle
double Temperature = 0.728, fx, fy, fz, U_rc, U, f_rc, KE, PE, TE, Force;
double x_rel, y_rel, z_rel, r;
double rc =2.5;
double l = 7.5595;
double T, Totalenergy,  net_force;

//Generating random numbers
double randomnumber(){
	double b=genrand_real2();	
	return b;
}


int main(){
	
	unsigned long a = 44859;

  init_genrand(a); 
  
  FILE* fp = fopen("A2_q2.txt","w");
  
  double *x = (double*) malloc(sizeof(double) * n);
	double *y = (double*) malloc(sizeof(double) * n);
	double *z = (double*) malloc(sizeof(double) * n);
	
	int m=1;
	for(int i=1; i<=6; i++){
		for(int j=1; j<=6; j++){
			for(int k=1; k<=6; k++){
				if(m<=n){
					x[m] = i*l/6;
					y[m] = j*l/6;
					z[m] = k*l/6;
				}
				//printf("%lf %lf %lf\n", x[m], y[m], z[m]);
				m++;
			}
		}
	}
	
	//potential at cut-off radius 
	
	// force at cut-off radius
	f_rc = 4.0*(12.0*pow(rc,-13) - 6.0*pow(rc,-7));
	U_rc = 4.0*(pow(rc,-12) - pow(rc,-6))+ f_rc* rc;
	
	PE = 0.0;
	net_force = 0.0;
	for(int i=1; i<=n; i++){
	//printf("%lf %lf %lf\n", x[i], y[i], z[i]);
		
		double l_half = 0.5*l;
		U=0.0;
		fx=0.0;
		fy=0.0;
		fz=0.0;
		for(int j=i+1; j<=n; j++){
			x_rel = x[i] - x[j];
			if(fabs(x_rel) > l_half && x_rel > 0) x_rel = l - x_rel;
			else if(fabs(x_rel) > l_half && x_rel < 0) x_rel = l + x_rel;
			
			y_rel = y[i] - y[j];
			if(fabs(y_rel) > l_half && y_rel > 0) y_rel = l - y_rel;
			else if(fabs(y_rel) > l_half && y_rel < 0) y_rel = l + y_rel;
			
			z_rel = z[i] - z[j];
			if(fabs(z_rel) > l_half && z_rel > 0) z_rel = l - z_rel;
			else if(fabs(z_rel) > l_half && z_rel < 0) z_rel = l + z_rel;
			
			r = sqrt(x_rel*x_rel + y_rel*y_rel + z_rel*z_rel);
			
			//printf("%lf %lf %lf %lf\n", x_rel, y_rel, z_rel, r);
		
			if(r<2.5){
				U = U + 4.0*(pow(r,-12) -pow(r,-6)) - U_rc;
				//force
				fx = fx + (4.0*( 12.0*pow(r,-13) - 6.0*pow(r,-7) ) - f_rc)*(x_rel/r);
				fy = fy + (4.0*( 12.0*pow(r,-13) - 6.0*pow(r,-7) ) - f_rc)*(y_rel/r);
				fz = fz + (4.0*( 12.0*pow(r,-13) - 6.0*pow(r,-7) ) - f_rc)*(z_rel/r);
				Force = sqrt(fx*fx + fy*fy + fz*fz);
				//printf("%lf %lf %lf %lf\n", U, fx, fy, fz);
			}
		}
		
		KE = 1.5*n*Temperature;
		PE = PE + U;
		Totalenergy = KE + PE;
		net_force = net_force + Force;
		printf("%d %lf %lf %lf \n", i, r, U, Force);
		//fprintf(fp," %lf %lf %lf \n", r, U, Force);
	}
	printf("\n");
	printf("Kinetic Energy : %lf Potential Energy: %lf Total Energy: %lf Net Force: %lf\n", KE, PE, Totalenergy, net_force);
	
	free(x);
	free(y);
	free(z);
	
	fclose(fp);
	return 0;
}
  

/*********Functions For Merssene Twister Random Number Generator (Start)************/

/* initializes mt[N] with a seed */

void init_genrand(unsigned long s)

{

    mt[0]= s & 0xffffffffUL;

    for (mti=1; mti<N; mti++) {

        mt[mti] =

	    (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti);

        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */

        /* In the previous versions, MSBs of the seed affect   */

        /* only MSBs of the array mt[].                        */

        /* 2002/01/09 modified by Makoto Matsumoto             */

        mt[mti] &= 0xffffffffUL;

        /* for >32 bit machines */

    }

}



/* generates a random number on [0,0xffffffff]-interval */

unsigned long genrand_int32(void)

{

    unsigned long y;

    static unsigned long mag01[2]={0x0UL, MATRIX_A};

    /* mag01[x] = x * MATRIX_A  for x=0,1 */



    if (mti >= N) { /* generate N words at one time */

        int kk;



        if (mti == N+1)   /* if init_genrand() has not been called, */

            init_genrand(5489UL); /* a default initial seed is used */



        for (kk=0;kk<N-M;kk++) {

            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);

            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];

        }

        for (;kk<N-1;kk++) {

            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);

            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];

        }

        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);

        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];



        mti = 0;

    }



    y = mt[mti++];



    /* Tempering */

    y ^= (y >> 11);

    y ^= (y << 7) & 0x9d2c5680UL;

    y ^= (y << 15) & 0xefc60000UL;

    y ^= (y >> 18);



    return y;

}





/* generates a random number on [0,1)-real-interval */

double genrand_real2(void)

{

    return genrand_int32()*(1.0/4294967296.0);

    /* divided by 2^32 */

}



/*********Functions For Merssene Twister Random Number Generator (End) ************/
  
  
  
  
  
  
  
  
  
  
  
  
  
	

