#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>

/* Parameters */
static double parms[50];
/* Thickness */

#define z0 parms[0]
#define z1 parms[1]
#define z2 parms[2]
#define z3 parms[3]
#define z4 parms[4]
#define z5 parms[5]
#define z6 parms[6] 
#define z7 parms[7]
#define z8 parms[8]
#define z9 parms[9]

/* Sinking Rate */ 

#define s0 parms[10]
#define s1 parms[11]
#define s2 parms[12]
#define s3 parms[13]
#define s4 parms[14]
#define s5 parms[15]
#define s6 parms[16] 
#define s7 parms[17]
#define s8 parms[18]
#define s9 parms[19]

/* Density Dependent Mortality */

#define m0 parms[20]
#define m1 parms[21]
#define m2 parms[22]
#define m3 parms[23]
#define m4 parms[24]
#define m5 parms[25]
#define m6 parms[26] 
#define m7 parms[27]
#define m8 parms[28]
#define m9 parms[29]


/* Reproduction Rate */

#define r0 parms[30]
#define r1 parms[31]
#define r2 parms[32]
#define r3 parms[33]
#define r4 parms[34]
#define r5 parms[35]
#define r6 parms[36] 
#define r7 parms[37]
#define r8 parms[38]
#define r9 parms[39]

/* Thickness */

#define k0 parms[40]
#define k1 parms[41]
#define k2 parms[42]
#define k3 parms[43]
#define k4 parms[44]
#define k5 parms[45]
#define k6 parms[46] 
#define k7 parms[47]
#define k8 parms[48]
#define k9 parms[49]




/* initialiser */

void odec(void(* odeparms)(int *, double *))
{
  int N=50;
  odeparms(&N, parms);
  
}

/* Driving Data Equations */

static double fz0;
static double fz1;
static double fz2;
static double fz3;
static double fz4;
static double fz5;
static double fz6;
static double fz7;
static double fz8;
static double fz9;
static double gz0;
static double gz1;
static double gz2;
static double gz3;
static double gz4;
static double gz5;
static double gz6;
static double gz7;
static double gz8;
static double gz9;
static double rz0;
static double rz1;
static double rz2;
static double rz3;
static double rz4;
static double rz5;
static double rz6;
static double rz7;
static double rz8;
static double rz9;
static double md0;
static double md1;
static double md2;
static double md3;
static double md4;
static double md5;
static double md6;
static double md7;
static double md8;
static double md9;
static double kd0;
static double kd1;
static double kd2;
static double kd3;
static double kd4;
static double kd5;
static double kd6;
static double kd7;
static double kd8;
static double kd9;
static double sd0;
static double sd1;
static double sd2;
static double sd3;
static double sd4;
static double sd5;
static double sd6;
static double sd7;
static double sd8;
static double sd9;





/* Balanced Equations */

void derivsc (int *neq, double *t, double *y, double *ydot, double *yout, 
              int *ip)
 { 

  
  /* Dynamic Variables */
  
  
  
  /* Diffusivity Gradient */
  
  kd0=0;
  kd1=k0/(z0+z1);
  kd2=k1/(z1+z2);
  kd3=k2/(z2+z3);
  kd4=k3/(z3+z4);
  kd5=k4/(z4+z5);
  kd6=k5/(z5+z6);
  kd7=k6/(z6+z7);
  kd8=k7/(z7+z8);
  kd9=k8/(z8+z9);
  
  /* Gravitational Loss Rate */
  
  sd0=s0/z0;
  sd1=s1/z1;
  sd2=s2/z2;
  sd3=s3/z3;
  sd4=s4/z4;
  sd5=s5/z5;
  sd6=s6/z6;
  sd7=s7/z7;
  sd8=s8/z8;
  sd9=s9/z9;
  
  
  /*Density Dependent Loss Rate */
  
  
  md0=m0/pow(z0, 2);
  md1=m1/pow(z1, 2);
  md2=m2/pow(z2, 2);
  md3=m3/pow(z3, 2);
  md4=m4/pow(z4, 2);
  md5=m5/pow(z5, 2);
  md6=m6/pow(z6, 2);
  md7=m7/pow(z7, 2);
  md8=m8/pow(z8, 2);
  md9=m9/pow(z9, 2);
  
  /* Vertical Diffusion Flux */
  fz0=0;
  fz1=kd1*((y[1]/z1)-(y[0]/z0));
  fz2=kd2*((y[2]/z2)-(y[1]/z1));
  fz3=kd3*((y[3]/z3)-(y[2]/z2));
  fz4=kd4*((y[4]/z4)-(y[3]/z3));
  fz5=kd5*((y[5]/z5)-(y[4]/z4));
  fz6=kd6*((y[6]/z6)-(y[5]/z5));
  fz7=kd7*((y[7]/z7)-(y[6]/z6));
  fz8=kd8*((y[8]/z8)-(y[7]/z7));
  fz9=kd9*((y[9]/z9)-(y[8]/z8));
  
  
  /* Sinking Flux */
  
  gz0=sd0*y[0];
  gz1=sd1*y[1];
  gz2=sd2*y[2];
  gz3=sd3*y[3];
  gz4=sd4*y[4];
  gz5=sd5*y[5];
  gz6=sd6*y[6];
  gz7=sd7*y[7];
  gz8=sd8*y[8];
  gz9=sd9*y[9];
  
  
  /* Growth in the surface cell */
  
  rz0=r0*y[0];
  rz1=r1*y[1];
  rz2=r2*y[2];
  rz3=r3*y[3];
  rz4=r4*y[4];
  rz5=r5*y[5];
  rz6=r6*y[6];
  rz7=r7*y[7];
  rz8=r8*y[8];
  rz9=r9*y[9];
  
  
  


/* Derivatives */


ydot[0] = -fz0 + fz1 -gz0 +       rz0 -md0*pow(y[0],2);
ydot[1] = -fz1 + fz2 -gz1 + gz0 + rz1 -md1*pow(y[1],2);
ydot[2] = -fz2 + fz3 -gz2 + gz1 + rz2 -md2*pow(y[2],2);
ydot[3] = -fz3 + fz4 -gz3 + gz2 + rz3 -md3*pow(y[3],2);
ydot[4] = -fz4 + fz5 -gz4 + gz3 + rz4 -md4*pow(y[4],2);
ydot[5] = -fz5 + fz6 -gz5 + gz4 + rz5 -md5*pow(y[5],2);
ydot[6] = -fz6 + fz7 -gz6 + gz5 + rz6 -md6*pow(y[6],2);
ydot[7] = -fz7 + fz8 -gz7 + gz6 + rz7 -md7*pow(y[7],2);
ydot[8] = -fz8 + fz9 -gz8 + gz7 + rz8 -md8*pow(y[8],2);
ydot[9] = -fz9       -gz9 + gz8 + rz9 -md9*pow(y[9],2);
ydot[10] = gz9;


}