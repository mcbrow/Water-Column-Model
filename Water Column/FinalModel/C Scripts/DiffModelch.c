#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <unistd.h>




struct par
{
  double z[74];
  double s[74];
  double r[74];
  double m[74];
  double Ipm[74];
  double theta0[74];
  double Hn[74];
  double HL[74];
  double kappab[74];
  double kappap[74];
  double V[74];
  double depth[75];
  double odechoice[1];
  double sinkingchoice[1];
  double growthchoice[1];
  double n[1];
  
  
};

struct par parms; 



/* initialiser */


/* Initialises Parameters to provide link between R and C. Parameters must be in same
order in R as they are in C, the structure is populated in the order declared. 
 Not using macros means if you miss typing a parameter it is easier to add it in. 

First element is the number of parameters copied over from R. int N=... must be the same
 number of parameters as in R or it will give an error. 
 */
void initmod(void(* odeparms)(int *, double *,  double *, double *, 
                  double *, double *, double *, double *, double *, 
                  double *, double *,  double *, double *, double *,
                  double *, double *, double *, double *, double *,
                  double *))
{
  int N=1115;
 
  odeparms(&N, &parms.z[0], &parms.s[0], &parms.r[0], &parms.m[0], 
           &parms.Ipm[0],
            &parms.theta0[0], &parms.HL[0],
            &parms.HL[0], &parms.kappab[0], &parms.kappap[0],
            &parms.V[0], &parms.depth[0], &parms.odechoice[0],
            &parms.sinkingchoice[0], &parms.growthchoice[0], 
            &parms.n[0]);
  
  
  
}


struct force{
  
  double k[75];
  double theta[75];
  double Ls[75];

  };



struct force forc;

void forcc(void (* odeforcs)(int*, double*, double *, double *))
{
  int N=225;
  odeforcs(&N, &forc.k[0], &forc.theta[75], &forc.Ls[75]);
  }




/* Driving Data Equations */


/* Balanced Equations */

void derivsc (int *neq, double *t, double *y, double *ydot)
{ 

  /* Dynamic Variables */
  
/* Create a structure to store calculations for dynamic variables. Faster
in C than in R. Structures are faster and less laborious to type out than macros */  
  struct var{
    double kd[75];
    double sd[75];
    double md[75];
    double gz[75];
    double fz[75];
    double rz[75];
    double Ip[75];
    double kappa[75];
    double Up[75];
    double Stheta[75];
    double  L[75];
    };
  
  struct var dyn;
  for(int i=0; i<=(int)parms.n[0]; ++i){
    
  dyn.kd[0]=0;
  dyn.fz[0]=0;
    }
  
  for(int i=1; i<=(int)parms.n[0]; ++i){
    
    
    
  dyn.kd[i]=forc.k[i-1]/(parms.z[i-1]+parms.z[i]);
    
    /* Vertical Diffusion Flux */
  dyn.fz[i]=dyn.kd[i]*((y[i]/parms.z[i])-(y[i-1]/parms.z[i-1]));
  }
  
  for(int i=0; i<(int)parms.n[0]; ++i){
    /* Gravitational Loss Rate */
    dyn.sd[i]=parms.s[i]/parms.z[i];
    /*Density Dependent Loss Rate */
    dyn.md[i]=parms.m[i]/pow(parms.z[i],2);
    /* Sinking Flux */
    dyn.gz[i]=dyn.sd[i]*y[i];
    /* Growth in the surface cell */
    dyn.rz[i]=parms.r[i]*y[i];
    /* */
    dyn.Stheta[i]=1-exp(-forc.theta[i]/parms.theta0[i]);
    /* */
    dyn.L[i]=forc.Ls[i]*exp(-dyn.kappa[i]*parms.z[i]);
    /* */ 
    dyn.Ip[i]=parms.Ipm[i]*dyn.Stheta[i];
    /* */
    dyn.kappa[i]=parms.kappab[i]+parms.kappap[i]*ydot[i]/parms.V[i];
    /* */
    dyn.Up[i]=ydot[(int)parms.n[75]+1]*dyn.Ip[i]/dyn.kappa[i]*parms.depth[i]*
    ydot[i]/ydot[i]+(parms.V[i]*parms.Hn[i])*log((forc.Ls[i]+parms.HL[i])/
      (forc.Ls[i]*exp((-dyn.kappa[i]*parms.depth[i])+parms.HL[i])));
  }
  
 /* Switching function between sinking/no sinking
  This is called as a parameter in same way as 'n' was and 
  converted from double to integer */ 
  switch((int)parms.sinkingchoice[0]){
  
  case 1:
  dyn.gz[(int)parms.n[0]] = 0;
    break;
    
  case 2:
  dyn.gz[(int)parms.n[0]] = (parms.s[(int)parms.n[0]]/parms.z[(int)parms.n[0]])*
                              y[(int)parms.n[0]];
  break;
  
    
 }
  
  
  /* Switching function between growth/no growth
   This is called as a parameter */ 
 switch((int)parms.growthchoice[0]){
 
 case 1:
  dyn.rz[(int)parms.n[0]] =  0;
   break;
   
 case 2:
   dyn.rz[(int)parms.n[0]] =  parms.r[(int)parms.n[0]]*y[(int)parms.n[0]];
   break;
   
 }  
  
  
  
  
  
  /* Derivatives */
  
  /* The indexing element of an array must be an integer, however the structure element parms.n[0] is called from R 
   as a double by default. To fix this coerce it into integer form.
  This is referred to in C as typecasting and the command to do this is (...), where in
   this case ... = int. */
  
  
    ydot[0]                 =    -dyn.fz[0]  +  dyn.fz[1]       -dyn.gz[0]  +        dyn.rz[0]           -dyn.md[0]*pow(y[0],2) + dyn.Up[0];
  
  for(int i=1; i<(int)parms.n[0]; ++i){
    
    ydot[i]                 =    -dyn.fz[i] +  dyn.fz[i+1]      -dyn.gz[i]  +             dyn.gz[i-1]  +               dyn.rz[i]           -dyn.md[i]*pow(y[i],2) + dyn.Up[i];
  }
  
  switch((int)parms.odechoice[0]){
  
  
  case 1:
    
    ydot[(int)parms.n[0]]  = 0;
    
    ydot[(int)parms.n[0]+1] = 0;
    
    break;
    
    
  case 2:
    
    ydot[(int)parms.n[0]]   =   -dyn.fz[(int)parms.n[0]] +          -dyn.gz[(int)parms.n[0]] +     dyn.gz[(int)parms.n[0]-1]  +  dyn.rz[(int)parms.n[0]]  -dyn.md[(int)parms.n[0]]*pow(y[(int)parms.n[0]],2) + dyn.Up[(int)parms.n[0]];
    
    ydot[(int)parms.n[0]+1] = 0;
    
    break;
      
      
  case 3:
    ydot[(int)parms.n[0]]   =    -dyn.fz[(int)parms.n[0]] +          -dyn.gz[(int)parms.n[0]] +     dyn.gz[(int)parms.n[0]-1]  +  dyn.rz[(int)parms.n[0]]  -dyn.md[(int)parms.n[0]]*pow(y[(int)parms.n[0]],2) + dyn.Up[(int)parms.n[0]] ;
  
    ydot[(int)parms.n[0]+1] =                                            dyn.gz[(int)parms.n[0]] + dyn.Up[(int)parms.n[0]+1];
    
   break;
   
  

}
 for(int i=(int)parms.n[0]+2; i<=74; ++i){
     
     ydot[i] = 0;
     
}
  
    
  
  
}





