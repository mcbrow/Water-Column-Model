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
                  double *, double *, double *, double *, double *))
{
  int N=300;
 
  odeparms(&N, &parms.z[0], &parms.s[0], 
           &parms.r[0], &parms.m[0], &parms.odechoice[0],
            &parms.sinkingchoice[0], &parms.growthchoice[0], 
            &parms.n[0]);
  
  
  
}


struct force{
  
  double k[75];
  };



struct force forc;

void forcc(void (* odeforcs)(int*, double*))
{
  int N=75;
  odeforcs(&N, &forc.k[0]);
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
  }
  
 /* Switching function between sinking/no sinking
  This is called as a parameter in same way as 'n' was and 
  converted from double to integer*/ 
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
  
  /* The indexing element of an array must be an integer (i.e y[i], where i is the indexing element
  must be a positive integer from 0 to inf), however the structure element parms.n[0] is called from R 
   as a double as that was the only way to retrieve it from R. To fix this we have to coerce it into integer form.
  This is referred to in C as type casting and the command to do this is (...), where in
   this case ... = int. The brackets around int are important, this is the syntax for typecasting. It 
  won't work without the brackets.*/
  
  
    ydot[0]                 =    -dyn.fz[0]  +  dyn.fz[1]       -dyn.gz[0]  +                                          dyn.rz[0]           -dyn.md[0]*pow(y[0],2);
  
  for(int i=1; i<(int)parms.n[0]; ++i){
    
    ydot[i]                 =    -dyn.fz[i] +  dyn.fz[i+1]      -dyn.gz[i]  +             dyn.gz[i-1]  +               dyn.rz[i]           -dyn.md[i]*pow(y[i],2);
  }
  
  switch((int)parms.odechoice[0]){
  
  
  case 1:
    
    ydot[(int)parms.n[0]]  = 0;
    
    ydot[(int)parms.n[0]+1] = 0;
    
    break;
    
    
  case 2:
    
    ydot[(int)parms.n[0]]   =   -dyn.fz[(int)parms.n[0]] +          -dyn.gz[(int)parms.n[0]] +     dyn.gz[(int)parms.n[0]-1]  +  dyn.rz[(int)parms.n[0]]  -dyn.md[(int)parms.n[0]]*pow(y[(int)parms.n[0]],2) ;
    
    ydot[(int)parms.n[0]+1] = 0;
    
    break;
      
      
  case 3:
    ydot[(int)parms.n[0]]   =    -dyn.fz[(int)parms.n[0]] +          -dyn.gz[(int)parms.n[0]] +     dyn.gz[(int)parms.n[0]-1]  +  dyn.rz[(int)parms.n[0]]  -dyn.md[(int)parms.n[0]]*pow(y[(int)parms.n[0]],2) ;
  
    ydot[(int)parms.n[0]+1] =                                            dyn.gz[(int)parms.n[0]];
    
   break;
   
   
   

}
 for(int i=(int)parms.n[0]+2; i<=74; ++i){
     
     ydot[i] = 0;
     
}
  
  
  
  
}





