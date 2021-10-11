setwd("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/FinalModel/C Scripts")

#Simple model of vertical distribution of particles numbers in a water column


# Driving data containing profile of Depths and Vertical Diffusivity
DrivingData<-read.csv("/Users/michael/Desktop/PhD/Research/Water Column Model/DUMMY/data.csv") # or other path...
#DrivingData<-DrivingData[rowSums(DrivingData[,c(2,3)])!=0,] # removes rows of zeroes #(these rows represent below the seabed)


times<-seq(0,3600,1)


n<-74 # fixed number of depth layers - in this code must = the hardwired value in the model function

# Set up the vertical thickness of each cell
#------------------
#z=rep(180,max(DrivingData$Layer))                   # thickness of each cell is identical
#----
#z<-c(10,20,40,80,150,300,300,300,300,300)  # or.. define a vector of unequal thicknesses
#----

# Extracted from Grid_W files, this is the numeric depth of each "Depth" layer in driving data
NEMO<-c(0.000000,   1.023907,    2.103190,    3.251309 ,   4.485053,    5.825238,
        7.297443,    8.932686,   10.767898,   12.845992,   15.215269,   17.927923,
        21.037571,   24.595987,   28.649651,   33.236965,   38.387100,   44.121010,
        50.454468,   57.402565,   64.984604,   73.228699,   82.175560,   91.881409,
        102.420174,  113.885239,  126.390930,  140.073975,  155.095047,  171.640244,
        189.922791,  210.184464,  232.696976,  257.762878,  285.715790,  316.919861,
        351.768005,  390.678619,  434.090546,  482.456329,  536.233215,  595.872070,
        661.805237,  734.432129,  814.105652,  901.117981,  995.688538, 1097.954102,
        1207.963013, 1325.672485, 1450.950439, 1583.582397, 1723.279785, 1869.693115,
        2022.424927, 2181.044434, 2345.101074, 2514.137207, 2687.699463, 2865.346924,
        3046.659180, 3231.240479, 3418.722900, 3608.769287, 3801.072021, 3995.354248,
        4191.367188, 4388.889648, 4587.725586, 4787.702148, 4988.667480, 5190.488281,
        5393.049316, 5596.249023, 5800.000000)


# thickness of NEMO-MEDUSA layers
z<-c()
for(i in 1:(length(NEMO)-1)){
  z[i]=NEMO[i+1]-NEMO[i]
}	


# Trim down z to correspond with bathymetry of driving data.

# z<-z[1:length(DrivingData$Layer)	]							



#cz<-(-z[1]/2)
#for(q in (2:Nz)){
#cz<-c(cz,cz[q-1]-+(z[q-1]/2)-(z[q]/2))
#}

# Centre of box
cz<-c()

for(i in 1:length(z)){
  cz[i]<-(NEMO[i]+NEMO[i+1])/2
}


zmax<-(sum(z))

m<-0:10

# Comment/uncomment these lines to generate scenarios of sinking rate
#------------------
sS<-3
sB<-0.001
#----
#s<-seq(sS,sB,by=((sB-sS)/(length(z)-1)))  # sets up a vertical gradient in sinking rate
#----
s<-rep(sS, length(z))                      # or.. constant vertical sinking rate



# Comment/uncomment these lines to generate scenarios of particle replication rate
#------------------
#rS<-0.0  # particle replication rate in the surface cell = 0
rS<-0.02  # particle replication rate in the surface cell = 0.02
rB<-0.0  # particle replication rate in the deepest cell = 0
#rB<-0.02  # particle replication rate in the deepest cell = 0.02
#----
#r<-seq(rS,rB,by=((rB-rS)/(length(z)-1)))    # sets up a vertical gradient in replication rate
#----
#r<-rep(rS,length(z))                        # constant replication rate at all depths
#----
r<-c(rS,rep(0,length(z)-1))                  # or ... replication only at the surface
#----
#r<-c(rep(0,length(z)-1),rS)                 # or ... replication only at the bottom


#------------------
# k in driving data as forcing


#------------------


# Comment/uncomment these lines to generate scenarios of particle mortality rate
#------------------
mS<-0.0005
#----
m<-c(mS,rep(0,length(z)-1))                  # mortality only at the surface
#----
#m<-c(rep(0,length(z)-1),mS)                 # mortality only at the bottom




#------------------
#------------------
#------------------
#------------------