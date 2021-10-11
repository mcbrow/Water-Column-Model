

# Set up the initial conditions
#------------------
xstart<-rep(0,length(z))  # set up a vector of zeros as the basis for the initial conditions
xstart[1]<-100000 # Start with particles only in the surface cell
#xstart[length(z)]<-100   # or ... start with particles only in the deepest cell
# element length(z)+1 is for the cumulative export flux