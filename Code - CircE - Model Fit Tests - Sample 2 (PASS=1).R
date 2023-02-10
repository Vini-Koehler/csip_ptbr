# To install the CircE package (archived on the CRAN) using R Version 3.4.1:

url <- "https://cran.r-project.org/src/contrib/Archive/CircE/CircE_1.1.tar.gz"
pkgFile <- "CircE_1.1.tar.gz"
download.file(url = url, destfile = pkgFile)

# Install dependencies

install.packages(c())

# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)
library(CircE)

# Brazilian (Portuguese)
R.PORT <- matrix(c(
  1,   .63, .35, .18, .09, .20, .24, .64,
  .63,   1, .72, .58, .37, .29, .13, .38,
  .35, .72,   1, .80, .52, .32, .13, .14,
  .18, .58, .80,   1, .70, .44, .25, .09,
  .09, .37, .52, .70,   1, .75, .49, .25,
  .20, .29, .32, .44, .75,   1, .73, .47,
  .24, .13, .13, .25, .49, .73,   1, .57,
  .64, .38, .14, .09, .25, .47, .57,   1
),8,8,byrow=TRUE)

v.names <- c("PA","BC","DE","FG","HI","JK","LM","NO")

eqradius.eqspace  <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=622,
                                equal.com=TRUE,equal.ang=TRUE,
                                mcsc="unconstrained",iterlim=250,
                                factr=1e06)
eqradius <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=622,
                       equal.com=TRUE,equal.ang=FALSE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)
eqspace  <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=622,
                       equal.com=FALSE,equal.ang=TRUE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)
unconstr <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=622,
                       equal.com=FALSE,equal.ang=FALSE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)