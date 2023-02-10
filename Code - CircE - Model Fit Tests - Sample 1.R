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
  1,   .83, .52, .37, .27, .41, .20, .71,
  .83,   1, .76, .63, .48, .49, .15, .61,
  .52, .76,   1, .80, .56, .45, .17, .38,
  .37, .63, .80,   1, .76, .56, .31, .33,
  .27, .48, .56, .76,   1, .80, .50, .40,
  .41, .49, .45, .56, .80,   1, .69, .57,
  .20, .15, .17, .31, .50, .69,   1, .55,
  .71, .61, .38, .33, .40, .57, .55,   1
),8,8,byrow=TRUE)

v.names <- c("PA","BC","DE","FG","HI","JK","LM","NO")

eqradius.eqspace  <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=904,
                                equal.com=TRUE,equal.ang=TRUE,
                                mcsc="unconstrained",iterlim=250,
                                factr=1e06)
eqradius <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=904,
                       equal.com=TRUE,equal.ang=FALSE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)
eqspace  <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=904,
                       equal.com=FALSE,equal.ang=TRUE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)
unconstr <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=904,
                       equal.com=FALSE,equal.ang=FALSE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)