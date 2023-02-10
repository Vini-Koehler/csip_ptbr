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
  1,   .65, .37, .20, .11, .22, .26, .66,
  .65,   1, .72, .59, .39, .31, .16, .42,
  .37, .72,   1, .79, .53, .34, .16, .18,
  .20, .59, .79,   1, .71, .45, .27, .12,
  .11, .39, .53, .71,   1, .75, .49, .25,
  .22, .31, .34, .45, .75,   1, .73, .46,
  .26, .16, .16, .27, .49, .73,   1, .58,
  .66, .42, .18, .12, .25, .46, .58,   1
),8,8,byrow=TRUE)

v.names <- c("PA","BC","DE","FG","HI","JK","LM","NO")

eqradius.eqspace  <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=712,
                                equal.com=TRUE,equal.ang=TRUE,
                                mcsc="unconstrained",iterlim=250,
                                factr=1e06)
eqradius <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=712,
                       equal.com=TRUE,equal.ang=FALSE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)
eqspace  <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=712,
                       equal.com=FALSE,equal.ang=TRUE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)
unconstr <- CircE.BFGS(R=R.PORT,v.names=v.names,m=3,N=712,
                       equal.com=FALSE,equal.ang=FALSE,
                       mcsc="unconstrained",iterlim=250,
                       factr=1e06)