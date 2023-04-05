require(shapes)
require(Morpho)
make.mvn <- function(mean, vcv) {
  logdet <- as.numeric(determinant(vcv, TRUE)$modulus)
  tmp <- length(mean) * log(2 * pi) + logdet
  vcv.i <- solve(vcv)
  function(x) {
    dx <- x - mean
    exp(-(tmp + rowSums((dx %*% vcv.i) * dx))/2)
  }
}
data(dna.dat)
plotshapestime3d(dna.dat)
require(mvtnorm);
require(scales);
a=c(-6,4);b=c(0,6);c=c(6,-2);d=c(0,-2); q1=rbind(a,b,c,d);
e=c(20,-3);f=c(15,4); g=c(20,2);h=c(24,6);q2=rbind(e,f,g,h);
cvq1 <- array(rep(1, 4*2*1), dim=c(4, 2, 1));
ccq1 <- array(rep(1, 4*2*1), dim=c(4, 2, 1));
cvq1[,,1]=q1;
ccq1[,,1]=q2;
require(mvtnorm);
require(scales);
a=c(-6,4);b=c(0,6);c=c(6,-2);d=c(0,-2); q1=rbind(a,b,c,d);
e=c(20,-3);f=c(15,4); g=c(20,2);h=c(24,6);q2=rbind(e,f,g,h);
cvq1 <- array(rep(1, 4*2*1), dim=c(4, 2, 1));
ccq1 <- array(rep(1, 4*2*1), dim=c(4, 2, 1));
cvq1[,,1]=q1;
ccq1[,,1]=q2;

rnv=function(h){
  s=1.5;
  return(rmvnorm(1, mean=h, sigma=(s^2)*diag(2)))
}
rnc=function(h){
  s=0.8;
  return(rmvnorm(1, mean=h, sigma=(s^2)*diag(2)))
}

cvq <- array(rep(1, 4*2*1000), dim=c(4, 2, 1000))
ccq <- array(rep(1, 4*2*1000), dim=c(4, 2, 1000))
cvq[,,1]=q1;
ccq[,,1]=q2;
for(k in 2:1000)
{
  aa=c(-6,4);bb=c(0,6);cc=c(6,-2);dd=c(0,-2);
  qq1=rbind(rnv(aa),rnv(bb),rnv(cc),rnv(dd));
  ee=c(20,-3);ff=c(15,4); gg=c(20,2);hh=c(24,6);
  qq2=rbind(rnc(ee),rnc(ff),rnc(gg),rnc(hh));
  cvq[,,k]=qq1;
  ccq[,,k]=qq2;
}
cct=ct=ccq
vvt=vt=cvq
for(i in 1:dim(ct)[3])
{cct[,,i]=
  Morpho::rotonto(ct[,,1],ct[,,i])$yrot
}

for(i in 1:dim(ct)[3])
{vvt[,,i]=
  Morpho::rotonto(vt[,,1],vt[,,i])$yrot
}
cct5000.10=MCMCpostPsample2D(1.5,rep(0.1,1),cct[,,1:10],5000)
PPLOTpostvar2D(cct5000.10, 1000)
vvt5000.10=c()
vvt5000.20=c()
vvt5000.50=c()
vvt5000.100=c()




vvt5000.10=MCMCpostPsample2D(1.5,rep(0.1,1),vt[,,1:10],5000);
vvt5000.20=MCMCpostPsample2D(1.5,rep(0.1,1),vt[,,1:20],5000);
vvt5000.50=MCMCpostPsample2D(1.5,rep(0.1,1),vt[,,1:50],5000);
vvt5000.100=MCMCpostPsample2D(1.5,rep(0.1,1),vt[,,1:100],5000);
vvt5000=cbind(vvt5000.10,vvt5000.20,vvt5000.50,vvt5000.100);
theta=1.5;
#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(vvt5000[,1]), xlim=c(0.7,1.7),ylim=c(0,12),col="purple", xlab=expression(paste(tilde(theta)~~(" \n data : Convex Quadrilateral"))),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of")~~tilde(theta)~~("Procruste Variance"))))
abline(v=theta, col="black",lwd=2, lty=1)
lines(density(vvt5000[,2]), ylim=c(0,12),col="blue")
lines(density(vvt5000[,3]), ylim=c(0,12),col="forestgreen")
lines(density(vvt5000[,4]), ylim=c(0,12),col="red")

legend("topleft",cex=0.5, c("n=10","n=20","n=50","n=100", expression(theta_0)), lty = c(1,1,1,1,1), col = c("purple","blue","forestgreen","red", "black"), lwd = c(1,1,1,1,2))
grid()

plot(density(vvt5000[,1]), xlim=c(0.7,1.7),ylim=c(0,12),col="purple", xlab=expression(paste(tilde(theta)~~(" \n data : Convex Quadrilateral"))),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of")~~tilde(theta)~~("Procruste Variance"))))
abline(v=theta, col="red",lwd=2, lty=1)
lines(density(vvt5000[,2]), ylim=c(0,12),col="blue")
lines(density(vvt5000[,3]), ylim=c(0,12),col="forestgreen")
lines(density(vvt5000[,4]), ylim=c(0,12),col="red")

legend("topleft",cex=0.5, c("n=10","n=20","n=50","n=100", expression(theta_0)), lty = c(1,1,1,1,1), col = c("purple","blue","forestgreen","red", "red"), lwd = c(1,1,1,1,2))
grid()

cct5000.10=c()
cct5000.20=c()
cct5000.50=c()
cct5000.100=c()




cct5000.10=MCMCpostPsample2D(1.5,rep(0.1,1),ct[,,1:10],5000);
cct5000.20=MCMCpostPsample2D(1.5,rep(0.1,1),ct[,,1:20],5000);
cct5000.50=MCMCpostPsample2D(1.5,rep(0.1,1),ct[,,1:50],5000);
cct5000.100=MCMCpostPsample2D(1.5,rep(0.1,1),ct[,,1:100],5000);

cct5000=cbind(cct5000.10,cct5000.20,cct5000.50,cct5000.100);

theta=0.8;
#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(cct5000[,1]), xlim=c(0.4,1),ylim=c(0,23),col="purple", xlab=expression(paste(tilde(theta)~~(" \n data : Concave Quadrilateral"))),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of")~~tilde(theta)~~("Procruste Variance"))))
abline(v=theta, col="blue",lwd=2, lty=1)
lines(density(cct5000[,2]), ylim=c(0,12),col="skyblue")
lines(density(cct5000[,3]), ylim=c(0,12),col="forestgreen")
lines(density(cct5000[,4]), ylim=c(0,12),col="blue")

legend("topleft",cex=0.5, c("n=10","n=20","n=50","n=100", expression(theta_0)), lty = c(1,1,1,1,1), col = c("purple","skyblue","forestgreen","blue", "blue"), lwd = c(1,1,1,1,2))
grid()
