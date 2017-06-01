
# ------------------------------------------------------------
# NRC Political Science Graduate Program Rankings
# ------------------------------------------------------------

# Open CSV file
path<-file.path("C:","Users","Miles","Documents","R","NRC Rankings","NRC 2010.csv")
nrc<-read.csv(path)
nrc$School<-as.character(nrc$School)

# ------------------------------------------------------------
# S Ranks
# ------------------------------------------------------------
nrc$S_Mid<-(nrc$S_Hi+nrc$S_Low)/2
n<-nrow(nrc)
term<-c(1:n)
term<-reorder(term,nrc$S_Mid)
school<-reorder(nrc$School,nrc$S_Mid)
estimate<-nrc$S_Mid
hi<-nrc$S_Hi
lo<-nrc$S_Low
windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(12,4,4,2)+0.1)
plot(school,estimate,
     ylab="S-Rank",ylab="",type="n",las=2,cex.axis=.75,
     main="NRC Rankings for Ph.D. Programs in Political Science")
abline(v=school,lty=1,col="lightgrey")
abline(h=c(25,50,75,100),lty=2,col="darkgrey")
points(school,estimate,pch=19)
points(school,hi,pch="-")
points(school,lo,pch="-")
segments(x0=school,y0=hi,y1=lo)
par(yaxt="s")
axis(2,at=seq(0,110,10),las=2)

# ------------------------------------------------------------
# R Ranks
# ------------------------------------------------------------
nrc$R_Mid<-(nrc$R_Hi+nrc$R_Low)/2
n<-nrow(nrc)
term<-c(1:n)
term<-reorder(term,nrc$R_Mid)
school<-reorder(nrc$School,nrc$R_Mid)
estimate<-nrc$R_Mid
hi<-nrc$R_Hi
lo<-nrc$R_Low
windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(12,4,4,2)+0.1)
plot(school,estimate,
     ylab="R-Rank",ylab="",type="n",las=2,cex.axis=.75,
     main="NRC Rankings for Ph.D. Programs in Political Science")
abline(v=school,lty=1,col="lightgrey")
abline(h=c(25,50,75,100),lty=2,col="darkgrey")
points(school,estimate,pch=19)
points(school,hi,pch="-")
points(school,lo,pch="-")
segments(x0=school,y0=hi,y1=lo)
par(yaxt="s")
axis(2,at=seq(0,110,10),las=2)

# ------------------------------------------------------------
# Faculty Research
# ------------------------------------------------------------
nrc$Research_Mid<-(nrc$Research_Hi+nrc$Research_Low)/2
n<-nrow(nrc)
term<-c(1:n)
term<-reorder(term,nrc$Research_Mid)
school<-reorder(nrc$School,nrc$Research_Mid)
estimate<-nrc$Research_Mid
hi<-nrc$Research_Hi
lo<-nrc$Research_Low
windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(12,4,4,2)+0.1)
plot(school,estimate,
     ylab="Faculty Research",ylab="",type="n",las=2,cex.axis=.75,
     main="NRC Rankings for Ph.D. Programs in Political Science")
abline(v=school,lty=1,col="lightgrey")
abline(h=c(25,50,75,100),lty=2,col="darkgrey")
points(school,estimate,pch=19)
points(school,hi,pch="-")
points(school,lo,pch="-")
segments(x0=school,y0=hi,y1=lo)
par(yaxt="s")
axis(2,at=seq(0,110,10),las=2)

# ------------------------------------------------------------
# Student Outcomes
# ------------------------------------------------------------
nrc$Students_Mid<-(nrc$Students_Hi+nrc$Students_Low)/2
n<-nrow(nrc)
term<-c(1:n)
term<-reorder(term,nrc$Students_Mid)
school<-reorder(nrc$School,nrc$Students_Mid)
estimate<-nrc$Students_Mid
hi<-nrc$Students_Hi
lo<-nrc$Students_Low
windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(12,4,4,2)+0.1)
plot(school,estimate,
     ylab="Student Outcomes",ylab="",type="n",las=2,cex.axis=.75,
     main="NRC Rankings for Ph.D. Programs in Political Science")
abline(v=school,lty=1,col="lightgrey")
abline(h=c(25,50,75,100),lty=2,col="darkgrey")
points(school,estimate,pch=19)
points(school,hi,pch="-")
points(school,lo,pch="-")
segments(x0=school,y0=hi,y1=lo)
par(yaxt="s")
axis(2,at=seq(0,110,10),las=2)

# ------------------------------------------------------------
# Diversity
# ------------------------------------------------------------
nrc$Diversity_Mid<-(nrc$Diversity_Hi+nrc$Diversity_Low)/2
n<-nrow(nrc)
term<-c(1:n)
term<-reorder(term,nrc$Diversity_Mid)
school<-reorder(nrc$School,nrc$Diversity_Mid)
estimate<-nrc$Diversity_Mid
hi<-nrc$Diversity_Hi
lo<-nrc$Diversity_Low
windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(12,4,4,2)+0.1)
plot(school,estimate,
     ylab="Diversity",ylab="",type="n",las=2,cex.axis=.75,
     main="NRC Rankings for Ph.D. Programs in Political Science")
abline(v=school,lty=1,col="lightgrey")
abline(h=c(25,50,75,100),lty=2,col="darkgrey")
points(school,estimate,pch=19)
points(school,hi,pch="-")
points(school,lo,pch="-")
segments(x0=school,y0=hi,y1=lo)
par(yaxt="s")
axis(2,at=seq(0,110,10),las=2)

# ------------------------------------------------------------
# Rank Index
# ------------------------------------------------------------
nrc$Rank<-nrc$R_Mid+nrc$S_Mid+nrc$Research_Mid+(0.5*nrc$Students_Mid)
nrc$Index<-((nrc$Rank-min(nrc$Rank))/(max(nrc$Rank)-min(nrc$Rank)))*100
n<-nrow(nrc)
term<-c(1:n)
term<-reorder(term,nrc$Index)
school<-reorder(nrc$School,nrc$Index)
estimate<-nrc$Index
windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(12,4,4,2)+0.1)
plot(school,estimate,
     ylab="Rank Index",ylab="",type="n",las=2,cex.axis=.75,
     main="NRC Rankings for Ph.D. Programs in Political Science")
abline(v=school,lty=1,col="lightgrey")
abline(h=c(25,50,75,100),lty=2,col="darkgrey")
points(school,estimate,pch=19)
par(yaxt="s")
axis(2,at=seq(0,100,10),las=2)


# ------------------------------------------------------------
# Correlations
# ------------------------------------------------------------
library(Hmisc)
res<-rcorr(as.matrix(nrc[,12:16]))
install.packages("corrplot")
library(corrplot)
windows()
corrplot(res$r,type="upper",order="hclust",tl.col="black",tl.pos="d",
         tl.cex=.75,p.mat=res$P,sig.level=0.05)