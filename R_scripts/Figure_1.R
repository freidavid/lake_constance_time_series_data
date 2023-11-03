library(vioplot)
library(RColorBrewer)
colors<-brewer.pal(12, name="Paired")
greys<-brewer.pal(9, name="Greys")
SF = colors[8]
GF = colors[4]
BF = colors[2]
K = greys[3]

dev.off()

pdf("Figure_1.pdf",height=10,width=8)

par(tcl=-0.5, mai=c(0.3,0.7,0.3,0.7))
layout(rbind(c(1,1,1),c(2,3,4),c(5,5,5),c(6,6,6),c(7,7,7)))


#first make the phosphate plot
library(readr)
library(circlize)
p <- read_delim("p_data.csv",  ";", escape_double = FALSE, col_names = F,   trim_ws = TRUE)
p<-as.data.frame(p)
x=c(1930,p[,1],2021)
y=c(NA,p[,2],NA)
polyCurve <- function(x, y, from, to, n = 50, miny,col = "red", border = col) {
  drawPoly <- function(fun, from, to, n = 50, miny, col, border) {
    Sq <- seq(from = from, to = to, length = n)
    polygon(x = c(Sq[1], Sq, Sq[n]),
            y = c(miny, fun(Sq), miny),
            col = col, border = border)
  }
  lf <- length(from)
  stopifnot(identical(lf, length(to)))
  if(length(col) != lf)
    col <- rep(col, length.out = lf)
  if(length(border) != lf)
    border <- rep(border, length.out = lf)
  if(missing(miny))
    miny <- min(y)
  interp <- approxfun(x = x, y = y)
  mapply(drawPoly, from = from, to = to, col = col, border = border,
         MoreArgs = list(fun = interp, n = n, miny = miny))
  invisible()
}
plot(x,y,ylim=c(0,85),xlab=" ",ylab=" ",type="l",col="cornflowerblue",xlim=c(1922,2040),yaxt="n",xaxt="n")
polyCurve(c(1949,p[1:length(p[,1]),1],2020),c(0,p[1:length(p[,1]),2],0),n=1000,from=1950,to=2019,col=add_transparency("cornflowerblue",0.8),border=NA)
mtext(side = 2, text = expression(paste("P"[tot]," [mg/",m^3,"]" )), line = 2.75,cex=0.7)
#axis(side=1,at=c(1940,1976.5,2015),labels=c("pre","during","post"))
axis(side=3,at=c(1940,1960,1980,2000,2020))
axis(side=2)
text(labels="Period of",x=1978,y=35,col="cornflowerblue")
text(labels="eutrophication",x=1978,y=25,col="cornflowerblue")


(help(axis))
#Add the PCA Plots
#load data and do pca
samples <- read.delim("samples.txt")
cov <- read.table("PCA.cov", quote="\"", comment.char="")
m<-as.matrix(cov)
e<-eigen(m)
par(tcl=-0.5, mai=c(0.3,0.4,0.1,0.4))

samples_pre<-samples[which(samples$Time=="pre"),]
plot(main=" ",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),e$vectors[which(samples$Time=="pre"),c(1,2)],ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=rep(3,length(samples_pre$pch)),col=samples_pre$Color,cex=2)
#text(e$vectors[which(samples$Time=="pre"),c(1)],e$vectors[which(samples$Time=="pre"),c(2)],labels=samples_pre$Label2,cex=0.5)

#during
par(tcl=-0.5, mai=c(0.3,0.4,0.1,0.4))

samples_during<-samples[which(samples$Time=="during"),]
test<-cbind(rev(e$vectors[which(samples$Time=="during"),c(1)]),rev(e$vectors[which(samples$Time=="during"),c(2)]))
plot(main=" ",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),test,ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=rep(3,length(samples_during$pch)),col=rev(samples_during$Color),cex=2)
#text(e$vectors[which(samples$Time=="during"),c(1)],e$vectors[which(samples$Time=="during"),c(2)],labels=samples_during$Label,cex=0.5)
#post
par(tcl=-0.5, mai=c(0.3,0.4,0.1,0.4))

samples_post<-samples[which(samples$Time=="post"),]
test<-cbind(rev(e$vectors[which(samples$Time=="post"),c(1)]),rev(e$vectors[which(samples$Time=="post"),c(2)]))
plot(main=" ",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),test,ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=rep(3,length(samples_post$pch)),col=rev(samples_post$Color),cex=2)
l_text1=c(expression(italic("C. gutturosus")),expression(italic("C. macrophthalmus")),expression(italic("C. wartmanni")), expression(italic("C. arenicolus")))
#legend("bottomright",legend=l_text1,pch=19,col=c(K,GF,BF,SF),bty="n",pt.cex=2,xjust=1)




par(tcl=-0.5, mai=c(0.3,0.7,0.3,0.7))

#then add the nucleotide diversity plot



#species level
kilch_pre <- read.delim("kilch_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
blaufelchen_pre <- read.delim("blaufelchen_pre_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
gangfisch_pre <- read.delim("gangfisch_pre_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
arenicolus_pre <- read.delim("arenicolus_pre_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
blaufelchen_during <- read.delim("blaufelchen_during_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
gangfisch_during <- read.delim("gangfisch_during_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
arenicolus_during <- read.delim("arenicolus_during_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
blaufelchen_post <- read.delim("blaufelchen_post_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
gangfisch_post <- read.delim("gangfisch_post_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)
arenicolus_post <- read.delim("arenicolus_post_theta_100_kb.thetasWindow.pestPG", comment.char="#",header=F)


colnames(kilch_pre)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(blaufelchen_pre)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(gangfisch_pre)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(arenicolus_pre)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(blaufelchen_during)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(gangfisch_during)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(arenicolus_during)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(blaufelchen_post)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(gangfisch_post)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")
colnames(arenicolus_post)<-c("coordinates","chr","center","tW","tP","tF","tH","tL","Tajima","fuf","fud","fayh","zeng","nSites")




kilch_pre <- kilch_pre[which(kilch_pre$nSites!=0),]
gangfisch_pre <- gangfisch_pre[which(gangfisch_pre$nSites!=0),]
blaufelchen_pre <- blaufelchen_pre[which(blaufelchen_pre$nSites!=0),]
arenicolus_pre <- arenicolus_pre[which(arenicolus_pre$nSites!=0),]
gangfisch_during <- gangfisch_during[which(gangfisch_during$nSites!=0),]
blaufelchen_during <- blaufelchen_during[which(blaufelchen_during$nSites!=0),]
arenicolus_during <- arenicolus_during[which(arenicolus_during$nSites!=0),]
gangfisch_post <- gangfisch_post[which(gangfisch_post$nSites!=0),]
blaufelchen_post <- blaufelchen_post[which(blaufelchen_post$nSites!=0),]
arenicolus_post <- arenicolus_post[which(arenicolus_post$nSites!=0),]

#pre
t_k_pre<-mean(na.omit(kilch_pre$tW/window))
t_k_pre_TD<-mean(kilch_pre$Tajima)

t_bf_pre<-mean(na.omit(blaufelchen_pre$tW/window))
t_bf_pre_TD<-mean(blaufelchen_pre$Tajima)

t_gf_pre<-mean(na.omit(gangfisch_pre$tW/window))
t_gf_pre_TD<-mean(gangfisch_pre$Tajima)

t_sf_pre<-mean(na.omit(arenicolus_pre$tW/window))
t_sf_pre_TD<-mean(arenicolus_pre$Tajima)

#during
t_bf_during<-mean(na.omit(blaufelchen_during$tW/window))
t_bf_during_TD<-mean(blaufelchen_during$Tajima)

t_gf_during<-mean(na.omit(gangfisch_during$tW/window))
t_gf_during_TD<-mean(gangfisch_during$Tajima)

t_sf_during<-mean(na.omit(arenicolus_during$tW/window))
t_sf_during_TD<-mean(arenicolus_during$Tajima)

#post
t_bf_post<-mean(na.omit(blaufelchen_post$tW/window))
t_bf_post_TD<-mean(blaufelchen_post$Tajima)

t_gf_post<-mean(na.omit(gangfisch_post$tW/window))
t_gf_post_TD<-mean(gangfisch_post$Tajima)

t_sf_post<-mean(na.omit(arenicolus_post$tW/window))
t_sf_post_TD<-mean(arenicolus_post$Tajima)




x<-c(1937,1935,1946,1946,1979,1980,1973,2015,2015,2015)
y<-c(t_k_pre,t_gf_pre,t_bf_pre,t_sf_pre,t_gf_during,t_bf_during,t_sf_during,t_gf_post,t_bf_post,t_sf_post)
t_pre<-c(t_k_pre,t_gf_pre,t_bf_pre,t_sf_pre)
t_during<-c(t_gf_during,t_bf_during,t_sf_during)
t_post<-c(t_gf_post,t_bf_post,t_sf_post)


plot(x,y,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),pch=NA,ylab=expression(italic(theta[omega])),xlab=" ",axes=F,xlim=c(1922,2040),ylim=c(min(y)*0.98,max(y)*1.02))
#axis(side=1,at=c(1940,1976.5,2015),labels=c("pre","during","post"))
#axis(side=3)
axis(side=2)
box()
lines(c(1935,1979,2015),c(t_gf_pre,t_gf_during,t_gf_post),col=GF,lty=2)
lines(c(1946,1980,2015),c(t_bf_pre,t_bf_during,t_bf_post),col=BF,lty=2)
lines(c(1946,1973,2015),c(t_sf_pre,t_sf_during,t_sf_post),col=SF,lty=2)

points(x,y,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),pch=3,cex=2)


#add tajimas d plot
x<-c(1937,1935,1946,1946,1979,1980,1973,2015,2015,2015)
y<-c(t_k_pre_TD,t_gf_pre_TD,t_bf_pre_TD,t_sf_pre_TD,t_gf_during_TD,t_bf_during_TD,t_sf_during_TD,t_gf_post_TD,t_bf_post_TD,t_sf_post_TD)
t_pre_TD<-c(t_k_pre_TD,t_gf_pre_TD,t_bf_pre_TD,t_sf_pre_TD)
t_during_TD<-c(t_gf_during_TD,t_bf_during_TD,t_sf_during_TD)
t_post_TD<-c(t_gf_post_TD,t_bf_post_TD,t_sf_post_TD)



plot(x,y,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),pch=NA,ylab="Tajima's D",xlab=" ",axes=F,xlim=c(1922,2040),,ylim=c(min(y)*1.1,max(y)*1.4))
#axis(side=1,at=c(1940,1976.5,2015),labels=c("pre","during","post"))
#axis(side=3)
axis(side=2)
box()

lines(c(1935,1979,2015),c(t_gf_pre_TD,t_gf_during_TD,t_gf_post_TD),col=GF,lty=2)
lines(c(1946,1980,2015),c(t_bf_pre_TD,t_bf_during_TD,t_bf_post_TD),col=BF,lty=2)
lines(c(1946,1973,2015),c(t_sf_pre_TD,t_sf_during_TD,t_sf_post_TD),col=SF,lty=2)

points(x,y,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),pch=3,cex=2)

legend("right",legend=l_text1,pch=3,col=c(K,GF,BF,SF),bty="n",pt.cex=2,xjust=1,y.intersp = 1.5)
help(legend)

#add relatedness
#load relatedness data
kilch <- read.delim("kilch_RESULT.txt")
are_pre <- read.delim("arenicolus_pre_RESULT.txt")
are_during <- read.delim("arenicolus_during_RESULT.txt")
are_post <- read.delim("arenicolus_post_RESULT.txt")
gang_pre <- read.delim("gangfisch_pre_RESULT.txt")
gang_during <- read.delim("gangfisch_during_RESULT.txt")
gang_post <- read.delim("gangfisch_post_RESULT.txt")
blau_pre <- read.delim("blaufelchen_pre_RESULT.txt")
blau_during <- read.delim("blaufelchen_during_RESULT.txt")
blau_post <- read.delim("blaufelchen_post_RESULT.txt")


plot(c(1946,1973,2015),c(mean(are_pre$rab),mean(are_during$rab),mean(are_post$rab)),xlim=c(1922,2040),ylim=c(-0.03,0.31),ylab="Mean relatedness",xlab=" ",col=NA,pch=3,cex=2,axes=F)
axis(side=2)
box()
lines(c(1946,1973,2015),c(mean(are_pre$rab),mean(are_during$rab),mean(are_post$rab)),col=SF,lty=2)
points(c(1946,1973,2015),c(mean(are_pre$rab),mean(are_during$rab),mean(are_post$rab)),col=SF,pch=3,cex=2)

lines(c(1935,1979,2015),c(mean(gang_pre$rab),mean(gang_during$rab),mean(gang_post$rab)),col=GF,lty=2)
points(c(1935,1979,2015),c(mean(gang_pre$rab),mean(gang_during$rab),mean(gang_post$rab)),col=GF,pch=3,cex=2)

lines(c(1946,1980,2015),c(mean(blau_pre$rab),mean(blau_during$rab),mean(blau_post$rab)),lty=2,col=BF)
points(c(1946,1980,2015),c(mean(blau_pre$rab),mean(blau_during$rab),mean(blau_post$rab)),col=BF,pch=3,cex=2)

points(1937,mean(kilch_RESULT$rab),col=K,pch=3,cex=2)

dev.off()

