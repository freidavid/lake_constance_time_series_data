#################################################################################################################################
kilch_sites<-read.csv("frequency_table_kilch.txt", sep="")
#################################################################################################################################
pdf("kilch_frequencies.pdf",width=12)
par(cex=1.4)
#make the plot with the first site
pre<-c(kilch_sites[1,]$kilch,kilch_sites[1,]$gangfisch_pre,kilch_sites[1,]$blaufelchen_pre,kilch_sites[1,]$arenicolus_pre)
during<-c(kilch_sites[1,]$gangfisch_during,kilch_sites[1,]$blaufelchen_during,kilch_sites[1,]$arenicolus_during)
post<-c(kilch_sites[1,]$gangfisch_post,kilch_sites[1,]$blaufelchen_post,kilch_sites[1,]$arenicolus_post)

y<-c(0.75,0.75,0.75,0.75,3,3,3,5,5,5)
x<-c(pre,during,post)
if(pre[1]<0.5){x=1-x}


plot(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),axes=F,ylab="Frequency",xlab=" ",ylim=c(0,1.05))
box()
axis(side=1,at=c(0.75,3,5),labels=c("pre","during","post"))
axis(side=2)


line_y<-y[c(2,5,8)]
lines(line_y,c(x[2],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
lines(line_y,c(x[3],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
lines(line_y,c(x[4],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))


#add other sites
for(i in 2:length(kilch_sites[,1])){
pre<-c(kilch_sites[i,]$kilch,kilch_sites[i,]$gangfisch_pre,kilch_sites[i,]$blaufelchen_pre,kilch_sites[i,]$arenicolus_pre)
during<-c(kilch_sites[i,]$gangfisch_during,kilch_sites[i,]$blaufelchen_during,kilch_sites[i,]$arenicolus_during)
post<-c(kilch_sites[i,]$gangfisch_post,kilch_sites[i,]$blaufelchen_post,kilch_sites[i,]$arenicolus_post)
x<-c(pre,during,post)
if(pre[1]<0.5){x=1-x}
points(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF))



line_y<-y[c(2,5,8)]
lines(line_y,c(x[2],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
lines(line_y,c(x[3],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
lines(line_y,c(x[4],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))
}

dev.off()


#################################################################################################################################
gangfisch_sites<-read.csv("frequency_table_gangfisch.txt", sep="")
#################################################################################################################################
pdf("gangfisch_frequencies.pdf",width=12)
#make the plot with the first site
par(cex=1.4)
pre<-c(gangfisch_sites[1,]$gangfisch_pre,gangfisch_sites[1,]$kilch,gangfisch_sites[1,]$blaufelchen_pre,gangfisch_sites[1,]$arenicolus_pre)
during<-c(gangfisch_sites[1,]$gangfisch_during,gangfisch_sites[1,]$blaufelchen_during,gangfisch_sites[1,]$arenicolus_during)
post<-c(gangfisch_sites[1,]$gangfisch_post,gangfisch_sites[1,]$blaufelchen_post,gangfisch_sites[1,]$arenicolus_post)

y<-c(0.75,0.75,0.75,0.75,3,3,3,5,5,5)
x<-c(pre,during,post)
if(pre[1]<0.5){x=1-x}

plot(y,x,pch=3,cex=2,col=c(GF,K,BF,SF,GF,BF,SF,GF,BF,SF),axes=F,ylab="Frequency",xlab=" ",ylim=c(0,1.05))
box()
axis(side=1,at=c(0.75,3,5),labels=c("pre","during","post"))
axis(side=2)

line_y<-y[c(2,5,8)]
lines(line_y,c(x[3],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
lines(line_y,c(x[4],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))
lines(c(0.75,3,5),c(x[1],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))


#add other sites
for(i in 2:length(gangfisch_sites[,1])){
  pre<-c(gangfisch_sites[i,]$gangfisch_pre,gangfisch_sites[i,]$kilch,gangfisch_sites[i,]$blaufelchen_pre,gangfisch_sites[i,]$arenicolus_pre)
  during<-c(gangfisch_sites[i,]$gangfisch_during,gangfisch_sites[i,]$blaufelchen_during,gangfisch_sites[i,]$arenicolus_during)
  post<-c(gangfisch_sites[i,]$gangfisch_post,gangfisch_sites[i,]$blaufelchen_post,gangfisch_sites[i,]$arenicolus_post)
  x<-c(pre,during,post)
  if(pre[1]<0.5){x=1-x}
  
  points(y,x,pch=3,cex=2,col=c(GF,K,BF,SF,GF,BF,SF,GF,BF,SF))

  
  line_y<-y[c(2,5,8)]
  lines(line_y,c(x[3],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
  lines(line_y,c(x[4],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))
  lines(c(0.75,3,5),c(x[1],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
}

dev.off()


#################################################################################################################################
blaufelchen_sites<-read.csv("frequency_table_blaufelchen.txt", sep="")
#################################################################################################################################
pdf("blaufelchen_frequencies.pdf",width=12)
#make the plot with the first site
par(cex=1.4)
pre<-c(blaufelchen_sites[1,]$blaufelchen_pre,blaufelchen_sites[1,]$kilch,blaufelchen_sites[1,]$gangfisch_pre,blaufelchen_sites[1,]$arenicolus_pre)
during<-c(blaufelchen_sites[1,]$gangfisch_during,blaufelchen_sites[1,]$blaufelchen_during,blaufelchen_sites[1,]$arenicolus_during)
post<-c(blaufelchen_sites[1,]$gangfisch_post,blaufelchen_sites[1,]$blaufelchen_post,blaufelchen_sites[1,]$arenicolus_post)

y<-c(0.75,0.75,0.75,0.75,3,3,3,5,5,5)
x<-c(pre,during,post)
if(pre[1]<0.5){x=1-x}

plot(y,x,pch=3,cex=2,col=c(BF,K,GF,SF,GF,BF,SF,GF,BF,SF),axes=F,ylab="Frequency",xlab=" ",ylim=c(0,1.05))
box()
axis(side=1,at=c(0.75,3,5),labels=c("pre","during","post"))
axis(side=2)



line_y<-y[c(2,5,8)]
lines(line_y,c(x[3],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
lines(line_y,c(x[4],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))
lines(c(0.75,3,5),c(x[1],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))


#add other sites
for(i in 2:length(blaufelchen_sites[,1])){
  pre<-c(blaufelchen_sites[i,]$blaufelchen_pre,blaufelchen_sites[i,]$kilch,blaufelchen_sites[i,]$gangfisch_pre,blaufelchen_sites[i,]$arenicolus_pre)
  during<-c(blaufelchen_sites[i,]$gangfisch_during,blaufelchen_sites[i,]$blaufelchen_during,blaufelchen_sites[i,]$arenicolus_during)
  post<-c(blaufelchen_sites[i,]$gangfisch_post,blaufelchen_sites[i,]$blaufelchen_post,blaufelchen_sites[i,]$arenicolus_post)
  x<-c(pre,during,post)
  if(pre[1]<0.5){x=1-x}
  
  points(y,x,pch=3,cex=2,col=c(BF,K,GF,SF,GF,BF,SF,GF,BF,SF))

  
  line_y<-y[c(2,5,8)]
  lines(line_y,c(x[3],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
  lines(line_y,c(x[4],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))
  lines(c(0.75,3,5),c(x[1],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
}

dev.off()



#################################################################################################################################
sandfelchen_sites<-read.csv("frequency_table_sandfelchen.txt", sep="")
#################################################################################################################################
#change the major/minor, so that the sf allele is the fixed one
sandfelchen_sites[which(sandfelchen_sites$arenicolus_pre<0.1),2:12]<-(1-sandfelchen_sites[which(sandfelchen_sites$arenicolus_pre<0.1),2:12])

pdf("sandfelchen_freuquencies.pdf",width=12)
par(cex=1.4)
#make the plot with the first site
pre<-c(sandfelchen_sites[1,]$arenicolus_pre,sandfelchen_sites[1,]$kilch,sandfelchen_sites[1,]$gangfisch_pre,sandfelchen_sites[1,]$blaufelchen_pre)
during<-c(sandfelchen_sites[1,]$gangfisch_during,sandfelchen_sites[1,]$blaufelchen_during,sandfelchen_sites[1,]$arenicolus_during)
post<-c(sandfelchen_sites[1,]$gangfisch_post,sandfelchen_sites[1,]$blaufelchen_post,sandfelchen_sites[1,]$arenicolus_post)

y<-c(0.75,0.75,0.75,0.75,3,3,3,5,5,5)
x<-c(pre,during,post)
if(pre[1]<0.5){x=1-x}



plot(y,x,pch=3,cex=2,col=c(SF,K,GF,BF,GF,BF,SF,GF,BF,SF),axes=F,ylab="Frequency",xlab=" ",ylim=c(0,1.05))
box()
axis(side=1,at=c(0.75,3,5),labels=c("pre","during","post"))
axis(side=2)



line_y<-y[c(2,5,8)]
lines(line_y,c(x[3],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
lines(line_y,c(x[4],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
lines(c(0.75,3,5),c(x[1],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))


#add other sites
for(i in 2:length(blaufelchen_sites[,1])){
  pre<-c(sandfelchen_sites[i,]$arenicolus_pre,sandfelchen_sites[i,]$kilch,sandfelchen_sites[i,]$gangfisch_pre,sandfelchen_sites[i,]$blaufelchen_pre)
  during<-c(sandfelchen_sites[i,]$gangfisch_during,sandfelchen_sites[i,]$blaufelchen_during,sandfelchen_sites[i,]$arenicolus_during)
  post<-c(sandfelchen_sites[i,]$gangfisch_post,sandfelchen_sites[i,]$blaufelchen_post,sandfelchen_sites[i,]$arenicolus_post)
  x<-c(pre,during,post)
  if(pre[1]<0.5){x=1-x}

  points(y,x,pch=3,cex=2,col=c(SF,K,GF,BF,GF,BF,SF,GF,BF,SF))
  
  
  line_y<-y[c(2,5,8)]
  lines(line_y,c(x[3],x[5],x[8]),col=adjustcolor(GF,alpha.f = 0.2))
  lines(line_y,c(x[4],x[6],x[9]),col=adjustcolor(BF,alpha.f = 0.2))
  lines(c(0.75,3,5),c(x[1],x[7],x[10]),col=adjustcolor(SF,alpha.f = 0.2))
}

dev.off()


#################################################################################################################################
vgll3<-read.csv("frequency_table_vgll3.txt", sep="")
#################################################################################################################################
pdf("vgll3_freqencies.pdf",width=12)

par(cex=1.4)
#only get the sites where at least one species has a frequency above 0.15
freq_threshold=0.15
vgll3<-vgll3_2[which(vgll3_2$gangfisch_pre>=freq_threshold | vgll3_2$blaufelchen_pre>=freq_threshold | vgll3_2$arenicolus_pre>=freq_threshold),]

#make the plot with the first site
pre<-c(vgll3[1,]$kilch,vgll3[1,]$gangfisch_pre,vgll3[1,]$blaufelchen_pre,vgll3[1,]$arenicolus_pre)
during<-c(vgll3[1,]$gangfisch_during,vgll3[1,]$blaufelchen_during,vgll3[1,]$arenicolus_during)
post<-c(vgll3[1,]$gangfisch_post,vgll3[1,]$blaufelchen_post,vgll3[1,]$arenicolus_post)
y<-c(0.7,0.9,1.1,1.3,2.8,3,3.2,4.8,5,5.2)
x<-c(pre,during,post)

plot(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),axes=F,ylab="Frequency",xlab=" ",ylim=c(-0.1,1.1),main=" ",lwd=3)
box()
axis(side=1,at=c(1,3,5),labels=c("pre","during","post"))
axis(side=2)
l_text1=c(expression(italic("C. gutturosus")),expression(italic("C. macrophthalmus")),expression(italic("C. wartmanni")), expression(italic("C. arenicolus")))
legend("topright",legend=l_text1,pch=3,col=c(K,GF,BF,SF),bty="n",pt.cex=1,pt.lwd=1.5,xjust=1)
#lines(y,x,col="gray")
lines(y[c(2,5,8)],x[c(2,5,8)],col=GF)
lines(y[c(3,6,9)],x[c(3,6,9)],col=BF)
lines(y[c(4,7,10)],x[c(4,7,10)],col=SF)

points(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),lwd=3)



for(i in 2:length(vgll3_2$chromo)){
  pre<-c(vgll3[i,]$kilch,vgll3[i,]$gangfisch_pre,vgll3[i,]$blaufelchen_pre,vgll3[i,]$arenicolus_pre)
  during<-c(vgll3[i,]$gangfisch_during,vgll3[i,]$blaufelchen_during,vgll3[i,]$arenicolus_during)
  post<-c(vgll3[i,]$gangfisch_post,vgll3[i,]$blaufelchen_post,vgll3[i,]$arenicolus_post)
  y<-c(0.7,0.9,1.1,1.3,2.8,3,3.2,4.8,5,5.2)
  x<-c(pre,during,post)
  #lines(y,x,col="gray")
  points(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),lwd=3)
}

dev.off()




#################################################################################################################################
gillraker<-read.csv("frequency_table_gillraker_position.txt", sep="")
#################################################################################################################################
pdf("gillraker_position_frequencies.pdf",width=12)
par(cex=1.4)
#make the plot with the first site
pre<-c(gillraker[1,]$kilch,gillraker[1,]$gangfisch_pre,gillraker[1,]$blaufelchen_pre,gillraker[1,]$arenicolus_pre)
during<-c(gillraker[1,]$gangfisch_during,gillraker[1,]$blaufelchen_during,gillraker[1,]$arenicolus_during)
post<-c(gillraker[1,]$gangfisch_post,gillraker[1,]$blaufelchen_post,gillraker[1,]$arenicolus_post)

y<-c(0.7,0.9,1.1,1.3,2.8,3,3.2,4.8,5,5.2)
x<-c(pre,during,post)

plot(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),axes=F,ylab="Frequency",xlab=" ",ylim=c(-0.1,1.1),main=" ",lwd=3)
box()
axis(side=1,at=c(1,3,5),labels=c("pre","during","post"))
axis(side=2)
l_text1=c(expression(paste(italic("C. gutturosus")," - very low GRC")), expression(paste(italic("C. arenicolus")," - low GRC")),expression(paste(italic("C. wartmanni")," - high GRC")),expression(paste(italic("C. macrophthalmus")," - very high GRC")))
legend("topright",legend=l_text1,pch=3,col=c(K,SF,BF,GF),bty="n",pt.cex=1,pt.lwd=1.5,xjust=1)
#lines(y,x,col="gray")
lines(y[c(2,5,8)],x[c(2,5,8)],col=GF)
lines(y[c(3,6,9)],x[c(3,6,9)],col=BF)
lines(y[c(4,7,10)],x[c(4,7,10)],col=SF)

points(y,x,pch=3,cex=2,col=c(K,GF,BF,SF,GF,BF,SF,GF,BF,SF),lwd=3)

dev.off()
