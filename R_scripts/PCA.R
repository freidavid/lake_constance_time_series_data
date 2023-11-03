#load data and do pca
samples <- read.delim("~/Dropbox/Time_Series/samples.txt")
cov <- read.table("PCA.cov", quote="\"", comment.char="")
m<-as.matrix(cov)
e<-eigen(m)


#PCA to check species assignment
pdf("species_assignment.pdf")

#all
plot(e$vectors[,c(1,2)],main="All timepoints",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=samples$pch,col=samples$Color,cex=1.5)
#text(e$vectors[,c(1)],e$vectors[,c(3)],labels=samples$Label,cex=0.5)
l_text1=c("pre-eutrophication","during-eutrophication","post-eutrophication")
legend("bottomleft",legend=l_text1,pch=c(3,2,19),bty="n",pt.cex=1,pt.lwd=1.5)


plot(e$vectors[,c(1,3)],main="All timepoints",ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=samples$pch,col=samples$Color,cex=1.5)
text(e$vectors[,c(1)],e$vectors[,c(3)],labels=samples$Label,cex=0.5)

#Species corrected
#all
plot(e$vectors[,c(1,2)],main="All timempoints - species corrected",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=samples$pch,col=samples$Color_corr,cex=1.5)
l_text1=c("pre-eutrophication","during-eutrophication","post-eutrophication")
legend("bottomleft",legend=l_text1,pch=c(3,2,19),bty="n",pt.cex=1,pt.lwd=1.5)

dev.off()


pdf("~/Dropbox/Time_Series/PCA/species_assignment_separate.pdf",height=10.5,width=31)
par(mfrow=c(1,3),cex=2)
#pre
samples_pre<-samples[which(samples$Time=="pre"),]
plot(main=" ",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),e$vectors[which(samples$Time=="pre"),c(1,2)],ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=rep(19,length(samples_pre$pch)),col=samples_pre$Color,cex=2)
#text(e$vectors[which(samples$Time=="pre"),c(1)],e$vectors[which(samples$Time=="pre"),c(2)],labels=samples_pre$Label2,cex=0.5)



#during
samples_during<-samples[which(samples$Time=="during"),]
test<-cbind(rev(e$vectors[which(samples$Time=="during"),c(1)]),rev(e$vectors[which(samples$Time=="during"),c(2)]))
plot(main=" ",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),test,ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=rep(19,length(samples_during$pch)),col=rev(samples_during$Color),cex=2)
#text(e$vectors[which(samples$Time=="during"),c(1)],e$vectors[which(samples$Time=="during"),c(2)],labels=samples_during$Label,cex=0.5)


#post
samples_post<-samples[which(samples$Time=="post"),]
test<-cbind(rev(e$vectors[which(samples$Time=="post"),c(1)]),rev(e$vectors[which(samples$Time=="post"),c(2)]))
plot(main=" ",xlim=c(-0.2,0.15),ylim=c(-0.3,0.1),test,ylab=paste0("PC2 (",round(e$values[2]/sum(e$values)*100,digits=2),"%)"),xlab=paste0("PC1 (",round(e$values[1]/sum(e$values)*100,digits=2),"%)"),pch=rep(19,length(samples_post$pch)),col=rev(samples_post$Color),cex=2)
l_text1=c(expression(italic("C. gutturosus")),expression(italic("C. macrophthalmus")),expression(italic("C. wartmanni")), expression(italic("C. arenicolus")))
legend("bottomright",legend=l_text1,pch=19,col=c(K,GF,BF,SF),bty="n",pt.cex=2,xjust=1)

#text(e$vectors[which(samples$Time=="post"),c(1)],e$vectors[which(samples$Time=="post"),c(2)],labels=samples_post$Label,cex=0.5)



dev.off()


