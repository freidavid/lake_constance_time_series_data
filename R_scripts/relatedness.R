library(vioplot)
library(RColorBrewer)
colors<-brewer.pal(12, name="Paired")
greys<-brewer.pal(9, name="Greys")
SF = colors[8]
GF = colors[4]
BF = colors[2]
K = greys[3]


#load data
kilch_RESULT <- read.delim("kilch_RESULT.txt")
are_pre <- read.delim("arenicolus_pre_RESULT.txt")
are_during <- read.delim("arenicolus_during_RESULT.txt")
are_post <- read.delim("arenicolus_post_RESULT.txt")
gang_pre <- read.delim("gangfisch_pre_RESULT.txt")
gang_during <- read.delim("gangfisch_during_RESULT.txt")
gang_post <- read.delim("gangfisch_post_RESULT.txt")
blau_pre <- read.delim("blaufelchen_pre_RESULT.txt")
blau_during <- read.delim("blaufelchen_during_RESULT.txt")
blau_post <- read.delim("blaufelchen_post_RESULT.txt")
dev.off()

pdf("Relatedness.pdf")
#each species separate
#gutturosus
vioplot(kilch_RESULT$rab,col=c(K),names=c("pre"),ylab="Relatedness",main=expression(bolditalic("C. gutturosus")))
stripchart(kilch_RESULT$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)

boxplot(kilch_RESULT$rab,col=c(K),names=c("pre"),ylab="Relatedness",main=expression(bolditalic("C. gutturosus")))
stripchart(kilch_RESULT$rab,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)

help("stripchart")
#Arenicolus

vioplot(are_pre$rab,are_during$rab,are_post$rab,col=c(SF,SF,SF),names=c("pre","during","post"),ylab="Relatedness",main=expression(bolditalic("C. arenicolus")))
stripchart(are_pre$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_during$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_post$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)


boxplot(are_pre$rab,are_during$rab,are_post$rab,col=c(SF,SF,SF),names=c("pre","during","post"),ylab="Relatedness",main=expression(bolditalic("C. arenicolus")))
stripchart(are_pre$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_during$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_post$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)


#wartmanni
vioplot(blau_pre$rab,blau_during$rab,blau_post$rab,col=c(BF,BF,BF),names=c("pre","during","post"),ylab="Relatedness",main=expression(bolditalic("C. wartmanni")))
stripchart(blau_pre$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_during$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_post$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)


boxplot(blau_pre$rab,blau_during$rab,blau_post$rab,col=c(BF,BF,BF),names=c("pre","during","post"),ylab="Relatedness",main=expression(bolditalic("C. wartmanni")))
stripchart(blau_pre$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_during$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_post$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)

#macrophthalmus
vioplot(gang_pre$rab,gang_during$rab,gang_post$rab,col=c(GF,GF),names=c("pre","during","post"),ylab="Relatedness",main=expression(bolditalic("C. macrophthalmus")))
stripchart(gang_pre$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_during$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_post$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)


boxplot(gang_pre$rab,gang_during$rab,gang_post$rab,col=c(GF,GF),names=c("pre","during","post"),ylab="Relatedness",main=expression(bolditalic("C. macrophthalmus")))
stripchart(gang_pre$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_during$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_post$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)


#all together
vioplot(kilch_RESULT$rab,are_pre$rab,are_during$rab,are_post$rab,gang_pre$rab,gang_during$rab,gang_post$rab,blau_pre$rab,blau_during$rab,blau_post$rab,col=c(K,SF,SF,SF,GF,GF,GF,BF,BF,BF),ylab="Relatedness",names=c("Kilch","SF_pre","SF_during","SF_post","GF_pre","GF_during","GF_post","BF_pre","BF_during","BF_post"))
stripchart(kilch_RESULT$rab,at=1,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_pre$rab,at=2,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_during$rab,at=3,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(are_post$rab,at=4,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_pre$rab,at=8,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_during$rab,at=9,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(blau_post$rab,at=10,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_pre$rab,at=5,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_during$rab,at=6,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)
stripchart(gang_post$rab,at=7,method = "jitter",pch = 19,vertical=TRUE,col = "darkgrey",add = TRUE)


boxplot(kilch_RESULT$rab,are_pre$rab,are_during$rab,are_post$rab,gang_pre$rab,gang_during$rab,gang_post$rab,blau_pre$rab,blau_during$rab,blau_post$rab,col=c(K,SF,SF,SF,GF,GF,GF,BF,BF,BF),ylab="Relatedness",names=c("Kilch","SF_pre","SF_during","SF_post","GF_pre","GF_during","GF_post","BF_pre","BF_during","BF_post"))


plot(c(1,2,3),c(mean(are_pre$rab),mean(are_during$rab),mean(are_post$rab)),ylab="Relatedness",xlab=" ",col=NA,pch=3,cex=2,axes=F,ylim=c(0,0.3))
axis(side=1,at=c(1,2,3),labels=c("pre","during","post"))
axis(side=2)
box()
lines(c(1,2,3),c(mean(are_pre$rab),mean(are_during$rab),mean(are_post$rab)),col=SF,lty=3)
points(c(1,2,3),c(mean(are_pre$rab),mean(are_during$rab),mean(are_post$rab)),col=SF,pch=3,cex=2)

lines(c(1,2,3),c(mean(gang_pre$rab),mean(gang_during$rab),mean(gang_post$rab)),col=GF,lty=3)
points(c(1,2,3),c(mean(gang_pre$rab),mean(gang_during$rab),mean(gang_post$rab)),col=GF,pch=3,cex=2)
lines(c(1,2,3),c(mean(blau_pre$rab),mean(blau_during$rab),mean(blau_post$rab)),lty=3,col=BF)
points(c(1,2,3),c(mean(blau_pre$rab),mean(blau_during$rab),mean(blau_post$rab)),col=BF,pch=3,cex=2)

 dev.off()
 
