k_fst<-read.delim("Fst_kilch_1bp.txt", comment.char="#",header=F)
k_fst<-k_fst[-1,]
colnames(k_fst)<-c("region","chr","position","Nsites","Fst")
k_fst[which(k_fst$Fst<0),]$Fst<-0
k_fst<-na.omit(k_fst)


sort(k_fst$Fst,decreasing = T)
top_k<-sort(k_fst$Fst,decreasing = T)
top_k<-top_k[1:50]
snps_k<-k_fst[which(k_fst$Fst==top_k[1]),]
for(i in 2:length(top_k)){
  snps_k<-rbind(snps_k,k_fst[which(k_fst$Fst==top_k[i]),])
}

write.table(cbind(snps_k$chr,as.numeric(snps_k$position)-1,snps_k$position),"kilch_50_fst.bed",quote=F,col.names =F,row.names = F)
k_quant<-k_fst[which(k_fst$Fst>quantile(k_fst$Fst,0.999)),]
write.table(cbind(k_quant$chr,as.numeric(k_quant$position)-1,k_quant$position),"kilch_quant_fst.bed",quote=F,col.names =F,row.names = F)






sf_fst<-read.delim("Fst_arenicolus_1bp.txt", comment.char="#",header=F)
sf_fst<-sf_fst[-1,]
colnames(sf_fst)<-c("region","chr","position","Nsites","Fst")
sf_fst[which(sf_fst$Fst<0),]$Fst<-0
sf_fst<-na.omit(sf_fst)
top_sf<-sort(sf_fst$Fst,decreasing = T)
top_sf<-top_sf[1:50]
snps_sf<-sf_fst[which(sf_fst$Fst==top_sf[1]),]
for(i in 2:length(top_sf)){
  snps_sf<-rbind(snps_sf,sf_fst[which(sf_fst$Fst==top_sf[i]),])
}
write.table(cbind(snps_sf$chr,as.numeric(snps_sf$position)-1,snps_sf$position),"sandfelchen_50_fst.bed",quote=F,col.names =F,row.names = F)
sf_quant<-sf_fst[which(sf_fst$Fst>quantile(sf_fst$Fst,0.999)),]
write.table(cbind(sf_quant$chr,as.numeric(sf_quant$position)-1,sf_quant$position),"andfelchen_quant_fst.bed",quote=F,col.names =F,row.names = F)



gf_fst<-read.delim("Fst_gangfisch_1bp.txt", comment.char="#",header=F)
gf_fst<-gf_fst[-1,]
colnames(gf_fst)<-c("region","chr","position","Nsites","Fst")
gf_fst[which(gf_fst$Fst<0),]$Fst<-0
gf_fst<-na.omit(gf_fst)
sort(gf_fst$Fst,decreasing = T)
top_gf<-sort(gf_fst$Fst,decreasing = T)
top_gf<-top_gf[1:50]
snps_gf<-gf_fst[which(gf_fst$Fst==top_gf[1]),]
for(i in 2:length(top_gf)){
  snps_gf<-rbind(snps_gf,gf_fst[which(gf_fst$Fst==top_gf[i]),])
}

write.table(cbind(snps_gf$chr,as.numeric(snps_gf$position)-1,snps_gf$position),"gangfisch_50_fst.bed",quote=F,col.names =F,row.names = F)
gf_quant<-gf_fst[which(gf_fst$Fst>quantile(gf_fst$Fst,0.999)),]
write.table(cbind(gf_quant$chr,as.numeric(gf_quant$position)-1,gf_quant$position),"gangfisch_quant_fst.bed",quote=F,col.names =F,row.names = F)


bf_fst<-read.delim("Fst_blaufelchen_1bp.txt", comment.char="#",header=F)
bf_fst<-bf_fst[-1,]
colnames(bf_fst)<-c("region","chr","position","Nsites","Fst")
bf_fst[which(bf_fst$Fst<0),]$Fst<-0
bf_fst<-na.omit(bf_fst)
top_bf<-sort(bf_fst$Fst,decreasing = T)
top_bf<-top_bf[1:50]
snps_bf<-bf_fst[which(bf_fst$Fst==top_bf[1]),]
for(i in 2:length(top_bf)){
snps_bf<-rbind(snps_bf,bf_fst[which(bf_fst$Fst==top_bf[i]),])
}

write.table(cbind(snps_bf$chr,as.numeric(snps_bf$position)-1,snps_bf$position),"blaufelchen_50_fst.bed",quote=F,col.names =F,row.names = F)
bf_quant<-bf_fst[which(bf_fst$Fst>quantile(bf_fst$Fst,0.999)),]
write.table(cbind(bf_quant$chr,as.numeric(bf_quant$position)-1,bf_quant$position),"blaufelchen_quant_fst.bed",quote=F,col.names =F,row.names = F)




#Pot Manhattan plots

order<-c("PGA_scaffold0__352_contigs__length_93459789",
         "PGA_scaffold1__210_contigs__length_43329510",
         "PGA_scaffold2__176_contigs__length_42764345",
         "PGA_scaffold4__243_contigs__length_45591172",
         "PGA_scaffold5__223_contigs__length_43692974" ,
         "PGA_scaffold7__351_contigs__length_68138733",
         "PGA_scaffold8__181_contigs__length_65193448" ,
         "PGA_scaffold9__196_contigs__length_60468309" ,
         "PGA_scaffold10__182_contigs__length_63177489",
         "PGA_scaffold11__203_contigs__length_63881516",
         "PGA_scaffold12__167_contigs__length_57740044",
         "PGA_scaffold13__147_contigs__length_47256133",
         "PGA_scaffold14__173_contigs__length_55641933",
         "PGA_scaffold15__168_contigs__length_54025139",
         "PGA_scaffold17__183_contigs__length_51949489",
         "PGA_scaffold18__164_contigs__length_59907985",
         "PGA_scaffold19__147_contigs__length_54335267",
         "PGA_scaffold20__181_contigs__length_52945597",
         "PGA_scaffold22__199_contigs__length_52020451",
         "PGA_scaffold23__167_contigs__length_50329371",
         "PGA_scaffold24__152_contigs__length_51033154" ,
         "PGA_scaffold25__179_contigs__length_50922480",
         "PGA_scaffold26__192_contigs__length_48683376" ,
         "PGA_scaffold28__172_contigs__length_48977775",
         "PGA_scaffold29__157_contigs__length_48675208",
         "PGA_scaffold30__165_contigs__length_48446552",
         "PGA_scaffold32__183_contigs__length_44662967",
         "PGA_scaffold33__143_contigs__length_40727438",
         "PGA_scaffold35__141_contigs__length_42009912",
         "PGA_scaffold38__206_contigs__length_33962415",
         "PGA_scaffold39__3_contigs__length_1094305")


#correct chromosome order
k_fst_a<-k_fst[which(k_fst$chr==order[1]),]
for(i in 2:length(order)){
  k_fst_a<-rbind(k_fst_a,k_fst[which(k_fst$chr==order[i]),])
}
k_fst<-k_fst_a

gf_fst_a<-gf_fst[which(gf_fst$chr==order[1]),]
for(i in 2:length(order)){
  gf_fst_a<-rbind(gf_fst_a,gf_fst[which(gf_fst$chr==order[i]),])
}
gf_fst<-gf_fst_a

bf_fst_a<-bf_fst[which(bf_fst$chr==order[1]),]
for(i in 2:length(order)){
  bf_fst_a<-rbind(bf_fst_a,bf_fst[which(bf_fst$chr==order[i]),])
}
bf_fst<-bf_fst_a

sf_fst_a<-sf_fst[which(sf_fst$chr==order[1]),]
for(i in 2:length(order)){
  sf_fst_a<-rbind(sf_fst_a,sf_fst[which(sf_fst$chr==order[i]),])
}
sf_fst<-sf_fst_a



#manhattan plots
library(qqman)


#kilch
manhattan_data<-as.data.frame(k_fst[c(2:3,5,4)])
colnames(manhattan_data)<-c("CHR","BP","P","SNP")
manhattan_data$BP<-as.numeric(manhattan_data$BP)
#replace chromsomes with numbers
chr<-unique(manhattan_data$CHR)
new_chr<-rep(1,length(which(manhattan_data$CHR==chr[1])))
for(i in 2:length(chr)){
  new_chr<-c(new_chr,rep(i,length(which(manhattan_data$CHR==chr[i]))))
}
manhattan_data$CHR<-new_chr
#chromosome labels
chr_lab<-as.character(c(1:3,5,6,8:16,18:21,23:27,29:31,33,34,36,39,40))
#colors
manhattan_data<-na.omit(manhattan_data)


#create vector with names
names<-vector()
for(i in 1:length(which(manhattan_data$P>=limit))){
  names[i]<-paste0("snp",i)
}

#get indices of signifcatn snps
vec<-rep(0,length(manhattan_data$CHR))
indices<-which(manhattan_data$P>=quantile(k_fst$Fst,0.999))
for(i in 1:length(indices)){vec[indices[i]]<-names[i]}
manhattan_data<-cbind(manhattan_data,vec)
manhattan_data$vec<-as.character(manhattan_data$vec)




png("kilch_fst.png",width=15,height=4,unit="in",res=1200)
manhattan_custom(manhattan_data,p="P",highlight=names,snp="vec",chrlabs = chr_lab,suggestiveline=F,genomewideline=F,col=c("darkgrey","black"),logp=F,ylab=expression(italic("F")[ST]))
dev.off()



#gangfisch
manhattan_data<-as.data.frame(gf_fst[c(2:3,5,4)])
colnames(manhattan_data)<-c("CHR","BP","P","SNP")
manhattan_data$BP<-as.numeric(manhattan_data$BP)
#replace chromsomes with numbers
chr<-unique(manhattan_data$CHR)
new_chr<-rep(1,length(which(manhattan_data$CHR==chr[1])))
for(i in 2:length(chr)){
  new_chr<-c(new_chr,rep(i,length(which(manhattan_data$CHR==chr[i]))))
}
manhattan_data$CHR<-new_chr
#chromosome labels
chr_lab<-as.character(c(1:3,5,6,8:16,18:21,23:27,29:31,33,34,36,39,40))
#colors
manhattan_data<-na.omit(manhattan_data)


#create vector with names
names<-vector()
for(i in 1:length(which(manhattan_data$P>=limit))){
  names[i]<-paste0("snp",i)
}

#get indices of signifcatn snps
vec<-rep(0,length(manhattan_data$CHR))
indices<-which(manhattan_data$P>=quantile(gf_fst$Fst,0.999))
for(i in 1:length(indices)){vec[indices[i]]<-names[i]}
manhattan_data<-cbind(manhattan_data,vec)
manhattan_data$vec<-as.character(manhattan_data$vec)



png("gangfisch_fst.png",width=15,height=4,unit="in",res=1200)
manhattan_custom(manhattan_data,p="P",highlight=names,snp="vec",chrlabs = chr_lab,suggestiveline=F,genomewideline=F,col=c(GF,"darkgreen"),logp=F,ylab=expression(italic("F")[ST]))
dev.off()



#blaufelchen
manhattan_data<-as.data.frame(bf_fst[c(2:3,5,4)])
colnames(manhattan_data)<-c("CHR","BP","P","SNP")
manhattan_data$BP<-as.numeric(manhattan_data$BP)
#replace chromsomes with numbers
chr<-unique(manhattan_data$CHR)
new_chr<-rep(1,length(which(manhattan_data$CHR==chr[1])))
for(i in 2:length(chr)){
  new_chr<-c(new_chr,rep(i,length(which(manhattan_data$CHR==chr[i]))))
}
manhattan_data$CHR<-new_chr
#chromosome labels
chr_lab<-as.character(c(1:3,5,6,8:16,18:21,23:27,29:31,33,34,36,39,40))
#colors
manhattan_data<-na.omit(manhattan_data)

#create vector with names
names<-vector()
for(i in 1:length(which(manhattan_data$P>=limit))){
  names[i]<-paste0("snp",i)
}

#get indices of signifcatn snps
vec<-rep(0,length(manhattan_data$CHR))
indices<-which(manhattan_data$P>=limit)
for(i in 1:length(indices)){vec[indices[i]]<-names[i]}
manhattan_data<-cbind(manhattan_data,vec)
manhattan_data$vec<-as.character(manhattan_data$vec)


png("blaufelchen_fst.png",width=15,height=4,unit="in",res=1200)
manhattan_custom(manhattan_data,p="P",highlight=names,snp="vec",chrlabs = chr_lab,suggestiveline=F,genomewideline=F,col=c(BF,"midnightblue"),logp=F,ylab=expression(italic("F")[ST]))
dev.off()



#sandfelchen
manhattan_data<-as.data.frame(sf_fst[c(2:3,5,4)])
colnames(manhattan_data)<-c("CHR","BP","P","SNP")
manhattan_data$BP<-as.numeric(manhattan_data$BP)
#replace chromsomes with numbers
chr<-unique(manhattan_data$CHR)
new_chr<-rep(1,length(which(manhattan_data$CHR==chr[1])))
for(i in 2:length(chr)){
  new_chr<-c(new_chr,rep(i,length(which(manhattan_data$CHR==chr[i]))))
}
manhattan_data$CHR<-new_chr
#chromosome labels
chr_lab<-as.character(c(1:3,5,6,8:16,18:21,23:27,29:31,33,34,36,39,40))
#colors
manhattan_data<-na.omit(manhattan_data)

#create vector with names
names<-vector()
for(i in 1:length(which(manhattan_data$P>=limit))){
  names[i]<-paste0("snp",i)
}

#get indices of signifcatn snps
vec<-rep(0,length(manhattan_data$CHR))
indices<-which(manhattan_data$P>=quantile(sf_fst$Fst,0.999))
for(i in 1:length(indices)){vec[indices[i]]<-names[i]}
manhattan_data<-cbind(manhattan_data,vec)
manhattan_data$vec<-as.character(manhattan_data$vec)



png("sandfelchen_fst.png",width=15,height=4,unit="in",res=1200)
manhattan_custom(manhattan_data,p="P",highlight=names,snp="vec",chrlabs = chr_lab,suggestiveline=F,genomewideline = F,col=c(SF,"orange4"),logp=F,ylab=expression(italic("F")[ST]))
dev.off()




