#Draw the similarity computed by similarity_dataGenerate.R
#plot1: c(corss,JS,KL,L1,L2)~Compare
library(gplots)

#setwd('/Users/fuyincherng/Documents/EPFLCourse/semester project/Similarity/Result')
setwd("/Users/fuyincherng/Documents/EPFLCourse/semester project/Similarity/Result_TFIDF/TopK")
toplist <- c(100,200,300,400,500,600,700,800,900,1000,
             1100,1200,1300,1400,1500,1600,1700,1800,1900,
             2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000)
sample_all <- list()
for(top in toplist){

sample_1<-read.csv(paste0("SimilarityResult_TFIDF_top",top,"1.csv"))
sample_2 <- read.csv(paste0("SimilarityResult_TFIDF_top",top,"2.csv"))
sample <- rbind(sample_1, sample_2)

for(x in 3:5){
  sample_x <- read.csv(paste0("SimilarityResult_TFIDF_top",top,x,".csv"))
  sample <- rbind(sample,sample_x)
}

sample_all <- rbind(sample_all,sample)

}
write.csv(sample_all, file="SimilarityResult_TFIDF_AlltopK.csv",row.names = F)

# top K comparison
setwd('/Users/fuyincherng/Documents/EPFLCourse/semester project/Similarity/Result')
#corss Entropy
pdf(paste0("Fig/TopKCompare/TopKCompare_CrossEntropy_switch.pdf"), width=8, height=5.5)
plot(sample_all$crosentropy[sample_all$Compare == 'train_reactive']~sample_all$Top[sample_all$Compare == 'train_reactive'],col = "chartreuse4", ylim=c(0,18),xlab="Top K words", ylab = "crossEntropy")
points(sample_all$crosentropy[sample_all$Compare == 'train_test']~sample_all$Top[sample_all$Compare == 'train_test'],col = "cornflowerblue")
points(sample_all$crosentropy[sample_all$Compare == 'train_dsp']~sample_all$Top[sample_all$Compare == 'train_dsp'],col = "red")
points(sample_all$crosentropy[sample_all$Compare == 'dsp_reactive']~sample_all$Top[sample_all$Compare == 'dsp_reactive'],col = "darkorange")
legend("bottomright", "(x,y)",c("diver(train,reactive)","diver(train,test)","diver(train,dsp)","diver(dsp,reactive)"),lty=c(1,1,1,1), lwd=c(2,2,2,2),col=c("chartreuse4","cornflowerblue","red","darkorange"))
dev.off()

pdf(paste0("Fig/TopKCompare/TopKCompare_Delta_CrossEntropy_switch.pdf"), width=8, height=5.5)
plot(sample_all$crosentropy[sample_all$Compare == 'Delta2Stack']~sample_all$Top[sample_all$Compare == 'Delta2Stack'],col="cornflowerblue",type="p",xlab="Top K words", ylab = "crossEntropy")
points(sample_all$crosentropy[sample_all$Compare == 'Deta2Reactive']~sample_all$Top[sample_all$Compare == 'Deta2Reactive'],col="chartreuse4")
legend("bottomright", "(x,y)",c('Delta from diver(train,dsp) to diver(train,test)','Delta from diver(train,dsp) to diver(train,reactive)'),lty=c(1,1),lwd=c(2,2),col=c("cornflowerblue","chartreuse4"))
dev.off()

#JS
pdf(paste0("Fig/TopKCompare/TopKCompare_JS_switch.pdf"), width=8, height=5.5)
plot(sample_all$JS[sample_all$Compare == 'train_reactive']~sample_all$Top[sample_all$Compare == 'train_reactive'],col = "chartreuse4", xlab="Top K words",ylim=c(0,0.5), ylab = "JS")
points(sample_all$JS[sample_all$Compare == 'train_test']~sample_all$Top[sample_all$Compare == 'train_test'],col = "cornflowerblue")
points(sample_all$JS[sample_all$Compare == 'train_dsp']~sample_all$Top[sample_all$Compare == 'train_dsp'],col = "red")
points(sample_all$JS[sample_all$Compare == 'dsp_reactive']~sample_all$Top[sample_all$Compare == 'dsp_reactive'],col = "darkorange")
legend("topleft", "(x,y)",c("diver(train,reactive)","diver(train,test)","diver(train,dsp)","diver(dsp,reactive)"),lty=c(1,1,1,1), lwd=c(2,2,2,2),col=c("chartreuse4","cornflowerblue","red","darkorange"))
dev.off()

pdf(paste0("Fig/TopKCompare/TopKCompare_Delta_JS_switch.pdf"), width=8, height=5.5)
plot(sample_all$JS[sample_all$Compare == 'Delta2Stack']~sample_all$Top[sample_all$Compare == 'Delta2Stack'],col="cornflowerblue",type="p",xlab="Top K words", ylab = "JS")
points(sample_all$JS[sample_all$Compare == 'Deta2Reactive']~sample_all$Top[sample_all$Compare == 'Deta2Reactive'],col="chartreuse4")
legend("bottomright", "(x,y)",c('Delta from diver(train,dsp) to diver(train,test)','Delta from diver(train,dsp) to diver(train,reactive)'),lty=c(1,1),lwd=c(2,2),col=c("cornflowerblue","chartreuse4"))
dev.off()

#KL
pdf(paste0("Fig/TopKCompare/TopKCompare_KL_switch.pdf"), width=8, height=5.5)
plot(sample_all$KL[sample_all$Compare == 'train_reactive']~sample_all$Top[sample_all$Compare == 'train_reactive'],col = "chartreuse4", xlab="Top K words",ylim=c(0,8),ylab = "KL")
points(sample_all$KL[sample_all$Compare == 'train_test']~sample_all$Top[sample_all$Compare == 'train_test'],col = "cornflowerblue")
points(sample_all$KL[sample_all$Compare == 'train_dsp']~sample_all$Top[sample_all$Compare == 'train_dsp'],col = "red")
points(sample_all$KL[sample_all$Compare == 'dsp_reactive']~sample_all$Top[sample_all$Compare == 'dsp_reactive'],col = "darkorange")
legend("topleft", "(x,y)",c("diver(train,reactive)","diver(train,test)","diver(train,dsp)","diver(dsp,reactive)"),lty=c(1,1,1,1), lwd=c(2,2,2,2),col=c("chartreuse4","cornflowerblue","red","darkorange"))
dev.off()

pdf(paste0("Fig/TopKCompare/TopKCompare_Delta_KL_switch.pdf"), width=8, height=5.5)
plot(sample_all$KL[sample_all$Compare == 'Delta2Stack']~sample_all$Top[sample_all$Compare == 'Delta2Stack'],col="cornflowerblue",type="p",xlab="Top K words", ylab = "KL")
points(sample_all$KL[sample_all$Compare == 'Deta2Reactive']~sample_all$Top[sample_all$Compare == 'Deta2Reactive'],col="chartreuse4")
legend("bottomright", "(x,y)",c('Delta from diver(train,dsp) to diver(train,test)','Delta from diver(train,dsp) to diver(train,reactive)'),lty=c(1,1),lwd=c(2,2),col=c("cornflowerblue","chartreuse4"))
dev.off()

#L1
pdf(paste0("Fig/TopKCompare/TopKCompare_L1_switch.pdf"), width=8, height=5.5)
plot(sample_all$L1[sample_all$Compare == 'train_reactive']~sample_all$Top[sample_all$Compare == 'train_reactive'],col = "chartreuse4", xlab="Top K words",ylim=c(0,1.5), ylab = "L1")
points(sample_all$L1[sample_all$Compare == 'train_test']~sample_all$Top[sample_all$Compare == 'train_test'],col = "cornflowerblue")
points(sample_all$L1[sample_all$Compare == 'train_dsp']~sample_all$Top[sample_all$Compare == 'train_dsp'],col = "red")
points(sample_all$L1[sample_all$Compare == 'dsp_reactive']~sample_all$Top[sample_all$Compare == 'dsp_reactive'],col = "darkorange")
legend("topleft", "(x,y)",c("diver(train,reactive)","diver(train,test)","diver(train,dsp)","diver(dsp,reactive)"),lty=c(1,1,1,1), lwd=c(2,2,2,2),col=c("chartreuse4","cornflowerblue","red","darkorange"))
dev.off()

pdf(paste0("Fig/TopKCompare/TopKCompare_Delta_L1_switch.pdf"), width=8, height=5.5)
plot(sample_all$L1[sample_all$Compare == 'Delta2Stack']~sample_all$Top[sample_all$Compare == 'Delta2Stack'],col="cornflowerblue",type="p",xlab="Top K words", ylim=c(-0.1,0.6),ylab = "L1")
points(sample_all$L1[sample_all$Compare == 'Deta2Reactive']~sample_all$Top[sample_all$Compare == 'Deta2Reactive'],col="chartreuse4")
legend("bottomright", "(x,y)",c('Delta from diver(train,dsp) to diver(train,test)','Delta from diver(train,dsp) to diver(train,reactive)'),lty=c(1,1),lwd=c(2,2),col=c("cornflowerblue","chartreuse4"))
dev.off()

#L2
pdf(paste0("Fig/TopKCompare/TopKCompare_L2_switch.pdf"), width=8, height=5.5)
plot(sample_all$L2[sample_all$Compare == 'train_reactive']~sample_all$Top[sample_all$Compare == 'train_reactive'],col = "chartreuse4", xlab="Top K words",ylim=c(-0.0001,0.0025), ylab = "L2")
points(sample_all$L2[sample_all$Compare == 'train_test']~sample_all$Top[sample_all$Compare == 'train_test'],col = "cornflowerblue")
points(sample_all$L2[sample_all$Compare == 'train_dsp']~sample_all$Top[sample_all$Compare == 'train_dsp'],col = "red")
points(sample_all$L2[sample_all$Compare == 'dsp_reactive']~sample_all$Top[sample_all$Compare == 'dsp_reactive'],col = "darkorange")
legend("topleft", "(x,y)",c("diver(train,reactive)","diver(train,test)","diver(train,dsp)","diver(dsp,reactive)"),lty=c(1,1,1,1), lwd=c(2,2,2,2),col=c("chartreuse4","cornflowerblue","red","darkorange"))
dev.off()

pdf(paste0("Fig/TopKCompare/TopKCompare_Delta_L2_switch.pdf"), width=8, height=5.5)
plot(sample_all$L2[sample_all$Compare == 'Delta2Stack']~sample_all$Top[sample_all$Compare == 'Delta2Stack'],col="cornflowerblue",type="p",xlab="Top K words", ylab = "L2")
points(sample_all$L2[sample_all$Compare == 'Deta2Reactive']~sample_all$Top[sample_all$Compare == 'Deta2Reactive'],col="chartreuse4")
legend("bottomright", "(x,y)",c('Delta from diver(train,dsp) to diver(train,test)','Delta from diver(train,dsp) to diver(train,reactive)'),lty=c(1,1),lwd=c(2,2),col=c("cornflowerblue","chartreuse4"))
dev.off()

#plot figures for each K based on the comparison pairs
setwd('/Users/fuyincherng/Documents/EPFLCourse/semester project/Similarity/Result')
saveF_name = 
simiresult.matrix <- sample

pdf(paste0("Fig/",saveF_name,"/",saveF_name,"_crossEntropy.pdf"), width=8, height=5.5)
plotmeans(simiresult.matrix$crosentropy~simiresult.matrix$Compare, main="Similarity by CorssEntropy",xlab="",ylab ="", ylim=c(0,1),mean.labels=TRUE,digits=5,connect = FALSE)
dev.off()

pdf(paste0("Fig/",saveF_name,"/",saveF_name,"_JS.pdf"), width=8, height=5.5)
plotmeans(simiresult.matrix$JS~simiresult.matrix$Compare, main="Similarity by JS",xlab="",ylab ="",ylim=c(0,0.08),mean.labels=TRUE,digits=5,connect = FALSE)
dev.off()

pdf(paste0("Fig/",saveF_name,"/",saveF_name,"_KL.pdf"), width=8, height=5.5)
plotmeans(simiresult.matrix$KL~simiresult.matrix$Compare, main="Similarity by KL",xlab="",ylab ="", ylim=c(0,3),mean.labels=TRUE,digits=5,connect=FALSE)
dev.off()

pdf(paste0("Fig/",saveF_name,"/",saveF_name,"_L1.pdf"), width=8, height=5.5)
plotmeans(simiresult.matrix$L1~simiresult.matrix$Compare, main="Similarity by L1",xlab="",ylab ="", ylim=c(0, 0.3),mean.labels=TRUE,digits=5,connect=FALSE)
dev.off()

pdf(paste0("Fig/",saveF_name,"/",saveF_name,"_L2.pdf"), width=8, height=5.5)
plotmeans(simiresult.matrix$L2~simiresult.matrix$Compare, main="Similarity by L2",xlab="",ylab ="",ylim=c(0,0.001),mean.labels=TRUE,connect=FALSE)
dev.off()

# work in progress
#plotmeans(simiresult.matrix$chis~simiresult.matrix$Compare, main="Similarity by Chi-squared",xlab="",ylab ="",mean.labels=TRUE,connect=FALSE)
#plotmeans(simiresult.matrix$cosine_sim~simiresult.matrix$Compare, main="Similarity by CosineSimilarity",xlab="",ylab ="",mean.labels=TRUE,connect=FALSE)







