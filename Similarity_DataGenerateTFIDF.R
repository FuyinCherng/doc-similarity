library(plyr) #ddply
library(dplyr)
library(lsa)
#========= function ===========
ComputeSimilarity <- function(A, B){
  #print(paste0("LengthOfWordFreq: ",length(A)))
  #--- cross entropy (https://artax.karlin.mff.cuni.cz/r-help/library/FNN/html/crossentropy.html)
  #how well B fit A
  #print(paste0("cross.entropy_entropyPackage:",crossentropy(A, B,k=1, algorithm = "brute")))
  crosentropy <- -(sum(A * log2(B)))
  #crosentropy3 <- entropy.empirical(A, unit="log2") + KL
  print(paste0("cross.entropy:", crosentropy))
  #print(paste0("cross.entropy_KL:", crosentropy3))
  
  #---- Jensen-Shannon Divergence  JS divergence
  m <- 0.5 * (A + B)
  JS <- 0.5 * (sum(A * log2(A / m)) + sum(B * log2(B / m)))
  print(paste0("JS: ",JS))
  
  #---- Kullback–Leibler divergence (the difference between cross entropy and entropy)
  KL <- (sum(A * log2(A / B))) #Dkl(A|B)
  print(paste0("KL: ",KL))
  
  #---- L1 distance
  L1 <- sum(abs(A-B))
  print(paste0("L1: ",L1))
  
  #---- L2 distance
  L2 <- sum((A-B)^2)
  print(paste0("L2: ",L2))
  
  #---- Chi-square test
  #chis <- sum((B-A)^2/A)
  #print(paste0("Chi-square: ",chis))
  
  #---- cosine similarity
  #cosine_sim <- cosine(A,B)
  #print(paste0("Cosine Similarity: ",cosine_sim))
  
  #return length, corss-entropy,JS, KL, L1, L2
  return(cbind.data.frame(length(A), crosentropy, JS, KL,L1,L2))
}

AbsoluteDiscounting <- function(matrix){
  wordFreqTFIDF <- matrix
  length_A <- nrow(wordFreqTFIDF) - length(wordFreqTFIDF[,2][wordFreqTFIDF[,2]==0])
  length_B <- nrow(wordFreqTFIDF) - length(wordFreqTFIDF[,3][wordFreqTFIDF[,3]==0])
  #smoothing: absolute discounting
  esp <- 1e-10
  ac <- esp* ((nrow(wordFreqTFIDF)-length_A) / length_A)
  bc <- esp* ((nrow(wordFreqTFIDF)-length_B) / length_B)
  
  for(i in 1:nrow(wordFreqTFIDF)){
    #print(union_wordlist[i])
    if(wordFreqTFIDF[,2][i] != 0){
      #get the probability of the word in train
      wordFreqTFIDF[,2][i] <- wordFreqTFIDF[,2][i] - ac
    }
    else{
      wordFreqTFIDF[,2][i] <- esp
    }
    
    if(wordFreqTFIDF[,3][i] != 0){
      #get the probability of the word in test
      wordFreqTFIDF[,3][i] <- wordFreqTFIDF[,3][i] - bc
    }
    else{
      wordFreqTFIDF[,3][i] <- esp
    }
  }
  return(wordFreqTFIDF)
}
#========= function ===========
toplist <- c(100,200,300,400,500,600,700,800,900,1000,
             1100,1200,1300,1400,1500,1600,1700,1800,1900,
             2000,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000)

for(top in toplist){
for(s in 1:5){
setwd("/Users/fuyincherng/Documents/EPFLCourse/semester project/Dataset/WordFreq_List/TFIDF")
train_test <- read.csv(paste0("WF_StackTrain_StackTest_",s,".csv"))
train_dsp <- read.csv(paste0("WF_StackTrain_MoocDsp_",s,".csv"))
train_reactive <- read.csv(paste0("WF_StackTrain_MoocReactive_",s,".csv"))
dsp_reactive <- read.csv(paste0("WF_MoocDsp_MoocReactive.csv"))

#sorted the word by Freq
train_test_sorted <- train_test[order(-train_test$StackTrain, -train_test$StackTest),][1:top,]
train_dsp_sorted <- train_dsp[order(-train_dsp$StackTrain, -train_dsp$MoocDsp),][1:top,]
train_reactive_sorted <- train_reactive[order(-train_reactive$StackTrain, -train_reactive$MoocReactive),][1:top,]
dsp_reactive_sorted <- dsp_reactive[order(-dsp_reactive$MoocDsp, -dsp_reactive$MoocReactive),][1:top,]

#switch sorted priority
#train_test_sorted_switch <- train_test[order(-train_test$StackTest,-train_test$StackTrain),][1:top,]
#train_dsp_sorted_switch <- train_dsp[order(-train_dsp$MoocDsp,-train_dsp$StackTrain),][1:top,]
#train_reactive_sorted_switch <- train_reactive[order(-train_reactive$MoocReactive,-train_reactive$StackTrain),][1:top,]
dsp_reactive_sorted_switch <- dsp_reactive[order(-dsp_reactive$MoocReactive,-dsp_reactive$MoocDsp),][1:top,]


#smoothing
train_test_smooth <- AbsoluteDiscounting(train_test_sorted)
train_dsp_smooth <- AbsoluteDiscounting(train_dsp_sorted)
train_reactive_smooth <- AbsoluteDiscounting(train_reactive_sorted)
dsp_reactive_smooth <- AbsoluteDiscounting(dsp_reactive_sorted)


#similarity of _smooth
r.train_test <- ComputeSimilarity(train_test_smooth[,2],train_test_smooth[,3])
r.train_dsp <- ComputeSimilarity(train_dsp_smooth[,2],train_dsp_smooth[,3])
r.train_reactive <- ComputeSimilarity(train_reactive_smooth[,2],train_reactive_smooth[,3])
r.dsp_reactive <- ComputeSimilarity(dsp_reactive_smooth[,2],dsp_reactive_smooth[,3])

#output file
Delta2Stack <- r.train_dsp - r.train_test
Delta2Stack$wordFreqtype <- "union"
Delta2Stack$Compare <- "Delta2Stack"
Delta2Stack$Top <- top

Deta2Reactive <- r.train_reactive - r.train_dsp
Deta2Reactive$wordFreqtype <- "union"
Deta2Reactive$Compare <- "Deta2Reactive"
Deta2Reactive$Top <- top


r.train_test$wordFreqtype <- "union"
r.train_test$Compare <- "train_test"
r.train_test$Top <- top

r.train_dsp$wordFreqtype <- "union"
r.train_dsp$Compare <- "train_dsp"
r.train_dsp$Top <- top

r.train_reactive$wordFreqtype <- "union"
r.train_reactive$Compare <- "train_reactive"
r.train_reactive$Top <- top

r.dsp_reactive$wordFreqtype <- "union"
r.dsp_reactive$Compare <- "dsp_reactive"
r.dsp_reactive$Top <- top

setwd("/Users/fuyincherng/Documents/EPFLCourse/semester project/Similarity/Result_TFIDF/TopK")
write.csv(rbind(Delta2Stack,Deta2Reactive,r.train_test,r.train_dsp,r.train_reactive,r.dsp_reactive),file=paste0('SimilarityResult_TFIDF_top',top,s,'.csv'), row.names = F)
}
}



