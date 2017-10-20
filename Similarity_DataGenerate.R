#this program is to compute cross-entropy between stack data and MOOC data
#to know whether we can apply stacl model to  MOOC data
library(plyr) #ddply
library(dplyr)
library(entropy)
library(FNN)
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
  
  #return length, corss-entropy,JS, KL, L1, L2
  return(cbind.data.frame(length(A), crosentropy, JS, KL,L1,L2))
}

GenerateUnion <- function(A, B){
  all_wordFreq <- merge(A, B, all=TRUE)
  union_wordlist <- unique(all_wordFreq$word) #generate the union word list for two corpus words
  #print(union_wordlist[1:10])
  
  #smoothing: absolute discounting
  esp <- 1e-10
  ac <- esp* ((length(union_wordlist)-length(A)) / length(A))
  bc <- esp* ((length(union_wordlist)-length(B)) / length(B))
  
  list_A <- list()
  list_B <- list()
  for(i in 1:length(union_wordlist)){
    #print(union_wordlist[i])
    if(union_wordlist[i] %in% A$word){
      #get the probability of the word in train
      prob_A <- A$frequency[match(union_wordlist[i],A$word)] - ac
    }
    else{
      prob_A <- esp
    }
    #print(prob_A)
    list_A <- c(list_A, prob_A)
    
    if(union_wordlist[i] %in% B$word){
      #get the probability of the word in test
      prob_B <- B$frequency[match(union_wordlist[i],B$word)] - bc
    }
    else{
      prob_B <- esp
    }
    #print(prob_B)
    list_B <- c(list_B, prob_B)
  }
  
  return(cbind.data.frame(union_wordlist, cbind(list_A, list_B)))
}

GenerateUnion_overall <- function(A, B, union_wordlist){
  #print(union_wordlist[1:10])
  
  #smoothing: absolute discounting
  esp <- 1e-10
  ac <- esp* ((length(union_wordlist)-length(A)) / length(A))
  bc <- esp* ((length(union_wordlist)-length(B)) / length(B))
  
  list_A <- list()
  list_B <- list()
  for(i in 1:length(union_wordlist)){
    #print(union_wordlist[i])
    if(union_wordlist[i] %in% A$word){
      #get the probability of the word in train
      prob_A <- A$frequency[match(union_wordlist[i],A$word)] - ac
    }
    else{
      prob_A <- esp
    }
    #print(prob_A)
    list_A <- c(list_A, prob_A)
    
    if(union_wordlist[i] %in% B$word){
      #get the probability of the word in test
      prob_B <- B$frequency[match(union_wordlist[i],B$word)] - bc
    }
    else{
      prob_B <- esp
    }
    #print(prob_B)
    list_B <- c(list_B, prob_B)
  }
  
  return(cbind.data.frame(union_wordlist, cbind(list_A, list_B)))
}


#========= function end ===========
for(s in 1:2){
  s=1
  all = TRUE
  overall = TRUE
  sample_num = paste0("_",s)
  k = "_all"

  #--- input WordFreq A and B
  setwd('/Users/fuyincherng/Documents/EPFLCourse/semester project/Dataset/WordFreq_List/25022017')
  stack_wfreq_train <- read.csv(paste0("WordFreq_Stack_Train",sample_num,".csv"))
  stack_wfreq_test <- read.csv(paste0("WordFreq_Stack_Test",sample_num,".csv"))
  dsp_mooc <- read.csv("WordFreq_MOOC.csv")
  reactive_mooc <- read.csv("WordFreq_Reactive.csv")



  # order from high frequency to low frequency
  stack_wfreq_train <- stack_wfreq_train[order(-stack_wfreq_train$frequency),]
  stack_wfreq_test <- stack_wfreq_test[order(-stack_wfreq_test$frequency),]
  dsp_mooc <- dsp_mooc[order(-dsp_mooc$frequency),]
  reactive_mooc <- reactive_mooc[order(-reactive_mooc$frequency),]

#---- create unite word frenquency list for two corpus 
# generate intersection
intersection_wordFreq_stack <- merge(stack_wfreq_train, stack_wfreq_test, by="word")
intersection_wordFreq_train_dsp <- merge(stack_wfreq_train, dsp_mooc, by="word")
intersection_wordFreq_train_reactive <- merge(stack_wfreq_train, reactive_mooc, by="word") 
intersection_wordFreq_dsp_reactive <- merge(dsp_mooc, reactive_mooc, by="word") 
intersection_wordFreq_dsp_reactive <- intersection_wordFreq_dsp_reactive[order(-intersection_wordFreq_dsp_reactive$frequency.x,-intersection_wordFreq_dsp_reactive$frequency.y),]
write.csv(intersection_wordFreq_dsp_reactive,file="intersection_dsp_reactive.csv")

# generate union
#all
if(!overall){
if(all){
  Stack_train_test_union <- GenerateUnion(stack_wfreq_train, stack_wfreq_test)
  Stack_train_dsp_union <- GenerateUnion(stack_wfreq_train, dsp_mooc)
  Stack_train_reactive_union <- GenerateUnion(stack_wfreq_train, reactive_mooc)
  Stack_dsp_reactive_union <- GenerateUnion(dsp_mooc, reactive_mooc)
}

#rank top k
if(!all){
  Stack_train_test_union <- GenerateUnion(stack_wfreq_train[1:k,], stack_wfreq_test[1:k,])
  Stack_train_dsp_union <- GenerateUnion(stack_wfreq_train[1:k,], dsp_mooc[1:k,])
  Stack_train_reactive_union <- GenerateUnion(stack_wfreq_train[1:k,], reactive_mooc[1:k,])
  Stack_train_reactive_union <- GenerateUnion(stack_wfreq_train[1:k,], reactive_mooc[1:k,])
  }
}

if(overall){
#overall union
all_wordFreq <- merge(stack_wfreq_train, stack_wfreq_test, all=TRUE)
all_wordFreq <- merge(all_wordFreq,dsp_mooc,all=TRUE)
all_wordFreq <- merge(all_wordFreq, reactive_mooc, all=TRUE)
union_wordlist <- unique(all_wordFreq$word) #generate the union word list for two corpus words
Stack_train_test_union <- GenerateUnion_overall(stack_wfreq_train, stack_wfreq_test,union_wordlist)
Stack_train_dsp_union <- GenerateUnion_overall(stack_wfreq_train, dsp_mooc,union_wordlist)
Stack_train_reactive_union <- GenerateUnion_overall(stack_wfreq_train, reactive_mooc,union_wordlist)

}

result <- ComputeSimilarity(as.numeric(Stack_dsp_reactive_union$list_A),as.numeric(Stack_dsp_reactive_union$list_B))

#set the path for saving data
setwd('/Users/fuyincherng/Documents/EPFLCourse/semester project/Similarity/Result')
#--- assigned probability distribution
print("---------- intersection, all ------------")
print("Stack_train X Stack_test: ")
train_test <- ComputeSimilarity(intersection_wordFreq_stack$frequency.x, intersection_wordFreq_stack$frequency.y)

print("Stack_train X dsp: ")
train_dsp <- ComputeSimilarity(intersection_wordFreq_train_dsp$frequency.x, intersection_wordFreq_train_dsp$frequency.y)

print("Stack_train X reactive: ")
train_reactive <- ComputeSimilarity(intersection_wordFreq_train_reactive$frequency.x, intersection_wordFreq_train_reactive$frequency.y)

#compute difference (train,test)----------------(train,dsp)------------------(train,reactive)
#                                 Theta2Stack                Theta2Reactive
Theta2Stack <- train_dsp - train_test
Theta2Stack$wordFreqtype <- "intersection"
Theta2Stack$Compare <- "Theta2Stack"

Theta2Reactive <- train_reactive - train_dsp
Theta2Reactive$wordFreqtype <- "intersection"
Theta2Reactive$Compare <- "Theta2Reactive"

train_test$wordFreqtype <- "intersection"
train_test$Compare <- "train_test"
train_dsp$wordFreqtype <- "intersection"
train_dsp$Compare <- "train_dsp"
train_reactive$wordFreqtype <- "intersection"
train_reactive$Compare <- "train_reactive"

if(all){
 write.csv(rbind(train_test,train_dsp,train_reactive,Theta2Stack,Theta2Reactive),file=paste0('SimilarityResult_intersection',sample_num,'.csv'), row.names = F)
}

print("---------- union, all ------------")
print("Stack_train X Stack_test: ")
train_test <- ComputeSimilarity(as.numeric(Stack_train_test_union[,2]), as.numeric(Stack_train_test_union[,3]))

print("Stack_train X dsp: ")
train_dsp <- ComputeSimilarity(as.numeric(Stack_train_dsp_union[,2]),as.numeric(Stack_train_dsp_union[,3]))

print("Stack_train X reactive: ")
train_reactive <- ComputeSimilarity(as.numeric(Stack_train_reactive_union[,2]), as.numeric(Stack_train_reactive_union[,3]))

#print("dsp X reactive: ")
#dsp_reactive <- ComputeSimilarity(as.numeric(dsp_reactive_union[,2]), as.numeric(dsp_reactive_union[,3]))

#compute difference (train,test)----------------(train,dsp)------------------(train,reactive)
#                                 Theta2Stack                Theta2Reactive
Theta2Stack <- train_dsp - train_test
Theta2Stack$wordFreqtype <- "union"
Theta2Stack$Compare <- "Theta2Stack"

Theta2Reactive <- train_reactive - train_dsp
Theta2Reactive$wordFreqtype <- "union"
Theta2Reactive$Compare <- "Theta2Reactive"

train_test$wordFreqtype <- "union"
train_test$Compare <- "train_test"
train_dsp$wordFreqtype <- "union"
train_dsp$Compare <- "train_dsp"
train_reactive$wordFreqtype <- "union"
train_reactive$Compare <- "train_reactive"

write.csv(rbind(train_test,train_dsp,train_reactive,Theta2Stack,Theta2Reactive),file=paste0('SimilarityResult_overallunion',sample_num,'_',k,'.csv'), row.names = F)

# save union word list
  if(all){
    write.csv(cbind.data.frame(Stack_train_test_union$union_wordlist, as.numeric(Stack_train_test_union$list_A), as.numeric(Stack_train_test_union$list_B)) ,file=paste0("union_all_wordlist/overallunion_wordlist_train_test",sample_num,".csv"), row.names=F)
    write.csv(cbind.data.frame(Stack_train_dsp_union$union_wordlist, as.numeric(Stack_train_dsp_union$list_A), as.numeric(Stack_train_dsp_union$list_B)) ,file=paste0("union_all_wordlist/overallunion_wordlist_train_dsp",sample_num,".csv"), row.names=F)
    write.csv(cbind.data.frame(Stack_train_reactive_union$union_wordlist, as.numeric(Stack_train_reactive_union$list_A), as.numeric(Stack_train_reactive_union$list_B)) ,file=paste0("union_all_wordlist/overallunion_wordlist_train_reactive",sample_num,".csv"), row.names=F)
    write.csv(cbind.data.frame(Stack_train_dsp_union$union_wordlist, as.numeric(Stack_train_dsp_union$list_A), as.numeric(Stack_train_dsp_union$list_B)) ,file=paste0("union_all_wordlist/overallunion_wordlist_train_dsp",sample_num,".csv"), row.names=F)
  }
}

