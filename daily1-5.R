new <- cbind(sample(Vocab$education, 6), sample(Vocab$vocabulary,5))
Vocab <- Vocab

x <- c(1,2,3,4,5,6,7,8,9,10)
y<- c(1,2,3,4,5,6,7,8,9,10)
new<-cbind(sample(x,2), sample(y,2))

Vsummary1<-function(){
  samp<-sample(Vocab,48)
  samp<-Vocab[sample(nrow(Vocab),size=48,replace=FALSE),]
  Vsub<-Vocab[,3:4]
  avgE<-mean(Vsub$education)
  avgS<-mean(Vsub$vocabulary)
  corES<-cor(Vsub$education,Vsub$vocabulary)
  vect<-c(avgE,avgS,corES)
  
}

replicate(22, Vsummary1(Vocab))
Vsummary2<-function(x){
  samp<-sample(Vocab,48)
  samp<-Vocab[sample(nrow(Vocab),size=48,replace=FALSE),]
  Vsub<-Vocab[,3:4]
  avgE<-mean(Vsub$education)
  avgS<-mean(Vsub$vocabulary)
  corES<-cor(Vsub$education,Vsub$vocabulary)
  vect<-c(avgE,avgS,corES)
  
}


Vocab.yr<-Vocab[Vocab$year==2006,]
samp<-Vocab.yr[sample(nrow(Vocab.yr),size=48,replace=FALSE),]
Vsub<-Vocab.yr[,3:4]
?write.table
