# TD question 12-14
source ("readfasta.r")
myseq<-readfasta("suzuki_and_pand2014_ali.fa")

# fonction pour compter les nuc dans un vecteur
comptenuc <- function(vec){
  vectcount<-rep(0,5)
  names(vectcount)=c("A","T","G","C","-")
  for (i in names(vectcount)){
    vectcount[i]<-length(which(vec==i))
  }
  return(vectcount)
}

# fonction pour calculer les freq des nuc dans un vecteur
freqnuc <- function (vec){
  r<-comptenuc(vec)
  tot<-sum(r)
  r<-r/tot
  return(r)
} 

# creation tableau des séquences
tabseq<-c()
for (i in (1:length(myseq))){
    v<-unlist(strsplit(myseq[i],""))
    names(v)=""
    tabseq<-rbind(tabseq,v)
}
rownames(tabseq)<-names(myseq)
ncol(tabseq)
nrow(tabseq)
