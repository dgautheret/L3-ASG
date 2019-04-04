# TD question 12-14
source ("readfasta.r")
myseq<-readfasta("suzuki_and_pand2014_ali.faa")

# fonction pour compter les nuc dans un vecteur
comptenuc <- function(vec){
  vectcount<-rep(0,5)
  names(vectcount)=c("A","T","G","C","-")
  for (i in names(vectcount)){
    vectcount[i]<-length(which(vec==i))
  }
  return(vectcount)
}

# fonction pour compter les nuc dans un vecteur
compteaa <- function(vec){
  vectcount<-rep(0,21)
  names(vectcount)=c("I", "M", "T", "N", "K", "S", "R", "L", "P", "H", "Q", "V",
                     "A", "D", "E", "G", "F", "Y", "-", "C", "W")
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

# fonction pour calculer les freq des nuc dans un vecteur
freqaa <- function (vec){
  r<-compteaa(vec)
  tot<-sum(r)
  r<-r/tot
  return(r)
} 

# fonction pour calculer l'entropie d'un vecteur
entropie <- function (vec){
  fvec<-freqaa(vec)
  H<-0
  for (i in (1:length(fvec))){
    f<-fvec[i]
    if (f!=0){
      H<-H+(f*log2(f))
    }
  }
  return(-H)
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

# entropy of all columns
H<-c()
for (i in (1:ncol(tabseq))){
  H<-c(H,entropie(tabseq[,i]))
}

# x and y axes for plot
x=1:length(H)
y=H

# ugly 
plot (H, type="l")

# nice
plot (H,type="n") #plot just axes
lisse = smooth.spline(x, y, spar=0.35)
lines(lisse)

# entropie la plus elevee
print (max(H))

# position d'entropie la plus elevee
print (which(H==max(H)))

# 10% des positions de plus haute entropie
order(H,decreasing = T)[1:(length(H)/10)]