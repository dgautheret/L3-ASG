# fonctions
# ---------------------------------------

# lit un fichier fasta
# retourne un vecteur de character avec une sequence par element
# chaque element porte le nom de sequence commencant par ">"
readfasta <- function (fname) {
  lignes = readLines(fname)
  nli=length(lignes)
  allseq=c()
  seqnum=0
  i=1
  while (i <=nli) {
    l=lignes[i]
    if (grepl (">", l)) {
      seqnmame=l
      s=""
      i=i+1
      l=lignes[i]
      while ((i<=nli) & (!grepl (">", l))) {
        s=paste(s,l)
        i=i+1
        l=lignes[i]
      } 
      s=gsub('\\s','',s)
      names(s)=seqnmame
      allseq=c(allseq,s)
    }else{
      i=i+1
    }
  }
  return (allseq)
}

# convertit liste de strings (résultat de readfasta) en tableau de char
string2table <- function (stringvec)  {
  tabseq<-c()
  for (l in stringvec) {
    v <- strsplit(l,"")   # convertit string en liste
    v <-  unlist(v)       # unlist the result to obtain a vector 
    tabseq = rbind (tabseq,v)  # ajoute le vecteur v a tabseq 
  }
  return(tabseq)
}

# fonction de comptage de chaque lettres d'un vecteur
# ajoute: test pour ignorer les "-"
count <- function (x) {
  lettres <- unique (x)  # liste des lettres du vecteur x
  cnt <- rep (0, length(lettres)) # initialise le vec de comptes
  names(cnt) <- lettres
  for (i in x) {
    if (i != "-") {
      cnt[i] <- cnt[i]+1
    }
  }
  return (cnt)
}

# fonction mesurant les frequences des lettres dans un vecteur 
# retourne un vecteur de frequences
freq <- function (x){
  y <- count(x)
  return (y / sum(y))
}

# fonction mesurant l'entropie des lettres dans un vecteur
# retourne une valeur unique d'entropie
entropie <- function (x) {
  f <- freq(x)
  h=0
  for (i in f) {
    if (i != 0) {
      h=h+i*log2(i)
    }
  }
  return(-h)
}

