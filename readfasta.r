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
