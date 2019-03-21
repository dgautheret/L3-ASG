#!/bin/bash
# Script Bash pour telecharger une liste d'accessions Fasta

# accessions of EbolaV GP prot from Suzuki & Gojobori, Mol. Biol. Evol. 1997.
accessions=(U23069 U23152 U23187 U23416 U23417 U28006 U28077 U28134 U31033)

touch gp.fa
for acc in ${accessions[@]}
do
  curl 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id='$acc'&rettype=fasta&retmode=text' >> gp.fa
  sleep 2   # avoid smapping the NCBI server
done


