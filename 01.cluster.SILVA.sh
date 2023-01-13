#first we download the SILVA database
wget https://www.arb-silva.de/fileadmin/silva_databases/current/Exports/SILVA_138.1_SSURef_tax_silva.fasta.gz

#then we convert names to have underscore for later formatting
zcat SILVA_138.1_SSURef_tax_silva.fasta.gz | tr " " "_" | gzip > SILVA_138.1_SSURef_tax_underscore.fasta.gz

#now we remove all the non-Bacteria, and the uncultred and unidentified samples, since those clusters would have useless names
zcat SILVA_138.1_SSURef_tax_underscore.fasta.gz | grep "Bacteria;" | grep -v "unidentified" | grep -v "uncultured" | sed "s/^>//" > SILVA_138.1_SSURef_bacteria_ids.txt

#pull those names
seqtk subseq SILVA_138.1_SSURef_tax_underscore.fasta.gz SILVA_138.1_SSURef_bacteria_ids.txt | gzip > SILVA_138.1_SSURef_Bacteria_only.fasta.gz

#now we classify to the 95% similarity
vsearch-2.21.1-linux-x86_64-static/bin/vsearch --threads 16 --notrunclabels --cluster_fast SILVA_138.1_SSURef_Bacteria_only.fasta.gz --uc SILVA_138.1_SSURef_bacteria_0.95.txt --id 0.95

#now extract names of clusters
grep "^C" SILVA_138.1_SSURef_bacteria_0.95.txt | cut -f 9 | cut -f 1 -d " " > SILVA_138.1_SSURef_bacteria_0.95_samples_ids.txt

#and extract clusters
seqtk subseq SILVA_138.1_SSURef_tax_underscore.fasta.gz SILVA_138.1_SSURef_bacteria_0.95_samples_ids.txt | gzip > SILVA_138.1_SSURef_Bacteria_0.95_clustered.fasta.gz
