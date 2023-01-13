mkdir minimap2_hits

for i in {01..05}
do echo $i & minimap2 -t 6 --secondary=no -ax map-ont SILVA_138.1_SSURef_Bacteria_0.95_clustered.fasta.gz barcodes/barcode$i.fastq.gz > minimap2_hits/barcode$i.hits.sam
samtools view -@ 4 -F 4 minimap2_hits/barcode$i.hits.sam | cut -f 3 | sort | uniq -c | sort -n > minimap2_hits/barcode$i.summary.txt
done
