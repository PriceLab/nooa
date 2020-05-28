library(biomaRt)
db = useEnsembl(biomart="snp", dataset="hsapiens_snp")
listFilters(db)
listAttributes(db)

snpList <- c("rs6857", "rs769449", "rs2075650")

coi <- c('refsnp_id', 'chr_name','chrom_start','chrom_end',
         'minor_allele','minor_allele_freq',
         'consequence_allele_string', 'phenotype_description',
         'pmid')

# maybe also: phenotype_source, clinical_signficance, polyphen*, sift* pub_short_ref
# band_start, band_end

tbl.3 <- getBM(attributes=coi, filters='snp_filter', values=snpList, mart=db)
dim(tbl.3) # 7842 9
colnames(tbl.3) <- c("rsid", "chrom", "start", "end", "var", "maf", "subst", "phenotype", "pmid")
tbl.3$chrom <- paste0("chr", tbl.3$chrom)
head(tbl.3)
lapply(tbl.3, class)

tbl.ref <- unique(tbl.3[, c("rsid", "pmid")])
dim(tbl.ref)  # 290 2

# pmid's shared by at least 2 of the 3  snps: 20460622 27764101 28968815
tbl.ref.tiny <- subset(tbl.ref, pmid %in% c("20460622", "27764101", "28968815"))
dim(tbl.ref.tiny)

write.table(tbl.ref.tiny, sep=",", row.names=FALSE, quote=FALSE, file="snpToPubmed.csv")

write.table(tbl.3[, c("rsid", "pmid")], sep=",", row.names=FALSE, quote=FALSE, file="snpToPubmed.csv")


write.table(tbl.3, sep=",", row.names=FALSE, quote=FALSE, file="tbl.3.csv")

tbl.3.a <- unique(tbl.3[, 1:7])
write.table(tbl.3.a, sep=",", row.names=FALSE, quote=FALSE, file="tbl.3a.csv")

head(tbl.3.a)

tbl.pmid <- unique(tbl.3[, c("rsid", "pmid")])
pmids <- sort(unique(tbl.pmid$pmid))
save(pmids, file="pmid.RData")

write.table(tbl.pmid, sep=",", row.names=FALSE, quote=FALSE, file="tbl.pmid.csv")


tbl.3.a


tbl.600 <- read.table("fromPaola.tsv", sep="\t", as.is=TRUE, header=TRUE, row.names="SNP", nrow=-1)
all(snpList %in% rownames(tbl.600))  # TRUE

tbl.600.anno <- getBM(attributes=coi, filters='snp_filter', values=rownames(tbl.600), mart=db)
dim(tbl.600.anno)  # 10345 10


