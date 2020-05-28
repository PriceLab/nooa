library(XML)
library(httr)

load("pmid.RData")
length(pmids)

#----------------------------------------------------------------------------------------------------
pmidToTable <- function(pmid)
{
   printf("---- pmid: %s", pmid)
   url <- sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&amp;id=%s&amp;retmode=xml&amp;rettype=abstract", pmid)
   x <- GET(url)
   # browser()
   Sys.sleep(1)
   contents <- content(x, as="parsed")
   parsed <- xmlParse(contents)
   record <- xmlToList(parsed)
   title <- paste(record$PubmedArticle$MedlineCitation$Article$ArticleTitle, collapse=" ")
   if(is.null(title)) return(data.frame())
   # abstract <- record$PubmedArticle$MedlineCitation$Article$AbstractText
   year <- record$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year
   journal <- record$PubmedArticle$MedlineCitation$MedlineJournalInfo$MedlineTA
   firstAuthor <- record$PubmedArticle$MedlineCitation$Article$AuthorList$Author$LastName

   if(is.null(journal))
      journal <- "NA"

   if(is.null(firstAuthor))
       firstAuthor <- "NA"

   if(is.null(year))
       year <- "NA"

   data.frame(pmid=pmid, title=title, year=year, journal=journal, firstAuthor=firstAuthor)

} # pmidToTable
#----------------------------------------------------------------------------------------------------
pmid.tables <- lapply(as.character(pmids), pmidToTable)
tbl.pmids <- do.call(rbind, pmid.tables)

write.table(tbl.pmids, sep=",", row.names=FALSE, quote=FALSE, file="pmids.csv")

