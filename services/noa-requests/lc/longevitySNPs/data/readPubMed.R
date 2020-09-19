library(XML)
library(httr)
library(RUnit)
#----------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_pmidToTable()
   test_pmidsToTable()

} # runTests
#----------------------------------------------------------------------------------------------------
pmidToTable <- function(pmid)
{
   printf("---- pmid: %s", pmid)
   url <- sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&amp;id=%s&amp;retmode=xml&amp;rettype=abstract", pmid)
   x <- GET(url)
   #browser()
   Sys.sleep(1)
   contents <- content(x, as="parsed")
   parsed <- xmlParse(contents)
   record <- xmlToList(parsed)
   title <- paste(record$PubmedArticle$MedlineCitation$Article$ArticleTitle, collapse=" ")
   if(is.null(title)) return(data.frame())
   abstract <- record$PubmedArticle$MedlineCitation$Article$Abstract$AbstractText
   if(all(is.null(abstract))){
       abstract <- ""
   } else {
      if(class(abstract) == "list")
          abstract <- abstract$text
      }
   #browser()
   year <- record$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year
   journal <- record$PubmedArticle$MedlineCitation$MedlineJournalInfo$MedlineTA
   firstAuthor <- record$PubmedArticle$MedlineCitation$Article$AuthorList$Author$LastName

   if(is.null(journal))
      journal <- "NA"

   if(is.null(firstAuthor))
       firstAuthor <- "NA"

   if(is.null(year))
       year <- "NA"

   data.frame(pmid=pmid, title=title, year=year, journal=journal, firstAuthor=firstAuthor,
              abstract=abstract, stringsAsFactors=FALSE)

} # pmidToTable
#----------------------------------------------------------------------------------------------------
pmidsToTable <- function(pmids)
{
   printf("---- length(pmids): %d", length(pmids))
   pmid.string <- paste(pmids, collapse=",")
   url <- sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&amp;id=%s&amp;retmode=xml&amp;rettype=abstract",
                  pmid.string)
   x <- GET(url)
   Sys.sleep(1)
   contents <- content(x, as="parsed")
   parsed <- xmlParse(contents)
   records <- xmlToList(parsed)
   tables <- list()
   i <- 0
   for(record in records){
       i <- i + 1
       #browser()
       pmid <- record$MedlineCitation$PMID$text
       title <- paste(record$MedlineCitation$Article$ArticleTitle, collapse=" ")
       if(is.null(title)) return(data.frame())
       abstract <- record$MedlineCitation$Article$Abstract$AbstractText
       if(all(is.null(abstract))){
           abstract <- ""
       } else {
           if(class(abstract) == "list")
               abstract <- abstract$text
       }
       #browser()
       year <- record$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year
       journal <- record$MedlineCitation$MedlineJournalInfo$MedlineTA
       firstAuthor <- record$MedlineCitation$Article$AuthorList$Author$LastName

       if(is.null(journal))
           journal <- "NA"

       if(is.null(firstAuthor))
           firstAuthor <- "NA"

       if(is.null(year))
           year <- "NA"

       tbl <- data.frame(pmid=pmid, title=title, year=year, journal=journal, firstAuthor=firstAuthor,
                         abstract=abstract, stringsAsFactors=FALSE)
       tables[[i]] <- tbl
       } # for record

   do.call(rbind, tables)

} # pmidsToTable
#----------------------------------------------------------------------------------------------------
test_pmidToTable <- function()
{
   printf("--- test_pmidToTable")

   tbl <- pmidToTable("31875163")
   checkEquals(dim(tbl), c(1, 6))
   checkEquals(colnames(tbl), c("pmid", "title", "year", "journal", "firstAuthor", "abstract"))

      # an erratum report: no first author, no abstract
   tbl <- pmidToTable("29939220")
   checkEquals(dim(tbl), c(1, 6))
   checkEquals(nchar(tbl$abstract), 0)
   checkEquals(tbl$firstAuthor, "NA")

} # test_pmidToTable
#----------------------------------------------------------------------------------------------------
test_pmidsToTable <- function()
{
   printf("--- test_pmidsToTable")

   pmids <- c("32678081", "32635607", "32371953", "31875163", "31789593", "31737995")

      # first, just one pmid, getting back a 1-row data.frame
   tbl <- pmidsToTable(pmids[1])
   checkEquals(dim(tbl), c(1, 6))
   checkEquals(colnames(tbl), c("pmid", "title", "year", "journal", "firstAuthor", "abstract"))
   checkEquals(tbl$year[1], "2020")
   checkEquals(tbl$title, "Multivariate genomic scan implicates novel loci and haem metabolism in human ageing.")
   checkTrue(nchar(tbl$abstract) > 1000)

      # now, two pmids, getting back a 2-row data.frame
   tbl <- pmidsToTable(pmids[1:2])
   checkEquals(dim(tbl), c(2, 6))
   checkEquals(colnames(tbl), c("pmid", "title", "year", "journal", "firstAuthor", "abstract"))
   checkEquals(tbl$year[1], "2020")
   checkEquals(tbl$title[1], "Multivariate genomic scan implicates novel loci and haem metabolism in human ageing.")
   checkTrue(nchar(tbl$abstract[1]) > 1000)

   checkEquals(tbl$title[2], "Astaxanthin as a Putative Geroprotector: Molecular Basis and Focus on Brain Aging.")

      # now all 6
   tbl <- pmidsToTable(pmids)
   checkEquals(dim(tbl), c(6, 6))
   checkEquals(tbl$pmid[1], "32678081")
   checkEquals(tbl$title[2], "Astaxanthin as a Putative Geroprotector: Molecular Basis and Focus on Brain Aging.")
   checkEquals(tbl$year[3], "2020")
   checkEquals(tbl$journal[4], "PeerJ")
   checkEquals(tbl$firstAuthor[5], "Hagenbuchner")
   checkEquals(tbl$title[6], "Proteomics of Long-Lived Mammals.")

      # 30 pmids at a time
   pmids.180 <- readLines("pmids-180.txt")
   tbl <- pmidsToTable(pmids.180[1:30])
   checkEquals(dim(tbl), c(30, 6))
   checkTrue(all(nchar(tbl$firstAuthor) > 0))

      # 60 pmids at a time
   tbl <- pmidsToTable(pmids.180[1:60])
   checkEquals(dim(tbl), c(60, 6))
   checkTrue(all(nchar(tbl$firstAuthor) > 0))

      # 90 pmids at a time
   tbl <- pmidsToTable(pmids.180[1:90])
   checkEquals(dim(tbl), c(90, 6))
   checkTrue(all(nchar(tbl$firstAuthor) > 0))

      # 180 pmids at a time
   tbl <- pmidsToTable(pmids.180)
   checkEquals(dim(tbl), c(180, 6))
   checkTrue(all(nchar(tbl$firstAuthor) > 0))

} # test_pmidToTable
#----------------------------------------------------------------------------------------------------
run <- function()
{
   load("pmid.RData")
   length(pmids)

   pmid.tables <- lapply(as.character(pmids), pmidToTable)
   tbl.pmids <- do.call(rbind, pmid.tables)
   write.table(tbl.pmids, sep=",", row.names=FALSE, quote=FALSE, file="pmids.csv")

} # run
#----------------------------------------------------------------------------------------------------

