#' merges srametadata with intron counts 
#' 
#' saves srametaintrons.Rda with 20,172 entries
#' saves srametaintrons_subset.Rda with 2,969 entries 
#' containing complete sex records from SRA metadata database
#' 
#' @param file1 file1 is a file containing intron counts on chrY for sra metadata database
#' @param file2 file2 is a file containing intron counts on whole chromosomes for sra metadata database
#' @param file3 file3 is a file containing all run accessions for sra metadata database
#' @return return a list of dataframes: srametaintrons.Rda, srametaintrons_subset.Rda 
#' 
#' @export
#'
#' @examples
#' get_srameta_introns("chrYintrons.txt","totalintrons.txt","run_accession.txt")


get_srameta_introns <- function(file1,file2,file3){
  
  chrYintronsfile <- read.table(file1, header = FALSE, sep = "\t")
  id <- str_split(chrYintronsfile$V1," ") %>% lapply('[[',1) %>% unlist()
  chrYintrons <- str_split(chrYintronsfile$V1," ") %>% lapply('[[',2) %>% unlist()
  SRAchrYintrons <- cbind(id,chrYintrons)
  colnames(SRAchrYintrons) <- c("id","chrYintrons")
  
  
  totalintronsfile <- read.table(file2, header = FALSE, sep = "\t")
  id <- str_split(totalintronsfile$V1," ") %>% lapply('[[',1) %>% unlist()
  totalintrons <- str_split(totalintronsfile$V1," ") %>% lapply('[[',2) %>% unlist()
  SRAtotalintrons <- cbind(id,totalintrons)
  names(SRAtotalintrons) <- c("id", "totalintrons")
  
  run_accession <- read.table(file3, header = FALSE, sep = '\t')
  names(run_accession) <- c("id","run_accession")
  temp <- merge(SRAtotalintrons,SRAchrYintrons,by="id",all=TRUE) 
  SRAintrons <- merge(run_accession, temp, by="id", all = TRUE)
  
  SRAintrons <- SRAintrons[is.na(SRAintrons$totalintrons)==FALSE,]
  SRAintrons$chrYintrons <- as.vector(SRAintrons$chrYintrons) %>% unlist()
  SRAintrons$chrYintrons <- sapply(SRAintrons$chrYintrons,function(x) as.numeric(x))
  SRAintrons$chrYintrons[which(is.na(SRAintrons$chrYintrons))] <- 0
  SRAintrons$ratio <- as.numeric(SRAintrons$chrYintrons)/as.numeric(SRAintrons$totalintrons)
  
  load("metadata.Rda")
  meta <- data.frame(run_accession = metadata$run_accession, sex = metadata$sex)
  srametaintrons <- merge(meta,SRAintrons,by="run_accession",all = FALSE)
  save(srametaintrons, file = "srametaintrons.Rda")
  
  srametaintrons_subset <- srametaintrons[is.na(srametaintrons$sex)==FALSE,]
  srametaintrons_subset <-srametaintrons_subset[sample(1:nrow(srametaintrons_subset),
                                                       nrow(srametaintrons_subset), 
                                                       replace=FALSE),]
  srametaintrons_subset <- srametaintrons_subset[srametaintrons_subset$sex == "M" 
                                                 | srametaintrons_subset$sex == "F",]  
  save(srametaintrons_subset, file = "srametaintrons_subset.Rda")
  
  return(list(srametaintrons,srametaintrons_subset))
  
}


