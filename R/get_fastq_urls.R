#' @import srametadata
#' Get urls for downloading fastq files 
#' 
#' Gets all urls, file names and md5 values associated with run accessions.

#' @param runacc A vector with run accessions

#' @return urls A nested list with run accessions separated depending on whether they are associated with one, two or three files.
#' If all run accessions provided are only associated with one category, a data frame is returned for that 
#' category.
#'
#' @export
#' 
#' @examples
#' 
#' # Get path to sqlite database
#' sqlfile <- file.path('.', 'SRAmetadb.sqlite')
#' 
#' # Create connection
#' conn <- dbConnect(SQLite(),sqlfile)
#' 
#' # one file
#' get_fastq_urls(runacc = c("DRR001482"), db_con = conn)
#' 
#' # two files
#' get_fastq_urls(runacc = c("DRR001627"),  db_con = conn)
#' 
#' # three files
#' get_fastq_urls(runacc = c("ERR204978"),  db_con = conn)
#' 
#' # one file and two files
#' get_fastq_urls(runacc = c("DRR001482", "DRR001627"),  db_con = conn)
#' 
#' # one file and three files
#' get_fastq_urls(runacc = c("DRR001482", "ERR204978"),  db_con = conn)
#' 
#' # one file and three files
#' get_fastq_urls(runacc = c("DRR001627", "ERR204978"),  db_con = conn)
#' 
#' # one file, two files and three files
#' get_fastq_urls(runacc = c("DRR001482", "DRR001627", "ERR204978"),  
#'                db_con = conn)

get_fastq_urls <- function(runacc, db_conn){
    # Removing blank spaces around string. Converting to upper case
    runacc <- str_trim(runacc) %>% str_to_upper()
    # Query database
    frun <- paste0("SELECT run_accession, file_name, md5 FROM fastq WHERE 
                   run_accession IN ('", 
                   paste(runacc, collapse = "', '"), "');") %>%
        dbGetQuery(db_conn, .)
    
    if(!nrow(frun)){
        return(rep(NA, length(runacc)))
    }
    
    # Construct associated urls
    x <- which(str_count(frun$run_accession) < 10)
    frun$url[x]<- file.path('ftp://ftp.sra.ebi.ac.uk/vol1/fastq', 
                            str_sub(frun$run_accession[x], 1, 6), 
                            frun$run_accession[x], frun$file_name[x])
    
    y <- which(!str_count(frun$run_accession) < 10)
    formatting <- lapply(frun$run_accession[y], function(x){
        paste(c(str_dup('0', 12 - str_count(x)), 
                substring(x, 10, str_count(x))),
              collapse = '')}) %>% unlist
    
    frun$url[y] <- file.path('ftp://ftp.sra.ebi.ac.uk/vol1/fastq', 
                             str_sub(frun$run_accession[y], 1, 6), 
                             formatting, 
                             frun$run_accession[y], frun$file_name[y])
    
    ######################################################################
    # Deal with runs with 2 associated files
    ######################################################################
    dup_files <- frun[duplicated(frun$run_accession), "run_accession"]
    one_file <- frun[!frun$run_accession %in% dup_files, ]
    dup_files <- frun[frun$run_accession %in% dup_files, ]
    
    i <- duplicated(dup_files$run_accession)
    two_and_three_files <- merge(dup_files[!i,], dup_files[i,], 
                                 by = "run_accession", all = TRUE)
    dup_files <- two_and_three_files[duplicated(two_and_three_files$run_accession),
                                     "run_accession"]
    two_files <- two_and_three_files[!two_and_three_files$run_accession %in% dup_files, ]
    
    names(two_files) <- c("run_accession", "file_name", "md5_1", 
                          "url_1", "file_name_2", "md5_2", "url_2") 
    
    #######################################################################
    # Deal with runs with 3 associated files
    #######################################################################
    dup_files <- two_and_three_files[two_and_three_files$run_accession %in%
                                         dup_files, ]
    
    if(nrow(one_file) & !nrow(two_files) & !nrow(dup_files)){
        return(one_file)
    }else if(nrow(one_file) & nrow(two_files) & !nrow(dup_files)){
        urls <- list(one_file = one_file, two_files = two_files)
        return(urls)
    }else if(!nrow(one_file) & nrow(two_files) & !nrow(dup_files)){
        return(two_files)
    }
    
    i <- duplicated(dup_files$run_accession)
    three_files <- merge(dup_files[!i,], dup_files[i,], 
                         by = "run_accession", all = TRUE)
    three_files <- as.data.frame(t(apply(three_files,1,unique)))
    names(three_files) <- c("run_accession", "file_name", "md5", "url", 
                            "file_name_1", "md5_1", "url_1", "file_name_2", "md5_2", "url_2") 
    
    if(!nrow(one_file) & !nrow(two_files) & nrow(three_files)){
        return(three_files)
    }else if(nrow(one_file) & !nrow(two_files) & nrow(three_files)){
        urls <- list(one_file = one_file, three_files = three_files)
        return(urls)
    }else if(!nrow(one_file) & nrow(two_files) & nrow(three_files)){
        urls <- list(two_files = two_files, three_files = three_files)
        return(urls)
    }
    
    urls <- list(one_file = one_file, 
                 two_files =  two_files, 
                 three_files = three_files)
    return(urls)
}
