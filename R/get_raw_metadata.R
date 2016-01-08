#' Get all raw metadata from SRA
#' 
#' Get all raw metadata from SRA associated with SRA accessions numbers
#'
#' @param run_accession A vector with run accessions
#' @param sample_accession A vector with sample accessions
#' @param experiment_accession A vector with experiment accessions
#' @param study_accession A vector with study accessions
#' @param submission_accession A vector with submission accessions
#' @param platform Sequencing platform, e.g "ILLUMINA"
#' @param db_connn Connection to the sqlite database
#' @return A dataframe with all metadata associated with the filters provided to 
#' the function.
#'
#' @export
#' 
#' @examples 
#' library('srametadata')
#'
#' sqlfile <- file.path('.', 'SRAmetadb.sqlite')
#'
#' # Create connection
#' conn <- dbConnect(SQLite(),sqlfile)
#' 
#' get_raw_metadata(run_accession = c("DRR023427", "SRR946066", "ERR204978"), 
#'                  experiment_accession = "SRX329581", 
#'                  sample_accession = c("SRS465599","DRS014276"),
#'                  study_accession = "SRP028344",
#'                  submission_accession = "SRA096347", 
#'                  platform = "ILLUMINA",
#'                  db_conn = conn)
#' 
#' get_raw_metadata(experiment_accession = "SRX329581", 
#'                  db_conn = conn)
#' 
#' get_raw_metadata(
#'     run_accession = c("DRR023427", "SRR946066", "ERR204978"), 
#'     db_conn = conn)
#' 
#' get_raw_metadata(
#'     sample_accession = c("SRS465599","DRS014276"),
#'     db_conn = conn)
#' 
#' all_illumina_sra <- get_raw_metadata(platform = "ILLUMINA", db_conn = conn)


get_raw_metadata <- function(run_accession = NULL, 
                             sample_accession = NULL, 
                             experiment_accession = NULL, 
                             study_accession = NULL,
                             submission_accession = NULL,
                             platform = NULL, 
                             db_conn){
    
    md <- list(
        run_accession = run_accession, 
        sample_accession = sample_accession, 
        experiment_accession =  experiment_accession, 
        study_accession = study_accession,
        submission_accession = submission_accession
    ) %>% lapply(str_trim)
    
    labs <- names(md)
    md <- lapply(seq_along(md), function(i){
        if(length(md[[i]])){
            query <- paste0("SELECT ", paste(names(md), collapse = ", ") ,
                            " FROM sra WHERE ", names(md)[[i]] ,
                            " IN ('", paste(md[[i]], collapse = "', '"), "');")
            dbGetQuery(db_conn, query)
            
        }
    }
    )
    names(md) <- labs
    ids <- lapply(md, length) %>% unlist() %>% sum()
    if(length(platform) & ids){
        platform <- str_trim(platform) %>% str_to_upper()
        runacc <- Reduce(intersect,lapply(md,"[[",1))
        query <- paste0("SELECT ", paste(labs, collapse = ", ") ," FROM sra WHERE
                   platform IN ('", paste(platform, 
                                          collapse = "', '"), "')
                        AND run_accession IN ('", 
                        paste(runacc, collapse = "', '"), "');")
        
        md$platform <- dbGetQuery(db_conn, query)
    }else if(length(platform) & !ids){
        platform <- str_trim(platform) %>% str_to_upper()
        query <- paste0("SELECT * FROM sra WHERE
                   platform IN ('", paste(platform, 
                                          collapse = "', '"), "');")
        
        md$platform <- dbGetQuery(db_conn, query)
        return(md$platform)
    }
    
    runacc <- unique(Reduce(union,lapply(md,"[[",1)))
    
    query  <- paste0("SELECT * FROM sra WHERE
                   run_accession IN ('", paste(runacc, 
                                               collapse = "', '"), "');")
    md <- dbGetQuery(db_conn, query)
    if(nrow(md)){
        return(md)
    }else{
        return(NA)
    }
    
}