#' Get metadata from SRA
#' 
#' This function grabs metadata from the sample, experiment, study and experiment attribute in the
#' SRA metadata database.

#' @param runacc A vector with run accessions
#' @param field A string specifying the field of interest (e.g. "sex", "cell_line", "tissue")
#' @param type Type of metadata which is associated with runs: 'sample', 'experiment', 'study' or 'run'
#' @param db_conn Connection to the sqlite database

#' @return \code{resfield} A dataframe with run_accessions and values associated with the field of interest. 
#' If field does not exist, NAs are returned
#'
#' @export
#' 
#' @examples
#' search_field(runacc = c("DRR023427", "SRR946066", "ERR204978"),  
#'            field = "disease", 
#'            type = "sample",
#'            db_con = conn)
#' 

search_field <- function(runacc, field, type, db_conn) {
    # Removing blank spaces around string
    type <- str_trim(type) %>% str_to_lower()
    
    # Validate type of metadata associated with run
    if(type == "sample" | type == "experiment" | type == "study" | type == "run"){
        
        # Removing blank spaces around string
        runacc <- str_trim(runacc)
        # Query database
        att <- paste0(type, "_", "attribute")
        query <- paste0("SELECT run_accession, ", att,  
                        " FROM sra WHERE ", 
                        "run_accession IN ('")
        run_sample_attribute <- paste0(query, 
                                       paste(runacc, collapse = "', '"), "');") %>%
            dbGetQuery(db_conn, .)
        
        column <- paste0(str_replace(field, " ", "_"), "_", type)
        run_sample_attribute[,column] <- str_split(run_sample_attribute[,att], "\\|\\|") %>% 
            lapply(str_trim) %>%
            lapply(str_extract, regex(paste0(field, ":.*"), ignore_case = TRUE)) %>%
            lapply(na.omit) %>%
            lapply(as.vector) %>% ifelse(. == 'character(0)', 'NA', .) %>%
            lapply(`[[`, 1) %>%
            unlist() %>% 
            str_replace_all(regex(paste0(field, ": "), ignore_case = TRUE), "")
        resfield <- data.frame(run_sample_attribute$run_accession,
                               run_sample_attribute[,column])
        names(resfield) <- c("run_accession", column)
        
        if(nrow(resfield)){
            return(resfield)
        } else {
            return(NA)
        }
        
    } else {
        stop("Type of metadata not valid. 
             Try: 'sample', 'experiment', 'study' or 'run'")
    }
    
}