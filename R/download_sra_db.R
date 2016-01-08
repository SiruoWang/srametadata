#' Download SRA metadata database
#' 
#' Downloads and extracts SRA metadata database
#' 
#' @param file.name File name for uncompressed database. Default: "SRAmetadb.sqlite"
#' @param path Path used to save database. If not provided, current directory is used.
#' @param method Method for downloading database: "auto", "internal", "libcurl", "wget", "curl", "lynx". See
#' \code{\link{download.file}} function.
#'
#' @export
#'

download_sra_db <- function(file.name = "SRAmetadb.sqlite", 
                            path = getwd(), method = "auto"){
    # Database sponsored by Meltzerlab:
    # http://gbnci.abcc.ncifcrf.gov/sra/
    url_sra <- 'http://gbnci.abcc.ncifcrf.gov/backup/SRAmetadb.sqlite.gz'
    sqlfile <- file.path('.', 'SRAmetadb.sqlite.gz')
    download.file(url_sra, 
                  destfile = sqlfile, 
                  method = method)
    gzip(filename = "SRAmetadb.sqlite.gz", destname = file.name, remove = FALSE)
}