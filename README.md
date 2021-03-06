
# Example
## srametadata R package

`srametadata` is an R package for getting metadata from Sequence Read Archive (SRA) and making traits prediction.

## Installation

    devtools::install_github("SiruoWang/srametadata")
    
Package can be loaded into R as follows:

    library('srametadata')

## Getting started

Example: Get fastq urls from SRA.

    library('srametadata')

    # Get sqlite database
    download_sra_db()

    # Get path to sqlite database
    sqlfile <- file.path('.', 'SRAmetadb.sqlite')

    # Create connection
    conn <- dbConnect(SQLite(),sqlfile)

    # one file, two files and three files associated with corresponding run accessions
    get_fastq_urls(runacc = c("DRR001482", "DRR001627", "ERR204978"), db_con = conn)
    
    
Example: predict sex from SRA using run accessions.

    predictsex(run_accession = "SRR1296065")
    predictsex(run_accession = c("SRR1296065","SRR1044429"))
    
    
Example: normalize gene expression from SRA.

    normalized(file = scan("sra_samples.txt", what="", sep="\n"), path = path, chr = "chrY", start = 14813160, end = 14972768)
    

# Bug reports
Report bugs and issues on this [Repository](https://github.com/joseah/srametadata)

# Contributors
* [Siruo (Sara) Wang](https://github.com/SiruoWang)
* [Jose Alquicira Hernandez](https://github.com/joseah)

