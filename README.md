# srametadata

### What is srametadata?

**srametadata** is an R package for getting metadata from Sequence Read Archive (SRA)


# Installation


```r
devtools::install_github("SiruoWang/srametadata")
```

Package can be loaded into R as follows:
```r
library('srametadata')
```

# Get started with **srametadata** 

*Example*: Get fastq urls from SRA.

```r
library('srametadata')

# Get sqlite database
download_sra_db()

# Get path to sqlite database
sqlfile <- file.path('.', 'SRAmetadb.sqlite')

# Create connection
conn <- dbConnect(SQLite(),sqlfile)

# one file, two files and three files associated with corresponding run accessions
get_fastq_urls(runacc = c("DRR001482", "DRR001627", "ERR204978"),
               db_con = conn)
```

# Bug reports
Report bugs and issues on this [Repository](https://github.com/joseah/srametadata)

# Contributors

* [Jose Alquicira Hernandez](https://github.com/joseah)
* [Siruo (Sara) Wang](https://github.com/SiruoWang)
