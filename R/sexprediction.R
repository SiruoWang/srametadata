#' predict sex information based on intron counts for SRA metadata database
#'
#' @param run_accession A vector with run accessions
#' @return predicted sex and prediction accuracy to the corresponding run accession input
#'
#' @export
#' 
#' @examples 
#' predictsex(run_accession = "SRR1296065")
#' predictsex(run_accession = c("SRR1296065","SRR1044429"))


predictsex <- function(run_accession = run_accession){
  
  load("srametaintrons.Rda")
  load("srametaintrons_subset.Rda")
  
  set.seed(2015)
  
  x <- srametaintrons_subset
  x$sex <- droplevels(x$sex)
  
  group_idx <- sample(cut(seq(1,nrow(x)),breaks=10,label=FALSE))
  
  result <- list()
  true <- list()
  
  for(k in 1:10){
    
    data_train <- data.frame(y=x$sex[setdiff(1:nrow(x),which(group_idx==k))],
                             cbind(x$totalintrons[setdiff(1:nrow(x),which(group_idx==k))],
                                   x$chrYintrons[setdiff(1:nrow(x),which(group_idx==k))]))
    data_test <- data.frame(cbind(x$totalintrons[which(group_idx==k)],x$chrYintrons[which(group_idx==k)]))
    true[[k]] <- x$sex[which(group_idx==k)]
    
    sra.rf <- randomForest(y ~ ., data=data_train, ntree=100,
                           do.trace=FALSE, importance=TRUE)
    result[[k]] <- predict(sra.rf,data_test)
    
  }
  
  
  results <- confusionMatrix(data=unlist(result),unlist(true))
  accuracy <- as.matrix(results,what="overall")
  
  training <- x
  testing <- srametaintrons
  
  data_train <- data.frame(y=training$sex,cbind(training$totalintrons,training$chrYintrons))
  data_test <- data.frame(cbind(testing$totalintrons,testing$chrYintrons))
  
  
  sra.rf <- randomForest(y ~ ., data=data_train, ntree=300,
                         do.trace=FALSE, importance=TRUE)
  
  result <- predict(sra.rf,data_test)
  predict_sex <- as.vector(result)
  
  meta_predictsex <- srametaintrons
  meta_predictsex$predictsex <- predict_sex
  row.names(meta_predictsex) <- NULL
  
  xrow <- which(apply(meta_predictsex,1,function(x) any(x %in% run_accession)))
  sex <- meta_predictsex[xrow,c(1,7)]
  
  return(list(sex,accuracy))
  
}


