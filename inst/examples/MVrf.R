
# NA imputation, simple use of linear regression, each column's NAs
# replaced by fitted values

# mainly for illustrating barriers

# example

#  at thread 0 do
#  
#     data(NHISlarge)
#     nhis.large <- regtools::factorsToDummies(nhis.large,dfOut=FALSE)
#     nhis.large <- nhis.large[,-(1:4)]  # omit ID etc.
#     setup(nhis.large)
#  
#  at all threads do
#  
#     doImputation()
#  
#  at any thread do
#  
#     dta <- rthreadsSGget('dta')
#     head(dta)
#  
#  at some other thread do
#  
#     head(nhis.large)

# library(qeML)

setup <- function(dta)  # run in thread 0
{
   z <- dim(dta)
   nr <- z[1]
   nc <- z[2]
   rthreadsMakeSharedVar('dta',nr,nc,initVal=nhis.large)
   rthreadsInitBarrier()
}

doImputation <- function()  
{
   if (myGlobals$myID > 0) {
      rthreadsAttachSharedVar('dta')
   }
   nThreads <- sharedGlobals$nThreads[1,1]
   dta <- sharedGlobals$dta
   myID <- myGlobals$myID
   nc <- ncol(dta)

   # need data frame for qeML ftns
   dta1 <- as.data.frame(dta[,])
   for (i in 1:nc) names(dta1)[i] <- paste0('V',i)

   # each thread is assigned columns to work on
   myCols <- parallel::splitIndices(nc,nThreads)[[myID+1]]

   myImputes <- NULL
   dta1 <- as.matrix(dta1)
   for (col in myCols) {
      # impute this column, if needed
      NAelements <- which(is.na(dta1[,col]))
      if (length(NAelements) > 0) {
         # lmOut <- lm(dta[,col] ~ dta[,-col])
         # imputes <- lmOut$fitted.values[NAelements]
         Y <- dta1[,col]
         Y <- as.vector(Y)
         intactRowNums <- setdiff(1:nrow(dta1),NAelements)
         intactY <- Y[intactRowNums]
         # rfOut <- qeRFranger(dta1[intactY,],names(dta1)[col])
         frm <- paste0(colnames(dta1)[col],' ~ .')
         frm <- as.formula(frm)
         rfOut <- ranger(frm,dta1[intactRowNums,])
         preds <- predict(rfOut,dta1[NAelements,-col])$predictions
         imputes <- preds
         # note: if a row in dta1[,-col] has more than 1 NA,
         # its predicted (and thus imputed)  value will still be NA
         tmp <- 
            list(colNum=col,imputes=imputes,NAelements=NAelements)
      } else tmp <- NULL
      myImputes[[col]] <- tmp

   }

   # impute data
   rthreadsBarrier()  # can't change dta1 while possbily still in use
   for (col in myCols) {
      myimps <- myImputes[[col]]
      if (!is.null(myimps)) {
        NAelements <- myimps$NAelements
        imputes <- myimps$imputes
        dta[NAelements,col] <- imputes
      }
   }

}

