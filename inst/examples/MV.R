
# NA imputation, simple use of linear regression, each column's NAs
# replaced by fitted values

# mainly for illustrating barriers

setup <- function(dta)  # run in thread 0
{

   dta <- regtools::factorsToDummies(dta,omitLast=FALSE,outDF=FALSE)
   z <- dim(dta)
   nr <- z[1]
   nc <- z[2]
   rthMakeSharedVar('dta',nr,nc,initVal=nhis.large)
   rthInitBarrier()
}

doImputation <- function()  
{
   myID <- rthMyID()
   if (myID > 0) {
      rthAttachSharedVar('dta')
   }
   nThreads <- sharedGlobals$nThreads[1,1]
   dta <- sharedGlobals$dta
   nc <- ncol(dta)

   # each thread is assigned columns to work on
   myCols <- parallel::splitIndices(nc,nThreads)[[myID+1]]

   myImputes <- NULL
   for (col in myCols) {
      # impute this column, if needed
      NAelements <- which(is.na(dta[,col]))
      if (length(NAelements) > 0) {
         lmOut <- lm(dta[,col] ~ dta[,-col])
         imputes <- lmOut$fitted.values[NAelements]
         # note: if a row in dta[,-col] has more than 1 NA,
         # its predicted (and thus imputed)  value will still be NA
         tmp <- 
            list(colNum=col,imputes=imputes,NAelements=NAelements)
      } else tmp <- NULL
      myImputes[[col]] <- tmp

   }

   # impute data
   rthBarrier()  # can't change dta while possbily still in use
   for (col in myCols) {
      myimps <- myImputes[[col]]
      if (!is.null(myimps)) {
        NAelements <- myimps$NAelements
        imputes <- myimps$imputes
        dta[NAelements,col] <- imputes
      }
   }

}

