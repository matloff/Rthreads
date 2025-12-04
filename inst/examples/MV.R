
# NA imputation, simple use of linear regression, each column's NAs
# replaced by fitted values

# note that NA elements imputed in one column will be used as inputs to
# imputation in later columns 

# mainly for illustrating barriers; could be made faster in various ways

# example
#
#    at thread 0 do
#       data(NHISlarge)
#       nhis.large <- regtools::factorsToDummies(nhis.large,dfOut=FALSE)
#       nhis.large <- nhis.large[,-(1:4)]  # omit ID etc.
#       setup(nhis.large)
#
#    at all threads do
#       doImputation()
#
#    at any thread do
#       dta <- rthreadsSGget('dta')
#       head(dta)
#
#    at thread 0 do
#       head(nhis.large)
#
#    notice imputed NAs

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
   nc <- ncol(dta)
   if (nThreads > nc) stop('more threads than columns')

   # in each round, each thread works on one column; they then update
   # their columns
   nRounds <- ceiling(nc/nThreads)
   numPerRound <- floor(nc/nRounds)
   for (i in 1:nRounds) {
      myColNum <- (i-1)*numPerRound + myGlobals$myID + 1
      # impute this column, if needed
      myImputes <- NULL
      if (myColNum <= nc) {
         NAelements <- which(is.na(dta[,myColNum]))
         if (length(NAelements) > 0) {
            lmOut <- lm(dta[,myColNum] ~ dta[,-myColNum])
            imputes <- lmOut$fitted.values[NAelements]
            # note: if a row in dta[,-myColNum] has more than 1 NA,
            # its predicted (and thus imputed)  value will still be NA
            myImputes <- 
               list(colNum=myColNum,imputes=imputes,NAelements=NAelements)
         }
      }

      # round complete, update data
      rthreadsBarrier()  # can't change dta while possbily still in use
      if (!is.null(myImputes)) {
         colNum <- myImputes$colNum
         NAelements <- myImputes$NAelements
         imputes <- myImputes$imputes
         dta[NAelements,colNum] <- imputes
      }

      # some threads may still be writing to dta, can't start next round
      # until everyone is done
      rthreadsBarrier()  
   }

}

