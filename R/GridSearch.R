
# threaded parallel grid search, enabling the user to pursue optimizing
# values of tuning parameters/hyperparameters

# example:
# library(regtools)
# data(mlb)
# library(randomForest)
# rthGridSearch('randomForest(Weight ~ .,data=trainData)',mlb,
#    pars=list(nTree=c(10,100),nodesize=c(5,25,75)),5,1000)

# that first string will be combined with the parameters to make an
# executable call when run through 'evalr'

# 'trainData' is the actual name of a variable in the code, data that in
# this example will be a subject of 'mlb'

# for each combination of parameter values in 'pars', the function will
# do 'nXval' repetitions of cross-validation with test set 'testData'

# 'classif' is true for classification problems, FALSE for regression

# the role of 'predFtn' is as follows: 1. one can use custom loss
# functions, e.g. (Y - predY)^2 instead of |Y - predY|; 2. for typical
# packages, when the output of the user's call is fed directly into
# predict(), the predicted Y values are returned, which the code here
# assumes by default; but for packages such as 'ranger', predict()
# returns an object whose components include the predicted values, which
# one can accommodate with 'predFtn'

rthGridSearch <- 
   function(basicCall,data,pars,nXval,nTest,classif=FALSE,predFtn=NULL)
{
   # form results matrix, parameter values in the first length(pars)
   # columns and experiment results in the last two
   combs <- do.call(expand.grid,pars)
   combs$means <- rep(NA,nrow(combs))
   combs$stdErrs <- rep(NA,nrow(combs))
   combs <- as.matrix(combs)

   # form shared version of combs; this eventually will hold the results
   # of the function
   myID <- rthMyID()  
   if (myID == 0) {
     rthMakeSharedVar('Combs',nrow(combs),ncol(combs),combs)
   }
   rthBarrier()
   if (myID > 0) rthAttachSharedVar('Combs')

   nthreads <- sharedGlobals$nThreads[1,1]
   myRows <- parallel::splitIndices(nrow(combs),nthreads)[[myID +1]]

   losses <- vector(length=nXval)
   parNames <- names(pars)
   for (myrow in myRows) {
      # form full call
      theCall <- basicCall
      for (i in 1:length(pars))
         theCall <- 
            paste0(theCall,',',parNames[i],'=',combs[myrow,i]) 
      theCall <- paste0(theCall,')')
      # do the computation for this comb
      for (i in 1:nXval) {
         splitTrainTest(data,nTest)  # produces trainData, testData, etc.
         outObj <- evalr(theCall)
         preds <- predict(outObj,testX)
         losses[i] <- 
            if (classif) mean(preds != trainY)
            else mean(abs(preds - trainY))
      }
      # record outcome
      Combs$means[myRow] <- mean(losses)
      Combs$stdErrs[myRow] <- sd(losses)
   }

   return(as.data.frame(Combs))

}

