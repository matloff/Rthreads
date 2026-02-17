
library(gtools)

# threaded parallel grid search, enabling the user to pursue optimizing
# values of tuning parameters/hyperparameters

# example (using the optional tmux tool):
#
#    start tmux in a termal window, tmux new -s abc
#
# library(Rthreads)
# tmRthreadsInit(2,'abc')
# tmSendKeys('abc',
#    'data(svcensus); library(randomForest)')
# tmSendKeys('abc',
#    'rthGridSearch("randomForest(wageinc ~ .,data=trainData",svcensus,
#    "wageinc",pars=list(nTree=c(10,100),nodesize=c(5,25,75)),5,1000)')

# that first string will be combined with the parameters to make an
# executable call when run through 'evalr'

# for each combination of parameter values in 'pars', the function will
# do 'nXval' repetitions of cross-validation with test set 'testData';
# 'trainData' and 'testData' are names of internal variables in the
# code, NOT specified by the user

# 'classif' is true for classification problems, FALSE for regression

# the roles of 'predFtnReg' and 'predFtnClassif' are as follows: 

# althogh for typical packages, when the output of the user's call is
# fed directly into predict(), the predicted Y values are returned,
# which the code here assumes by default; but for packages such as
# 'ranger', predict() returns an object whose components include the
# predicted values, which one can accommodate with 'predFtnReg' or
# 'predFtnClassif'

rthGridSearch <- 
   function(basicCall,dta,yName,pars,nXval,nTest,classif=FALSE, 
      predFtnReg=NULL,predFtnClassif=NULL)
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
   nPars <- length(pars)
   yCol <- which(names(dta) == yName)
   if (!is.null(predFtnReg)) predict <- predFtnReg
   if (!is.null(predFtnClassif)) predict <- predFtnClassif
   for (myrow in myRows) {
      # form full call
      theCall <- basicCall
      for (i in 1:nPars)
         theCall <- 
            paste0(theCall,',',parNames[i],'=',combs[myrow,i]) 
      theCall <- paste0(theCall,')')
      # do the computation for this comb
      for (i in 1:nXval) {
         splitTrainTest(dta,nTest,yCol)  # produces trainData, testData, etc.
         outObj <- evalr(theCall)
         preds <- predict(outObj,testX)
         losses[i] <- 
            if (classif) mean(preds != testY)
            else mean(abs(preds - testY))
      }
      # record outcome
      sharedGlobals[['Combs']][myrow,nPars+1] <- mean(losses)
      sharedGlobals[['Combs']][myrow,nPars+2] <- 
         sd(losses) / sqrt(length(losses))

   }

   rthBarrier()
   # return(as.data.frame(as.matrix(Combs)))
   CombsDF <- as.data.frame(as.matrix(sharedGlobals[['Combs']][,]))
   names(CombsDF) <- c(names(pars),'acc','accSE')
   CombsDF

}

splitTrainTest <- defmacro(data,
testSetSize,yCol,expr={ rows <- 1:nrow(data)
     testRows <- sample(rows,testSetSize)
     trainRows <- setdiff(rows,testRows)
     testData <- data[testRows,]
     testY <- data[testRows,yCol]
     testX <- data[testRows,-yCol]
     trainData <- data[trainRows,]
     trainY <- data[trainRows,yCol]
     trainX <- data[trainRows,-yCol]
   }
)


