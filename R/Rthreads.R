
library(bigmemory)
library(synchronicity)

sharedGlobals <- new.env(parent=emptyenv()) 
topDir <- '/tmp'

# executed only by thread 0
rthSetup <- function(nThreads) 
{ 

   rthMakeSharedVar('myIDs',nThreads,1,NA)
   sharedGlobals[['myIDs']][1,1] <- Sys.getpid()

   # set up shared globals
   rthMakeSharedVar('nThreads',1,1,nThreads)
   rthMakeSharedVar('nJoined',1,1,1)
   rthMakeSharedVar('nDone',1,1,0)
   rthMakeMutex('mutex0')
   rthMakeBarrier()
   
}

# run by all threads, including 0
rthJoin <- function() 
{
   # check in 
   if (is.null(sharedGlobals$myIDs)) {  # not thread 0
      rthAttachSharedVar('nThreads')
      rthAttachSharedVar('nJoined')
      rthAttachMutex('mutex0')
      # get my thread ID
      nj <- rthAtomicInc('nJoined') 
      rthAttachSharedVar('myIDs')  
      sharedGlobals$myIDs[nj+1,1] <- Sys.getpid()
      rthAttachSharedVar('nDone')
      rthAttachSharedVar('barrier0')
      rthAttachMutex('barrMutex0')
   } 

   # wait for everyone else
   while (sharedGlobals$nJoined[1,1] < sharedGlobals$nThreads[1,1]) {}
}

# atomically increases sharedV by increm, returning old value; sharedV
# is the quoted name of a shared variable (NOT prefixed by 'sharedGlobals$');
# same for mtx

# element [1,] is incremented; so, can have vector incrementing vector
rthAtomicInc <- function(sharedV,mtx='mutex0',increm=1) 
{
   mtx <- sharedGlobals[[mtx]]
   synchronicity::lock(mtx)
   shrdv <- sharedGlobals[[sharedV]]
   oldVal <- shrdv[1,]
   newVal <- oldVal + increm
   shrdv[1,] <- newVal
   sharedGlobals[[sharedV]] <- shrdv
   synchronicity::unlock(mtx)
   return(oldVal)
}

rthMakeBarrier <- function()
{
   rthMakeMutex('barrMutex0')
   rthMakeSharedVar('barrier0',1,2,
      initVal=c(sharedGlobals$nThreads[1,1],0))
}

rthInitBarrier <- function() 
{
   sharedGlobals$barrier0[1,] <- c(sharedGlobals$nThreads[,],0)
}

# create a variable shareable across threads; must be a matrix, even if
# only 1x1
rthMakeSharedVar <- function(varName,nr,nc,initVal=NULL) 
{
   tmp <- big.matrix(nr,nc,type='double')
   if (!is.null(initVal)) {
      tmp[,] <- initVal
   } 
   desc <- describe(tmp)
   descFileName <- paste0(topDir,'/',varName,'.desc')
   dput(desc,file=descFileName)
   sharedGlobals[[varName]] <- tmp
}

# look up ID for this thread
rthMyID <- function() 
{
   tmp <- which(sharedGlobals[['myIDs']][,] == Sys.getpid())
   if (length(tmp) == 0) stop('thread ID lookup failed')
   tmp - 1
}

# make and attach shared variable, within the same code
rthMakeAttachSharedVar <- function(varName,nr,nc,initVal=NULL) 
{
   myID <- rthMyID()
   if (myID == 0) rthMakeSharedVar(varName,nr,nc,initVal)
   rthBarrier()
   if (myID > 0) rthAttachSharedVar(varName)
   rthBarrier()
}

# create a mutex shareable across threads
rthMakeMutex <- function(mutexName) 
{
   tmp <- boost.mutex()
   desc <- describe(tmp)
   descFile <- paste0(topDir,'/',mutexName,'.desc')
   dput(desc,file=descFile)
   sharedGlobals[[mutexName]] <- tmp
}

rthAttachSharedVar <- function(varName) 
{
   descFile <- paste0(topDir,'/',varName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[varName]] <- attach.big.matrix(desc)
}

rthAttachMutex <- function(mutexName) 
{
   descFile <- paste0(topDir,'/',mutexName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[mutexName]] <- attach.mutex(desc)
}

rthWaitDone <- function() 
{
   rthAtomicInc('nDone')
   while (sharedGlobals$nDone[1,1] < sharedGlobals$nThreads) {}
}

rthBarrier <- function() 
{
   mtx <- sharedGlobals$barrMutex0
   barr <- sharedGlobals$barrier0
   synchronicity::lock(mtx)
   count <- barr[1,1] - 1
   barr[1,1] <- count
   sense <- barr[1,2]
   if (count == 0) {  # all done
      barr[1,1] <- sharedGlobals$nThreads[1,1]
      barr[1,2] <- 1 - barr[1,2]
      synchronicity::unlock(mtx)
      return()
   } else {
      synchronicity::unlock(mtx)
      while (barr[1,2] == sense) {}
   }
}

# analogous to R's 'split' function; inputs a shared matrix M (specified
# as a quoted name), and splits the rows according to the R factor
# splitFactor; returns an R list of shared matrices (note that each is
# just a reference, thus eligible as components of a list) 

# one can then use list functions; e.g. for output 'z'
#
#    sapply(z,function(i) mean(i[1,]))
#
# returns the mean of column 1 of each group

# shared matrices must have names, even though they may not be very
# useful in this context; they are implemented here in the form
# prefix.suffix, where suffix is the corresponding splitFactor levels
# name, e.g. split.1 and split.qq if levels(splitFactor) = c('1','qq')

rthSplit <- function(M,splitFactor,prefix='split') 
{

   M <- get(M,envir=sharedGlobals)
   nthreads <- sharedGlobals$nThreads[1,1]
   myID <- rthMyID()
   lvls <- levels(splitFactor)
   nLvls <- length(lvls)
   ncolM <- ncol(M)
   nrowM <- nrow(M)
   if (nLvls > nrowM) stop('too many levels')
   myRows <- parallel::splitIndices(nrow(M),nthreads)[[myID +1]] 

   # this thread now calls a variant of standard R 'split', resulting in
   # an R list, 'splitOut', with one element per level of the factor;
   # element k of the list shows the rows among 'myRows' that have level
   # k of this factor

   splitOut <- rowSplit(myRows,splitFactor)  
   myCounts <-sapply(splitOut,function(i) length(i))
 
   # start building the output, 'outList',an R list; element i will be
   # (a memory reference to) a shared matrix consisting of the rows of M
   # for which splitFactor has level i 

   # numerous opportunities for optimization here; e.g.  a problem is
   # how large to make these shared matrices; here we just use the size
   # of M for convenience, but this could be problematic if M is very
   # large

   # start to form 'outList', row numbers only at this point, merging
   # the various threads' 'splitOut' lists; row i of 'rowNums' will
   # contain the row numbers within all of M with 'splitFactor' level i;
   # there will be NA values as padding on the right

   rthMakeAttachSharedVar('rowNums',nLvls,nrowM)

   # we form row i of 'rowNums' by concatenating the corresponding row
   # numbers for each thread; this is not done via the c(), but rather
   # by keeping track of where in the 'rowNums' row i the empty space
   # (NAs) currently begins, and filling starting there; 'numNon0s'
   # keeps track of this

   rthMakeAttachSharedVar('numNon0s',nLvls,1,0)

   mtx <- sharedGlobals$mutex0
   for (k in 1:nLvls) {
      # merge my list of row numbers for factor level k with those of
      # the other threads; thread 0 will get things started, while the
      # other threads wait
      if (myID == 0) {
         sok <- splitOut[[k]]
         sharedGlobals$rowNums[k,1:length(sok)] <- sok
         sharedGlobals$numNon0s[k,1] <- length(sok)
      }
      rthBarrier()
      if (myID > 0) {
         synchronicity::lock(mtx)
         sok <- splitOut[[k]]
         if (length(sok) > 0) {
            whereToPutIt <- 
               (sharedGlobals$numNon0s[k,1]+1):
               (sharedGlobals$numNon0s[k,1]+myCounts[[k]])
            sharedGlobals$rowNums[k,whereToPutIt] <- sok
            sharedGlobals$numNon0s[k,1] <- 
               sharedGlobals$numNon0s[k,1]+myCounts[[k]]
         }
         synchronicity::unlock(mtx)
      }
   }

   outList <- list()
   for (i in 1:nLvls) {
      varName <- paste0(prefix,'.',lvls[i])
      # how many in this row?
      nn0s <- sharedGlobals$numNon0s[i,1]
      if (nn0s == 0) outList[[i]] <- NULL
      else {
         rni <- sharedGlobals$rowNums[i,1:nn0s]
         # form submatrix
         m <- M[rni,]
         rthMakeAttachSharedVar(varName,length(rni),ncolM,m)
         outList[[i]] <- get(varName,envir=sharedGlobals)
      }
   }

   outList

}

library(gtools)

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
   function(basicCall,data,yName,pars,nXval,nTest,classif=FALSE,predFtn=NULL)
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
   ycol <- which(names(dta) == yName)
   for (myrow in myRows) {
      # form full call
      theCall <- basicCall
      for (i in 1:length(pars))
         theCall <- 
            paste0(theCall,',',parNames[i],'=',combs[myrow,i]) 
      theCall <- paste0(theCall,')')
      # do the computation for this comb
      for (i in 1:nXval) {
         splitTrainTest(data,nTest,yCol)  # produces trainData, testData, etc.
         outObj <- evalr(theCall)
         preds <- predict(outObj,testX)
         losses[i] <- 
            if (classif) mean(preds != testY)
            else mean(abs(preds - testY))
      }
      # record outcome
      Combs$means[myRow] <- mean(losses)
      Combs$stdErrs[myRow] <- sd(losses)
   }

   return(as.data.frame(Combs))

}

splitTrainTest <- defmacro(dta,testSetSize,yCol,expr={
     rows <- 1:nrow(dta)
     testRows <- sample(rows,testSetSize)
     trainRows <- setdiff(rows,testRows)
     testData <- dta[testRows,]
     testY <- dta[testRows,yCol]
     testX <- dta[testRows,-yCol]
     trainData <- dta[trainRows,]
     trainY <- dta[trainRows,yCol]
     trainX <- dta[trainRows,-yCol]
   }
)


# utils to get around "hidden" namespace
# rows, cols are ranges, e.g. 5:25

rthSGset <- function(sharedVarName,rows,cols,value) 
{
   tmp <- paste0('sharedGlobals$',sharedVarName)
   sv <- evalr(tmp)
   sv[rows,cols] <- value
}

rthSGget <- 
   function(sharedVarName,rows='all',cols='all',pointerOnly=FALSE)
{
   tmp <- paste0('sharedGlobals$',sharedVarName)
   sv <- evalr(tmp)
   if (pointerOnly) return(sv)
   if (identical(rows,'all')) rows <- 1:nrow(sv)
   if (identical(cols,'all')) cols <- 1:ncol(sv)
   sv[rows,cols]
}

### # print all of myGlobals
### rthPrintMG <- function() 
### {
###    for(nm in names(myGlobals)) print(myGlobals[[nm]])
### }

# print all of sharedGlobals
rthPrintSG <- function() 
{
   for(nm in names(sharedGlobals)) cat(nm,' = ',sharedGlobals[[nm]][1,],'\n')
}

# device to get user access to Rthreads workspace
rthDoCmds <- function()
{
   while(1) {
      cmd <- readline('enter cmd or blank line to quit: ')
      if (cmd == '\n') return()
      evalr(cmd)
   }
}

rthSrcExamples <- function(exName)
{
   fl <- system.file('examples',exName,package='Rthreads')
   source(fl)
}

evalr <- function (toexec)
{
    eval(parse(text = toexec), parent.frame())
}

# splits the values in rowNumbers according to their levels in the factor f
rowSplit <- function(rowNumbers,f) 
{
   frn <- f[rowNumbers]
   split(rowNumbers,frn)
}

# convenience function; say factor f has unused levels; removes them and
# creates a new factor, indentical to f but with the correct levels
shortFactor <- function(f) 
{
   f <- as.character(f)
   as.factor(f)
}

quickstart <- function() 
{
   browseURL(system.file("doc", "QuickStart.pdf", package = "Rthreads"))
}
