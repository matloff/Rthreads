
library(bigmemory)
library(synchronicity)

sharedGlobals <- new.env(parent=emptyenv())
topDir <- '/tmp'

# executed only by thread 0
rthreadsSetup <- function(nThreads) 
{ 

   rthreadsMakeSharedVar('myIDs',nThreads,1,NA)
   sharedGlobals[['myIDs']][1,1] <- Sys.getpid()

   # set up shared globals
   rthreadsMakeSharedVar('nThreads',1,1,nThreads)
   rthreadsMakeSharedVar('nJoined',1,1,1)
   rthreadsMakeSharedVar('nDone',1,1,0)
   rthreadsMakeMutex('mutex0')
   rthreadsMakeBarrier()
   
}

# run by all threads, including 0
rthreadsJoin <- function() 
{
   # check in 
   if (is.null(sharedGlobals$myIDs)) {  # not thread 0
      rthreadsAttachSharedVar('nThreads')
      rthreadsAttachSharedVar('nJoined')
      rthreadsAttachMutex('mutex0')
      # get my thread ID
      nj <- rthreadsAtomicInc('nJoined') 
      rthreadsAttachSharedVar('myIDs')  
      sharedGlobals$myIDs[nj+1,1] <- Sys.getpid()
      rthreadsAttachSharedVar('nDone')
      rthreadsAttachSharedVar('barrier0')
      rthreadsAttachMutex('barrMutex0')
   } 

   # wait for everyone else
   while (sharedGlobals$nJoined[1,1] < sharedGlobals$nThreads[1,1]) {}
}

# atomically increases sharedV by increm, returning old value; sharedV
# is the quoted name of a shared variable (NOT prefixed by 'sharedGlobals$');
# same for mtx

# element [1,] is incremented; so, can have vector incrementing vector
rthreadsAtomicInc <- function(sharedV,mtx='mutex0',increm=1) 
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

rthreadsMakeBarrier <- function()
{
   rthreadsMakeMutex('barrMutex0')
   rthreadsMakeSharedVar('barrier0',1,2,
      initVal=c(sharedGlobals$nThreads[1,1],0))
}

rthreadsInitBarrier <- function() 
{
   sharedGlobals$barrier0[1,] <- c(sharedGlobals$nThreads[,],0)
}

# create a variable shareable across threads; must be a matrix, even if
# only 1x1
rthreadsMakeSharedVar <- function(varName,nr,nc,initVal=NULL) 
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
rthreadsMyID <- function() 
{
   tmp <- which(sharedGlobals[['myIDs']][,] == Sys.getpid())
   if (length(tmp) == 0) stop('thread ID lookup failed')
   tmp - 1
}

# make and attach shared variable, within the same code
rthreadsMakeAttachSharedVar <- function(varName,nr,nc,initVal=NULL) 
{
   myID <- rthreadsMyID()
   if (myID == 0) rthreadsMakeSharedVar(varName,nr,nc,initVal)
   rthreadsBarrier()
   if (myID > 0) rthreadsAttachSharedVar(varName)
}

# create a mutex shareable across threads
rthreadsMakeMutex <- function(mutexName) 
{
   tmp <- boost.mutex()
   desc <- describe(tmp)
   descFile <- paste0(topDir,'/',mutexName,'.desc')
   dput(desc,file=descFile)
   sharedGlobals[[mutexName]] <- tmp
}

rthreadsAttachSharedVar <- function(varName) 
{
   descFile <- paste0(topDir,'/',varName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[varName]] <- attach.big.matrix(desc)
}

rthreadsAttachMutex <- function(mutexName) 
{
   descFile <- paste0(topDir,'/',mutexName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[mutexName]] <- attach.mutex(desc)
}

rthreadsWaitDone <- function() 
{
   rthreadsAtomicInc('nDone')
   while (sharedGlobals$nDone[1,1] < sharedGlobals$nThreads) {}
}

rthreadsBarrier <- function() 
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

# shared matrices must have names, which will be of the form
# prefix.suffix, where suffix is the corresponding splitFactor levels
# name, e.g. split.1 and split.qq if levels(splitFactor) = c('1','qq')

rthreadsSplit <- function(M,splitFactor,prefix='split') 
{

   M <- get(M,envir=sharedGlobals)
   nthreads <- sharedGlobals$nThreads
   myID <- rthreadsMyID()
   lvls <- levels(splitFactor)
   nLvls <- length(lvls)
   ncolM <- ncol(M)
   nrowM <- nrow(M)
   if (nLvls > nrowM) stop('too many levels')
   myRows <- parallel::splitIndices(nrow(M),nthreads[1,1])[[myID+1]] 

   # this thread now calls a variant of standard R 'split', resulting in
   # an R list, 'splitOut', with one element per level of the factor;
   # element k of the list shows the rows among 'myRows' that have level
   # k of this factor
   splitOut <- rowSplit(myRows,splitFactor)  
   
 
   # start building the output, an R list; element i will be (a memory
   # reference to) a shared matrix consisting of the rows of M for which
   # splitFactor has level i 

   # a problem is how large to make these shared matrices; here we just
   # use the size of M for convenience, but this could be problematic if
   # M is very large

   # start to form outList, row numbers only for now, by merging the
   # various threads' splitOut lists
   rthreadsMakeAttachSharedVar('rowNums',nThreads,ncolM)
   mtx <- sharedGlobals$mutex0
   for (k in 1:nLvls) {
      # merge my list of row numbers for factor level k with those of
      # the other threads
      synchronicity::lock(mtx)
      if (myID == 0) rowNums[k,] <- splitOut[[k]]
      else rowNums[k,] <- cat(rowNums[k,],splitOut[[k]])
      synchronicity::unlock(mtx)
   }

   outList <- list()
   for (i in 1:nLvls) {
      varName <- paste0(prefix,'.',lvls[i])
      outListNames[i] <- varName
      rni <- rowNums[i]
      if (length(rni) == 0) outList[[i]] <- NULL
      else {
         # form submatrix
         m <- M[rowNums[i],]
         rthreadsMakeAttachSharedVar(varName,length(rni),ncolM,m)
         outList[[i]] <- get(varName,envir=sharedGlobals)
      }
   }

   outList

}
   
# utils to get around "hidden" namespace
# rows, cols are ranges, e.g. 5:25

rthreadsSGset <- function(sharedVarName,rows,cols,value) 
{
   tmp <- paste0('sharedGlobals$',sharedVarName)
   sv <- evalr(tmp)
   sv[rows,cols] <- value
}

rthreadsSGget <- 
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
### rthreadsPrintMG <- function() 
### {
###    for(nm in names(myGlobals)) print(myGlobals[[nm]])
### }

# print all of sharedGlobals
rthreadsPrintSG <- function() 
{
   for(nm in names(sharedGlobals)) cat(nm,' = ',sharedGlobals[[nm]][1,],'\n')
}

# device to get user access to Rthreads workspace
rthreadsDoCmds <- function()
{
   while(1) {
      cmd <- readline('enter cmd or blank line to quit: ')
      if (cmd == '\n') return()
      evalr(cmd)
   }
}

rthreadsSrcExamples <- function(exName)
{
   fl <- system.file('examples',exName,package='Rthreads')
   source(fl)
}

evalr <- function (toexec)
{
    eval(parse(text = toexec), parent.frame())
}

# splits the values in rowNums according to their levels in the factor f
rowSplit <- function(rowNums,f) 
{
   frn <- f[rowNums]
   split(rowNums,frn)
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
