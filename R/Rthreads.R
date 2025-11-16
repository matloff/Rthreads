
library(bigmemory)
library(synchronicity)

myGlobals <- new.env(parent=emptyenv())
sharedGlobals <- new.env(parent=emptyenv())
topDir <- getwd()
dir.create('descFiles')

# executed only by thread 0
# both sharedVars and nonsharedVars are R lists
rthreadsSetup <- function(nThreads) 
{ 

   # set up myGlobals
   myGlobals$myID <- 0

   # set up shared globals
   rthreadsMakeSharedVar('nThreads',1,1,nThreads)
   rthreadsMakeSharedVar('nJoined',1,1,0)
   rthreadsMakeSharedVar('nDone',1,1,0)
   rthreadsMakeMutex('mutex0')
   rthreadsMakeBarrier()
   
}

# run by all threads, including 0
rthreadsJoin <- function() 
{
   # check in and get my ID
   zeroThread <- 'myID' %in% names(myGlobals)
   if (!zeroThread) {
      rthreadsAttachSharedVar('nThreads')
      rthreadsAttachSharedVar('nJoined')
      rthreadsAttachMutex('mutex0')
      nj <- rthreadsAtomicInc('nJoined') 
      myGlobals$myID <- nj
      rthreadsAttachSharedVar('nDone')
      rthreadsAttachSharedVar('barrier0')
      rthreadsAttachMutex('barrMutex0')
   } else nj <- rthreadsAtomicInc('nJoined') 

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
   rthreadsMakeSharedVar('barrier0',1,2,initVal=c(sharedGlobals$nThreads[1,1],0))
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

# utils to get around "hidden" namespace
# rows, cols are ranges, e.g. 5:25

rthreadsSGset <- function(sharedVarName,rows,cols,value) 
{
   tmp <- paste0('sharedGlobals$',sharedVarName)
   sv <- evalr(tmp)
   sv[rows,cols] <- value
}

rthreadsSGget <- function(sharedVarName,rows,cols,pointerOnly=FALSE) 
{
   tmp <- paste0('sharedGlobals$',sharedVarName)
   sv <- evalr(tmp)
   if (pointerOnly) return(sv)
   sv[rows,cols]
}

# print all of myGlobals
rthreadsPrintMG <- function() 
{
   for(nm in names(myGlobals)) print(myGlobals[[nm]])
}

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

evalr <- function (toexec)
{
    eval(parse(text = toexec), parent.frame())
}

quickstart <- function() 
{
   browseURL(system.file("doc", "QuickStart.pdf", package = "Rthreads"))
}
