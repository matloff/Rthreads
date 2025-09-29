
library(bigmemory)
library(synchronicity)

myGlobals <- new.env(parent=emptyenv())
sharedGlobals <- new.env(parent=emptyenv())

# executed only by thread 0
rthreadsSetup <- function(nThreads,sharedVars,nonsharedGlobals) 
{ 
   # set up myGlobals
   myGlobals$myID <- 0

   # set up shared globals
   sharedGlobals$nThreads <- nThreads
   sharedGlobals$nJoined <- 1
   sharedGlobals$nDone <- 0

   for (nm in names(sharedVars)) {
      sharedGlobals[[nm]] <- sharedGlobals[[nm]]
   }

   rthreadsMakeMutex('mutex0',infoDir=infoDir)
   rthreadsMakeBarrier()
   
}

rthreadsJoin <- function(infoDir= '~/') 
{

   # check in and get my ID
   mgrThread <- !is.null(myGlobals$myID) && myGlobals$myID == 0
   if (!mgrThread) {
      rthreadsAttachSharedVar('nJoined',infoDir=infoDir)
      rthreadsAttachMutex('mutex0',infoDir=infoDir)
      nj <- rthreadsAtomicInc('nJoined') 
      myGlobals$myID <- nj
      myGlobals$info <- info
      rthreadsAttachSharedVar('nDone',infoDir=infoDir)
      rthreadsAttachSharedVar('barrier0',infoDir=infoDir)
      rthreadsAttachMutex('barrMutex0',infoDir=infoDir)
   }

   # wait for everyone else
   while (sharedGlobals$nJoined[1,1] < myGlobals$info$nThreads) {}

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
   shrdv[1,1] <- newVal
   synchronicity::unlock(mtx)
   return(oldVal)
}

rthreadsMakeBarrier <- function()
{
   rthreadsMakeMutex('barrMutex0')
   rthreadsMakeSharedVar('barrier0',1,2,initVal=c(myGlobals$info$nThreads,0))
}

rthreadsInitBarrier <- function() 
{
   sharedGlobals$barrier0[1,] <- c(myGlobals$info$nThreads,0)
}

# create a variable shareable across threads
rthreadsMakeSharedVar <- function(varName,nr,nc,infoDir='~/',initVal=NULL) 
{
   tmp <- big.matrix(nr,nc,type='double')
   if (!is.null(initVal)) {
      tmp[,] <- initVal
   }
   desc <- describe(tmp)
   descFile <- paste0(infoDir,varName,'.desc')
   dput(desc,file=descFile)
   sharedGlobals[[varName]] <- tmp
}

# create a mutex shareable across threads
rthreadsMakeMutex <- function(mutexName,infoDir='~/') 
{
   tmp <- boost.mutex()
   desc <- describe(tmp)
   descFile <- paste0(infoDir,mutexName,'.desc')
   dput(desc,file=descFile)
   sharedGlobals[[mutexName]] <- tmp
}

rthreadsAttachSharedVar <- function(varName,infoDir='~/') 
{
   descFile <- paste0(infoDir,varName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[varName]] <- attach.big.matrix(desc)
}

rthreadsAttachMutex <- function(mutexName,infoDir='~/') 
{
   descFile <- paste0(infoDir,mutexName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[mutexName]] <- attach.mutex(desc)
}

rthreadsWaitDone <- function() 
{
   rthreadsAtomicInc('nDone')
   while (sharedGlobals$nDone[1,1] < myGlobals$info$nThreads) {}
}

rthreadsBarrier <- function() 
{
   ### mtx <- get('barrMutex0',envir=sharedGlobals)
   mtx <- sharedGlobals$barrMutex0
   ### barr <- get('barrier0',envir=sharedGlobals)
   barr <- sharedGlobals$barrier0
   synchronicity::lock(mtx)
   count <- barr[1,1] - 1
   barr[1,1] <- count
   sense <- barr[1,2]
   if (count == 0) {  # all done
      barr[1,1] <- myGlobals$info$nThreads
      barr[1,2] <- 1 - barr[1,2]
      synchronicity::unlock(mtx)
      return()
   } else {
      synchronicity::unlock(mtx)
      while (barr[1,2] == sense) {}
   }
}

# utils to get around "hidden" namespace

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

quickstart <- function() 
{
   browseURL(system.file("doc", "QuickStart.pdf", package = "Rthreads"))
}
