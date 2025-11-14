
# threads configuration: run rthreadsSetup(nThreads=2) or other number
# of threads

# a general issue in parallel computation is that of "load balance,"
# meaning that each thread ends up doing approximately the same amount
# of work; here we aim for that via dynamic thread assignment, but if we
# had a very large number of rows, random pre-assignment would probably
# work fine, and would not have the overhead of engaging with a mutex

setup <- function(vecLengths=1000)  # run in thread 0
{
   rthreadsMakeSharedVar('nextRowNum',1,1)
   rthreadsMakeSharedVar('m',10,vecLengths+1)
   # generate vectors to be sorted, of different sizes
   tmp <- c(round(0.3*vecLengths),vecLengths)
   set.seed(9999)
   nvals <- sample(tmp,10,replace=TRUE)  # lengths of 10 vectors to sort
   m <- sharedGlobals$m
   for (i in 1:10) {
      n <- nvals[i]
      m[i,1:(n+1)] <- c(n,runif(n))  # 1st column is length
   }
   sharedGlobals$nextRowNum[1,1] <- sharedGlobals$nThreads[1,1] + 1
}

doSorts <- function()  # run in all threads, maybe with system.time()
{

    if (myGlobals$myID != 0) {
        rthreadsAttachSharedVar("nextRowNum")
        rthreadsAttachSharedVar("m")
    } 
   
   m <- sharedGlobals$m

   rowNum <- myGlobals$myID+1  # my first vector to sort

   while (rowNum <= nrow(m)) {
      # as illustration of parallel operation, see which threads execute
      # sorts on which rows
      print(rowNum)
      n <- m[rowNum,1]  # vector length
      x <- m[rowNum,2:(n+1)]
      m[rowNum,2:(n+1)] <- sort(x)
      rowNum <- rthreadsAtomicInc('nextRowNum') 
   }

   rthreadsBarrier()  # not really needed

}
