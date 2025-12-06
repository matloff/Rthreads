
# to run the code after launching Rthreads, first run setup() at thread
# 0, then findMinDists() at all nodes

# NOTE: if rerun findMinDists(), say with different arguments, must
# rerun status() first

# example:

# adjMat <- rbind(
#              c(0,1,1,0,0),
#              c(0,0,0,1,0),
#              c(0,1,0,1,1),
#              c(1,0,0,0,1),
#              c(0,0,1,0,0))
# setup()
# findMinDists(adjMat,5)

# check output:
# rthreadsSGget('done')

# as written, code finds lengths of shortest paths, not the paths
# themselves 

# basic plan: each thread operates on its assigned set of vertices, i.e.
# its assigned set of rows in the adjacency matrix adjm; the k-th power
# of that matrix shows numbers of k-step paths; matrix partitioning is
# used so that each thread calculates only its own rows in the power
# matrices

# illustrative value of the code is as an example of "embarrassing
# parallel" computation

setup <- function()  # run in thread 0
{
   rthreadsMakeSharedVar('nDone',1,1,initVal=0)
   rthreadsInitBarrier()
}

findMinDists <- function(adjMat,destVertex)  
{
   adjm <- adjMat
   adjmPow <- adjm
   n <- nrow(adjm)

   myID <- myGlobals$myID
   if (myID == 0) 
      rthreadsMakeSharedVar('done',n,1,initVal=rep(0,n))  # see above
   rthreadsBarrier()
   if (myID > 0) {
      rthreadsAttachSharedVar('done')
      rthreadsAttachSharedVar('nDone')
   } 

   # for brevity, make copies of some shared objects; since they are
   # references, writing to the copy will make the same change to the
   # original
   nThreads <- sharedGlobals$nThreads
   nDone <- sharedGlobals$nDone
   done <- sharedGlobals$done

   # each thread will work on its assigned group of threads
   myRows <- parallel::splitIndices(n,nThreads[,])[[myID+1]]
   mySubmatrix <- adjm[myRows,]
   
   # now iterate over powers of the adjacency matrix, thus generating
   # all possible paths; iteration i generates all paths of length i,
   # meaning i jumps, and thus i+1 vertices counting the vertex of
   # origin; since we assume an acyclic graph, the max path length
   # not counting the origin is n-1, thus iterations go only through
   # n-1, not n
   for (iter in 1:(n-1)) {
      if (nDone[1,1] == nThreads[1,1]) break
      if (iter > 1) adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
      for (myRow in myRows) {
         if (done[myRow,1] > 0) next 
         # this vertex myRow not decided yet as to a path to destVertex
         if (adjmPow[myRow,destVertex] > 0) {  # success!
            done[myRow,1] <- iter
         } 
      }

      # the 'done' matrix was initialized to all 0s; if 
      # for some i in myRows we have done[i,1] == 0, that means we
      # have no yet completed analysis of paths from vertex i
      if (all(done[myRows,1] > 0)) {
         rthreadsAtomicInc('nDone')
         break
      }
   }

   rthreadsBarrier()
}

