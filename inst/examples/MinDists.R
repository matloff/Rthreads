
# to run the code after launching Rthreads, first run setup() at thread
# 0, then findMinDists() at all nodes

# NOTE: if rerun findMinDists(), must rerun status() first

# example:

# adjMat <- rbind(
#              c(0,1,1,0,0),
#              c(0,1,0,0,0),
#              c(0,1,0,1,1),
#              c(1,0,0,0,1),
#              c(0,0,0,0,1))
# setup()
# findMinDists(adjMat,5)

# check output:
# rthreadsSGget('done')
# column 2 of row i is 1 or 2, depending on whether a path exists from
# vertex i to the destination vertex (column 1 gives the corresponding
# path length)

# as written, code finds lengths of shortest paths, not the paths
# themselves 

# basic plan: each thread operates on its assigned set of vertices, i.e.
# its assigned set of rows in the adjacency matrix adjm; the k-th power
# of that matrix shows numbers of k-step paths; matrix partitioning is
# used so that each thread calculates only its own rows in the power
# matrices

# illustrative value of the code is as an example of "embarrassing
# parallel" computation; some extra (nonparallel) speedup is obtained
# via use of "dead ends" to reduce workload (at the cost of considerable
# edge-case checking)

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
      rthreadsMakeSharedVar('done',n,2,initVal=rep(0,2*n))  # see above
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
   myRows <- setdiff(myRows,destVertex)
   mySubmatrix <- adjm[myRows,]

   # find "dead ends," vertices that lead nowhere but to themselves; also
   # known as "absorbing states," as in Markov chain terminology); we
   # should avoid checking their corresponding rows during the iteration
   # to find shortest paths
   deadEnds <- rep(0,n)  # value 1 means yes, a dead end for (i in 1:n)
   for (i in 1:n) {
      if (adjm[i,i] == 1 && sum(adjm[i,]) == 1) deadEnds[i] <- 1 
   }
   deadEnds[destVertex] <- 1
   whichDEs <- which(deadEnds==1)
   nDEs <- sum(deadEnds)
   if (nDEs > 0) {
      done[whichDEs,1] <- 1
      done[whichDEs,2] <- 2
   }
   
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
            done[myRow,2] <- 1
         } else {
            currDests <- which(adjmPow[myRow,] > 0)
            # can only go to dead ends from here?
            if (identical(intersect(currDests,whichDEs),currDests))  {
               done[myRow,1] <- iter
               done[myRow,2] <- 2
            }
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
