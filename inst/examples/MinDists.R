
# threads configuration: run
#    rthreadsSetup(nThreads=2)

# to run the code, first run setup() at thread 0, then findMinDists() at
# all nodes

# example:

# adjMat <- rbind(
#              c(0,1,1,0,0),
#              c(0,1,0,0,0),
#              c(0,1,0,1,1),
#              c(1,0,0,0,1),
#              c(0,0,0,0,1))
# setup(adjMat,5)
# findMinDists()

# check output:
# rthreadsSGget('done')
# column 2 of row i is 1 or 2, depending on whether a path exists from
# vertex i to the destination vertex

# as written, code finds lengths of shortest paths, not the paths
# themselves 

# algorithm assumes a Directed Acyclic Graph (DAG)

# illustrative value of the code is mainly the use of barriers, and use
# of "dead ends" to reduce workload, though DEs involve lots of edge cases

setup <- function(adjMat,destVertex)  # run in thread 0
{
   adj <- adjMat  # not a Big Memory object
   n <- nrow(adj)
   rthreadsMakeSharedVar('adjm',n,n,initVal=adj)
   rthreadsMakeSharedVar('adjmPow',n,n,initVal=adj)

   rthreadsMakeSharedVar('imDone',1,1,initVal=0)
   rthreadsMakeSharedVar('nDone',1,1,initVal=0)
   rthreadsMakeSharedVar('dstVrtx',1,1,initVal=destVertex)

   # role of the 'done' variable: if in row i = (u,v), u is not 0 then
   # it means this path search ended after iteration u; v = 1 means
   # reached the destination, v = 2 means no paths to destination exist;
   # thus for each vertex i, the final value of done[i,2] will indicate
   # whether a path exists from i to dstVrtx (done[i,2] = 1), and if so,
   # the length of the path
   rthreadsMakeSharedVar('done',n,2,initVal=rep(0,2*n))

   rthreadsInitBarrier()
   return()
}

findMinDists <- function()  
   # run in all threads, maybe with system.time()
{
   myID <- myGlobals$myID
   if (myID > 0) {
      rthreadsAttachSharedVar('adjm')
      rthreadsAttachSharedVar('adjmPow')
      rthreadsAttachSharedVar('done')
      rthreadsAttachSharedVar('nDone')
      rthreadsAttachSharedVar('dstVrtx')
   } 
   destVertex <- sharedGlobals$dstVrtx[1,1]

   # for brevity, make copies of some shared objects; since they are
   # references, writing to the copy will make the same change to the
   # original
   adjm <- sharedGlobals$adjm
   done <- sharedGlobals$done
   adjmPow <- sharedGlobals$adjmPow
   nThreads <- sharedGlobals$nThreads

   n <- nrow(adjm)
   # each thread will work on its assigned group of threads
   myRows <- parallel::splitIndices(n,nThreads[,])[[myID+1]]
   myRows <- setdiff(myRows,destVertex)
   mySubmatrix <- adjm[myRows,]

   # find "dead ends," vertices that lead nowhere but themselves also
   # known as "absorbing states," as in Markov chain terminology); we
   # should avoid checking their corresponding rows during the iteration
   # to find shortest paths
   deadEnds <- rep(0,n)  # value 1 means yes, a dead end for (i in 1:n)
   for (i in 1:n) {
      if (adjm[i,i] == 1 && sum(adjm[i,]) == 1) deadEnds[i] <- 1 
   }
   whichDEs <- which(deadEnds==1)
   whichDEs <- setdiff(whichDEs,destVertex)
   nDEs <- sum(deadEnds)
   if (nDEs > 0) {
      # recycling doesn't do what we want here
      done[whichDEs,1] <- 1
      done[whichDEs,2] <- 2
   }
   # also, we don't need a path from destVertex to itself
   done[destVertex,] <- c(1,2)
   imDone <- FALSE  # TRUE means this thread is done with all computation
   
   # now iterate over powers of the adjacency matrix, thus generating
   # all possible paths; iteration i generates all paths of length i,
   # meaning i jumps, and thus i+1 vertices counting the vertex of
   # origin; since we assume an acyclic graph, the max path length
   # not counting the origin is n-1, thus iterations go only through
   # n-1, not n
   for (iter in 1:(n-1)) {
      rthreadsBarrier()  # make all threads are on the same iteration
      if (sharedGlobals$nDone[1,1] == nThreads[1,1]) return()
      if (iter > 1 && (iter <= n-1))
         adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
      if (!imDone) {
         for (myRow in myRows) {
            if (done[myRow,1] > 0) next 
            if (myRow %in% whichDEs) {
                  done[myRow,1] <- iter
                  done[myRow,2] <- 2
               next
            }
            if (done[myRow,1] == 0) {  # this vertex myRow not decided yet
               if (adjmPow[myRow,destVertex] > 0) {
                  done[myRow,1] <- iter
                  done[myRow,2] <- 1
               } else {
                  currDests <- which(adjmPow[myRow,] > 0)
                  # check subset
                  currDestsEmpty <- (length(currDests) == 0)
                  if (currDestsEmpty ||
                      !currDestsEmpty &&
                         identical(intersect(currDests,whichDEs),currDests))  {
                     done[myRow,1] <- iter
                     done[myRow,2] <- 2
                  }
               }
            }
         }

         # the 'done' matrix was initialized to all 0s; if 
         # for some i in myRows we have done[i,1] == 0, that means we
         # have no yet completed analysis of paths from vertex i
         if (sum(done[myRows,1] == 0) == 0) {
            imDone <- TRUE
            rthreadsAtomicInc('nDone')
         }
      }
   }

}
