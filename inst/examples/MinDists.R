
# threads configuration: run
#    rthreadsSetup(nThreads=2)

# algorithm assumes a Directed Acyclic Graph (DAG); for test cases, an
# easy 

setup <- function(preDAG,destVertex)  # run in "manager thread"
{
   library(bnlearn)
   # to generate a DAG, take any data frame and run it through, say,
   # bnlearn:hc
   adj <- amat(hc(preDAG))
   n <- nrow(adj)
   rthreadsMakeSharedVar('adjm',n,n,initVal=adj)
   rthreadsMakeSharedVar('adjmPow',n,n,initVal=adj)
   # if in row i = (u,v), u is not 0 then it means this path search ended
   # after iteration u; v = 1 means reached the destination, v = 2
   # means no paths to destination exist
   rthreadsMakeSharedVar('done',n,2,initVal=rep(0,2*n))
   rthreadsMakeSharedVar('imDone',1,1,initVal=0)
   rthreadsMakeSharedVar('NDone',1,1,initVal=0)
   rthreadsMakeSharedVar('dstVrtx',1,1,initVal=destVertex)
   rthreadsInitBarrier()
   return()
}

findMinDists <- function()  
   # run in all threads, maybe with system.time()
{
   ### if (myID > 0) {
   if (myGlobals$myID > 0) {
      rthreadsAttachSharedVar('adjm')
      rthreadsAttachSharedVar('adjmPow')
      rthreadsAttachSharedVar('done')
      rthreadsAttachSharedVar('NDone')
      rthreadsAttachSharedVar('dstVrtx')
   } 
   destVertex <- sharedGlobals$dstVrtx[1,1]
   adjm <- sharedGlobals$adjm
   n <- nrow(adjm)
   myRows <- 
      parallel::splitIndices(n,sharedGlobals$nThreads[,])[[myGlobals$myID+1]]
   mySubmatrix <- adjm[myRows,]

   # find "dead ends," vertices to lead nowhere
   tmp <- rowSums(adjm[,])
   deadEnds <- which(tmp == 0)
   done <- sharedGlobals$done
   done[deadEnds,1] <- 1
   done[deadEnds,2] <- 2
   # and don't need a path from destVertex to itself
   done[destVertex,] <- c(1,2)
   deadEndsPlusDV <- c(deadEnds,destVertex)

   imDone <- FALSE
   for (iter in 1:(n-1)) {
      rthreadsBarrier()
      if (NDone[1,1] == sharedGlobals$nThreads) return()
      if (iter > 1 && (iter <= n-1))
         adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
      if (!imDone) {
         for (myRow in setdiff(myRows,deadEndsPlusDV)) {
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
                         identical(intersect(currDests,deadEnds),currDests))  {
                     done[myRow,1] <- iter
                     done[myRow,2] <- 2
                  }
               }
            }
         }
         if (sum(done[myRows,1] == 0) == 0) {
            imDone <- TRUE
            rthreadsAtomicInc('NDone')
         }
      }
   }

}
