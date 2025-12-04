# Rthreads -- Threads(-Like) Package for R!

# Filling an Important Gap

* *Threading* is a major mechanism for parallel operations in the world
  of computing.

* Using C/C++, a popular threading library is OpenMP.

* R does not have native threading.

* Lack of threading in R means that developers of fast packages 
  like **data.table** must rely on threads at the C++ level (and
  only implementing specific functions).

* Alternative to threads is *message-passing*, e.g. R's **parallel**
  package, including via **foreach** interface.

* Threaded coding tends to be clearer and faster, compared to
  message-passing.

* Thus having a threads capability in R would greatly enhance
  R's capabilities in parallel processing.

# What Is the Difference between Threading and Message Passing?

* Say we are on a quad-core machine. Here is an overview of the two
  paradigms (lots of variants):

* Message passing:

  * We would write 2 functions, a worker and a manager. There would be,
    say, 4 copies of the worker code, running fully independently.

  * The manager would communicate with the workers by sending/receiving
    messages across the network (though still within the same machine,
    in this example).

  * The manager would break a task into 4 chunks, say by breaking the
    data into 4 chunks, and send the chunks to the workers.

  * Each worker would work on its chunk, then send the result back
    to the manager, which would combine the received results.

  * Example: Sorting. Each worker sorts its chunk, then the manager
    merges the results to obtain the final sorted vector.

* Threading:

  * There would be no manager, just 4 copies of worker code (different
    code from the message-passing case).

  * The threads assign themselves chunks of work to do, instead of being
    assigned by manager code.

  * Similarly, the threads combine the results of work on chunks on
    their own, rather than manager code doing this.

  * The workers operate mostly independently, but interact via variables
    in shared RAM. One thread might modify a shared variable **x**, and
    then another thread might read the new value.

  * Say **x** is shared and **y** is nonshared. Then there is only one
    copy of **x** but 4 copies of **y**.

  * By the way, what about Python? It does have threading capability,
    but has always been useless for parallel computation, as its *Global
    Interpreter Lock* disallows more than one thread running at a time.
    However, the newest version of Python has an experimental GIL-less
    option.

# Rthreads Implementation

* True physical shared RAM, via **bigmemory** package.

* 100% R; no C/C++ code to compile.

* Parallel computation under the shared-memory paradigm.

* Coding style and performance essentially equivalent to
  that of "threaded R" if it were to exist.

* Each thread is a separate instance of R.

* Formerly the **Rdsm** package, but fully rewritten.

# How Rthreads Works

* The sole data type is matrix (or vectors, as one-column matrices), a
  **bigmemory** constraint. A matrix  must be explicitly written with
  two (possibly empty) subscripts, e.g. **x[3,2]**, **x[,1:5]**, **x[,]**.

* The related **synchronicity** package provides mutex support.

* Each thread must run in its own terminal window.

  * This is to facilitate debugging application code. 

    Note: Parallel programming is hard, in any form, and thus one may 
    spend much more time debugging code than writing it.

  * Use **tmux** if screen space is an issue. See below.

* Run **rthreadsSetup** in the first window, 
  then run **rthreadsJoin** in each window (including the first).

  Now call your application function code in each window.

* Some in the computing field believe that one should avoid
  having global variables. However, globals are the essence of
  threading. In R, the generally accepted implementation of globals 
  is to place them in an R environment, which we do here:

  * Shared variables are in the environment **sharedGlobals**.

  * No-shared variables are in the environment **myGlobals**.

# Installation

``` r
devtools::install_github('https://github.com/matloff/Rthreads')
```

# Getting Acquainted with Rthreads (5 minutes)

Create two terminal windows, and in each of them start R and load 
**Rthreads**. Then go through the sequence below, where a blank entry
means to not type anything in that window in that step.

| Step | Window 0                                   | Window 1                          |
|-----:|:-------------------------------------------|:----------------------------------|
| 1    | rthreadsSetup(2)                           |                                   |
| 2    | rthreadsJoin()                             | rthreadsJoin()                    |
| 3    | rthreadsMakeSharedVar('x',1,1,initVal=3)   |                                   |
| 4    |                                            | rthreadsAttachSharedVar('x')      |
| 5    | rthreadsSGget('x',1,1)                       | rthreadsSGget('x',1,1)     |
| 6    |                                            | rthreadsSGset('x',1,1,8)
| 7    | rthreadsSGget('x',1,1)                       | rthreadsSGget('x',1,1)     |
| 8    | rthreadsSGset('x',1,1,12)
| 9    | rthreadsSGget('x',1,1)                       | rthreadsSGget('x',1,1)     |
| 10   | myGlobals[['myID']]                        | myGlobals[['myID']]                    |


: Get-acquainted run-through

(R environments use R list notation; **myGlobals[['myID']** and
**myGlobals$myID** are equivalent.)

Here is what happens:

* Step 1: Set up 2 threads (which will be numbered 0 and 1).

* Step 2: Each thread checks in. The first thread has already done so,
  so just one thread checks in this case, but there still must be code
  that forces even thread 0 to wait until everyone has joined. Note that
  each thread, upon checking in, will then wait for the others to check
  in.

* Step 3: Thread 0 creates a shared variable **x**, initial value 3.

* Step 4: Thread 1 attaches **x**.

* Step 5: We view **x** from each thread, seeing the value 3.

* Step 6: Thread 1 changes the value of **x** to 8.

* Step 7: We confirm that both threads now see the value 8 for **x**.

* Step 8: Thread 0 changes the value of **x** to 12.

* Step 9: We confirm that both threads now see the value 12 for **x**.

* Step 10: We confirm that the thread IDs are 0 and 1. Note that they
  are not shared.

**Note:** Though only one thread runs **rthreadsSetup**, which we have
done with thread 0 here, that thread does NOT play the role of a
"manager" as in message-passing. All threads play symmetric roles.

# Example: Sorting many long vectors

(Note: The code for all of the examples here are in **inst/examples**.)

We have a number of vectors, each to be sorted.

``` r
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
```

To run, say with just 2 threads:

1. Let's refer to the 2 terminal windows as W0 and W1.

2. Load the code for this example.

3. Start **Rthreads** as shown above.

   In W0, run

   ``` r
   setup()
   ```

   This sets up the various shared variables for this example, including
   **m**, our data matrix. Each row will contain a vector to be sorted.

4. In both W0 and W1, run **doSorts()** to do the sorting.

   Sorted rows are now available in **sharedGlobals$m**.

Overview of the code:

* Each thread works on one row of **m** at a time. (See comments in the
  code for other approaches.)

* When a thread finishes sorting a row, it determines the next row to 
  sort by inspecting the shared variable **nextRowNum**. It increments that
  variable by 1, using the old value as the row it will now sort.

* The incrementing must be done *atomically*. Remember, **nextRowNum**
  is a shared variable. Say its value is currently 7, and two threads
  execute the incrementation at about the same time. We'd like one
  thread to next sort row 7 and the other to sort row 8, with the new
  value of **nextRowNum** now being 9. But if there is no constraint on
  simultaneous access, both threads may get the value 7, with
  **nextRowNum** now being 8 (called a *race condition*). Use of
  **rthreadsAtomicInc** ensures that only one thread can access
  **nextRowNum** at a time.

* Here is the internal code for **rthreadsAtomicInc**:

  ``` r
  rthreadsAtomicInc <- function(sharedV,mtx='mutex0',increm=1)
  {
     mtx <- sharedGlobals[[mtx]]
     synchronicity::lock(mtx)
     shrdv <- sharedGlobals[[sharedV]]
     oldVal <- shrdv[1, ]
     newVal <- oldVal + increm
     shrdv[1, ] <- newVal
     sharedGlobals[[sharedV]] <- shrdv
     synchronicity::unlock(mtx)
     return(oldVal)
  }  
  ```
  
  The key here is use of a *mutex* (short for "mutual exclusion"), which
  can be locked and unlocked. While locked, no other thread is allowed to
  enter the given section of code, i.e. the lines beteween the lock and
  unlock operations.  (Termed a *critical section* in the parallel
  processing field.)

  If one thread has locked the mutex and another thread reaches the
  lock line, it will be blocked until the mutex is unlocked. 

* As noted earlier, in threads programming, the threads assign work to
  themselves, instead of manager code doing so. Here this is done via
  the shared variable **nextRowNum**.

  Another possibility would be to have threads *pre*-assign work to
  themselves. With 10 rows and 2 threads, for instance, the first thread
  could work on rows 1 through 5, with the second handling rows 6 to 10.
  But this may not work well in settings with *load imbalance*, where
  some rows require more work than others (as with our test case here).

# Example: Missing value imputation

Here the application is imputation of NAs in a large dataset. Note that
I've kept it simple, so as to best illustrate the parallelization
principles involved; no implication is intended that this is an
effective imputation method, nor for that matter is the parallelization
optimal. 

We have a large data frame with numerical entries, but with NA values
here and there. A common imputation approach for a given column is to run
some kind of regression method (parametric or nonparametric) to predict
this column from the others, then replace the NAs by their predicted
values.

We will do this column by column, with each thread temporarily saving
its imputed values rather than immediately writing them back
to the dataset. Only after all threads have computed imputations in the
given round do we update the actual dataset. 

Since we are working column by column, this means earlier imputations
can be employed in the regression actions of later columns, hopefully
improving imputation accuracy. (Again, this may or may not be the case,
but that is the motivation behind this imputation algorithm.) 

The purpose of this example is mainly to illustrate the concept of a
*barrier*, an important threads concept. When a thread reaches that
line, it may not proceed further until *all* threads have reached the
line. Why is this needed here? Actually, we don't need it in this
particular case, but I've included it to explain the barrier concept, as
follows.

Here is the code:

``` r
# threads configuration: run
#    rthreadsSetup(nThreads=2)

# NA imputation, simple use of linear regression, each column's NAs
# replaced by fitted values

# note that NA elements imputed in one column will be used as inputs to
# imputation in later columns (after the curren "round"; see below)

# mainly for illustrating barriers; could be made faster in various ways

# for more of a computational-time challenge, try say k-NN instead of
# linear regresion

setup <- function()  # run in "manager thread"
{
   data(NHISlarge)
   nhis.large <- regtools::factorsToDummies(nhis.large,dfOut=FALSE)
   nhis.large <- nhis.large[,-(1:4)]  # omit ID etc.
   z <- dim(nhis.large)
   nr <- z[1]
   nc <- z[2]
   rthreadsMakeSharedVar('dta',nr,nc,initVal=nhis.large)
   rthreadsInitBarrier()
}

doImputation <- function()  
{
   if (myGlobals$myID > 0) {
      rthreadsAttachSharedVar('dta')
   }
   nc <- ncol(sharedGlobals$dta)
   nThreads <- myGlobals$info$nThreads

   # in each round, each thread works on one column; they then update
   # the data
   nRounds <- ceiling(nc/myGlobals$info$nThreads)
   numPerRound <- floor(nc/nRounds)
   for (i in 1:nRounds) {

      myColNum <- (i-1)*numPerRound + myGlobals$myID + 1
   
      # impute this column, if needed
      myImputes <- NULL
      if (myColNum <= nc) {
         print(myColNum)
         NAelements <- which(is.na(sharedGlobals$dta[,myColNum]))
         if (length(NAelements) > 0) {
            lmOut <- 
               lm(sharedGlobals$dta[,myColNum] ~ sharedGlobals$dta[,-myColNum])
            # note: if a row has more than 1 NA, imputed value 
            # will still be NA
            imputes <- lmOut$fitted.values[NAelements]
            myImputes <- 
               list(colNum=myColNum,imputes=imputes,NAelements=NAelements)
         }
      }

      # update data, where needed
      rthreadsBarrier()  # can't change dta while possbily still in use

      if (!is.null(myImputes)) {
         colNum <- myImputes$colNum
         NAelements <- myImputes$NAelements
         imputes <- myImputes$imputes
         sharedGlobals$dta[NAelements,colNum] <- imputes
      }

      rthreadsBarrier()  # some threads may still be writing to dta

   }

}
```

To run the code, first run **setup** in one window, then **doImputation**
in all windows. The shared object **sharedGlobals$dta** contains the dataset
throughout the code, and upon completion the
imputed dataset will there.

Work is done on groups of columns, called "rounds" here.

``` r
nc <- ncol(sharedGlobals$dta)
nThreads <- myGlobals$info$nThreads

# in each round, each thread works on one column; they then update
# the data
nRounds <- ceiling(nc/myGlobals$info$nThreads)
numPerRound <- floor(nc/nRounds)
for (i in 1:nRounds) {
```

Within a round, each thread checks its assigned column for NAs, and
performs the imputation (if any) in **myImputes**:

``` r
myImputes <- NULL
if (myColNum <= nc) {
   print(myColNum)
   NAelements <- which(is.na(sharedGlobals$dta[,myColNum]))
   if (length(NAelements) > 0) {
      lmOut <- 
         lm(sharedGlobals$dta[,myColNum] ~ sharedGlobals$dta[,-myColNum])
      # note: if a row has more than 1 NA, imputed value 
      # will still be NA
      imputes <- lmOut$fitted.values[NAelements]
      myImputes <- 
         list(colNum=myColNum,imputes=imputes,NAelements=NAelements)
   }
}
```

At the end of a round, each thread will update the dataset with the
imputations it found. However, it must first make sure all threads are
done with their imputation processes; otherwise, this thread might write
to the dataset while another thread is still using the old version of the
dataset. This is accomplished by the barrier operation:

``` r
      myImputes <- 
         list(colNum=myColNum,imputes=imputes,NAelements=NAelements)
   }
}

# update data, where needed
rthreadsBarrier()  # can't change dta while possbily still in use
```

Similarly, after performing the update and going on to the next round,
we must first make sure all threads are done with their update operations,
thus another barrier:

``` r
if (!is.null(myImputes)) {
   colNum <- myImputes$colNum
   NAelements <- myImputes$NAelements
   imputes <- myImputes$imputes
   sharedGlobals$dta[NAelements,colNum] <- imputes
}

rthreadsBarrier()  
```
 
# Example: Shortest paths in a graph

We have a *graph* or *network*, consisting of people, cities or
whatever, with links between some of them, and wish to find the shortest
path from all vertices A to a vertex B. This ia a famous problem, with
lots of applications. Here we take a matrix approach, with a parallel
solution via **Rthreads**.

We treat the case of *directed* graphs, meaning that a direct link from
vertex i to vertex j does not imply that a link exists in the opposite
direction.  For a graph of v vertices, the *adjacency matrix* M of the
graph is of size v X v, with the row i, column j element being 1 or 0,
depending on whether there is a link from i to j. We also assume the
matrix is a *directed acylic graph* (DAG), meaning that any path leading
out of vertex i cannot return to i.

Here is the code:

``` r
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

   # each thread will work on its assigned group of threads
   myRows <- parallel::splitIndices(n,nThreads[,])[[myID+1]]
   myRows <- setdiff(myRows,whichDEs)
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
```

As a test run, first do

``` r
setup()
```

in the first window, and then  

``` r
adjMat <- rbind(
             c(0,1,1,0,0),
             c(0,1,0,0,0),
             c(0,1,0,1,1),
             c(1,0,0,0,1),
             c(0,0,0,0,1))
findMinDists(adjMat,5)

```

in all windows. Check the output by running

``` r
rthreadsSGget('done')
```

in some window.  The shared matrix **done** has Column 2 of row i equal
to 1 or 2, depending on whether a path exists from vertex i to the
destination vertex (column 1 gives the corresponding path length.

How does it work?

A key property is that the k-th power of M tells us whether there is a
k-link path from i to j, according to whether the row i, column j
element in the power is nonzero. The matrix powers are computed in
parallel, with each thread being responsible for a subset of rows:

``` r
myRows <- parallel::splitIndices(n,nThreads[,])[[myID+1]]
myRows <- setdiff(myRows,whichDEs)
mySubmatrix <- adjm[myRows,]
...
if (iter > 1) adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
```

Here we have used the fact that in a matrix product W = UV, row i of W
is equal to the product of row i of U with V.

Here is where the main work is done:

``` r
for (myRow in myRows) {
   if (done[myRow,1] > 0) next 
   # this vertex myRow not decided yet as to a path to destVertex
   if (adjmPow[myRow,destVertex] > 0) {  # success!
      done[myRow,1] <- iter
      done[myRow,2] <- 1
   } else {
      ...
```

This type of application, in which the threads do not interact with each
other--no barriers, no autoincrement etc.--is often called
*embarrassingly parallel*, alluding to the fact it is so easy to code.
That is not quite the case here, though, because the code also makes use
of "dead ends," vertices that lead only to themselves. This is simple in
concept but involves keeping track of various edge cases.

# Regarding Repeat Runs

Objects created by **bigmemory** are persistent until removed or until
the R session ends. This can have implications if you do a number of
runs of an **Rthreads** application. Be sure your application's **setup**
function re-initializes **bigmemory** objects as needed.

Note too that **myID** and **info** are also persistent.

# Facilitating Rthreads Use via 'screen' or 'tmux'

In using **Rthreads**, one needs a separate terminal window for each
thread. Some users may have concerns over the screen real estate that is
used.  The popular Unix (Mac or Linux) **screen** and **tmux** utilities
can be very helpful in this regard. Basically, they allow multiple
terminal windows to share the same screen space.

Methods for automating the process of setting up the windows,
automatically running e.g. **rthreadsJoin** in each one, would be
desirable.  The above utilities can accomplish this by enabling us to
have code running in one window write a specified string to another
window. E.g. say we are in the shell of Window A. We can do, e.g. 

``` bash
screen -S WindowBScreen
```

in Window B to start a **screen** session there. Then in Window A
we might run

``` bash
screen -S WindowB -X stuff 'ls'$'\n'
```

and the Unix **ls** command will run in Window B just as if we had typed
it there ourself! Moreover, we might be running R in Window A, in which
case we can run the above shell command via R's **system** function

In other words, we can for instance automate the running of
**rthreadsJoin** in all the windows, instead of having to type the
command in each one.

# To Learn More

* [N. Matloff, *Parallel Computing for Data Science
With Examples in R, C++ and CUDA*](https://www.google.com/books/edition/Parallel_Computing_for_Data_Science/SsbECQAAQBAJ?hl=en&gbpv=0)

* [Tutorial on accessing OpenMP via **Rcpp**](https://mfasiolo.github.io/sc2-2019/rcpp_advanced_iii/1_openmp/)

# Legal

Freely copyable providing attribution is given. No warranty is given of
any kind.

