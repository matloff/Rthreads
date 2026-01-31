# Rthreads: a Threads(-Like) Package for R!

# Threading and Rthreads at a Glance

## Filling an Important Gap

* *Threading* is a major mechanism for parallel operations in the world
  of computing.

* In C/C++, a popular threading library is OpenMP.

* R does not have native threading.

* Lack of threading in R means that developers of fast packages 
  like **data.table** must rely on threads at the C++ level (and
  only implementing specific functions).

* An alternative to threads is *message-passing*, e.g. R's **parallel**
  package, including via interfaces such as **foreach** and **futures**.
  However, message-passing approaches may run slowly due to the overhead
  of sending messages.

* Threaded coding tends to be clearer and produce greater speedup,
  compared to message-passing.

* Thus having a threads capability in R would greatly enhance
  R's capabilities in parallel processing.

<!-- 
## What Is the Difference between Threading and Message Passing?

* Say we are on a quad-core machine. Here is an overview of the two
  paradigms (lots of variants):

* Message passing, using the **parallel** package:

  * We would call a **parallel** function to set up a *cluster* of size
    4, consisting of 4 R processes.

  * We would write 2 functions, a worker and a manager. There would be,
    say, 4 copies of the worker code, running fully independently
    on the cluster nodes.

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

  * One needs to avoid situations in which one worker is reading a
    shared variable while others are reading it or writing to it.
    This is handled via variables known as *mutexes*.

  * Say **x** is shared and **y** is nonshared. Then there is only one
    copy of **x** but 4 copies of **y**.

  * By the way, what about Python? It does have threading capability,
    but has always been useless for parallel computation, as its *Global
    Interpreter Lock* disallows more than one thread running at a time.
    However, the newest version of Python has an experimental GIL-less
    option.
-->

## Rthreads Implementation

* True physical shared RAM, via **bigmemory** package.

* 100% R; no C/C++ code to compile.

* Parallel computation under the shared-memory paradigm.

* Coding style and performance essentially equivalent to
  that of "threaded R" if it were to exist.

* Each thread is a separate instance of R. These can be accessed 
  individually or via included **tmux** wrappers.

* Formerly the **Rdsm** package, but fully rewritten.

## How Rthreads Works

* The sole data type is matrix (or vectors, as one-column matrices), a
  constraint due to **bigmemory**. A matrix  must be explicitly written with
  two (possibly empty) subscripts, e.g. **x[3,2]**, **x[,1:5]**, **x[,]**.

* The related **synchronicity** package provides mutex support.

* Each thread runs in its own terminal window. 

  * This is in contrast to the **parallel** package, which runs 
    multiple R instances but in which those instances run without 
    terminals, inaccessible to the user. 

  * This is to facilitate users debugging their application code. 
    Users can use ordinary R debugging commands in the terminal windows.

  * But use of the included **tmux** wrappers makes managing
    the R instances much more convenient, by automating the process 
    of setting up the windows, running R in them, etc.

    Moreover, use of the wrappers is necessary when writing Rthreads
    code as R scripts, to be saved in .R files etc., rather than 
    interactive one-offs.

<!--
* Setup: First **rthSetup** is run in the first window, and
  then **rthJoin** is run in each window including the first.
  Again, this is automated via the **tmux** wrappers.

  Now call your application function code in each window (again, can
  be done via a single **tmux** wrapper call).
-->

* Global variables are the essence of threading. In R, the generally 
  accepted implementation of globals is to place them in an R 
  environment, which we do here: Shared variables are in the 
  environment **sharedGlobals**.

## Installation

``` r
devtools::install_github('https://github.com/matloff/Rthreads')
```

# Getting Acquainted with Rthreads (5 minutes)

We first present use of the package "by hand," i.e. without the
**tmux** wrappers. This is the inconvenient approach, but it illustrates
the actions of the package well. We then present usage with the
wrappers, the standard way to use the package.

## A Run-through "By Hand"

Create two terminal windows, and in each of them start R and load 
**Rthreads**. Then go through the sequence below, where a blank entry
means to not type anything in that window in that step.

| Step | Window 0                                   | Window 1                          |
|-----:|:-------------------------------------------|:----------------------------------|
| 1    | rthSetup(2)                           |                                   |
| 2    | rthJoin()                             | rthJoin()                    |
| 3    | rthMakeSharedVar('x',1,1,initVal=3)   |                                   |
| 4    |                                            | rthAttachSharedVar('x')      |
| 5    | rthSGget('x',1,1)                       | rthSGget('x',1,1)     |
| 6    |                                            | rthSGset('x',1,1,8)
| 7    | rthSGget('x',1,1)                       | rthSGget('x',1,1)     |
| 8    | rthSGset('x',1,1,12)
| 9    | rthSGget('x',1,1)                       | rthSGget('x',1,1)     |
| 10    | rthMyID()                       | rthMyID()     |


Here is what happens:

* Step 1: Set up 2 threads (which will be numbered 0 and 1).

* Step 2: Each thread checks in. The first thread has already done so,
  so just one thread checks in here in this case, but there still must be code
  that forces even thread 0 to wait until everyone has joined. 
  Each thread, upon checking in, will then wait for the others to check
  in; you'll see whichever thread you called **rthJoin** on first won't
  return to the '>' prompt right away.

* Step 3: Thread 0 creates a shared variable **x**, with 1 row and 1
  column, initial value 3.

* Step 4: Thread 1 attaches **x**.

* Step 5: We view **x** from each thread, seeing the value 3 in
  both cases, illustrating the shared nature of that variable..

* Step 6: Thread 1 changes the value of **x** to 8.

* Step 7: We confirm that both threads now see the value 8 for **x**.

* Step 8: Thread 0 changes the value of **x** to 12.

* Step 9: We confirm that both threads now see the value 12 for **x**.

* Step 10: We confirm that the thread IDs are 0 and 1. Note that the
  **myID** variable (whose value is the one returned by this call) is
  not shared.

**Note:** Though only one thread runs **rthSetup**, which we have
done with thread 0 here, that thread does NOT play the role of a
"manager" as in message-passing. All threads play symmetric roles.

## The Same Example But Using the Convenience Wrappers

Although this code can be executed by simply copying-and-pasting it into
the original R window, normally it would be stored in a .R file (or IDE
project), and executed by running **source** or similar on the file or
project.

First, start a **tmux** session, by typing 

``` bash
tmux new -s 'abc'
```

in a terminal window; the **tmux** session name will be 'abc'. Then
in your original R window (or R console etc.), type

``` r

# split tmux window into 2 windows, one for each thread 
# to be created; start R and Rthreads in them, 
# and create the threads
tmRthreadsInit(2)  
# thread 0, create shared x 
tmSendKeys('abc','rthMakeSharedVar("x",1,1,initVal=3)',0)
# thread 1, attach shared x 
tmSendKeys('abc','rthAttachSharedVar("x")',1)
# take a look at x (see below note re [,] etc.)
tmSendKeys('abc','sharedGlobals[["x"]][,]') # no thread spec., do all
# thread 1, change x to 2
tmSendKeys('abc','rthSGset("x",1,1,2)',1)  
# check that all threads see the new value
tmSendKeys('abc','rthSGget("x",1,1)')
# etc.
```

Explanation of **sharedGlobals** etc.: The shared variables are stored
in the R environment of that name. So they can be read or written to
directly in that environment, as seen here, providing a direct
alternative to **rthSGset** and **rthSGget**.

A shared variable **x** is a **bigmemory** object, which is a pointer to
the corresponding matrix. So to access the matrix, we need to specify
row and column numbers, or for the full matrix, [,].

The **tmRthreadsInit** function has a lot to do. It creates the various
**tmux** windows, runs R in them, and handles the thread checkins.

*Multiplexing windows:*

What does "creating the **tmux** windows mean? What had been a single
window will now become two or more, with the number of windows being
equal to the number of threads. So, each thread runs in its own window.
But these windows occupy the same physical real estate on your screen,
with only one appearing at a time; we say that they are *multiplexed*.
The instances of R running in these windows continue executing, even
though only one thread is visible at a time.

To manually switch from viewing one thread to another, type ctrl-b n or
ctrl-b p ('next' and 'previous'). This can be done programmatically as
well.

The **tmux** system can also split windows rather than multiplexing
them. One window is split into one or more, either horizontally or
vertically. That has the advantage of having all thread actions visible
at once, but since one typically runs at least four threads (on a 4-core
machine), it occupies too much space.

# Examples

In each example, start a **tmux** window in addition to an R window (or
R Console in an IDE etc.). In the latter, run

``` r
library(Rthreads)
tmRthreadsInit(2)  # argument is number of threads
```

## Example: Sorting many long vectors

(Note: The code for all of the examples here are in **inst/examples**,
or in the package code itself.)

We have a number of vectors, each to be sorted.

``` r
# a general issue in parallel computation is that of "load balance,"
# meaning that each thread ends up doing approximately the same amount
# of work; here we aim for that via dynamic thread assignment, but if we
# had a very large number of rows, random pre-assignment would probably
# work fine, and would not have the overhead of engaging with a mutex

setup <- function(vecLengths=1000)  # run in thread 0
{
   rthMakeSharedVar('nextRowNum',1,1)
   rthMakeSharedVar('m',10,vecLengths+1,NA)  # will be our vectors to sort
   # generate vectors to be sorted, of different sizes
   tmp <- c(round(0.3*vecLengths),vecLengths)
   set.seed(9999)
   nvals <- sample(tmp,10,replace=TRUE)  # lengths of 10 vectors to sort
   m <- sharedGlobals$m  
   for (i in 1:10) {  # fill in the vector
      n <- nvals[i]
      m[i,1:(n+1)] <- c(n,runif(n))  # 1st column is length
   }
   sharedGlobals$nextRowNum[1,1] <- sharedGlobals$nThreads[1,1] + 1
}

doSorts <- function()  # run in all threads, maybe with system.time()
{

   myID <- rthMyID()
   if (myID != 0) {
      rthAttachSharedVar("nextRowNum")
      rthAttachSharedVar("m")
   } 
   
   m <- sharedGlobals$m

   rowNum <- myID+1  # my first vector to sort

   while (rowNum <= nrow(m)) {
      # as illustration of parallel operation, see which threads execute
      # sorts on which rows
      print(rowNum)
      n <- m[rowNum,1]  # vector length
      x <- m[rowNum,2:(n+1)]
      m[rowNum,2:(n+1)] <- sort(x)
      rowNum <- rthAtomicInc('nextRowNum') 
   }

   rthBarrier()  
}
```

To run:

``` r
# read in app code
tmSendKeys('abc','rthSrcExamples("Sorts.R")')
# run 'setup' in thread 0
tmSendKeys('abc','setup()',0)
# run 'doSorts' in all threads 
tmSendKeys('abc','doSorts()')
```

To check results:

``` r
tmSendKeys('abc','rthSGget("m",cols=1:5)')
```

Sure enough, the rows appear to be in ascending numerical order.

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
  **rthAtomicInc** ensures that only one thread can access
  **nextRowNum** at a time.

* Here is the internal code for **rthAtomicInc**:

  ``` r
  rthAtomicInc <- function(sharedV,mtx='mutex0',increm=1)
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

## Example: Missing value imputation

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

The purpose of this example is mainly to illustrate the concept of a
*barrier*, an important threads concept. When a thread reaches that
line, it may not proceed further until *all* threads have reached the
line. As noted in the code comments, we need a situation in which
one thread changes **dta** while other threads are still making use of
the original version.

Here is the code:

``` r
# NA imputation, simple use of linear regression, each column's NAs
# replaced by fitted values

# mainly for illustrating barriers

# example

#  at thread 0 do
#  
#     data(NHISlarge)
#     nhis.large <- regtools::factorsToDummies(nhis.large,dfOut=FALSE)
#     nhis.large <- nhis.large[,-(1:4)]  # omit ID etc.
#     setup(nhis.large)
#  
#  at all threads do
#  
#     doImputation()
#  
#  at any thread do
#  
#     dta <- rthSGget('dta')
#     head(dta)
#  
#  at some other thread do
#  
#     head(nhis.large)

setup <- function(dta)  # run in thread 0
{
   z <- dim(dta)
   nr <- z[1]
   nc <- z[2]
   rthMakeSharedVar('dta',nr,nc,initVal=nhis.large)
   rthInitBarrier()
}

doImputation <- function()  
{
   if (myGlobals$myID > 0) {
      rthAttachSharedVar('dta')
   }
   nThreads <- sharedGlobals$nThreads[1,1]
   dta <- sharedGlobals$dta
   myID <- myGlobals$myID
   nc <- ncol(dta)

   # each thread is assigned columns to work on
   myCols <- parallel::splitIndices(nc,nThreads)[[myID+1]]

   myImputes <- NULL
   for (col in myCols) {
      # impute this column, if needed
      NAelements <- which(is.na(dta[,col]))
      if (length(NAelements) > 0) {
         lmOut <- lm(dta[,col] ~ dta[,-col])
         imputes <- lmOut$fitted.values[NAelements]
         # note: if a row in dta[,-col] has more than 1 NA,
         # its predicted (and thus imputed)  value will still be NA
         tmp <- 
            list(colNum=col,imputes=imputes,NAelements=NAelements)
      } else tmp <- NULL
      myImputes[[col]] <- tmp

   }

   # impute data
   rthBarrier()  # can't change dta while possbily still in use
   for (col in myCols) {
      myimps <- myImputes[[col]]
      if (!is.null(myimps)) {
        NAelements <- myimps$NAelements
        imputes <- myimps$imputes
        dta[NAelements,col] <- imputes
      }
   }

}

```

An example and directions for running it are given in the code comments.
 
## Example: Shortest paths in a graph

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

In this version, we find the lengths of the shortest paths from all
vertices to a given one. We do not find the paths themselves.  Here is
the code:

``` r
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
# rthSGget('done')

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
   rthMakeSharedVar('nDone',1,1,initVal=0)
   rthInitBarrier()
}

findMinDists <- function(adjMat,destVertex)  
{
   adjm <- adjMat
   adjmPow <- adjm
   n <- nrow(adjm)

   myID <- myGlobals$myID
   if (myID == 0) 
      rthMakeSharedVar('done',n,1,initVal=rep(0,n))  # see above
   rthBarrier()
   if (myID > 0) {
      rthAttachSharedVar('done')
      rthAttachSharedVar('nDone')
   } 

   # for brevity, make copies of some shared objects; since they are
   # references, writing to the copy will make the same change to the
   # original
   nThreads <- sharedGlobals$nThreads
   nDone <- sharedGlobals$nDone
   done <- sharedGlobals$done

   # each thread will work on its assigned group of threads
   myRows <- parallel::splitIndices(n,nThreads[,])[[myID+1]]
   
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
         rthAtomicInc('nDone')
         break
      }
   }

   rthBarrier()
}

```

How does it work?

A key property is that the k-th power of M tells us whether there is a
k-link path from i to j, according to whether the row i, column j
element in the power is nonzero. The matrix powers are computed in
parallel, with each thread being responsible for a subset of rows:

``` r
myRows <- parallel::splitIndices(n,nThreads[,])[[myID+1]]
myRows <- setdiff(myRows,whichDEs)
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
automatically running e.g. **rthJoin** in each one, would be
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
**rthJoin** in all the windows, instead of having to type the
command in each one.

# To Learn More

* [N. Matloff, *Parallel Computing for Data Science
With Examples in R, C++ and CUDA*](https://www.google.com/books/edition/Parallel_Computing_for_Data_Science/SsbECQAAQBAJ?hl=en&gbpv=0)

* [Tutorial on accessing OpenMP via **Rcpp**](https://mfasiolo.github.io/sc2-2019/rcpp_advanced_iii/1_openmp/)

# Legal

Freely copyable providing attribution is given. No warranty is given of
any kind.

