

# code to automate setup and use of Rthreads, using the R 'parallel' pkg
# and the Unix 'tmux' utility

# once Rthreads is running in the windows, one can continue to run
# commands in them via tmSendKeys, or by manually typing into the
# windows

# IDEs (Positron, VS Code etc.) are not suitable for parallel
# computation; everything here is based on terminal windows

# user first does:
#
# 1. Open a terminal window (the "control window"). Start R in it, and
# load Rthreads.
# 
# 2. Open a terminal window (the "computation window"), and type
# 
#       tmux new -s abc
#
# (or any name instead of 'abc') into the shell.
#
# 3. Call tmRthreadsInit from the control window, specifying the desired
# number of threads.
#
# 4. Done!  Call the functions below from the control window as needed.

# NOTE: to end the tmux session, do not just kill the windows; do
#
#    tmux kill-session -t abc
#
# from any shell window, or call tmQuit from R

# send 'msg' to windows listed in 'recip'
tmSendKeys <- function(tmName,msg,recip='all')
{
   nw <- tmGetNWindows(tmName)
   if (identical(recip,'all')) recip <- 0:(nw-1)
   for (i in recip) {
      dst <- paste0(tmName,":",i)
      cmd <- paste0("tmux send-keys -t ",dst,' ',"'",msg,"'","  C-m")
      system(cmd)
   }
}

# creates nWindows-1 additional windows, runs R in the new ones and
# Rthreads in each of the nWindows windows; assumes we had already
# started tmux from this shell window, and are now running  R in it

tmRthreadsInit <- function(nWindows,tmName='abc') 
{
   for (i in 1:(nWindows-1)) system('tmux new-window')
   tmSendKeys(tmName,'R')
   tmSendKeys(tmName,'library(Rthreads)')
   setupCall <- paste0('rthSetup(',nWindows,')')
   tmSendKeys(tmName,setupCall,0)
   checkinCall <- 'rthJoin()'
   tmSendKeys(tmName,checkinCall,0)
   for (i in 1:(nWindows-1)) tmSendKeys(tmName,checkinCall,i)
}

# go to next window; or manually via ctrl-b n
tmNxt <- function() system('tmux next-window')

# go to previous window; or manually via ctrl-b p
tmPrv <- function()  system('tmux previous-window')

# go to window k; or manually via ctrl-b k
tmWinK <- function(k)  
{
   cmd <- paste0('tmux select-window -t ',k)
   system(cmd)
}

tmQuit <- function(tmName='abc')
{
   cmd <- paste0('tmux kill-session -t ',tmName)
   system(cmd)
}
 
# get number of windows
tmGetNWindows <- function(tmName='abc') 
{
   cmd <- paste0("tmux list-windows -t ",tmName," | wc -l")
   cmdOut <- system(cmd,intern=TRUE)
   as.numeric(cmdOut)
}

# removes the specified current thread set, indexing starting at 0; e.g.
# toRemove = 4:5 removes threads 4 and 5

# assumes no code is currently active, e.g. no threads waiting in
# barriers; user should ensure that all initializations, e.g. like the
# 'setup' functions in 'Rthreads::examples', are redone

# to increase the number of threads, essentially start over, by exiting
# R in each thread, and then calling rthreadsSetup

tmReduceNThreads <- function(toRemove,tmName='abc') 
{
   # update nThreads
   tmp <- 'nthreads <- rthSGget("nThreads",1,1)'
   tmSendKeys(tmName,tmp)
   nRemove <- length(toRemove)
   tmp <- paste0('nremove <- ',sprintf('%d',nRemove))
   tmSendKeys(tmName,tmp)
   tmp <- 'newNthreads <- nthreads - nremove'
   tmSendKeys(tmName,tmp)
   tmSendKeys(tmName,'rthreadsSGset("nThreads",1,1,newNthreads)')
   # remove the threads
   tmSendKeys(tmName,'quit(save="no")',toRemove)
}

