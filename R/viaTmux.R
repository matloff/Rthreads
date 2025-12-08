

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
   if (recip == 'all') recip <- 0:(nw-1)
   for (i in recip) {
      dst <- paste0(tmName,":",i)
      cmd <- paste0("tmux send-keys -t ",dst,' ','\"',msg,'\"',"  C-m")
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
   setupCall <- paste0('rthreadsSetup(',nWindows,')')
   tmSendKeys(tmName,setupCall,0)
   checkinCall <- 'rthreadsJoin()'
   tmSendKeys(tmName,checkinCall)
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

