
############# under construction ************

# code to automate setup and use of Rthreads, using the R 'parallel' pkg

# user first does:
# 
# open a terminal window
# 
# tmux new -s abc
# 
# hit ctrl-b c k-1 times for k-1 new tmux windows, k = number of threads

tmGetNWindows <- function(tmName='abc') 
{
   cmd <- paste0("tmux list-windows -t ",tmName," | wc -l")
   cmdOut <- system(cmd,intern=TRUE)
   as.numeric(cmdOut)
}

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

# start tmux, nWindows windows, run R in each
tmRthreadsInit <- function(nWindows,tmName='abc') 
{
   tmSendKeys(tmName,'R')
   tmSendKeys(tmName,'library(Rthreads)')
   setupCall <- paste0('rthreadsSetup(',nWindows,')')
   tmSendKeys(tmName,setupCall,0)
   checkinCall <- 'rthreadsJoin()'
   tmSendKeys(tmName,checkinCall)
}

