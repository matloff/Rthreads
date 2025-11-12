
.onAttach <- function(libname, pkgname) {

   packageStartupMessage(
      'Type "quickstart()" for a quick overview!\n')

   topDir <- getwd()
   topDirEnv <- new.env(parent=emptyenv())
   topDirEnv$topDir <- topDir

   dir.create('descFiles')

}

