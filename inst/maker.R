fc <-"funcdefs.R"
f <- function(func){
    cat(paste('setGeneric("',func,'")\n',sep=''),file=fc,append=TRUE)
    cat(paste('setMethod("',func,'",signature=c(x="disord"),function(x){disord(',func,'(elements(x)),h=hash(x))})\n',sep=''),file=fc,append=TRUE)
}

cat("# This file is not intended to be human readable.  It is created by inst/maker.R.\n",file=fc,append=FALSE)


sapply(read.table("funcnames.txt")$V1,f)
