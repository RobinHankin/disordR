setGeneric("length")
setMethod("length","disord",function(x){length(elements(x))})

setGeneric("length<-")
setReplaceMethod("length","disord",function(x){stop("cannot set length of a disord object")})

setGeneric("names")
setMethod("names","disord",
          function(x){
            jj <- names(elements(x))
            if(is.null(jj)){
              return(NULL)
            } else {
              return(disord(jj,h=hash(x)))
            }
          } )

setGeneric("names<-")
setReplaceMethod("names",signature(x="disord",value="disord"),
                 function(x,value){
                   stopifnot(consistent(x,value))
                   jj <- elements(x)
                   names(jj) <- elements(value)
                   return(disord(jj,h=hash(x)))
                 } )

setReplaceMethod("names",signature(x="disord",value="ANY"),
                 function(x,value){
                   stopifnot(length(value)==1)
                   jj <- elements(x)
                   names(jj) <- elements(value)
                   return(disord(jj,h=hash(x)))
                 } )

setGeneric("rev")
setMethod("rev",signature=c(x="disord"),function(x){
    disord(rev(elements(x)),h=paste(rev(strsplit(hash(x), "")[[1]]), collapse = ""))
    } )
    
setGeneric("sort")
setMethod("sort",signature=c(x="disord"),function(x,decreasing=FALSE,...){
    sort(elements(x),decreasing=decreasing,...)
    } )

setGeneric("min")  # NB not perfect, eg, min(1,disord(3)) fails
`mindispair` <- function(x,y,na.rm=FALSE){min(elements(x),elements(y),na.rm=na.rm)}
setMethod("min",
    signature(x = "disord"),
    function (x, ..., na.rm = FALSE){
        a <- list(...)
        if(nargs() < 3){
            return(min(elements(x),na.rm=na.rm))  #  min(a)
        } else if(nargs() ==3){ # min(a,b)
            return(do.call("mindispair",c(x, a ,na.rm=na.rm)))
        } else { # min(a,b,c)
            return(do.call("min",c(disord(mindispair(x,a[[1]],na.rm=na.rm)),a[-1] ,na.rm=na.rm)))
        }
    }
)

setGeneric("max")  # NB not perfect, eg, max(1,disord(3)) fails
`maxdispair` <- function(x,y,na.rm=FALSE){max(elements(x),elements(y),na.rm=na.rm)}
setMethod("max",
    signature(x = "disord"),
    function (x, ..., na.rm = FALSE){
        a <- list(...)
        if(nargs() < 3){
            return(max(elements(x),na.rm=na.rm))  #  max(a)
        } else if(nargs() ==3){ # max(a,b)
            return(do.call("maxdispair",c(x, a ,na.rm=na.rm)))
        } else { # max(a,b,c)
            return(do.call("max",c(disord(maxdispair(x,a[[1]],na.rm=na.rm)),a[-1] ,na.rm=na.rm)))
        }
    }
)
