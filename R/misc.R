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

setGeneric("min")  # NB not perfect, eg, min(1,disord(3)) and min(disord(3),1,disord(3)) fail
setGeneric("minpair", function(x,y,na.rm){standardGeneric("minpair")})
setMethod("minpair", c("disord", "disord"), function(x,y,na.rm=FALSE){min(elements(x),elements(y),na.rm=na.rm)})
setMethod("minpair", c("disord", "ANY"   ), function(x,y,na.rm=FALSE){min(elements(x),y          ,na.rm=na.rm)})
setMethod("minpair", c("ANY", "disord"   ), function(x,y,na.rm=FALSE){min(x,elements(y)          ,na.rm=na.rm)})
setMethod("minpair", c("ANY", "ANY"      ), function(x,y,na.rm=FALSE){min(x,y                    ,na.rm=na.rm)})

setMethod("min",
    signature(x = "disord"),
    function (x, ..., na.rm = FALSE){
        if(nargs() < 3){
            return(min(elements(x),na.rm=na.rm))  # regular R min()
        } else {
            return(minpair(min(x), min(..., na.rm=na.rm),na.rm=na.rm))
        }
    }
)

setGeneric("max")  # NB not perfect, eg, max(1,disord(3)) and max(disord(3),1,disord(3)) fail
setGeneric("maxpair", function(x,y,na.rm){standardGeneric("maxpair")})
setMethod("maxpair", c("disord", "disord"), function(x,y,na.rm=FALSE){max(elements(x),elements(y),na.rm=na.rm)})
setMethod("maxpair", c("disord", "ANY"   ), function(x,y,na.rm=FALSE){max(elements(x),y          ,na.rm=na.rm)})
setMethod("maxpair", c("ANY", "disord"   ), function(x,y,na.rm=FALSE){max(x,elements(y)          ,na.rm=na.rm)})
setMethod("maxpair", c("ANY", "ANY"      ), function(x,y,na.rm=FALSE){max(x,y                    ,na.rm=na.rm)})

setMethod("max",
    signature(x = "disord"),
    function (x, ..., na.rm = FALSE){
        if(nargs() < 3){
            return(max(elements(x),na.rm=na.rm))  # regular R max()
        } else {
            return(maxpair(max(x), max(..., na.rm=na.rm),na.rm=na.rm))
        }
    }
)
