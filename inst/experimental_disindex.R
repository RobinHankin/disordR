setClass("disindex",slots = list(value = "numeric", hash="character"))
setMethod("show", "disindex", function(object){disindex_show(object)})

`disindex_show` <- function(x){
    cat("A disind object with hash",hash(x), "and", length(.value(x)), "(implementation-specific) elements\n")
    return(invisible(x))
}

setMethod("summary","disindex",function(object,...){stop("disindex objects are for extraction only")})

setGeneric("which")
setMethod("which","disord",function(x, arr.ind = FALSE, useNames = TRUE){new("disindex",value=which(elements(x)),hash=hash(x))})
setMethod("which","disindex",function(x, arr.ind = FALSE, useNames = TRUE){stop("which() not defined on disindex objects")})

`.value` <- function(x){x@value}  # DO NOT EXPORT THIS

setMethod("[", signature(x="disord",i="disindex",j="missing",drop="ANY"),  # makes things like a[which(a>4)] work
          function(x,i,j,drop=TRUE){
            stopifnot(identical(hash(x),hash(i)))
            out <- (elements(x))[.value(i)]
            out <- disord(out, hashcal(c(hash(x),hash(i))),drop=FALSE)  # NB newly generated hash, stops things like a[a>4]+ a[a<3] but allows a[x<3] <- x[x<3]
            if(drop){
              return(drop(out))
            } else {
              return(out)
            }
          } )

setMethod("[", signature(x="disord",i="disindex",j="ANY",drop="ANY"),  # stops a[which(a>4),3]
          function(x,i,j,drop=TRUE){stop("second index not implemented for disindex extraction")
          } )

setMethod("[", signature(x="ANY",i="disindex",j="ANY",drop="ANY"),  # stops a[which(a>4),3]
          function(x,i,j,drop=TRUE){stop("disindex objects only extract from disords")
          } )

setReplaceMethod("[",signature(x="disord",i="disindex",j="missing",value="ANY"),  # e.g. d[ind] <- 33
                 function(x,i,j,value){
                   stopifnot(identical(hash(x),hash(i)))
                   if(is.disord(value)){stop("replace methods for disindex do not take disords")}
                   jj <- elements(x)
                   jj[.value(i)] <- value
                   return(disord(jj))
                 } )

setReplaceMethod("[",signature(x="disord",i="disindex",j="ANY",value="ANY"),
                 function(x,i,j,value){stop("second index not implemented for disindex replacement methods")
                 } )

setMethod("[[", signature("disord",i="disindex"),  # x[[ind]]
          function(x,i){
            stopifnot(identical(hash(x),hash(i)))
            elements(x)[[.value(i)]]
          } )

setMethod("[[", signature("ANY",i="disindex"),  # stops x[[ind]]
          function(x,i){stop("disindex only accesses disord lists")
            stopifnot(identical(hash(x),hash(i)))
            elements(x)[[.value(i)]]
          } )

d <- disord(c(4,6,1,2,3,4,5,1))
ind <- which(d>4)

d[ind]  # should work
d[ind] <- 99 # should work

dl <- sapply(d,function(x){seq(from=5,to=x)})

## dl[[ind]] # This would fail, trying to access two elements with double square brackets

indl <- which(unlist(lapply(dl,function(x){length(x) == 3})))

dl[[indl]]  # should work, double square brackets access a single element


indm <- which(unlist(lapply(dl,function(x){length(x) >= 3})))
dl[[indm]] # should fail, error from `[[()` accessing  >1 element


