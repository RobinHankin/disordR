setClass("disindex",slots = list(value = "numeric", hash="character"))
setMethod("show", "disindex", function(object){disindex_show(object)})

`values` <- function(x){  disord(x@value,h=hash(x)) }  # exported in versions >= 0.9-3

setValidity("disindex", function(object) {
  jj <- values(object)
  if(!is.numeric(jj)){
    return("not numeric")
  } else if(any(jj <= 0)){
    return("must be >0")
  } else {
    return(TRUE)
  }
})

`disindex_show` <- function(x){
    cat("A disind object with hash",hash(x), "and", length(values(x)), "(implementation-specific) elements\n")
    return(invisible(x))
}

setGeneric("summary")
setMethod("summary","disindex",function(object,...){stop("disindex objects are for extraction only")})

setGeneric("which")
setMethod("which","disord",function(x, arr.ind = FALSE, useNames = TRUE){new("disindex",value=which(elements(x)),hash=hash(x))})
setMethod("which","disindex",function(x, arr.ind = FALSE, useNames = TRUE){stop("which() not defined on disindex objects")})


setGeneric("length")
setMethod("length","disindex",function(x){length(values(x))})

setMethod("[", signature(x="disord",i="disindex",j="missing",drop="ANY"),  # makes things like a[which(a>4)] work
          function(x,i,j,drop=TRUE){
            stopifnot(identical(hash(x),hash(i)))
            out <- (elements(x))[values(i)]
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

setMethod("[", signature(x="ANY",i="disindex",j="ANY",drop="ANY"),  # stops which(a>4)[which(a>4)]
          function(x,i,j,drop=TRUE){stop("disindex objects only extract from disords")
          } )

setReplaceMethod("[",signature(x="disord",i="disindex",j="missing",value="ANY"),  # e.g. d[ind] <- 33
                 function(x,i,j,value){
                   stopifnot(identical(hash(x),hash(i)))
                   if(is.disord(value)){stop("replace methods for disindex do not take disords")}
                   jj <- elements(x)
                   jj[values(i)] <- value
                   return(disord(jj))
                 } )

setReplaceMethod("[",signature(x="disord",i="disindex",j="ANY",value="ANY"),
                 function(x,i,j,value){stop("second index not implemented for disindex replacement methods")
                 } )

setMethod("[[", signature("disord",i="disindex"),  # x[[ind]]
          function(x,i){
            stopifnot(identical(hash(x),hash(i)))
            stopifnot(length(i) == 1)
            elements(x)[[values(i)]]
          } )

setMethod("[[", signature("ANY",i="disindex"),  # stops x[[ind]]
          function(x,i){stop("disindex only accesses disord lists")
          } )


setReplaceMethod("[[",signature(x="disord",i="disindex",j="missing",value="ANY"),  # e.g. d[[ind]] <- 33
                 function(x,i,j,value){
                   stopifnot(identical(hash(x),hash(i)))
                   if(is.disord(value)){stop("replace methods for disindex do not take disords")}
                   if(length(i) !=  1){stop("double square bracket replacement methods x[[i]] <- value with i a disindex object require length(i)==1")}
                   jj <- elements(x)
                   jj[[values(i)]] <- value
                   return(disord(jj))  # NB hash changed!
                 } )

setReplaceMethod("[[",signature(x="ANY",i="disindex",j="ANY",value="ANY"),  # e.g. d[ind] <- 33
                 function(x,i,j,value){stop("replacement method not meaningful in this context")})


