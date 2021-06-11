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
                   
setGeneric("as.character")
setMethod("as.character",signature=c(x="disord"),function(x){disord(as.character(elements(x)),h=hash(x))})
  
                   
