`hash` <- function(x){x@hash}
`elements` <- function(x){if(is.disord(x)){return(x@.Data)}else{return(x)}}  # no occurrences of '@' below this line

setClass("disord", contains = "vector", slots=c(.Data="vector",hash="character"),
         validity = function(object){
             if(length(hash(object)) != 1){
                 return("hash should be a character vector of length 1")
             } else if(nchar(object) == 0){
                 return("must have a non-null hash")
             } else {
                 return(TRUE)}
         } )

setValidity("disord", function(object){
             if(length(hash(object))==0){return("must have a non-null hash")}else{return(TRUE)}}
         )

 setMethod("initialize", "disord", 
         function(.Object, ...) {
           .Object <- callNextMethod()
           if(length(hash(.Object))==0){
               stop("initialize() problem, hash is null")
           }
           return(.Object)
         })

`is.disord` <- function(x){inherits(x,"disord")}

`disord` <- function(v,h){ # v is a vector but it needs a hash attribute
    if(missing(h)){h <- digest::sha1(v)}
    new("disord",.Data=v,hash=h)  # this is the only occurence of new() in the package
}

`allsame` <- function(x){length(unique(elements(x)))==1}

`consistent` <- function(x,y){allsame(x) || allsame(y) || identical(hash(x),hash(y))}

`%~%` <- function(x,y){consistent(x,y)}

setGeneric("drop")
setMethod("drop","disord",function(x){if(allsame(x)){return(elements(x))}else{return(x)}})

`rdis` <- function(n=9){disord(runif(n))}

setMethod("show", "disord", function(object){disord_show(object)})

`disord_show` <- function(x){
    cat("A disord object with hash",hash(x), "and elements\n")
    print(elements(x))
    cat("(in some order)\n")
    return(invisible(x))
}



`disord_arith_disord` <- function(e1,e2){
    stopifnot(consistent(e1,e2))
  switch(.Generic,
         "+"  = disord_plus_disord(e1, e2),
         "-"  = disord_plus_disord(e1,disord_negative(e2)),
         "*"  = disord_prod_disord(e1, e2),
         "/"  = disord_prod_disord(e1, disord_inverse(e2)),
         "^"  = disord_power_disord(e1, e2),
         "%%" = disord_mod_disord(e1, e2),
         stop(paste("binary operator \"", .Generic, "\" not defined for disord objects"))
         )
}

`disord_arith_numeric` <- function(e1,e2){  # e1 disord, e2 numeric
    stopifnot(length(e2)==1)
    switch(.Generic,
           "+"  = disord_plus_numeric (e1,  e2),
           "-"  = disord_plus_numeric (e1, -e2),
           "*"  = disord_prod_numeric (e1,  e2),
           "/"  = disord_prod_numeric (e1,1/e2),
           "^"  = disord_power_numeric(e1,  e2),
           "%%" = disord_mod_numeric(e1,  e2),
           stop(paste("binary operator \"", .Generic, "\" not defined for disord objects"))
           )
}

`numeric_arith_disord` <- function(e1,e2){ # e1 numeric, e2 onion
    stopifnot(length(e1)==1)
    switch(.Generic,
           "+" = disord_plus_numeric(e2,  e1),
           "-" = disord_plus_numeric(-e2, e1),
           "*" = disord_prod_numeric(e2,  e1),
           "/" = disord_prod_numeric(disord_inverse(e2),e1),  
           "^" = numeric_power_disord(e1,e2),
           "%%" = numeric_mod_disord(e1,e2),
           stop(paste("binary operator \"", .Generic, "\" not defined for disords"))
           )
}


#setMethod("+", signature(e1 = "disord", e2 = "missing"), function(e1,e2){disord_positive(e1)})
#setMethod("-", signature(e1 = "disord", e2 = "missing"), function(e1,e2){disord_negative(e1)})


`disord_positive` <- function(a){disord(+elements(a),hash(a))}
`disord_negative` <- function(a){disord(-elements(a),hash(a))}
`disord_inverse` <- function(a){disord(1/elements(a),hash(a))}

`disord_arith_unary` <- function(e1,e2){
    switch(.Generic,
           "+" = disord_positive(e1),
           "-" = disord_negative(e1),
           stop(paste("Unary operator \"", .Generic, "\" not defined for disords"))
           )
}

setMethod("Arith",signature(e1 = "disord" , e2="missing"), disord_arith_unary)
setMethod("Arith",signature(e1 = "disord" , e2="disord" ), disord_arith_disord )
setMethod("Arith",signature(e1 = "disord" , e2="numeric"), disord_arith_numeric)
setMethod("Arith",signature(e1 = "numeric", e2="disord" ), numeric_arith_disord)

`disord_plus_disord`  <- function(a,b){disord(elements(a)+elements(b)   ,hash(a))}
`disord_plus_numeric` <- function(a,b){disord(elements(a)+b             ,hash(a))}
`disord_prod_disord`  <- function(a,b){disord(elements(a)*elements(b)   ,hash(a))}
`disord_prod_numeric` <- function(a,b){disord(elements(a)*b             ,hash(a))}
`disord_power_disord` <- function(a,b){disord(elements(a)^elements(b)   ,hash(a))}
`disord_power_numeric`<- function(a,b){disord(elements(a)^b             ,hash(a))}
`numeric_power_disord`<- function(a,b){disord(a^elements(b)             ,hash(b))}
`disord_mod_disord`   <- function(a,b){disord(elements(a) %% elements(b),hash(a))}
`disord_mod_numeric`  <- function(a,b){disord(elements(a) %% b          ,hash(a))}
`numeric_mod_disord`  <- function(a,b){disord(elements(a) %% b          ,hash(a))}
`numeric_mod_disord`  <- function(a,b){disord(a %% elements(b)          ,hash(b))}

`disord_compare_disord` <- function(e1,e2){
    
    stopifnot(consistent(e1,e2))
    a1 <- elements(e1)
    a2 <- elements(e2)
    switch(.Generic,
           "==" = disord(a1==a2,hash(e1)),
           "!=" = disord(a1!=a2,hash(e1)),
           ">"  = disord(a1> a2,hash(e1)),
           "<"  = disord(a1< a2,hash(e1)),
           ">=" = disord(a1>=a2,hash(e1)),
           "<=" = disord(a1<=a2,hash(e1)),
           stop(paste(.Generic, "not supported for disord objects"))
           )
}

`disord_compare_any` <- function(e1,e2){
    stopifnot(length(e2)==1)
    a1 <- elements(e1)
    switch(.Generic,
           "==" = disord(a1==e2,hash(e1)),
           "!=" = disord(a1!=e2,hash(e1)),
           ">"  = disord(a1> e2,hash(e1)),
           "<"  = disord(a1< e2,hash(e1)),
           ">=" = disord(a1>=e2,hash(e1)),
           "<=" = disord(a1<=e2,hash(e1)),
           stop(paste(.Generic, "not supported for disord objects"))
           )
}

`any_compare_disord` <- function(e1,e2){
    stopifnot(length(e1)==1)
    a2 <- elements(e2)
    switch(.Generic,
           "==" = disord(e1==a2,hash(e2)),
           "!=" = disord(e1!=a2,hash(e2)),
           ">"  = disord(e1> a2,hash(e2)),
           "<"  = disord(e1< a2,hash(e2)),
           ">=" = disord(e1>=a2,hash(e2)),
           "<=" = disord(e1<=a2,hash(e2)),
           stop(paste(.Generic, "not supported for disord objects"))
           )
}

setMethod("Compare", signature(e1="disord", e2="disord"), disord_compare_disord)
setMethod("Compare", signature(e1="disord", e2="ANY"   ), disord_compare_any   )
setMethod("Compare", signature(e1="ANY"   , e2="disord"), any_compare_disord   )

`disord_logic_disord` <- function(e1,e2){
    stopifnot(consistent(e1,e2))
    a1 <- elements(e1)
    a2 <- elements(e2)
    switch(.Generic,
           "&" = disord(a1 & a2,hash(e1)),
           "|" = disord(a1 | a2,hash(e1)),
           stop(paste(.Generic, "not supported for disord objects"))
           )
}

`disord_logic_any` <- function(e1,e2){
    stopifnot(length(e2)==1)
    a1 <- elements(e1)
    switch(.Generic,
           "&" = disord(a1 & e2,hash(e1)),
           "|" = disord(a1 | e2,hash(e1)),
           stop(paste(.Generic, "not supported for disord objects"))
           )
}

`any_logic_disord` <- function(e1,e2){
    stopifnot(length(e1)==1)
    a2 <- elements(e2)
    switch(.Generic,
           "&" = disord(e1 & a2,hash(e2)),
           "|" = disord(e1 | a2,hash(e2)),
           stop(paste(.Generic, "not supported for disord objects"))
           )
}

`disord_logical_negate` <- function(a){disord(-a,hash(a))}

`disord_logic_missing` <- function(e1,e2){
    switch(.Generic,
           "!" = disord_logical_negate(e1),
           stop(paste("Unary operator \"", .Generic, "\" not defined for disords"))
           )
}

    
setMethod("Logic",signature(e1="disord",e2="ANY"), disord_logic_any)
setMethod("Logic",signature(e1="ANY",e2="disord"), any_logic_disord)
setMethod("Logic",signature(e1="disord",e2="disord"), disord_logic_disord)
setMethod("Logic",signature(e1="disord",e2="missing"), disord_logic_missing)

setClassUnion("index", members =  c("numeric", "logical", "character")) # taken from the Matrix package.

setMethod("[", signature("disord",i="index",j="missing",drop="ANY"),
          function(x,i,j,drop){
            jj <- seq_along(x)
            if(identical(sort(jj[i]),jj)){  # that is, extract every element
              return(disord(x,digest::sha1(c(hash(x),i))))
            } else {
              stop("if using a regular index to extract, must extract all elements")
            }
          } )

setMethod("[", signature("disord",i="disord",j="missing",drop="ANY"),  # makes things like a[a>4] work
          function(x,i,j,drop=TRUE){
              stopifnot(consistent(x,i))
              out <- elements(x)[elements(i)]
              out <- disord(out, digest::sha1(c(hash(x),hash(i))))  # NB newly generated hash, stops things like a[a>4] + a[a<3] but allows a[x<3] <- x[x<3]
              if(drop){
                  return(drop(out))
              } else {
                  return(out)
              }
          })

setMethod("[", signature("disord",i="index",j="ANY",drop="ANY"),function(x,i,j,drop){stop("cannot have two index args")})

setMethod("[", signature("disord",i="missing",j="missing",drop="ANY"), # x[]
          function(x,i,j,drop){
            out <- disord(x,digest::sha1(0L,c(elements(x))))
            if(drop){out <- drop(out)}
            return(out)
          } )

setReplaceMethod("[",signature(x="disord",i="index",j="missing",value="ANY"),  # a[1:5] <- a[1:5] + 33  = fake
                 function(x,i,j,value){stop("cannot use a regular index to extract, only a disord object")}
                 )

setReplaceMethod("[",signature(x="disord",i="disord",j="missing",value="disord"),  # x[x<3] <- x[x<3] + 100
                 function(x,i,j,value){
                     stopifnot(consistent(x,i))
                     stopifnot(consistent(x[i],value))
                     jj <- elements(x)
                     jj[elements(i)] <- elements(value)  # the meat
                     disord(jj,hash(x))   # needs same hash as x
                 } )

setReplaceMethod("[",signature(x="disord",i="disord",j="missing",value="ANY"), # x[x<3] <- 333
                 function(x,i,j,value){
                     stopifnot(length(value) == 1)
                     stopifnot(consistent(x,i))
                     jj <- elements(x)
                     jj[elements(i)] <- value   # the meat; OK because x %~% i
                     disord(jj,hash(x))
                 } )

setReplaceMethod("[",signature(x="disord",i="missing",j="missing",value="ANY"), # x[] <- numeric
                 function(x,i,j,value,drop=TRUE){
                   stopifnot(length(value)==1)
                   out <- elements(x)
                   out[] <- value   # the meat
                   out <- disord(out)
                   if(drop)(out <- drop(out))
                   return(out)
                 } )
                   

setReplaceMethod("[",signature(x="disord",i="missing",j="missing",value="disord"), # x[] <- disord
                 function(x,i,j,value){stop("x[] <- disord not defined")
                 } )

setGeneric("sort")
setMethod("sort", signature(x = "disord"),
          function (x, decreasing = FALSE, ...){sort(elements(x),decreasing=decreasing, ...)
          } )

setGeneric("rev")
setMethod("rev",signature=c(x="disord"),
          function(x){
            disord(rev(elements(x)),h=paste(rev(strsplit(hash(x), "")[[1]]), collapse = ""))
          } )

setGeneric("pmindispair",function(x,y, na.rm=FALSE){standardGeneric("pmindispair")})
setMethod("pmindispair",c("disord","disord"),
          function(x,y,na.rm=FALSE){
            stopifnot(consistent(x,y))
            disord(pmin(elements(x),elements(y),na.rm=na.rm),hash(x))
          } )

setMethod("pmindispair",c("disord","ANY"),
          function(x,y,na.rm=FALSE){
            stopifnot(consistent(x,y))
            disord(pmin(elements(x),elements(y),na.rm=na.rm),hash(x))
          } ) 

setMethod("pmindispair",c("ANY","disord"),
          function(x,y,na.rm=FALSE){
            stopifnot(consistent(x,y))
            disord(pmin(elements(x),elements(y),na.rm=na.rm),hash(y))
          } ) 

setGeneric("pmindis",function(x, ..., na.rm=FALSE){standardGeneric("pmindis")})
setMethod("pmindis", signature(x="disord"),
          function(x, ..., na.rm=FALSE){
              a <- list(...)
              if(length(a)==0){  #pmindis(a)
                  return(x)
              } else if(length(a)==1){  # pmindis(a,b)
                  return(pmindispair(x,a[[1]],na.rm=na.rm))
              } else {
                  return(do.call("pmindis",c(list(pmindispair(x,a[[1]],na.rm=na.rm)),a[-1],na.rm=na.rm)))
              }
          } )

setGeneric("pmaxdispair",function(x,y, na.rm=FALSE){standardGeneric("pmaxdispair")})
setMethod("pmaxdispair",c("disord","disord"),
          function(x,y,na.rm=FALSE){
            stopifnot(consistent(x,y))
            disord(pmax(elements(x),elements(y),na.rm=na.rm),hash(x))
          } )

setMethod("pmaxdispair",c("disord","ANY"),
          function(x,y,na.rm=FALSE){
            stopifnot(consistent(x,y))
            disord(pmax(elements(x),elements(y),na.rm=na.rm),hash(x))
          } ) 

setMethod("pmaxdispair",c("ANY","disord"),
          function(x,y,na.rm=FALSE){
            stopifnot(consistent(x,y))
            disord(pmax(elements(x),elements(y),na.rm=na.rm),hash(y))
          } ) 

setGeneric("pmaxdis",function(x, ..., na.rm=FALSE){standardGeneric("pmaxdis")})
setMethod("pmaxdis", signature(x="disord"),
          function(x, ..., na.rm=FALSE){
            a <- list(...)
            if(length(a)==0){  #pmaxdis(a)
              return(x)
            } else if(length(a)==1){  # pmaxdis(a,b)
              return(pmaxdispair(x,a[[1]],na.rm=na.rm))
            } else {
              return(do.call("pmaxdis",c(list(pmaxdispair(x,a[[1]],na.rm=na.rm)),a[-1],na.rm=na.rm)))
            }
          } )

setMethod("sapply",signature(X="disord"),
          function(X,FUN,...,simplify=TRUE,USE.NAMES=TRUE){
            disord(sapply(elements(X),FUN,...,simplify=simplify,USE.NAMES=USE.NAMES),h=hash(X))
          } )

setMethod("c","disord",function(x, ..., recursive){stop("c() does not make sense for disord")})

