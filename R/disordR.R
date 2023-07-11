`hash` <- function(x){x@hash}  # extractor method
`hashcal` <- function(x,ultra_strict=FALSE){
    if(ultra_strict){
        return(digest::sha1(list(x,date(),runif(1))))
    } else {
        return(digest::sha1(x))
    }
}

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

`disord` <- function(v,h,drop=TRUE){ # v is a vector but it needs a hash attribute
    if(is.disord(v)){v <- elements(v)}
    if(missing(h)){h <- hashcal(v,ultra_strict=TRUE)}
    out <- new("disord",.Data=v,hash=h)  # this is the only occurence of new() in the package
    if(drop){out <- drop(out)}
    return(out)
}

`allsame` <- function(x){length(unique(elements(x)))<=1}

`consistent` <- function(x,y){
  if(allsame(x) || allsame(y)){return(TRUE)}
  if(is.disord(x) && is.disord(y)){
    return(identical(hash(x),hash(y)))
  } else {
    return(FALSE)
  }
}

`%~%` <- function(x,y){consistent(x,y)}

setGeneric("match")
setMethod("match",signature(x="disord",table="ANY"),
          function(x,table, nomatch,incomparables){
            disord(match(elements(x),elements(table),nomatch,incomparables),hash(x))
          } )

setMethod("match",signature(x="ANY",table="disord"),
          function(x,table, nomatch,incomparables){
            stop("match() not defined if second argument a disord")
          } )
setMethod("match",signature(x="disord",table="disord"),
          function(x,table, nomatch,incomparables){
            stop("match() not defined if second argument a disord")
          } )

setGeneric("%in%")
setMethod("%in%",signature("disord","ANY"),function(x,table){disord(match(elements(x),table,nomatch=0L)>0L,hash(x),drop=FALSE)})
setMethod("%in%",signature("ANY","disord"),function(x,table){match(x,elements(table),nomatch=0L)>0L})
setMethod("%in%",signature("disord","disord"),function(x,table){disord(match(elements(x),elements(table),nomatch=0L)>0L,hash(x),drop=FALSE)})

setGeneric("drop")
setMethod("drop","disord",function(x){if(allsame(x)){return(elements(x))}else{return(x)}})

setGeneric("is.na")
setMethod("is.na","disord",
          function(x){
              disord(is.na(elements(x)),hash(x))
          } )

setGeneric("is.na<-")
setMethod("is.na<-","disord",
          function(x,value){
              ignore <- check_matching_hash(x,value,match.call())
              jj <- elements(x)
              is.na(jj) <- value
              disord(jj,hash(x))
          } )

`rdis` <- function(n=9){disord(sample(n,replace=TRUE))}

setMethod("show", "disord", function(object){disord_show(object)})

`disord_show` <- function(x){
    cat("A disord object with hash",hash(x), "and elements\n")
    print(elements(x))
    cat("(in some order)\n")
    return(invisible(x))
}

`disord_arith_disord` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
  switch(.Generic,
         "+"  = disord_plus_disord(e1, e2),
         "-"  = disord_plus_disord(e1,disord_negative(e2)),
         "*"  = disord_prod_disord(e1, e2),
         "/"  = disord_prod_disord(e1, disord_inverse(e2)),
         "^"  = disord_power_disord(e1, e2),
         "%%" = disord_mod_disord(e1, e2),
         stop(gettextf("binary operator %s not defined for disord objects", dQuote(.Generic)))
         )
}

`disord_arith_numeric` <- function(e1,e2){  # e1 disord, e2 numeric
    ignore <- check_matching_hash(e1,e2,match.call())
    switch(.Generic,
           "+"  = disord_plus_numeric (e1,  e2),
           "-"  = disord_plus_numeric (e1, -e2),
           "*"  = disord_prod_numeric (e1,  e2),
           "/"  = disord_prod_numeric (e1,1/e2),
           "^"  = disord_power_numeric(e1,  e2),
           "%%" = disord_mod_numeric(e1,  e2),
           stop(gettextf("binary operator %s not defined for disord objects", dQuote(.Generic)))
           )
}

`numeric_arith_disord` <- function(e1,e2){ # e1 numeric, e2 onion
    ignore <- check_matching_hash(e1,e2,match.call())
    switch(.Generic,
           "+" = disord_plus_numeric(e2,  e1),
           "-" = disord_plus_numeric(-e2, e1),
           "*" = disord_prod_numeric(e2,  e1),
           "/" = disord_prod_numeric(disord_inverse(e2),e1),  
           "^" = numeric_power_disord(e1,e2),
           "%%" = numeric_mod_disord(e1,e2),
           stop(gettextf("binary operator %s not defined for disord objects", dQuote(.Generic)))
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
           stop(gettextf("unary operator %s not defined for disord objects", dQuote(.Generic)))
           )
}

setMethod("Arith",signature(e1 = "disord" , e2="missing"), disord_arith_unary)
setMethod("Arith",signature(e1 = "disord" , e2="disord" ), disord_arith_disord )
setMethod("Arith",signature(e1 = "disord" , e2="numeric"), disord_arith_numeric)
setMethod("Arith",signature(e1 = "numeric", e2="disord" ), numeric_arith_disord)

`disord_plus_disord`  <- function(a,b){disord(elements(a)+elements(b)   ,hash(a))}
`disord_plus_numeric` <- function(a,b){disord(elements(a)+b            ,hash(a))}
`disord_prod_disord`  <- function(a,b){disord(elements(a)*elements(b)   ,hash(a))}
`disord_prod_numeric` <- function(a,b){disord(elements(a)*b            ,hash(a))}
`disord_power_disord` <- function(a,b){disord(elements(a)^elements(b)   ,hash(a))}
`disord_power_numeric`<- function(a,b){disord(elements(a)^b            ,hash(a))}
`numeric_power_disord`<- function(a,b){disord(a^elements(b)            ,hash(b))}
`disord_mod_disord`   <- function(a,b){disord(elements(a) %% elements(b),hash(a))}
`disord_mod_numeric`  <- function(a,b){disord(elements(a) %% b         ,hash(a))}
`numeric_mod_disord`  <- function(a,b){disord(elements(a) %% b         ,hash(a))}
`numeric_mod_disord`  <- function(a,b){disord(a %% elements(b)         ,hash(b))}

`disord_compare_disord` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
    a1 <- elements(e1)
    a2 <- elements(e2)
    switch(.Generic,
           "==" = disord(a1==a2,hash(e1)),
           "!=" = disord(a1!=a2,hash(e1)),
           ">"  = disord(a1> a2,hash(e1)),
           "<"  = disord(a1< a2,hash(e1)),
           ">=" = disord(a1>=a2,hash(e1)),
           "<=" = disord(a1<=a2,hash(e1)),
           stop(gettextf("%s not supported for disord objects", dQuote(.Generic)))
           )
}

`disord_compare_any` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
    a1 <- elements(e1)
    switch(.Generic,
           "==" = disord(a1==e2,hash(e1),drop=FALSE),
           "!=" = disord(a1!=e2,hash(e1),drop=FALSE),
           ">"  = disord(a1> e2,hash(e1),drop=FALSE),
           "<"  = disord(a1< e2,hash(e1),drop=FALSE),
           ">=" = disord(a1>=e2,hash(e1),drop=FALSE),
           "<=" = disord(a1<=e2,hash(e1),drop=FALSE),
           stop(gettextf("%s not supported for disord objects", dQuote(.Generic)))
           )
}

`any_compare_disord` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
    a2 <- elements(e2)
    switch(.Generic,
           "==" = disord(e1==a2,hash(e2),drop=FALSE),
           "!=" = disord(e1!=a2,hash(e2),drop=FALSE),
           ">"  = disord(e1> a2,hash(e2),drop=FALSE),
           "<"  = disord(e1< a2,hash(e2),drop=FALSE),
           ">=" = disord(e1>=a2,hash(e2),drop=FALSE),
           "<=" = disord(e1<=a2,hash(e2),drop=FALSE),
           stop(gettextf("%s not supported for disord objects", dQuote(.Generic)))
           )
}

setMethod("Compare", signature(e1="disord", e2="disord"), disord_compare_disord)
setMethod("Compare", signature(e1="disord", e2="ANY"   ), disord_compare_any   )
setMethod("Compare", signature(e1="ANY"   , e2="disord"), any_compare_disord   )

`disord_logic_disord` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
    a1 <- elements(e1)
    a2 <- elements(e2)
    switch(.Generic,
           "&" = disord(a1 & a2,hash(e1),drop=FALSE),
           "|" = disord(a1 | a2,hash(e1),drop=FALSE),
           stop(gettextf("%s not supported for disord objects", dQuote(.Generic)))
           )
}

`disord_logic_any` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
    a1 <- elements(e1)
    switch(.Generic,
           "&" = disord(a1 & e2,hash(e1),drop=FALSE),
           "|" = disord(a1 | e2,hash(e1),drop=FALSE),
           stop(gettextf("%s not supported for disord objects", dQuote(.Generic)))
           )
}

`any_logic_disord` <- function(e1,e2){
    ignore <- check_matching_hash(e1,e2,match.call())
    a2 <- elements(e2)
    switch(.Generic,
           "&" = disord(e1 & a2,hash(e2),drop=FALSE),
           "|" = disord(e1 | a2,hash(e2),drop=FALSE),
           stop(gettextf("%s not supported for disord objects", dQuote(.Generic)))
           )
}

`disord_logical_negate` <- function(x){
  disord(!elements(x),hash(x))
}
setMethod("!","disord",disord_logical_negate)

setMethod("Logic",signature(e1="disord",e2="ANY"), disord_logic_any)
setMethod("Logic",signature(e1="ANY",e2="disord"), any_logic_disord)
setMethod("Logic",signature(e1="disord",e2="disord"), disord_logic_disord)

setMethod("[", signature("disord",i="index",j="missing",drop="ANY"),
          function(x,i,j,drop){
            jj <- seq_along(x)
            jji <- jj[i]
            if(identical(sort(jji),jj)){  # that is, extract every element
              return(disord(x,hashcal(c(hash(x),i)))) # NB new hash code
            } else if(length(jji)==0){
                return(disord(jji,hash(x)))         # NB same hash code as x
            } else {
              stop("if using a regular index to extract, must extract each element once and once only (or none of them)")
            }
          } )

setMethod("[", signature("disord",i="disord",j="missing",drop="ANY"),  # makes things like a[a>4] work
          function(x,i,j,drop=TRUE){
              ignore <- check_matching_hash(x,i,match.call())
              out <- elements(x)[elements(i)]
              out <- disord(out, hashcal(c(hash(x),i)),drop=FALSE)  # NB newly generated hash, stops things like a[a>4]+ a[a<3] but allows a[x<3] <- x[x<3]
              if(drop){
                  return(drop(out))
              } else {
                  return(out)
              }
          })

setMethod("[", signature("disord",i="index",j="ANY",drop="ANY"),function(x,i,j,drop){stop("cannot have two index args")})

setMethod("[", signature("disord",i="missing",j="missing",drop="ANY"), # x[]
          function(x,i,j,drop){
            out <- disord(x,hashcal(c(hash(x),0)))
            if(drop){out <- drop(out)}
            return(out)
          } )

setReplaceMethod("[",signature(x="disord",i="index",j="missing",value="ANY"),  
                 function(x,i,j,value){
                     if(allsame(i) & is.logical(i)){
                         jj <- elements(x)
                         jj[i] <- elements(value)
                         return(disord(jj,hash(x)))
                     } else {
                         stop("if using a regular index to replace, must specify each element once and once only")
                     }
                 } )

setReplaceMethod("[",signature(x="disord",i="disord",j="missing",value="disord"),  # x[x<3] <- x[x<3] + 100
                 function(x,i,j,value){
                     ignore <- check_matching_hash(x,i,match.call())
                     ignore <- check_matching_hash(x[i],value,match.call())
                     jj <- elements(x)
                     jj[elements(i)] <- elements(value)  # the meat
                     disord(jj,hash(x))   # needs same hash as x
                 } )

setReplaceMethod("[",signature(x="disord",i="disord",j="missing",value="ANY"), # x[x<3] <- 333
                 function(x,i,j,value){
                     ignore <- check_matching_hash(x,i,match.call())
                     ignore <- check_matching_hash(x[i],value,match.call())
                     if((length(value)>1) & (!allsame(value)) & (is.disord(x[i,drop=FALSE]))){stop("disord discipline problem")}
                     jj <- elements(x)
                     jj[elements(i)] <- value   # the meat; OK because x %~% i
                     disord(jj,hash(x))
                 } )

setReplaceMethod("[",signature(x="disord",i="missing",j="missing",value="ANY"), # x[] <- numeric
                 function(x,i,j,value){
                   ignore <- check_matching_hash(x,value,match.call())
                   out <- elements(x)
                   out[] <- value   # the meat
                   out <- disord(out,hash(x))
                   return(out)
                 } )
 
setReplaceMethod("[",signature(x="disord",i="missing",j="missing",value="disord"), # x[] <- disord
                 function(x,i,j,value){stop("x[] <- disord not defined")
                 } )

setMethod("[[", signature("disord",i="index"),  # x[[index]]
          function(x,i){
            stop("double square extraction x[[index]] not implemented")
          } )

setReplaceMethod("[[",signature(x="disord",i="index",value="ANY"), function(x,i,j){stop("list replacement not currently implemented")})

setGeneric("sort")
setMethod("sort", signature(x = "disord"),
          function (x, decreasing = FALSE, ...){sort(elements(x),decreasing=decreasing, ...)
          } )

setGeneric("rev")
setMethod("rev",signature=c(x="disord"),
          function(x){
            disord(rev(elements(x)),h=paste(rev(strsplit(hash(x), "")[[1]]), collapse = ""))
          } )

setMethod("sapply",signature(X="disord"),
          function(X,FUN,...,simplify=TRUE,USE.NAMES=TRUE){
            disord(sapply(elements(X),FUN,...,simplify=simplify,USE.NAMES=USE.NAMES),h=hash(X))
          } )

setGeneric("lapply")
setMethod("lapply",signature(X="disord"),
          function(X,FUN,...){
            disord(lapply(elements(X),FUN,...),h=hash(X))
          } )

setGeneric("unlist")
setMethod("unlist","disord",
          function(x,recursive=TRUE){
            stopifnot(recursive)
            out <- unlist(elements(x),recursive=recursive)
            if(length(out) == length(x)){
              return(disord(out,h=hash(x)))
            } else {
              return(disord(out))
            }
          } )

setMethod("c","disord",function(x, ..., recursive){stop("c() does not make sense for disord")})

setAs("disord","logical"  ,function(from){disord(as.logical  (elements(from)),hash(from))})
setAs("disord","numeric"  ,function(from){disord(as.numeric  (elements(from)),hash(from))})
setAs("disord","double"   ,function(from){disord(as.double   (elements(from)),hash(from))})
setAs("disord","list"     ,function(from){disord(as.list     (elements(from)),hash(from))})
setAs("disord","character",function(from){disord(as.character(elements(from)),hash(from))})
setAs("disord","complex"  ,function(from){disord(as.complex  (elements(from)),hash(from))})

setMethod("as.logical"  ,"disord",function(x){as(x,"logical"  )})
setMethod("as.numeric"  ,"disord",function(x){as(x,"numeric"  )})
setMethod("as.double"   ,"disord",function(x){as(x,"double"   )})
setMethod("as.list"     ,"disord",function(x){as(x,"list"     )})
setMethod("as.character","disord",function(x){as(x,"character")})
setMethod("as.complex"  ,"disord",function(x){as(x,"complex"  )})

setGeneric("paste")
setMethod("match",signature(x="disord",table="ANY"),
          function(x,table, nomatch,incomparables){
            disord(match(elements(x),elements(table),nomatch,incomparables),hash(x))
          } )

`summary.disord` <- function(object, ...){
  out <- list(
      hash    = hash(object),
      summary = summary(elements(object))
  )
  class(out) <- "summary.disord"
  return(out)
}

"print.summary.disord" <- function(x, ...){
  cat("a disord object with hash ")  
  cat(x[[1]],"\n\n")
  print(x[[2]])
}
  
`check_matching_hash` <- function(e1,e2,use=NULL){
  if(consistent(e1,e2)){
    return(TRUE)
  } else {
    message("\ndisordR discipline error in:\n")
    print(use)
    if(is.disord(e1) & is.disord(e2)){
      m <- gettextf("\nhash codes %s and %s do not match",hash(e1),hash(e2))
    } else if( is.disord(e1) & !is.disord(e2)){
      m <- gettextf("\ncannot combine disord object with hash code %s with a vector",hash(e1))
    } else if(!is.disord(e1) &  is.disord(e2)){
      m <- gettextf("\ncannot combine disord object with hash code %s with a vector",hash(e2))
    } else {
      m <- gettextf("\nfunction check_matching_hash() called with two non-disords?")
    }
    stop(m)
  }
}

setMethod("length<-","disord",function(x,value){stop("cannot change the length of a disord object")})
setMethod("diff","disord",function(x){stop("cannot take the diff() of a disord object")})
