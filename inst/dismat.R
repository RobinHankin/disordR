setClass("dismat",contains = "VIRTUAL", slots = c(x="matrix",hash="character"))
setMethod("elements","dismat",function(x){x@x})  # no occurences of "@" below this line

setMethod("dim","dismat",function(x){dim(elements(x))})

setClass("dismat_rd_ci",
         slots    = c(x="matrix",hash="character"),
         contains = "dismat"
         )

setClass("dismat_ri_cd",
         slots    = c(x="matrix",hash="character"),
         contains = "dismat"
         )

setClass("dismat_rd_cd",
         slots    = c(x="matrix",hash="character"),
         contains = "dismat"
         )

setClass("dismat_symm",
         slots    = c(x="matrix",hash="character"),
         contains = "dismat"
         )

`dismat_rd_ci` <- function(M){new("dismat_rd_ci",x=M,hash=hashcal(M))}
`dismat_ri_cd` <- function(M){new("dismat_ri_cd",x=M,hash=hashcal(M))}

setMethod("apply",signature(X="dismat_rd_ci"),
          function (X, MARGIN, FUN, check=TRUE, ..., simplify = TRUE){
              out <- apply(elements(X),MARGIN,FUN,...,simplify=simplify)
              if(MARGIN == 1){
                  return(disord(out,h=hash(X)))
                  } else {
                      if(check){
                          stop("argument 'check' is TRUE by default.  Set to FALSE if you are _sure_ that FUN is disord-invariant")
                          } else {
                              return(out)
                          }
                  }
          } )

setMethod("[", signature(x="dismat_rd_ci",i="disindex",j="ANY",drop="ANY"),
          function(x,i,j,drop=TRUE){
              ignore <- check_matching_hash(x,i,match.call())
              if(missing(j)){
                  out <- elements(x)[i,,drop=drop]
              } else {
                  out <- elements(x)[i, .value(j),drop=drop]
              }
              out <- dismat_ri_cd(out, hash(c(x,i)),drop=FALSE)
              if(drop){
                  return(drop(out))
              } else {
                  return(out)
              }
          } )

setMethod("[", signature(x="dismat_rd_ci",i="ANY",j="ANY",drop="ANY"),
          function(x,i,j,drop=TRUE){
              stop("for objects of class dismat_rd_ci, row accessor must be of class disindex and column accessor must be of class index")
          } )

