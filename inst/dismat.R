setClass("dismat",contains = "VIRTUAL", slots = c(x="matrix",hash="character"))

setClass("dismat_ri_cd",
         slots    = c(x="matrix",hash="character"),
         contains = "dismat"
         )

setClass("dismat_rd_ci",
         slots    = c(x="matrix"),
         contains = "dismat"
         )

setClass("dismat_rd_cd",
         slots    = c(x="matrix"),
         contains = "dismat"
         )

setClass("dismat_symm",
         slots    = c(x="matrix"),
         contains = "dismat"
         )

setMethod("apply",signature(X="dismat_rd_ci"),
          function (X, MARGIN, FUN, ..., simplify = TRUE){
              out <- apply(X,MARGIN,FUN,...,simplify=simplify)
              if(MARGIN == 1){
                  return(disord(out,hash=hash(X)))
                  } else {
                      stop("wrong margin")
                  }
          } )

`dismat_ri_cd` <- function(M){new("dismat_ri_cd",x=M,hash=hashcal(M))}


setMethod("[", signature(x="dismat_rd_ci",i="ANY",j="index",drop="ANY"),
          function(x,i,j,drop=TRUE){
              stop("for objects of class dismat_rd_ci, row accessor must be of class disindex and column accessor must be of class index")
          } )

setMethod("[", signature(x="dismat_rd_ci",i="disindex",j="ANY",drop="ANY"),
          function(x,i,j,drop=TRUE){
              stop("for objects of class dismat_rd_ci, row accessor must be of class disindex and column accessor must be of class index")
          } )

setMethod("[", signature(x="dismat_rd_ci",i="disindex",j="index",drop="ANY"),
          function(x,i,j,drop=TRUE){
              ignore <- check_matching_hash(x,j,match.call())
              out <- elements(x)[i, .value(j),drop=drop]
              out <- dismat_ri_cd(out, hash(c(x,i)),drop=FALSE)
              if(drop){
                  return(drop(out))
              } else {
                  return(out)
              }
          } )




