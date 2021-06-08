`hash` <- function(x){x@h}
`elements` <- function(x){x@v}

setClass("disord",
         slots = c(v = "vector", h = "character"),
         validity = function(object){
             if(!is.vector(elements(object))){
                 return("'v' not a vector")
             } else if(length(hash(object)) != 1){
                 return("hash must be length 1")
             } else {
                 return(TRUE)
             }
         }
         )

`disord` <- function(v,h){ # v is a vector but it needs a hash key 'h'
    if(missing(h)){h <- digest::sha1(v)}
    new("disord",v=v, h=h)
}

`is.disord` <- function(x){inherits(x,"disord")}

`consistent` <- function(x,y){identical(hash(x),hash(y))}
`%~%` <- function(x,y){consistent(x,y)}


`rdisord` <- function(n=20){disord(sample(n))}

setMethod("show", "disord", function(object){disord_show(object)})

`disord_show` <- function(x){
    cat("A disord object with hash",hash(x), "and elements\n")
    print(elements(x))
    cat("(in some order)\n")
    return(invisible(x))
}

setGeneric("length")
setMethod("length","disord",function(x){length(elements(x))})


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
           "^" = numeric_mod_disord(e1,e2),
           stop(paste("binary operator \"", .Generic, "\" not defined for disords"))
           )
}


#setMethod("+", signature(e1 = "disord", e2 = "missing"), function(e1,e2){disord_positive(e1)})
#setMethod("-", signature(e1 = "disord", e2 = "missing"), function(e1,e2){disord_negative(e1)})


`disord_positive` <- function(a){disord(+elements(a),hash(a))}
`disord_negative` <- function(a){disord(-elements(a),hash(a))}
`disord_inverse` <- function(a){disord(1/elements(a),hash(a))}


`disord_unary` <- function(e1,e2){
    switch(.Generic,
           "+" = disord_positive(e1),
           "-" = disord_negative(e1),
           stop(paste("Unary operator \"", .Generic, "\" not defined for disords"))
           )
}

setMethod("Arith",signature(e1 = "disord" , e2="missing"), disord_unary)
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
`disord_power_numeric`<- function(a,b){disord(elements(a)%%b            ,hash(a))}
`numeric_mod_disord`  <- function(a,b){disord(a^elements(b)             ,hash(b))}

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

`any_compare_disord` <- function(e1,e2){ disord_compare_any(e2,e1) }

setMethod("Compare", signature(e1="disord", e2="disord"), disord_compare_disord)
setMethod("Compare", signature(e1="disord", e2="ANY"   ), disord_compare_any   )
setMethod("Compare", signature(e1="ANY"   , e2="disord"), any_compare_disord   )

`disord_logic` <- function(e1,e2){
  stop("No logic currently implemented for disord objects")
}

setMethod("Logic",signature(e1="disord",e2="ANY"), disord_logic)
setMethod("Logic",signature(e1="ANY",e2="disord"), disord_logic)
setMethod("Logic",signature(e1="disord",e2="disord"), disord_logic)


setClassUnion("index", members =  c("numeric", "logical", "character")) # taken from the Matrix package.

setMethod("[", signature("disord",i="index",j="missing",drop="ANY"),
          function(x,i,j,drop){stop("cannot use a regular index to extract, only a disord object")}
          )

setMethod("[", signature("disord",i="disord",j="missing",drop="ANY"),  # makes things like a[a>4] work
          function(x,i,j,drop){
              stopifnot(consistent(x,i))
              out <- elements(x)[elements(i)]
              new("disord",v=out,h=digest::sha1(out))  # NB newly generated hash, stops things like a[a>4] + a[a<3]
          })

setMethod("[", signature("disord",i="index",j="ANY",drop="ANY"),function(x,i,j,drop){stop("cannot have two index args")})

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
                     }
                 )

setReplaceMethod("[",signature(x="disord",i="disord",j="missing",value="ANY"), # x[x<3] <- 333
                 function(x,i,j,value){
                     stopifnot(length(value) == 1)
                     stopifnot(consistent(x,i))
                     jj <- elements(x)
                     jj[elements(i)] <- value   # the meat; OK because x %~% i
                     disord(jj,hash(x))
                     }
                 )

