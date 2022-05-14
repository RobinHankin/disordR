test_that("disordR", {
  
  v <- disord(runif(15))
  w <- disord(runif(15))

  expect_silent(elements(v))
  expect_silent(elements(1:3))

  expect_error(new("disord",3:5))

  expect_true(consistent(v,v))
  expect_true(consistent(v,33))
  expect_true(consistent(v,v+4))
  expect_true(consistent(v,v*v))

  expect_false(consistent(v,1:2))
  expect_false(consistent(v,w))
  expect_false(v %~% w)


  expect_silent(+v)
  expect_silent(-v)
  
  expect_silent(v+2)
  expect_silent(v*7)
  expect_silent(v-4)
  expect_silent(v/3)
  expect_silent(v %% 2)
  
  expect_silent(2+v) 
  expect_silent(7*v) 
  expect_silent(4-v) 
  expect_silent(3/v) 
  expect_silent(2^v) 
  
  expect_silent(v+v)
  expect_silent(v*v)
  expect_silent(v/v)
  expect_silent(v^v)

  expect_silent(v >  0.3)
  expect_silent(v >= 0.3)
  expect_silent(v == 0.3)
  expect_silent(v <  0.3)
  expect_silent(v <= 0.3)
  expect_silent(v != 0.3)

  expect_silent(0.3 >  v)
  expect_silent(0.3 >= v)
  expect_silent(0.3 == v)
  expect_silent(0.3 <  v)
  expect_silent(0.3 <= v)
  expect_silent(0.3 != v)

  expect_silent(v >  v)
  expect_silent(v >= v)
  expect_silent(v == v)
  expect_silent(v <  v)
  expect_silent(v <= v)
  expect_silent(v != v)

  expect_silent(v & v)
  expect_silent(v | v)

  expect_silent(v & TRUE)
  expect_silent(v | TRUE)

  expect_silent(TRUE & v)
  expect_silent(TRUE | v)

  expect_error(v[2])
  expect_error(v[2] <- 3)
  expect_error(v[2] <- v[3])

  expect_silent(v[v<0.5])
  expect_silent(v[])

  expect_error(v+w)
  expect_error(v-w)
  expect_error(v+1:2)
  expect_error(v-1:2)

  
  jj <- v
  expect_silent(jj[jj<0.5] <- 0.2)
  expect_silent(jj[] <- 5)

  jj <- v
  expect_silent(jj[jj < 0.4] <-   jj[jj < 0.4]*5)
  
  expect_true (is.disord(disord(1:5) %in% disord(1:3)))
  expect_true (is.disord(disord(1:5) %in%       (1:3)))
  expect_false(is.disord(      (1:5) %in% disord(1:3)))

  expect_error (match(      (1:5), disord(1:3)))
  expect_error (match(disord(1:5), disord(1:3)))
  expect_silent(match(disord(1:5),       (1:3)))

  jj <- disord(1:5)
  expect_silent(is.na(jj[jj>3]) <- TRUE)
  expect_true(sum(is.na(jj))==2)
  expect_output(print(jj))

  expect_true(is.disord(rdis()))

  jj <- disord(1:5)
  expect_true(all(jj-jj==0))
  expect_true(all(jj%%jj==0))

  expect_true(is.disord(4%%jj))

  jj <- disord(1:10) > 4.4
  expect_true(sum(jj) + sum(!jj) == 10)
  
  expect_true(all(diff(sort(rdis())) >= 0))

  jj <- disord(1:10)
  expect_silent(jj[1:10])
  expect_error(jj + jj[1:10])
  expect_error(jj[] <- jj) 
  expect_true(all(jj == rev(rev(jj))))
  expect_error(jj == rev(jj))


  a <- rdis(100)
  b <- a^2 + 0.15

  expect_silent(a[F])
  
  expect_true(all(a <= pmax(a,b)))
  expect_true(all(b <= pmax(a,b)))
  expect_true(all(a >= pmin(a,b)))
  expect_true(all(b >= pmin(a,b)))

  expect_silent(unlist(lapply(disord(sapply(1:9,seq_len)),function(x){max(x)%%2==1})))
  expect_error(unlist(lapply(disord(sapply(1:9,seq_len)),function(x){x[x<5]})))

  expect_error(c(rdis(),rdis()))
  expect_true(is.disord(sapply(disord(1:10),seq_len)))

  expect_true(is.logical(as.logical(a>0.5)))
  expect_true(is.double(as.double(a)))
  expect_true(is.numeric(as.numeric(a)))
  expect_true(is.list(as.list(a)))
  expect_true(is.character(as.character(a)))
  expect_true(is.complex(as.complex(a)))
  expect_warning(as.numeric(1i+a))

  expect_error(new("disord",1:59)) # null hash problem
  expect_error(disord(1:5)[1,2])  # cannot have two index args
  expect_silent(as.logical(disord(c(0,0,1,1,0,1))))
  expect_silent(as(disord(c(0,1)),"numeric"))
  expect_silent(as.numeric(disord(c(T,F))))
  expect_silent(ignore <- summary(rdis()))
  expect_output(print(summary(rdis())))
})

