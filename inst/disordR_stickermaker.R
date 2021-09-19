library("hexSticker")
library("lattice")
library("ggplot2")
library("partitions")
library("magrittr")

string <- "disordR"
Aa <- c(LETTERS,letters)
string               %>%
  strsplit("")        %>%
  `[[`(1)              %>%
  match(Aa)             %>%
  multiset               %>%
  `[`(Aa,.)               %>%
matrix(nrow=nchar(string)) %>%
apply(2,paste,collapse="")  -> d

imgurl <- system.file("figures/cat.png", package="hexSticker")
s <- sticker(subplot=imgurl,package="disordR", p_size=24, s_x=1, s_y=.75, s_width=0,h_fill="#7733FF",h_color="#000000",
             filename="hexcat.png")

jjs <- 10

s <- s + geom_pkgname(d[2457],size=jjs, x=0.6,y=1.0)
s <- s + geom_pkgname(d[0135],size=jjs, x=0.6,y=0.8)
s <- s + geom_pkgname(d[0121],size=jjs, x=0.6,y=0.6)
s <- s + geom_pkgname(d[1672],size=jjs, x=0.6,y=0.4)

s <- s + geom_pkgname(d[0087],size=jjs, x=1.4,y=1.0)
s <- s + geom_pkgname(d[1529],size=jjs, x=1.4,y=0.8)
s <- s + geom_pkgname(d[2217],size=jjs, x=1.4,y=0.6)
s <- s + geom_pkgname(d[1156],size=jjs, x=1.4,y=0.4)

save_sticker("disordR.png", s, dpi = 300)
