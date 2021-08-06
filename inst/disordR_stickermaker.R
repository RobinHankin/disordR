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
s <- sticker(subplot=imgurl,package="disordR", p_size=6, s_x=1, s_y=.75, s_width=0,h_fill="#7733FF",h_color="#000000",
             filename="hexcat.png")

s <- s + geom_pkgname(d[0135],size=3.5,x=0.6,y=1.15)
s <- s + geom_pkgname(d[2457],size=3.5,x=0.6,y=0.95)
s <- s + geom_pkgname(d[1417],size=3.5,x=0.6,y=0.75)
s <- s + geom_pkgname(d[1672],size=3.5,x=0.6,y=0.55)
s <- s + geom_pkgname(d[0722],size=3.5,x=1.4,y=1.15)
s <- s + geom_pkgname(d[0928],size=3.5,x=1.4,y=0.95)
s <- s + geom_pkgname(d[2217],size=3.5,x=1.4,y=0.75)
s <- s + geom_pkgname(d[1156],size=3.5,x=1.4,y=0.55)

save_sticker("disordR.png", s, dpi = 300)
