library(scales)
#library(colorblindcheck)

# updated 1/24/24, the pnk1 was too dark/close to dark blue

# pallet option1
dkbl1 <- "#2d2d8a"
grn1 <- "#00b050"
ylw1 <- "#ffc000"
pnk1 <- "#ff6ada"
#pnk1 <- "#cc0099"
dkpnk1 <- "#990073"
ltbl1 <- "#daedef"

show_col(colours = c(dkbl1, pnk1))
#palette_check(c(dkbl1, pnk1), plot = T)

show_col(colours = c(dkbl1, grn1, ylw1, pnk1, ltbl1, dkpnk1))

# pallet option2
rd2 <- "#ff0000"
ltrd2 <- "#faa0a0"
ylw2 <- "#fdb462"
dkpr2 <- "#2e0e4b"
bl2 <- "#1e90ff"
pr2 <- "#551a8b"

show_col(c(rd2, bl2, pr2, ylw2, dkpr2, ltrd2))

#--choose this i guess
show_col(colours = c(dkbl1, grn1, ylw1, pnk1, ltbl1, dkpnk1))


# labels ------------------------------------------------------------------

myyieldlab <- (expression(atop("Dry grain yield", paste("(Mg "~ha^-1*")"))))

mymeanyieldlab <- (expression(atop("Mean grain yield", paste("(Mg "~ha^-1*")"))))

