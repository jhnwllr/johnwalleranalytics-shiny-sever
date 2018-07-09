

library(plyr)
# roundCoordinates = function(x,roundTo) {
# coord = ifelse(x < 0,plyr::round_any(x, roundTo,f = floor),plyr::round_any(x, roundTo,f = ceiling))
# }


roundCoordinates = function(x,roundTo) {

out = c()
for(i in 1:length(x)) {
occ = x[i]

if(!is.na(occ)) {

negative = occ < 0 
occ = abs(occ)
coord = plyr::round_any(occ, roundTo,f = floor)
if(negative) coord = -1*coord
out[i] = coord

} else { 
out[i] = NA
}
}
return(out)
}

# source("C:/Users/ftw712/Desktop/ebv/brazilEBV/roundCoordinates.r")

# load("C:/Users/ftw712/Desktop/ebv/data/brazilBirdViews/brazilAves.rda")
# str(brazilAves)
# D = brazilAves
# D$decimallatitude

# roundTo = seq(0.5,5,0.5)
# roundTo = roundTo[1]
# length(D$decimallatitude)
# roundCoordinates(x=D$decimallatitude,roundTo)

# D$decimallongitudeRounded = roundCoordinates(D$decimallongitude,roundTo)



