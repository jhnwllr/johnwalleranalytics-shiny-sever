

library(plyr)
roundCoordinates = function(x,roundTo) {
coord = ifelse(x < 0,plyr::round_any(x, roundTo,f = floor),plyr::round_any(x, roundTo,f = ceiling))
}



