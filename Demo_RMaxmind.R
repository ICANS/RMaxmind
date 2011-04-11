## Copyright (c) ICANS GmbH and individual contributors.
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without modification,
## are permitted provided that the following conditions are met:
## 
##     1. Redistributions of source code must retain the above copyright notice, 
##        this list of conditions and the following disclaimer.
##     
##     2. Redistributions in binary form must reproduce the above copyright 
##        notice, this list of conditions and the following disclaimer in the
##        documentation and/or other materials provided with the distribution.
## 
##     3. Neither the name of ICANS nor the names of its contributors may be used
##        to endorse or promote products derived from this software without
##        specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
## ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
## ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#########################################################################################################################
##
## Examples for using the RMaxmind plugin in R
##
#########################################################################################################################


## Convert IP address strings to long integer format

ipAddressToIpnum <- function(x) {
	s <- data.frame(do.call(rbind, lapply(strsplit(x, ".", fixed=TRUE), "as.numeric")))
	s <- transform(s, ipnum = 16777216*X1 + 65536*X2 + 256*X3 + X4)
	
	return(s$ipnum)
}
	

## Examples 

ipAddresses <- c("24.24.24.24",	
		 "80.24.24.24")	


## Load RMaxmind plugin

dyn.load("RMaxmind.so")

geoCountryNames <- .Call("fetchMaxmindCountryName", ipAddressToIpnum(ipAddresses))
geoCityData <- .Call("fetchMaxmindGeoDataName", ipAddressToIpnum(ipAddresses))	

## print results
cbind(ipAddresses, geoCountryNames)

geoCityData$ipAddress <- ipAddresses
do.call(cbind, geoCityData)



# calculate great-circle distance on earth surface between two latitute/logitude coordinates 

toRad <- function(x) return(x * pi /180)

haversine <- function(lat1, long1, lat2, long2) {

	dLat  <- toRad(lat2-lat1)
	dLong <- toRad(long2-long1)
	
	a <- sin(dLat/2)*sin(dLat/2) + cos(toRad(lat1))*cos(toRad(lat2))*sin(dLong/2)*sin(dLong/2)
	c <- 2 * atan2(sqrt(a), sqrt(1-a))
	
	d = 6371 * c  # km
	
	return(d)
	
}

haversine(geoCityData$latitude[1], geoCityData$longitude[1], geoCityData$latitude[2], geoCityData$longitude[2])



# generate URL to create a static map using the Google Maps API

googleMapsUrl <- function(geoCityDataset) {
	
	## center map view
	centerLatitude = min(geoCityDataset$latitude) + (max(geoCityDataset$latitude) - min(geoCityDataset$latitude)) / 2				     
	centerLongitude = min(geoCityDataset$longitude)	+ (max(geoCityDataset$longitude) - min(geoCityDataset$longitude)) / 2  
	
	urlHead <- paste("http://maps.google.com/maps/api/staticmap?center=", 
				centerLatitude, ",", 
				centerLongitude,
				"&zoom=1&size=512x512&maptype=roadmap&", sep="")

	## add markers for each position
	geoPositions <- 
		lapply(data.frame(rbind(geoCityDataset$latitude, 
			geoCityDataset$longitude,
			geoCityDataset$city)), 
		function (x) 			
			paste("markers=",  
				## random color
				sprintf("color:0x%06X", round(runif(1)*((2^24)-1))),
				## use first letter of city name as label
				"|label:", substr(x[3], 1, 1),
				## geolocation
				"|", x[1], ",", x[2], sep="")				
		)
	
	geoPositions <- paste(geoPositions, collapse="&")
		
	return(paste(urlHead, geoPositions, "&sensor=false", sep=""))

}


## more IP addresses
ipAddressVector <- c("192.106.51.100", "147.251.48.1",
"134.102.101.18", "193.75.148.28", "194.244.83.2",   
"151.28.39.114", "151.38.70.94", "193.56.4.124", "195.142.146.198",
"139.20.112.104", "139.20.112.3", "145.236.125.211", "149.225.169.61")


## query Maxmind API
geoData <- .Call("fetchMaxmindGeoDataName", ipAddressToIpnum(ipAddressVector))	

## generate URL
googleMapsUrl(geoData)



## Draw geolocated positions on a world map

library(maps)
library(ggplot2)

# read the IP address list included with the Maxmind API and query geolocations
ipAddressTable <- read.table("GeoIP-1.4.6/test/country_test.txt")
geoData <- .Call("fetchMaxmindGeoDataName", ipAddressToIpnum(as.character(ipAddressTable$V1)))

# extract polygon from map data
world.df <- map_data("world")

# create a plot of the world
worldmap <- ggplot(world.df, aes(long, lat)) + geom_polygon(aes(group = group), data = world.df, colour = "grey", fill = NA) 

# add geolocations
worldmap <- worldplot + geom_point(aes(x=geoData$longitude, y=geoData$latitude, colour=geoData$country_name), size=3, alpha=0.5)

# draw the map
print(worldmap)





