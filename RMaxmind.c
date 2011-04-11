/*
Copyright (c) ICANS GmbH and individual contributors.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, 
       this list of conditions and the following disclaimer.
    
    2. Redistributions in binary form must reproduce the above copyright 
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of ICANS nor the names of its contributors may be used
       to endorse or promote products derived from this software without
       specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <R.h>
#include <Rdefines.h>
#include "GeoIP-1.4.6/libGeoIP/GeoIP.h"
#include "GeoIP-1.4.6/libGeoIP/GeoIPCity.h"

static const char * _mk_NA( const char * p ){
 return p ? p : "N/A";
}


/* returns character vectors of geolocated countries based on IP addresses
in long integer format */

SEXP fetchMaxmindCountryName(SEXP ipVect) {
	
	unsigned int vectLen;
	vectLen = length(ipVect);
	
	SEXP countryStrs;
	PROTECT(countryStrs = allocVector(STRSXP, vectLen));
		
	/* Lookup Maxmind GeoIP  */
	const char *str = NULL;
	GeoIP *gi = NULL;
		
	/* change the path of the GeoIP.dat file, if necessary */
	gi = GeoIP_open("GeoIP.dat", 0);
	
	if (gi == NULL) {
		Rprintf("RMaxmind GeoIP Country - ERROR: Could not open GeoIP.dat!\n");
	} else {
		
		for (unsigned int i = 0; i < vectLen; i++) {
			str = GeoIP_country_name_by_ipnum(gi, (unsigned long)REAL(ipVect)[i]);
			
			SET_STRING_ELT(countryStrs, i, mkChar(_mk_NA(str)));	
		}		
	}
	
	GeoIP_delete(gi);
			
	UNPROTECT(1);
		
	return countryStrs;
}



		
/* returns a list of vectors with geolocated informations (name of country, 
country code, region, city, timezone, latitude, and longitude) based on 
IP addresses in long integer format. */

SEXP fetchMaxmindGeoDataName(SEXP ipVect) {
	
	const char *time_zone = NULL;
	unsigned int vectLen;
	vectLen = length(ipVect);
	
	char *names[7] = {"country_name", "country_short", "region", "city", "timezone", "latitude", "longitude"};
	SEXP list, list_names;
	
	double *p_latitude, *p_longitude;
		
	SEXP latitudeVect;
	SEXP longitudeVect;
	SEXP countryStrs;
	SEXP countryCodes;
	SEXP regionStrs;
	SEXP cityStrs;
	SEXP timeZoneStrs;
	
	PROTECT(countryStrs = allocVector(STRSXP, vectLen));
	PROTECT(countryCodes = allocVector(STRSXP, vectLen));
	PROTECT(regionStrs = allocVector(STRSXP, vectLen));
	PROTECT(cityStrs = allocVector(STRSXP, vectLen));
	PROTECT(timeZoneStrs = allocVector(STRSXP, vectLen));
	PROTECT(latitudeVect = NEW_NUMERIC(vectLen));
	PROTECT(longitudeVect = NEW_NUMERIC(vectLen));
	
	p_latitude = NUMERIC_POINTER(latitudeVect);
	p_longitude = NUMERIC_POINTER(longitudeVect);
	
	
	/* set list names */
	PROTECT(list_names = allocVector(STRSXP,7));
	for (unsigned int i = 0; i < 7; i++) 
		SET_STRING_ELT(list_names, i, mkChar(names[i]));
			
	/* create list*/
	PROTECT(list = allocVector(VECSXP, 7));
		
			
	/* Lookup Maxmind GeoIP */
	GeoIPRecord *gir = NULL;
	GeoIP *gi = NULL;
		
	gi = GeoIP_open("GeoIPCity.dat", 0);
	
	if (gi == NULL) {
		Rprintf("RMaxmind GeoIP Data - ERROR: Could not open GeoIPCity.dat!\n");
	} else {
		
		for (unsigned int i = 0; i < vectLen; i++) {
			gir = GeoIP_record_by_ipnum(gi, (unsigned long)REAL(ipVect)[i]);
		
			if (gir != NULL) {
				time_zone = GeoIP_time_zone_by_country_and_region(gir->country_code, gir->region);
							
				SET_STRING_ELT(countryStrs, i, mkChar(_mk_NA(gir->country_name)));	
				SET_STRING_ELT(countryCodes, i, mkChar(_mk_NA(gir->country_code)));	
				SET_STRING_ELT(regionStrs, i, mkChar(_mk_NA(GeoIP_region_name_by_code(gir->country_code, gir->region))));	
				SET_STRING_ELT(cityStrs, i, mkChar(_mk_NA(gir->city)));	
				SET_STRING_ELT(timeZoneStrs, i, mkChar(_mk_NA(time_zone)));	
				
				p_latitude[i]  = gir->latitude;
				p_longitude[i] = gir->longitude;
			
				GeoIPRecord_delete(gir);
			} else {
				SET_STRING_ELT(countryStrs, i, mkChar(_mk_NA(NULL)));	
				SET_STRING_ELT(countryCodes, i, mkChar(_mk_NA(NULL)));	
				SET_STRING_ELT(regionStrs, i, mkChar(_mk_NA(NULL)));	
				SET_STRING_ELT(cityStrs, i, mkChar(_mk_NA(NULL)));	
				SET_STRING_ELT(timeZoneStrs, i, mkChar(_mk_NA(NULL)));				
			}
		}	

		
		/* attach vectors to list */
		SET_VECTOR_ELT(list, 0, countryStrs);
		SET_VECTOR_ELT(list, 1, countryCodes);
		SET_VECTOR_ELT(list, 2, regionStrs);
		SET_VECTOR_ELT(list, 3, cityStrs);
		SET_VECTOR_ELT(list, 4, timeZoneStrs);
		SET_VECTOR_ELT(list, 5, latitudeVect);
		SET_VECTOR_ELT(list, 6, longitudeVect);
				
		
		/* attach names to list */
		setAttrib(list, R_NamesSymbol, list_names);
					
	}
	
	GeoIP_delete(gi);
		
	UNPROTECT(9);
		
	return list;
}


