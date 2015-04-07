#This function takes as input an M-Lab BigQuery  dataframe of completed tests, 
# where each column is of  "character" class. 
#This dataframe must have the following columns:  
#web100_log_entry_log_time, 
#web100_log_entry_snap_MinRTT, 
#connection_spec_client_geolocation_continent_code, 
#connection_spec_client_geolocation_country_name, 
#connection_spec_client_geolocation_city,
#connection_spec_server_geolocation_continent_code,
#connection_spec_server_geolocation_country_name,
#connection_spec_server_geolocation_city

#Return: a dataframe with 7 columns
#Schema: "logTime"              - integer,  UTC time
#        "RTT"                  - integer, roundtrip time
#        "clientContinent"      - character, continent code of the client (optional)
#        "clientCountry"        - character, country of client (optional)
#        "clientCity"           - character, city of client (optional)
#        "serverContinent"      - character, continent code of server (optional)
#        "serverCountry"        - character, country of server (optional)
#        "serverCity"           - character, city of server (optional)
#        "Date"                 - POSIXlt, format "y-m-d h:min:sec"
dataConverter <- function(dfChar){
        library(dplyr)
        
        #selecting the variables that will be needed 
        df <- select(dfChar, web100_log_entry_log_time, 
                     web100_log_entry_snap_MinRTT, 
                     connection_spec_client_geolocation_continent_code, 
                     connection_spec_client_geolocation_country_name, 
                     connection_spec_client_geolocation_city,
                     connection_spec_server_geolocation_continent_code,
                     connection_spec_server_geolocation_country_name,
                     connection_spec_server_geolocation_city)
        
        #converting the UTC time into date 
        df$date <- as.POSIXlt(as.integer(df$web100_log_entry_log_time),
                              origin = "1970-01-01", tz = "GMT")
        
        #renaming the variables, using more manageable names
        names(df)<- c("logTime", "RTT" , "clientContinent", "clientCountry", 
                      "clientCity", "serverContinent", "serverCountry",
                      "serverCity", "date")
        
        #creating a column that has the date in a month day year format
        df$moDayYear <- format(df$date, "%b %d %Y")
#changing the type of logTime from character to integer
df$logTime <- as.integer(df$logTime)
df<- df[order(df$logTime),]
#changing the type of RTT from character to double


df$RTT <- as.double(df$RTT)
#returning the resulting data frame df
return(df)
}