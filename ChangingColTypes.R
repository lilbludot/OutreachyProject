source("Web100Variables.R")
require("Rmpfr")
df_char <-read.csv("results-20150319-134440.csv", colClasses="character")

working_df <- df_char

col_names <- names(df_char)
for (i in (1:ncol(df_char))){
        long_name <- col_names[i]
        if (substr(long_name,1,22)=="web100_log_entry_snap_"){
            skip <- nchar("web100_log_entry_snap_")+1
            #print(skip)
            long_name_length <- nchar(long_name)
            #print(long_name_length)
            x<- substr(long_name, skip, long_name_length)
            #print(x)
            if ((x %in% web100_df$variableName) & 
                        (web100_df$bigQueryType[which(x == web100_df$variableName)]== "integer")
               ){
                    print(i)
                    print(x)
                    #str(c(mpfr(as.double(working_df[,i]),120)))
                    #working_df[,i] <- c(mpfr(as.double(working_df[,i]),120))
                    working_df[,i]<-as.double(working_df[,i])
            }
            else if ((x %in% web100_df$variableName) & 
                             (web100_df$bigQueryType[which(x == web100_df$variableName)]== "boolean")
            ){
                    working_df[,i]<-as.logical(as.integer((working_df[,i])))  
            }
        
       }
}
