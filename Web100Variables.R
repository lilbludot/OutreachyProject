#Reading in the file downloaded from https://cloud.google.com/bigquery/docs/tcp-kis.txt:
text_file = readLines("tcp-kis.txt")

#finding the variable names within the .txt file: 
var_names <- text_file[grep("VariableName", text_file)]
get_var_name <- function(x){
        skip <- nchar("VariableName:\t")+1
        x_length <- nchar(x)
        var_name <- substr(x, skip, x_length)
        return(var_name)
}
var_names <- unlist(lapply(var_names, get_var_name))



#finding the types of the variables within the .txt file
var_types <- text_file[grep("ProcType:", text_file)]
get_var_type <- function(x){
        skip <- nchar("Proctype:\t")+1
        x_length <- nchar(x)
        var_type <- substr(x, skip, x_length)
        return(var_type)
}
var_types <- unlist(lapply(var_types, get_var_type))


#creating a dataframe containing names and corresponing types of the variables
web100_df <- data.frame(var_names, var_types)

#renaming the columns of the data frame
names(web100_df)<-c("variableName", "variableType")

#finding the BigQuery types of the variables
integer_types <- c("Integer32", "Integer", "INTEGER", "Gauge32", "ZeroBasedCounter32",
                   "Unsigned32", "Unsigned16", "Counter32", "ZeroBasedCounter64")
big_query_typer <- function(x){
        if (x %in% integer_types){
                return("integer")
        }
        else if (x == "Ip_Address"){
                return("string")
        }
        else {return("boolean")}
}

bq_types <- unlist(lapply(web100_df$variableType, big_query_typer))

#adding a third column containing the appropriate BigQuery type of each variable        
web100_df$bigQueryType = bq_types

r_typer <- function(x){
        if (x %in% integer_types){
                return("numeric")
        }
        else if (x == "Ip_Address"){
                return("character")
        }
        else {return("logical")}
}
types_for_R <- unlist(lapply(web100_df$variableType, r_typer))

web100_df$typesForR = types_for_R
#write.csv(var_df, file = "Web100VariablesAndTypes.csv")

