require(dplyr)

# Use getwd() and use your filepath to 01 Data
setwd("~/Desktop/Desktop/UT/Fall 2015/Data Visualization/DV_FinalProject/01 Data/CSVs")

file_path <- "bank-additional.csv"

df <- rename(read.csv(file_path, stringsAsFactors = FALSE, sep = ";"), dflt = default)

# Capitalizing for readability
names(df) <- toupper(names(df))
# Replace "." (i.e., period) with "_" in the column names.
names(df) <- gsub("\\.+", "_", names(df))

#str(df) # Uncomment this and  run just the lines to here to get column types to use for getting the list of measures.
names(df)

# Make dataframe of our measures
measures <- c("AGE", "DURATION", "CAMPAIGN", "PDAYS", "PREVIOUS", "EMP_VAR_RATE", "CONS_PRICE_IDX", "CONS_CONF_IDX", "EURIBOR3M", "NR_EMPLOYED")

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

dimensions <- setdiff(names(df), measures)
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
    # Remove . in dimensions
    df[d] <- data.frame(lapply(df[d], gsub, pattern="\\.+",replacement= ""))
    # Remove - in dimensions
    df[d] <- data.frame(lapply(df[d], gsub, pattern="\\-+",replacement= ""))
  }
}

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
  }
}

write.csv(df, paste(gsub(".csv", "", file_path), ".reformatted.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    sql <- paste(sql, paste(d, "varchar2(4000),\n"))
  }
}
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
    else sql <- paste(sql, paste(m, "number(38,4)\n"))
  }
}
sql <- paste(sql, ");")
cat(sql)
