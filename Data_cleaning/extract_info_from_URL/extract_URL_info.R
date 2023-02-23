library(tidyverse)
library(rebus)

####Define functions
#Function returns the ID number extracted from the URL
#Inputs are the URL and then a "key" that indicates the information you want, e.g.,

extract_info <- function(URL, key){
                        #Define regex that will be matched
                        #The key is always the ID_type followed by a series of no.s
                        ID_pattern = key %R% one_or_more(DIGIT)
                        extracted_ID <- URL %>%
                                          #Extract a string that includes the ID type
                                          str_match(ID_pattern) %>%
                                          #Remove the ID type to just leave the ID no.
                                          str_replace(key,"")
                        return(extracted_ID)
}

####Read data
URL.df <- read_csv("sample_input.csv")

#Use the extract_Id function to populate a column with the ID
URL.df$Region_ID <- extract_info(URL.df$URL, "regionId=")

####Create output
write_csv(URL.df, "sample_output.csv")
