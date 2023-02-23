library(tidyverse)
library(rebus)

####Functions####
#Function to remove lines with a particular pattern
remove_line <- function(text, pattern){
                  lines <- str_which(text, pattern)
                  return(text[-lines])
                  }

#Function to remove all unwanted lines from zoom transcript
clean_text <- function(script){
##Create patterns for lines to be removed - the script will find lines that match these patterns and delete them
                    #Lines starting with WEBVTT
                    pattern1 <- START %R% "WEBVTT"
                    #Lines that only contain a single number (one or more digits) or timestamp
                    pattern2 <- START %R% one_or_more(DIGIT)
                    #Lines that are spaces
                    pattern3 <- START %R% END
                    
                    cleaned_text <- script %>% 
                                      remove_line(pattern1) %>%
                                      remove_line(pattern2) %>%
                                      remove_line(pattern3)
                    return(cleaned_text)
                    }

#Function to format transcript so a person's section dialogue appears as a single block of text
format_text <- function(text){
                    #set the person who speaks first by splitting the first line up into the person speaking and what they say
                    person <- str_split(text[1], ":")[[1]][1]

                    #variable for cleaned text - the for loop will add each bit of dialogue to this
                    formatted_text <- str_c(person, ":\n")


                    ##For loop to go through each line. 
                    #The loop checks first checks who is the person speaking.
                    #If the person speaking is the same as the previous line, it will combine the dialogue with what this person has already said
                    #If the person is new, it will start a new section of dialogue spoken by this new person
                    for (line in text){
                              #split the line into who's speaking and what they say
                              new_person <- str_split(line, ":")[[1]][1]
                              dialogue <- str_split(line, ":")[[1]][2]
                      
                              #check if the person speaking is same as last line. Paste this line to their previous dialogue if so
                              if(new_person == person){
                                formatted_text <- str_c(formatted_text,
                                                        dialogue, sep = "")
                              } else if(is.na(dialogue)){
                                #Sometimes zoom doesn't include a person, in those situations, assume it's the same person as before
                                formatted_text <- str_c(formatted_text,
                                                        new_person, sep = "")
                              }
                              else{
                                #if new person, start a new section and set this new person as the current speaker
                                formatted_text <- str_c(formatted_text,
                                                        "\n\n",
                                                        new_person,
                                                        ":\n",
                                                        dialogue, sep = "")
                                person <- new_person
                                #close else statement
                                }
                            #close for loop  
                            }
                          return(formatted_text)
                          }

####Read in and format####

filenames <- str_c("zoom_transcripts/", list.files(path="zoom_transcripts"), sep = "")

##For loop to go through all of the files that are in the zoom_transcript folder. 
#The loop first reads a file.
#Custom function is then used to remove all unwanted lines
#Custom function is then used to format the text properly
#The output is then written into a file with the same name as the zoom transcript into the clean folder
for (file in filenames){
          ##create cleaned file
          text <- readLines(file)
          #Remove unneeded lines
          cleaned_text <- clean_text(text)
          ##Improve formatting
          formatted_text <- format_text(cleaned_text)

          ##Create output filename
          output_name_temp <- str_split(file, "/")[[1]][2]
          output_name_temp2 <- str_split(output_name_temp, DOT)[[1]][1]
          output_filename <- str_c("clean_outputs/",
                                   output_name_temp2, 
                                   ".txt", sep = "")

          ##write out the new formatted text to a text file
          fileConn<-file(output_filename)
          writeLines(formatted_text, fileConn)
          close(fileConn)
}

