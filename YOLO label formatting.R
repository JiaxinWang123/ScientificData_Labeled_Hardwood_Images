
Label_path <- "./Label_path/"  #Set Label files path

setwd(Label_path)  # Set current working directory

fnames <- list.files(pattern = "*.txt", path = Label_path)  # List all label files

library(dplyr)

for (files in fnames){
  a <- read.csv(files, sep=" ", header = F)
  a <- as.data.frame(a)  # Convert table as a data frame
  a <- a[,c(1,3:6)]  # Drop the confidence column
  a$V2 <- format(round(a$V2,6), nsmall = 6)  # Format the coordinates, width, and height
  a$V3 <- format(round(a$V3,6), nsmall = 6)
  a$V4 <- format(round(a$V4,6), nsmall = 6)
  a$V5 <- format(round(a$V5,6), nsmall = 6)
  write.table(a, sep=" ",  col.names=FALSE, row.names = FALSE, quote=FALSE, paste0("",files))  # Save and replace label files
}

