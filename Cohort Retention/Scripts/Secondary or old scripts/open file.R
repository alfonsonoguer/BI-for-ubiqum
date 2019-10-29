#### open file - it will open the correct file if you name it correclty ####
# añadir en el script renombrar el archivo correctamente

data_path <- paste(getwd(), "/data", sep="")
file_name <- as.Date(Sys.time())
file_name <- gsub("-",  "", file_name) 
file_name <- list.files("data", pattern = paste("*", file_name, ".csv", sep=""))
data <- read_csv((paste(data_path,"/", file_name, sep = "")))
