pollutantmean <- function(directory, pollutant, id = 1:332) 
{
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

## Richard Seeton - 2014-08-03

#Initiate pollutantdata dataframe

pollutantdata <- data.frame()		
#Does directory exist
if (file.exists(directory))
{
	originalwd <- getwd()
    setwd(file.path(directory))
} else 
{
    return(message("Target working dir (", directory,") does not exist", sep=" " ))
}
#Requesting recognized pollutant
if (!(pollutant == "nitrate" | pollutant == "sulfate"))
{
   return(message("Selected pollutant (",pollutant,") not recognized.  Please select from [sulfate|nitrate]", sep=" " ))
}

# Zero pad numbers in list for filenames
filelist <- paste(sprintf( "%03d", id), ".csv", sep="")

for (filename in filelist)
{
	if (file.exists(filename))
	{	
		pollutantdata <- rbind(pollutantdata,read.csv(filename, header=TRUE, stringsAsFactor=FALSE))  # read csv file
	} else
	{
		message("Target file (", filename ,") does not exist", sep=" " )
	}
}
target_pollutant <- pollutantdata[,c(pollutant)]
target_pollutant <- target_pollutant[!is.na(target_pollutant)]
mean_pollutant   <- format(round(mean(target_pollutant), 3), nsmall = 3)

message("Average ", pollutant," : ", mean_pollutant)
message("Complete.")
setwd(originalwd) #Reset to original working directory

}