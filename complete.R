pollutantmean <- function (directory, pollutant, id = 1:332){
    # First step
    # Check whether directory is current working directory
    # Or a subridrectory of the current WD.
    
    if (!(directory == basename(getwd()))& directory %in% dir()) {
        
        setwd(directory)
    }
    # Second step
    #Each monitor writes its data to a file with naming convention
    # <monitor_id>.CSV. The Monitor ID is left padded with zero's to 
    # create a name-length of three characters.Thus monitor ID 1
    # creates a data-file called 001.CSV. In order not to have to oad the entire
    # directory, we first wecreate a cahracter vector with ID argumentconverted 
    # in a valid file-name list

    file_name <- as.character(id)

    start <- id[1]
    end <- id[length(id)]
    x <- 1

        for (i in start:end){

        if (nchar(file_name[x]) == 1){
            file_name[x] <- paste("00", file_name[x], sep="")
#            print(paste("run = ",x,"file_name - ", file_name[x], sep = " "))
        }
        
        if (nchar(file_name[x]) == 2) {
            file_name[x] <- paste("0", file_name[x], sep="")
        }
        x <- x+1
    }
    
    file_name <- paste(file_name,".csv", sep="")
    
    # Third step
    # Create a data frame containing the observations from the required files
    
    my_dataframe <- do.call(rbind,lapply(file_name,read.csv))
    
    # Fourth step
    # Select all observations from the pollutant argument in a vector
    
    my_data <- my_dataframe[ ,pollutant]
    
    #Fifth step
    # Calculating the mean, without the NA's
    # N.B. NA's are not removed from the dataframe  in case there are rows containing
    # a value in one of the variables, but not in the other
    
    my_mean <- mean(my_data, na.rm = TRUE)
    
    my_mean
    
    }
