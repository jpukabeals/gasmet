
# renaming files to have "fgi" in filename --------------------------------

# renaming txt files in a folder

# first I copy the folder and run the command on the copied folder

# make sure the file.rename code works as you intend before running it on the
# actual files!!


# testing on a copy folder first!!
folder <-  "D:\\Shared drives\\SEECRS (Gutknecht) lab\\Gasmet data\\FGI\\Gasmet Data 2020\\txt\\txt_eddyflux_20200519 - Copy"
setwd(folder)

# call all txt files
grep("txt", list.files(), value=TRUE)
# call all files
list.files()

# rename all files to "fgi_" + the filename
file.rename(list.files(), paste0("fgi ", list.files()))
# works!

# now on actual folder
folder <-  "D:\\Shared drives\\SEECRS (Gutknecht) lab\\Gasmet data\\FGI\\Gasmet Data 2020\\txt\\txt_eddyflux_20200519"
setwd(folder)
file.rename(list.files(), paste0("fgi ", list.files()))

# reset working directory
setwd("~/R projects/gasmet")


# to_lower ----------------------------------------------------------------
mywd <- "~/R projects/gasmet"

folder <-  "D:\\Shared drives\\SEECRS (Gutknecht) lab\\Gasmet data\\FGI\\Gasmet Data 2020\\txt"
setwd(folder)

list.files()

# making all filenames lowercase
file.rename(list.files(),tolower(list.files()))
list.files()

# resetting directory
setwd(mywd)




