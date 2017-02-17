#
# create an HTML presentation using RevealJS R package 
#
# 2016

# Install package if necessary
# install.packages("revealjs", type = "source")

# first write output to a markdown file

FolderName <- "/Users/robertdrummond/dev/R/projects/RevealJS_Presentation"

markdownFileName <- "my_revealjs_presentation.Rmd"
markdownPathName <- paste0(folderName,"/",markdownFileName)
 
markdownFileName <- "my_revealjs_presentation.Rmd"
markdownPathName <- paste0(folderName,"/",markdownFileName)
 
x <- "hello world!"

# to write a vector or text to the file
write(x, 
	file = markdownPathName,
    ncolumns = 1 ,	# number of columns to write data to
    append = FALSE,   # create new file or append
    sep = "\t")		# column separator

# or to write a data frame to file
# write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
#            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
#            col.names = TRUE, qmethod = c("escape", "double"),
#            fileEncoding = "")


# render the HTML file from your R markdown source file 
#
rmarkdown::render(markdownPathName)
