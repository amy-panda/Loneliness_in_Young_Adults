# Load the library
# library(tm)
library(textreadr)

# Read the documents in the folder
docs_list <- read_dir("./Data/Interviews_part1&2")

# Extract the unique list for document names
list <- unique(docs_list$document)

# Create function to read docx documents and save into txt files
for (name in list){
  docs <- read_docx(paste0("./Data/Interviews_part1&2/",name,".docx"))
  joined_docs <- paste(docs,collapse = " ")                 
  write.table(joined_docs,file=paste0(name," - part1&2.txt"),sep="\t",row.names=FALSE)
}
