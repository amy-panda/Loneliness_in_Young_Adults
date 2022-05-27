# Load the library
library(tm)
library(textreadr)

docs_list <- read_dir("./Data/Interviews_part1&2")

list <- unique(docs_list$document)

for (name in list){
  docs <- read_docx(paste0("./Data/Interviews_part1&2/",name,".docx"))
  joined_docs <- paste(docs, collapse = '')                 
  write.table(joined_docs,file=paste0(name," - part1&2.txt"),sep="\t",row.names=FALSE)
}