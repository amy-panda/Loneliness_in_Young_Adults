# Load the library--------------------------------------------------------------

library(textreadr)

# Part 1 & 2 :Convert the documents to txt files for part 1&2 ------------------

## Read the documents (part1&2) in the folder
docs_list <- read_dir("./Data/Interviews_part1&2")

## Extract the unique list for document names
list <- unique(docs_list$document)

## Create function to read docx documents and save into txt files
for (name in list){
  docs <- read_docx(paste0("./Data/Interviews_part1&2/",name,".docx"))
  joined_docs <- paste(docs,collapse = " ")                 
  write.table(joined_docs,file=paste0(name," - part1&2.txt"),
              sep="\t",row.names=FALSE)
                  }


# Part 1 only: Convert the documents to txt files-------------------------------

## Read the documents (part1 only) in the folder
docs_list_p1 <- read_dir("./Data/Interviews_part1")

## Extract the unique list for document names
list_p1 <- unique(docs_list_p1$document)

## Create function to read docx documents and save into txt files
for (name in list_p1){
  docs <- read_docx(paste0("./Data/Interviews_part1/",name,".docx"))
  joined_docs <- paste(docs,collapse = " ")                 
  write.table(joined_docs,file=paste0(name," - part1.txt"),sep="\t",row.names=FALSE)
}


# Part2 only: Convert the documents to txt files--------------------------------

## Read the documents (part1 only) in the folder
docs_list_p2 <- read_dir("./Data/Interviews_part2")

## Extract the unique list for document names
list_p2 <- unique(docs_list_p2$document)

## Create function to read docx documents and save into txt files
for (name in list_p2){
  docs <- read_docx(paste0("./Data/Interviews_part2/",name,".docx"))
  joined_docs <- paste(docs,collapse = " ")                 
  write.table(joined_docs,file=paste0(name,".txt"),sep="\t",row.names=FALSE)
}

















