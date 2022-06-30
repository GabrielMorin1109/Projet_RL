# library needed to run the script 
library(data.tree)
library(stringr)
library(dplyr)

# Extract the path of the current opened document ans setting it as the directory
script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_path)


# Function that will extract all functions name within a script. Code has been modify from : https://community.rstudio.com/t/is-there-a-way-to-extract-the-names-of-all-variables-in-another-r-script/77712/7
checkScriptVars = function(script){
  # read all line within a script
  myScript = readLines(script)

  #Remove Quoted text with "" or ''
  myScript = stringr::str_remove_all(myScript, "[\\\\]+\\\"")
  
  # extract all line with a source
  File.dependency <- str_extract_all(myScript, "(?<=source).*$")
  File.dependency <- str_extract_all(File.dependency, "\"\\s*(.*?)\\s*.\"")
  File.dependency <- gsub("\"", "", File.dependency)
  
  # File.dependency = str_extract_all(myScript[6], "\\*.R|\\*.r")
  
  
  
  myScript = stringr::str_remove_all(myScript, "\"[^\"]+\"")
  
  myScript = stringr::str_remove_all(myScript, "[\\\\]+\'")
  myScript = stringr::str_remove_all(myScript, "'[^']+'")

  # Remove comment lines
  myScript = stringr::str_remove_all(myScript, "#.*")

  #Merge script into one long string
  myScript = paste(myScript, collapse = " ")

  #Get the variable names
  vars = stringr::str_extract_all(myScript, '.\\s*[\\w\\.]+\\s*(<-|=)\\s*[^\\s\\(]+') %>% unlist()
  vars = vars[!stringr::str_detect(vars, "^,")]
  vars = stringr::str_match(vars, "([^\\s\\(]+)\\s*(<-|=)\\s*([^\\(]+)") %>% as.data.frame()

  vars = vars %>% filter(V4 == "function") %>% pull(V2) %>% unique()

  #Get the argument names from custom functions
  args = stringr::str_match_all(myScript, "function\\(([^\\)]+)\\)")[[1]][,2]
  
  args = stringr::str_match_all(args, "(^\\s*|\\s*,\\s*)([^\\s,]+)")

  args = sapply(args, function( arg ) arg[,3]) %>% unlist() %>% as.character() %>% unique()
  
  out <- data.frame(name = c(vars, args), type = c(rep("function", length(vars)), rep("arg", length(args))))
  out <- out[out$type == "function",]$name
  return(
    list(
      Function = out,
      File.dependency = File.dependency[File.dependency!="character(0)"]
    )
  )
}

# extract recursively all R file from the working directory
files <- gsub(
  getwd(),
  "",
  list.files(getwd(), full.names = TRUE, recursive = TRUE, pattern = "\\.r$|\\.R$") #"(*.R|*.r)")
)

full_path_files_algorithm <- paste0(getwd(), files)

Node_of_R_file <- gsub("/","$",files)
# command to adding node to the current tree
command <- paste0("mytree", Node_of_R_file)

# check which file has function into it
file_function_in_script <- lapply(full_path_files_algorithm, checkScriptVars) %>%
  lapply(function(x) x$Function)
file_dependency_in_script <- lapply(full_path_files_algorithm, checkScriptVars) %>%
  lapply(function(x) x$File.dependency)

# testing (boolean) if the script has function in it
test.if.has.function <- sapply(file_function_in_script, length) > 0
test.if.has.dependency <- sapply(file_dependency_in_script, length) > 0

test.if.has.something.to.nod <- test.if.has.function | test.if.has.dependency
# data frame of command to be compute
node_to_be_bind <- cbind(
  command = command[test.if.has.something.to.nod], 
  file_function_in_script = file_function_in_script[test.if.has.something.to.nod],
  file_dependency_in_script = file_dependency_in_script[test.if.has.something.to.nod]
)

# creating file configuration
(mytree <- data.tree::as.Node(data.frame(pathString = paste0("ProjetRL",files))))

# append all function within an script to his corresponding Node
for(i in seq_len(nrow(node_to_be_bind))){
  # adding function ----
  command.i <- unlist(node_to_be_bind[i,]$command)
  file_function_in_script.i <- unlist(node_to_be_bind[i,]$file_function_in_script)
  for(function_in_script in file_function_in_script.i){
    # test if logical 0 (meening that no additionnal node is needed)
    if(length(function_in_script)>0){
      eval(parse(text = command.i))$AddChild(paste0("-> function* : ", function_in_script))
    }
  }
  
  # adding dependency ----
  file_dependency_in_script.i <- unlist(node_to_be_bind[i,]$file_dependency_in_script)
  
  for(dependency_in_script in file_dependency_in_script.i){
    # test if logical 0 (meening that no additionnal node is needed)
    if(length(dependency_in_script)>0){
      eval(parse(text = command.i))$AddChild(paste0("-<|> Dependency with : ", dependency_in_script))
    }
  }
}
# important message
mytree$Library.R$AddChild(paste0("<!>LIST OF PACKAGES NEEDED TO BE INSTALLED<!>"))

# printing result
mytree
