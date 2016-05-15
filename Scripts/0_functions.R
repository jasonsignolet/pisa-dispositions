#########################################
## FUNCTIONS
#########################################



#########################################
## Use the Pisa2012 dictionary to set missing values to NA
#########################################

missingPISA <- function(input, dictionary){
  require(data.table)
  
  variables_input <- colnames(input)
  variables_dict <- unique(dictionary$variable)
  
  variables <- intersect(variables_input, variables_dict)
  
  for(i in variables){
    tryCatch(
      {
        missing <- 
          dictionary[variable == (i) & Label %in% c("N/A", "Missing", "Invalid"), value]
        }, 
      error = function(e){return(missing <- NA)
      }
    )
    
    
    if(is.na(missing[1])){
      next
    }
    
    input[eval(as.name(i)) %in% as.numeric(missing), eval(as.name(i)) := NA]
  }
  
  return(input)
  
}









#########################################
## Flag TRUE for each row where students 
## were shown all parts of the test
#########################################

all_seated <- function(DT) {
  apply(DT, 1, function(a) !7 %in% a)
}










