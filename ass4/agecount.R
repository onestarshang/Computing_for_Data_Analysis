agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if( is.null(age) ) stop( "cause is null" )
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")

  ## Extract ages of victims; ignore records where no age is
  ## given
  txt <- grep( sprintf(" %d years old", age), homicides ) 

  ## Return integer containing count of homicides for that age
  return( length(txt) )
}