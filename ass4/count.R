count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if( is.null(cause) ) stop( "cause is null" )
  
  ## Check that specific "cause" is allowed; else throw error
  cause_list <- c("asphyxiation" = 1, 
                  "blunt force" = 2, 
                  "other" = 3,
                  "shooting" = 4,
                  "stabbing" = 5,
                  "unknown" = 6
  )
  
  if( is.na(cause_list[cause]) ) stop( "invalid cause" )
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  txt <- grep( sprintf("Cause: %s", cause), homicides, ignore.case = TRUE )
  
  ## Return integer containing count of homicides for that cause
  return( length( txt ) )
}