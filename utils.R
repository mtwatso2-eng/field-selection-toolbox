isMissing <- function(x){
  x %in% c(NA, "NA", "", "NR", NaN)
}

isNumericLike <- function(vector){
  vector <- as.vector(vector)
  return(all(isMissing(vector) | !is.na(as.numeric(vector))))
}

wrappingModulus <- function(x, n){
  y <- x %% n
  if(y == 0)
    return(n)
  y
}

modulePanel <- function(title, value){
  tabPanel(
    title = title,
    value = value,
    eval(parse(text = value))$ui
  )
}

observationVariablesToTraitName <- function(x){
  ifelse(
    grepl("CO", x, fixed = T), 
    cache$traitInfo %>% 
    filter(traitDbId == tail(strsplit(x, ".", fixed = T)[[1]], n = 1)) %$%
    traitName,
    x
  )
}

tenacious_range_read <- function(ss, sheet, col_types){
  while(TRUE){ try({ return(
    range_read(
      ss = ss,
      sheet = sheet,
      col_types = col_types
    ) %>% 
    as.data.frame
  )})}
  
}

toUpperCamelCase <- function(x){sapply(x,
  function(y){
    if(!grepl(" ", y, fixed = TRUE)){return(y)}
    gsub("[^[:alnum:]._]","", str_to_title(y))
  }
)}

toTitle <- function(x){sapply(x,
  function(y){
    tools::toTitleCase(tolower(gsub("([A-Z]+)", " \\1", y)))
  }                                    
)}