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

matchSort <- function(x,y){
  x[order(match(x,y))]
}

modulePanel <- function(title, value){
  tabPanel(
    title = title,
    value = value,
    eval(parse(text = value))$ui
  )
}

getGenericGermplasmName <- function(germplasmNames){
  map_chr(germplasmNames,
    function(germplasmName){
      if(length(germplasmName) == 0)
    return(character(0))
  strsplit(germplasmName, "_", fixed = T)[[1]][1]
    }
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

autoStyleInterval <- function(thisDataTable, colorCount = 10){
  
  breaksFromData <- function(thisDataTable){
    thisDataTable %>% 
      {unlist(.$x$data[-1])} %>% 
      as.numeric() %>%
      {seq(range(., na.rm = T)[1], range(., na.rm = T)[2], by = diff(range(., na.rm = T) / colorCount))}
  }
  
  thisDataTable %>%
    {formatStyle(
      ., 
      names(.$x$data)[-1],
      backgroundColor = styleInterval(
        cuts = breaksFromData(.), 
        values = round(seq(255, 40, length.out = length(breaksFromData(.)) + 1), 0) %>%
          {paste0("rgb(255,", ., ",", ., ")")}
      )
    )}
}

nestSlice <- function(nestedList, n = 1){
  map_depth(nestedList, n - 1, function(x){names(x)}) %>% unlist %>% unname
}

isStandardFormat <- function(germplasmName){
  grepl("^[0-9]+-[0-9]+$", germplasmName)
}

toGenericPedigreeCode <- function(germplasmName){
  germplasmName %<>% as.character()
  if(!isStandardFormat(germplasmName))
    return(germplasmName)
  germplasmName <- strsplit(germplasmName, "-", fixed = T)[[1]][1]
  if(!is.na(as.numeric(germplasmName)))
    germplasmName <- as.character(as.numeric(germplasmName))
  return(germplasmName)
}

getParents <- function(germplasmName){
  germplasmName %<>% toGenericPedigreeCode()
  germplasm <- parents %>% filter(Code == germplasmName)
  if(nrow(germplasm) > 0)
    return(c(germplasm$Maternal[1], germplasm$Paternal[1]))
  germplasmParents <- c(NA, NA)
  try(silent = T, {
    germplasmParents <- strsplit(brapi_get_germplasm(brapi_db()$sweetpotatobase, germplasmName = germplasmName)$pedigree, "/")[[1]]
  })
  return(germplasmParents)
}

sharedCloseParentage <- function(clone1, clone2){
  
  pedigree1 <- clone1 %>% toGenericPedigreeCode %>% closeParents[[.]]
  pedigree2 <- clone2 %>% toGenericPedigreeCode %>% closeParents[[.]]
  
  x <- match(pedigree1, pedigree2, incomparables = NA) %>%
    {sum(!is.na(.)) * 2 / sum(!is.na(pedigree1), !is.na(pedigree2))}
  
  if(is.na(x))
    return(0)
  
  return(x)
  
}

## pedigree toolbox
isStandardFormat <- function(germplasmName){
  grepl("^[0-9]+-[0-9]+$", germplasmName)
}

toGenericPedigreeCode <- function(germplasmName){
  germplasmName %<>% as.character()
  if(!isStandardFormat(germplasmName))
    return(germplasmName)
  germplasmName <- strsplit(germplasmName, "-", fixed = T)[[1]][1]
  if(!is.na(as.numeric(germplasmName)))
    germplasmName <- as.character(as.numeric(germplasmName))
  return(germplasmName)
}

isTerminal <- function(germplasmName){
  all(getParents(germplasmName) %in% c(NA, "NA", ""))
}

pedigree <- local(function(germplasmName){
  recursiveGetParents <- function(germplasmName, thisPedigree = list()){
    try({thisParents <- getParents(germplasmName)})
    if(exists("thisParents")){
      for(i in 1:2){
        if(!thisParents[i] %in% c(NA, "NA", "")){
          thisPedigree[[i]] <- c(recursiveGetParents(thisParents[i]))
          names(thisPedigree)[i] <- thisParents[i]
        }
        else{
          if(i == 2 & !all(is.na(thisPedigree)))
            thisPedigree[["OP"]] <- list(NA, NA)
          else
            thisPedigree[[i]] <- NA
        }
      }
    }
    return(thisPedigree)
  } 
  thisPedigree <- list()
  thisPedigree[[germplasmName]] <- recursiveGetParents(germplasmName)
  return(thisPedigree)
})

nestDepth <- function(nestedList){
  i <- 1
  while(TRUE){
    if(is.null(nestSlice(nestedList, i)))
      return(i-1)
    i <- i + 1
  }
}

nestSlice <- function(nestedList, n = 1){
  map_depth(nestedList, n - 1, function(x){names(x)}) %>% unlist %>% unname
}

getPercentParents <- function(germplasmName){
  # look for pedigree unknowns (terminal and OP parents) by...
  unknowns <- germplasmName %>%
    #...getting the pedigree of germplasmName...
    pedigree(.) %>%
    #...looking for places in the pedigree that end in an unknown (NA) parent...
    list.search(., all(is.na(.))) %>%
    #...getting the names of these unknowns...
    names(.) %>%
    #...and undo the work that list.search does to make results unique, because we want duplicates
    substr(., 1, nchar(.) - 1)
  
  # add terminal parents by...
  # getting 
  terminalParents <- unknowns[union(which(duplicated(unknowns)), which(duplicated(unknowns, fromLast = T)))]
  names(terminalParents) <- terminalParents %>% 
    map(~ strsplit(.x, ".", fixed = T)[[1]] %>% last)
  terminalParents %<>% 
    map(~ 2 ^ -(1 + lengths(regmatches(.x, gregexpr(".", .x, fixed = T))))) %>%
    unlist(.) %>%
    tapply(., names(.), sum)
  
  # add unknown parents
  unknownParents <- setdiff(unknowns, unknowns[duplicated(unknowns)]) %>%
    map(~ 2 ^ -(1 + lengths(regmatches(.x, gregexpr(".", .x, fixed = T))))) %>%
    unlist(.) %>%
    sum()
  names(unknownParents) <- "Unknown"
  if(unknownParents == 0){unknownParents <- NULL}
  
  return(c(terminalParents, unknownParents) %>% signif(digits = 4) %>% sort(decreasing = T))
  
}


