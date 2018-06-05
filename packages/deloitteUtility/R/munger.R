##
## munger.R, generate necessary dataframes.
##
## Note: https://stackoverflow.com/a/13613183
##
munger <- function(filename, complete=FALSE) {
  ## load packages
  load_package(c('reshape2', 'openintro', 'Rmisc'))
  
  ## create ignored directories
  dir.create(file.path(cwd, 'visualization'), showWarnings = FALSE)
  
  ## load dfsets
  df <- read.delim(
    paste('data/', filename, sep=''),
    sep='\t', header=TRUE
  )
  
  ## rename columns
  colnames(df) <- tolower(c('place', 'division_place', 'bib', 'name', 'age', 'hometown', 'gun_time', 'net_time', 'pace'))
  
  ## cleanup dfset
  df$gun_time <- trimws(gsub('[#*a-zA-Z]', '', df$gun_time))
  df$net_time <- trimws(gsub('[#*a-zA-Z]', '', df$net_time))
  df$pace <- trimws(gsub('[#*a-zA-Z]', '', df$pace))
  df$age <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', df$age)))
  df$place <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', df$place)))
  df$bib <- as.numeric(trimws(gsub('[#*a-zA-Z]', '', df$bib)))
  df$division_place <- trimws(gsub('[#*a-zA-Z]', '', df$division_place))
  df$hometown <- trimws(gsub('[,.]', '', df$hometown))
  df$hometown <- trimws(df$hometown)
  
  ## convert time to seconds
  df$gun_time <- unlist(lapply(df$gun_time, ctimeToSeconds))
  df$net_time <- unlist(lapply(df$net_time, ctimeToSeconds))
  df$pace <- unlist(lapply(df$pace, ctimeToSeconds))
  
  ## insert delimiter
  df$hometown <- lapply(df$hometown, function(x) {
    return(strReverse(sub(' ', ' ,', strReverse(x))))
  })
  
  ## explode column: 'hometown' column into 'city', and 'state' columns
  df <- cbind(
    colsplit(df$hometown, ',', c('city', 'state')),
    df[,-which(names(df) == 'hometown')]
  )
  
  ## consistent lowercase
  df$city <- tolower(trimws(df$city))
  df$state <- tolower(abbr2state(trimws(df$state)))
  
  ## complete case: remove any rows with NA
  if (complete) {
    df <- df[complete.cases(df),]
    
    ## complete case: define division
    df$division <- create_division(df$age)
  }
  
  else {
    ## incomplete case: anticipate missing values
    df$state[df$city == 'silver spring' & is.na(df$state)] <- 'maryland'
    df$state[df$city == 'ellicott city' & is.na(df$state)] <- 'maryland'
    df$state[df$city == 'north potomac' & is.na(df$state)] <- 'maryland'
    df$state[df$city == 'fredericksburg' & is.na(df$state)] <- 'virginia'
    df$state[df$city == 'washington' & is.na(df$state)] <- 'district of columbia'
    df$state[df$city == 'potomac falls' & is.na(df$state)] <- 'maryland'
    df$city[df$city == 'potomac falls'] <- 'potomac'
    
    df$age[df$age < 1] <- NA
    df <- df[complete.cases(df),]
    df$division <- create_division(df$age)
  }
  
  ## return dataframe
  return(df)
}
