zipcodes_state <- function(states){

  states <- paste("^", states, "$", sep = "")
  dataset <- zipcodes[grep(paste(states, collapse = "|"), zipcodes$state,
                           ignore.case = TRUE),]


  return(dataset)
}

zipcodes_city <- function(city, county = FALSE, state = FALSE) {

    city <- paste("^", city, "$", sep = "")
    dataset <- zipcodes[grep(paste(city, collapse = "|"),
                             ignore.case = TRUE, zipcodes$city),]

  if (county != FALSE) {
    county <- paste("^", county, "$", sep = "")
    dataset <- dataset[grep(paste(county, collapse = "|"),
                             ignore.case = TRUE, dataset$county),]
  }

  if (state != FALSE) {
    state <- paste("^", state, "$", sep = "")
    dataset <- dataset[grep(paste(state, collapse = "|"),
                            ignore.case = TRUE, dataset$state),]
  }

  return(dataset)
}

zipcodes_county <- function(county, state = FALSE) {

  county <- paste("^", county, "$", sep = "")
    dataset <- zipcodes[grep(paste(county, collapse = "|"),
                             ignore.case = TRUE, zipcodes$county),]


  if (state != FALSE) {
    state <- paste("^", state, "$", sep = "")
    dataset <- dataset[grep(paste(state, collapse = "|"),
                            ignore.case = TRUE, dataset$state),]
  }

  return(dataset)
}

county_fips <- function(county_fips) {

  county_fips <- paste("^", county_fips, "$", sep = "")
  dataset <- zipcodes[grep(paste(county_fips, collapse = "|"),
                           ignore.case = TRUE, zipcodes$county_FIPS),]

  return(dataset)
}

state_fips <- function(state_fips) {

  state_fips <- paste("^", state_fips, "$", sep = "")
  dataset <- zipcodes[grep(paste(state_fips, collapse = "|"),
                           ignore.case = TRUE, zipcodes$state_FIPS),]

  return(dataset)
}

zipcode <- function(zipcode) {

  zipcode <- paste("^", zipcode, "$", sep = "")
  dataset <- zipcodes[grep(paste(zipcode, collapse = "|"),
                           ignore.case = TRUE, zipcodes$zipcode),]

  return(dataset)
}
