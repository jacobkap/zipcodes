globalVariables("state.name")

#' Provides geographic information about the inputted state
#'
#' @param states
#' The states that you would like info about.
#'
#' @return
#' A dataframe with the zipcode, city, county, county FIPS codes,
#' and state FIPS code corresponding to the inputted state.
#' @export
#'
#' @examples
#' zipcodes_state("california")
#' zipcodes_state("California")
#'
#' zipcodes_state(c("california", "wyoming"))
#' zipcodes_state(c("California", "Wyoming"))
zipcodes_state <- function(states){

  states <- paste("^", states, "$", sep = "")
  dataset <- zipcodes[grep(paste(states, collapse = "|"), zipcodes$state,
                           ignore.case = TRUE), ]


  return(dataset)
}

#' Provides geographic information about the inputted city
#'
#' @param city
#' The city name that you are interested in.
#' @param county
#' Optional county to be more specific in your queries.
#' @param state
#' Optional state to be more specific in your queries.
#'
#' @return
#' A dataframe with the zipcode, county, state, county FIPS codes,
#' and state FIPS code corresponding to the inputted city.
#' @export
#'
#' @examples
#' zipcodes_city(city = "Sacramento")
#' zipcodes_city(city = "Sacramento", county = "Sacramento")
#' zipcodes_city(city = "Sacramento", state = "California")
#' zipcodes_city(city = "Sacramento", county = "Sacramento",
#'               state = "California")
#'
#' zipcodes_city(city = c("Sacramento", "Walnut Creek"))
zipcodes_city <- function(city, county = FALSE, state = FALSE) {

    city <- paste("^", city, "$", sep = "")
    dataset <- zipcodes[grep(paste(city, collapse = "|"),
                             ignore.case = TRUE, zipcodes$city), ]

  if (county != FALSE) {
    county <- paste("^", county, "$", sep = "")
    dataset <- dataset[grep(paste(county, collapse = "|"),
                             ignore.case = TRUE, dataset$county), ]
  }

  if (state != FALSE) {
    state <- paste("^", state, "$", sep = "")
    dataset <- dataset[grep(paste(state, collapse = "|"),
                            ignore.case = TRUE, dataset$state), ]
  }

  return(dataset)
}

#' Provides geographic information about the inputted county
#'
#' @param county
#' The county name or names that you are interested in.
#' @param state
#' Optional state to be more specific in your queries.
#'
#' @return
#' A dataframe with the zipcode, city, state, county FIPS codes,
#' and state FIPS code corresponding to the inputted county.
#' @export
#'
#' @examples
#' zipcodes_county("sacramento")
#' zipcodes_county(county = "sacramento", state = "California")
#' zipcodes_county(county = c("sacramento", "Alameda"), state = "California")
zipcodes_county <- function(county, state = FALSE) {

  county <- paste("^", county, "$", sep = "")
    dataset <- zipcodes[grep(paste(county, collapse = "|"),
                             ignore.case = TRUE, zipcodes$county), ]


  if (state != FALSE) {
    state <- paste("^", state, "$", sep = "")
    dataset <- dataset[grep(paste(state, collapse = "|"),
                            ignore.case = TRUE, dataset$state), ]
  }

  return(dataset)
}

#' Provides geographic information about the inputted county FIPS code
#'
#' @param county_fips
#' The county FIPS code that you want info about.
#'
#' @return
#' A dataframe with the zipcode, city, county, state, and state FIPS code
#' corresponding to the inputted county FIPS code.
#' @export
#'
#' @examples
#' county_fips("01001")
#' county_fips(01001)
#'
#' county_fips(c("05043", "12033", "06065"))
#' county_fips(c(05043, 12033, 06065))
county_fips <- function(county_fips) {

  county_fips <- as.character(county_fips)

  for (i in 1:length(county_fips)) {
    if (nchar(county_fips[i]) < 5) {
      county_fips[i] <- paste("0", county_fips[i], sep = "")
    }}

  county_fips <- paste("^", county_fips, "$", sep = "")
  dataset <- zipcodes[grep(paste(county_fips, collapse = "|"),
                           ignore.case = TRUE, zipcodes$county_FIPS), ]

  return(dataset)
}

#' Provides geographic information about the inputted state FIPS code
#'
#' @param state_fips
#' State FIPS code or codes that you would like information about
#'
#' @return
#' A dataframe with the zipcode, city, county, state, and county FIPS code
#' corresponding to the inputted state FIPS code.
#' @export
#'
#' @examples
#' state_fips("01")
#' state_fips(01)
#'
#' state_fips(c("05", "25", "11"))
#' state_fips(c(05, 25, 11))
state_fips <- function(state_fips) {
  state_fips <- as.character(state_fips)

  for (i in 1:length(state_fips)) {
  if (nchar(state_fips[i]) < 2) {
    state_fips[i] <- paste("0", state_fips[i], sep = "")
  }}

  state_fips <- paste("^", state_fips, "$", sep = "")
  dataset <- zipcodes[grep(paste(state_fips, collapse = "|"),
                           ignore.case = TRUE, zipcodes$state_FIPS), ]

  return(dataset)
}

#' Provides geographic information about the inputted zipcodes.
#'
#' @param zipcode
#' Zipcode or zipcodes you would like information about
#'
#' @return
#' A dataframe with the city, county, state, county FIPS code, and state
#' FIPS code corresponding to the inputted zipcode.
#' @export
#'
#' @examples
#'
#' zipcode("19104")
#' zipcode(19104)
#'
#' zipcode(c(19104, 19105))
#'
#' example <- c(19104, 36067, 36003)
#' zipcode(example)
zipcode <- function(zipcode) {

  zipcode <- paste("^", zipcode, "$", sep = "")
  dataset <- zipcodes[grep(paste(zipcode, collapse = "|"),
                           ignore.case = TRUE, zipcodes$zipcode), ]

  return(dataset)
}

#' Converts zipcodes into county FIPS codes for those locations
#'
#' @param zipcode
#' The zipcodes that you want the county FIPS codes for.
#'
#' @return
#' A data.frame with one column, the county FIPS code that correspond
#' to the inputted zipcodes.
#' @export
#'
#' @examples
#' zips2fips(c("94526", "19104", "99999", 313, NA, "test"))
zips2fips <- function(zipcode) {

  zipcode[is.na(zipcode)] <- ""
  zipcode2 <- zipcode

  for (n in 1:nrow(zipcodes)) {
    zipcode[which(zipcode ==
                                    zipcodes$zipcode[n])] <-
      zipcodes$county_FIPS[n]
  }

  for (n in 1:length(zipcode)) {
    if (zipcode[n] == zipcode2[n]) {
    zipcode[n] <- NA
    }
  }


  return(zipcode)
}


#' City, county, state, county FIP code and state FIP code for 40569 zipcodes
#' in the United States
#'
#' A dataset containing the city, county, state, county FIPS code, and state
#' FIPS code for 40569 zipcodes in the United States
#'
#' @format A data frame with 40569 rows and 6 variables:
#' \describe{
#'   \item{zipcode}{Zipcode number}
#'   \item{city}{City name}
#'   \item{county}{County name}
#'   \item{state}{State name}
#'   \item{county_FIPS}{FIP code for the county}
#'   \item{state_FIPS}{FIP code for the state}
#'   ...
#' }
#' @source \url{zipcodestogo.com}
"zipcodes"
