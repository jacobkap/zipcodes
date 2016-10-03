zip_scraper <- function(){
  data = ""
  for (i in state.name) {
    if (i == "New Hampshire") {
      zipcode_site <- read_html("http://www.zipcodestogo.com/New%20Hampshire/")
    }
    else if (i != "New Hampshire") {
      zipcode_site <- read_html(paste("http://www.zipcodestogo.com/",
                                      i, "/", sep = ""))
    }
    zipcode <- zipcode_site %>%
      html_nodes("#leftCol tr+ tr td:nth-child(1) a") %>%
      html_text()

    city <- zipcode_site %>%
      html_nodes("#leftCol tr~ tr+ tr td:nth-child(2)") %>%
      html_text()

    county <- zipcode_site %>%
      html_nodes("#leftCol tr~ tr+ tr td:nth-child(3)") %>%
      html_text()

    zipdata <- cbind(zipcode, city, county, rep(i, length(zipcode)))
    row.names(zipdata) <- 1:nrow(zipdata)
    data <- data.frame(rbind(data, zipdata))
  }
  data <- data[2:nrow(data),]
  row.names(data) <- 1:nrow(data)
  names(data)[4] <- "state"
  data$zipcode <- as.numeric(as.character(data$zipcode))
  data$city <- as.character(data$city)
  data$county <- as.character(data$county)
  data$state <- as.character(data$state)

  dc_site <- read_html(paste("http://www.zillow.com/browse/homes/",
                                 "dc/district-of-columbia-county/",
                                 sep = ""))

  dc_zipcode <- dc_site %>%
    html_nodes(".zsg-sm-1-1 a") %>%
    html_text()
  dc_zipcode <- data.frame(dc_zipcode)
  names(dc_zipcode) <- "zipcode"
  dc_zipcode$zipcode <- as.numeric(as.character(dc_zipcode$zipcode))
  dc_zipcode$city <- "Washington DC"
  dc_zipcode$county <- "Washington DC"
  dc_zipcode$state <- "Washington DC"

  data <- rbind(data, dc_zipcode)

  return(data)
}

