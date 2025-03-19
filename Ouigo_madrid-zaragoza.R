#Ouigo：Madrid-Zaragoza
#Today+1
library(RSelenium)
library(tidyverse)
library(xml2)
library(RSelenium)
library(tidyverse)
library(xml2)
library(jsonlite)

#date_1

# Create a remote driver to control a Firefox browser using Selenium       
remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()
remDr$navigate("https://www.ouigo.com/es/")

# Get today's date
today <- Sys.Date()

# Define future dates based on today
date_1 <- today + 1
date_7 <- today + 7
date_14 <- today + 14
date_30 <- today + 30
dates <- as.character(c(today + 1, today + 7, today + 14, today + 30))

remDr$switchToFrame(0)

# origin station
departure = remDr$findElement(using = "id", "origin-station-input-field__input")
departure$clickElement()

departure$clearElement()
departure$sendKeysToElement(list("Madrid - Todas las estaciones"))
Sys.sleep(2)  
suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Madrid - Todas las estaciones", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}

# destination
destination <- remDr$findElement(using = "css selector", "input[id='destination-station-input-field__input']")
destination$clickElement()  
destination$sendKeysToElement(list("Zaragoza - Delicias"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Zaragoza - Delicias", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}


Sys.sleep(1)

#Search target date
date_input <- remDr$findElement(using = "css selector", "input[id='search-engine__inputfield__outbound-date__input']")
date_input$clickElement()
Sys.sleep(3)  

for (date in date_1) {
  xpath_query <- sprintf("//li[@data-date='%s']", date_1)
  date_element <- remDr$findElement(using = "xpath", xpath_query)
  date_element$clickElement()
  Sys.sleep(2)  
}

#Click search_button
search_button <- remDr$findElement(using = "css selector", "button[title='Actualizar el viaje']")
search_button$clickElement()

Sys.sleep(5)

# Extract trains data
# Get all open browser windows and switch to the latest one
all_windows <- remDr$getWindowHandles()
remDr$switchToWindow(all_windows[[length(all_windows)]])

# Find all train journey elements on the webpage
trains <- remDr$findElements(using = "css selector", "div[role='button'][data-testid^='e2e_journey-results_journeys_journey-item']")

# Create an empty list to store extracted train data
ouigo <- list()

# Loop through each train element to extract relevant information
for (train in trains) {
  
  # Try to extract the full text content of the train element
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  # Extract departure and arrival times
  departure_time <-  str_extract(raw_text, "^\\d{2}:\\d{2}")
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  
  # Arrival time is the last extracted time, if available 
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)
  
  # Extract departure station by finding the first <span> element
  departure_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  
  # Process the extracted text to get the station name
  split_text <- str_split(departure_station, "\n")[[1]]
  if (length(split_text) >= 2) {
    departure_station <- split_text[2]
  } else {
    departure_station <- NA 
  }
  
  # Extract arrival station by finding another <span> element
  arrival_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  
  # Process the extracted text to get the station name
  split_text <- str_split(arrival_station, "\n")[[1]]
  if (length(split_text) >= 4) {
    arrival_station <- split_text[4]
  } else {
    arrival_station <- NA  
  }
  
  # Extract the duration of the journey (formatted as "XhY" minutes)
  duration <- str_extract(raw_text, "\\d+h\\d+")
  
  # Extract price information
  price <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  
  # Process the extracted price information
  split_text <- str_split(price, "\n")[[1]]
  if (length(split_text) >= 5) {
    price<- split_text[5]
  } else {
    price <- NA  
  }
  
  # Clean and trim extracted values 
  departure_time <- str_trim(departure_time)
  departure_station <- str_trim(departure_station)
  arrival_time <- str_trim(arrival_time)
  arrival_station <- str_trim(arrival_station)
  duration <- str_trim(duration)
  price <- str_trim(price)
  
  # Store extracted data into a structured dataframe
  ouigo <- append(ouigo, list(data.frame(
    departure_time,
    departure_station,
    arrival_time,
    arrival_station,
    duration,
    price,
    "train_company" = "Ouigo",
    departure_date = date_1,
    scrapping_date = today,
    scrapping_web = "Ouigo",
    stringsAsFactors = FALSE
  )))
}

# Convert the list of dataframes into a single dataframe
ouigo <- do.call(rbind, ouigo)

# Data cleaning and transformation
ouigo <- ouigo |> 
  mutate(
    departure_station = str_replace(departure_station, "\\s*-.*", ""),  
    arrival_station = str_replace(arrival_station, "\\s*-.*", "") 
  ) |> 
  mutate(price = str_remove(price, "€") %>% as.numeric()) |> 
  rename(duration_raw = duration) |>  
  mutate(duration_min_clean = as.numeric(str_extract(duration_raw, "\\d+(?=h)")) * 60 +  
           as.numeric(str_extract(duration_raw, "(?<=h)\\d+"))) |> 
  relocate(duration_min_clean, .after = duration_raw)

View(ouigo)

# Save data
file_path <- here::here("Scrapped data", "ouigo.csv")

if (!file.exists(file_path)) {
  write_csv(ouigo, file_path)  
} else {
  write_csv(ouigo, file_path, append = TRUE)  
}

#Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.
remDr$close() 

###################################
#date_7 The date is 7 days later, and all other operations remain the same as for one day later.!!!

remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()
remDr$navigate("https://www.ouigo.com/es/")


remDr$switchToFrame(0)
departure = remDr$findElement(using = "id", "origin-station-input-field__input")
departure$clickElement()

departure$clearElement()
departure$sendKeysToElement(list("Madrid - Todas las estaciones"))
Sys.sleep(2)  

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Madrid - Todas las estaciones", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}


destination <- remDr$findElement(using = "css selector", "input[id='destination-station-input-field__input']")
destination$clickElement()  
destination$sendKeysToElement(list("Zaragoza - Delicias"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Zaragoza - Delicias", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}


Sys.sleep(1)

date_input <- remDr$findElement(using = "css selector", "input[id='search-engine__inputfield__outbound-date__input']")
date_input$clickElement()
Sys.sleep(3)  

for (date in date_7) {
  xpath_query <- sprintf("//li[@data-date='%s']", date_7)
  date_element <- remDr$findElement(using = "xpath", xpath_query)
  date_element$clickElement()
  Sys.sleep(2)  
}

search_button <- remDr$findElement(using = "css selector", "button[title='Actualizar el viaje']")
search_button$clickElement()

Sys.sleep(5)

all_windows <- remDr$getWindowHandles()
remDr$switchToWindow(all_windows[[length(all_windows)]])
trains <- remDr$findElements(using = "css selector", "div[role='button'][data-testid^='e2e_journey-results_journeys_journey-item']")

ouigo <- list()

for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  departure_time <-  str_extract(raw_text, "^\\d{2}:\\d{2}")
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)
  
  departure_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(departure_station, "\n")[[1]]
  if (length(split_text) >= 2) {
    departure_station <- split_text[2]
  } else {
    departure_station <- NA 
  }
  
  arrival_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(arrival_station, "\n")[[1]]
  if (length(split_text) >= 4) {
    arrival_station <- split_text[4]
  } else {
    arrival_station <- NA  
  }
  
  duration <- str_extract(raw_text, "\\d+h\\d+")
  
  price <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(price, "\n")[[1]]
  if (length(split_text) >= 5) {
    price<- split_text[5]
  } else {
    price <- NA  
  }
  
  departure_time <- str_trim(departure_time)
  departure_station <- str_trim(departure_station)
  arrival_time <- str_trim(arrival_time)
  arrival_station <- str_trim(arrival_station)
  duration <- str_trim(duration)
  price <- str_trim(price)
  
  ouigo <- append(ouigo, list(data.frame(
    departure_time,
    departure_station,
    arrival_time,
    arrival_station,
    duration,
    price,
    "train_company" = "Ouigo",
    departure_date = date_7,
    scrapping_date = today,
    scrapping_web = "Ouigo",
    stringsAsFactors = FALSE
  )))
}

ouigo <- do.call(rbind, ouigo)

ouigo <- ouigo|> 
  mutate(
    departure_station = str_replace(departure_station, "\\s*-.*", ""), 
    arrival_station = str_replace(arrival_station, "\\s*-.*", "") 
  ) |> 
  mutate(price = str_remove(price, "€") %>% as.numeric()) |> 
  rename(duration_raw = duration) |>  
  mutate(duration_min_clean = as.numeric(str_extract(duration_raw, "\\d+(?=h)")) * 60 +  
           as.numeric(str_extract(duration_raw, "(?<=h)\\d+"))) |> 
  relocate(duration_min_clean, .after = duration_raw)

View(ouigo)

file_path <- here::here("Scrapped data", "ouigo.csv")

if (!file.exists(file_path)) {
  write_csv(ouigo, file_path)  
} else {
  write_csv(ouigo, file_path, append = TRUE)  
}

#Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.
remDr$close() 

####################################
#date_14 The date is 14 days later, and all other operations remain the same as for one day later.!!!

remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()
remDr$navigate("https://www.ouigo.com/es/")


remDr$switchToFrame(0)
departure = remDr$findElement(using = "id", "origin-station-input-field__input")
departure$clickElement()

departure$clearElement()
departure$sendKeysToElement(list("Madrid - Todas las estaciones"))
Sys.sleep(2)  

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Madrid - Todas las estaciones", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}

destination <- remDr$findElement(using = "css selector", "input[id='destination-station-input-field__input']")
destination$clickElement()  
destination$sendKeysToElement(list("Zaragoza - Delicias"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Zaragoza - Delicias", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}

Sys.sleep(1)


date_input <- remDr$findElement(using = "css selector", "input[id='search-engine__inputfield__outbound-date__input']")
date_input$clickElement()
Sys.sleep(3) 

for (date in date_14) {
  xpath_query <- sprintf("//li[@data-date='%s']", date_14)
  date_element <- remDr$findElement(using = "xpath", xpath_query)
  date_element$clickElement()
  Sys.sleep(2)  
}

search_button <- remDr$findElement(using = "css selector", "button[title='Actualizar el viaje']")
search_button$clickElement()

Sys.sleep(5)

all_windows <- remDr$getWindowHandles()
remDr$switchToWindow(all_windows[[length(all_windows)]])
trains <- remDr$findElements(using = "css selector", "div[role='button'][data-testid^='e2e_journey-results_journeys_journey-item']")

ouigo <- list()

for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  departure_time <-  str_extract(raw_text, "^\\d{2}:\\d{2}")
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)
  
  departure_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(departure_station, "\n")[[1]]
  if (length(split_text) >= 2) {
    departure_station <- split_text[2]
  } else {
    departure_station <- NA  
  }
  
  arrival_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(arrival_station, "\n")[[1]]
  if (length(split_text) >= 4) {
    arrival_station <- split_text[4]
  } else {
    arrival_station <- NA  
  }
  
  duration <- str_extract(raw_text, "\\d+h\\d+")

  price <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(price, "\n")[[1]]
  if (length(split_text) >= 5) {
    price<- split_text[5]
  } else {
    price <- NA 
  }
  
  departure_time <- str_trim(departure_time)
  departure_station <- str_trim(departure_station)
  arrival_time <- str_trim(arrival_time)
  arrival_station <- str_trim(arrival_station)
  duration <- str_trim(duration)
  price <- str_trim(price)
  
  ouigo <- append(ouigo, list(data.frame(
    departure_time,
    departure_station,
    arrival_time,
    arrival_station,
    duration,
    price,
    "train_company" = "Ouigo",
    departure_date = date_14,
    scrapping_date = today,
    scrapping_web = "Ouigo",
    stringsAsFactors = FALSE
  )))
}

ouigo <- do.call(rbind, ouigo)

ouigo <- ouigo |> 
  mutate(
    departure_station = str_replace(departure_station, "\\s*-.*", ""),  
    arrival_station = str_replace(arrival_station, "\\s*-.*", "") 
  ) |> 
  mutate(price = str_remove(price, "€") %>% as.numeric()) |> 
  rename(duration_raw = duration) |>  
  mutate(duration_min_clean = as.numeric(str_extract(duration_raw, "\\d+(?=h)")) * 60 +  
           as.numeric(str_extract(duration_raw, "(?<=h)\\d+"))) |> 
  relocate(duration_min_clean, .after = duration_raw)


View(ouigo)

file_path <- here::here("Scrapped data", "ouigo.csv")

if (!file.exists(file_path)) {
  write_csv(ouigo, file_path)  
} else {
  write_csv(ouigo, file_path, append = TRUE)  
}

#Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.
remDr$close() 

##################################
#date_30 The date is 30 days later, and all other operations remain the same as for one day later.!!!

remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()
remDr$navigate("https://www.ouigo.com/es/")

remDr$switchToFrame(0)
departure = remDr$findElement(using = "id", "origin-station-input-field__input")
departure$clickElement()

departure$clearElement()
departure$sendKeysToElement(list("Madrid - Todas las estaciones"))
Sys.sleep(2)  

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Madrid - Todas las estaciones", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}


destination <- remDr$findElement(using = "css selector", "input[id='destination-station-input-field__input']")
destination$clickElement()  
destination$sendKeysToElement(list("Zaragoza - Delicias"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")

for (i in 1:length(suggestions)) {
  print(suggestions[[i]]$getElementText())
}

for (option in suggestions) {
  if (grepl("Zaragoza - Delicias", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}


Sys.sleep(1)


date_input <- remDr$findElement(using = "css selector", "input[id='search-engine__inputfield__outbound-date__input']")
date_input$clickElement()
Sys.sleep(3)  

for (date in date_30) {
  xpath_query <- sprintf("//li[@data-date='%s']", date_30)
  date_element <- remDr$findElement(using = "xpath", xpath_query)
  date_element$clickElement()
  Sys.sleep(2)  
}

search_button <- remDr$findElement(using = "css selector", "button[title='Actualizar el viaje']")
search_button$clickElement()

Sys.sleep(5)

all_windows <- remDr$getWindowHandles()
remDr$switchToWindow(all_windows[[length(all_windows)]])
trains <- remDr$findElements(using = "css selector", "div[role='button'][data-testid^='e2e_journey-results_journeys_journey-item']")

ouigo <- list()

for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  departure_time <-  str_extract(raw_text, "^\\d{2}:\\d{2}")
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)
  
  departure_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(departure_station, "\n")[[1]]
  if (length(split_text) >= 2) {
    departure_station <- split_text[2]
  } else {
    departure_station <- NA
  }
  
  arrival_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(arrival_station, "\n")[[1]]
  if (length(split_text) >= 4) {
    arrival_station <- split_text[4]
  } else {
    arrival_station <- NA  
  }
  
  duration <- str_extract(raw_text, "\\d+h\\d+")
  
  price <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)
  split_text <- str_split(price, "\n")[[1]]
  if (length(split_text) >= 5) {
    price<- split_text[5]
  } else {
    price <- NA 
  }
  
  departure_time <- str_trim(departure_time)
  departure_station <- str_trim(departure_station)
  arrival_time <- str_trim(arrival_time)
  arrival_station <- str_trim(arrival_station)
  duration <- str_trim(duration)
  price <- str_trim(price)
  
  ouigo <- append(ouigo, list(data.frame(
    departure_time,
    departure_station,
    arrival_time,
    arrival_station,
    duration,
    price,
    "train_company" = "Ouigo",
    departure_date = date_30,
    scrapping_date = today,
    scrapping_web = "Ouigo",
    stringsAsFactors = FALSE
  )))
}

ouigo <- do.call(rbind, ouigo)

ouigo <- ouigo |> 
  mutate(
    departure_station = str_replace(departure_station, "\\s*-.*", ""),  
    arrival_station = str_replace(arrival_station, "\\s*-.*", "")  
  ) |> 
  mutate(price = str_remove(price, "€") %>% as.numeric()) |> 
  rename(duration_raw = duration) |>  
  mutate(duration_min_clean = as.numeric(str_extract(duration_raw, "\\d+(?=h)")) * 60 +  
           as.numeric(str_extract(duration_raw, "(?<=h)\\d+"))) |> 
  relocate(duration_min_clean, .after = duration_raw)

View(ouigo)

file_path <- here::here("Scrapped data", "ouigo.csv")

if (!file.exists(file_path)) {
  write_csv(ouigo, file_path)  
} else {
  write_csv(ouigo, file_path, append = TRUE)  
}

#Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.
remDr$close() 
        