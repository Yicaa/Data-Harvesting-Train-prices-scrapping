###Today+1

rm(list = ls())
library(RSelenium)
library(stringr)
library(dplyr)
library(readr)
library(here)


#extract date
today <- Sys.Date() #gets the current date
scraping_date <- format(today, "%Y%m%d") #scraping_date formats today's date as YYYYMMDD (used for tracking the scraping date). 
search_date <- today + 1   #sets the target date (tomorrow).
formatted_search_date <- format(search_date, "%Y-%m-%d")  #converts the target date to YYYY-MM-DD format for input in the website.

departure_station <- "Madrid"
arrival_station <- "Barcelona" ##Stores the departure (Madrid) and arrival (Barcelona) station names as variables.

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "firefox"
)
remDr$open()
Sys.sleep(5) #Initializes a remote Selenium WebDriver session to automate Firefox.
#remDr$open() starts the browser.
#Sys.sleep(5) gives the browser time to fully load.

remDr$navigate("https://www.renfe.com/")
Sys.sleep(5)  #Directs the browser to Renfe’s website.
#Sys.sleep(5) allows the page to load.

# accept Cookie
cookie_btn <- tryCatch(
  remDr$findElement(using = "id", value = "onetrust-accept-btn-handler"),
  error = function(e) NULL
)
if (!is.null(cookie_btn)) {
  cookie_btn$clickElement()
  Sys.sleep(2)}    #Tries to locate and click the "Accept Cookies" button.
#If the button is not found, it avoids errors by returning NULL.

# origin station
origin_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu origen']")
origin_input$clickElement()
origin_input$clearElement()
origin_input$sendKeysToElement(list("Madrid"))
Sys.sleep(2) #Finds the input field for the departure station.
#Clicks, clears, and enters "Madrid".

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("MADRID", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }          #Finds all dropdown options and clicks the one containing "MADRID".
}
Sys.sleep(1)

# destination
destination_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu destino']")
destination_input$clickElement()
destination_input$clearElement()
destination_input$sendKeysToElement(list("Barcelona"))  #Similar to the departure station, but enters "Barcelona".
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("BARCELONA", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1) #Finds and selects "BARCELONA" from the dropdown.

# single way
date_button <- remDr$findElement(using = "xpath", "//label[contains(text(),'Fecha ida')]")
date_button$clickElement()
Sys.sleep(2)  #Clicks the departure date field.

solo_ida_btn <- remDr$findElement(using = "xpath", "//*[contains(translate(text(),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'viaje solo ida')]")
solo_ida_btn$clickElement()
Sys.sleep(2)   #Selects the "one-way" travel option.
# next month or not
select_date_smart <- function(target_date) {
  Sys.sleep(2)
  # Function to get the currently displayed month and year
  get_current_month_year <- function() {
    for (i in 1:5) {  # Retry up to 5 times in case of failure
      tryCatch({
        current_month <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-months")$getElementAttribute("value"))
        current_year <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-years")$getElementAttribute("value"))
        return(list(month = current_month, year = current_year))
      }, error = function(e) {
        Sys.sleep(2)  ## Wait and retry if an error occurs
      })
    }
  }
  # Convert target date into month and year 
  target_month <- as.numeric(format(target_date, "%m")) - 1
  target_year <- as.numeric(format(target_date, "%Y"))
  # Get the currently displayed month and year
  current_date_info <- get_current_month_year()
  current_month <- current_date_info$month
  current_year <- current_date_info$year
  # Click "Next Month" button until the correct month and year are displayed 
  while (current_year < target_year || (current_year == target_year && current_month < target_month)) {
    tryCatch({
      next_month_button <- remDr$findElement(using = "css selector", "button.lightpick__next-action")
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(next_month_button))
      Sys.sleep(1)
      next_month_button$clickElement()
      Sys.sleep(2)
    }, error = function(e) {
      break # Exit the loop if the button is not found
    })
    # Update the currently displayed month and year after clicking  
    current_date_info <- get_current_month_year()
    current_month <- current_date_info$month
    current_year <- current_date_info$year
    Sys.sleep(1)
  }
}
# Call the function to select the date
select_date_smart(search_date)  


# choose date
for (i in 1:2) { # Retry up to 2 times in case of failures
  Sys.sleep(2)
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-available')]")
  
  for (date in dates) {
    date_text <- trimws(date$getElementText()[[1]]) # Get the date number as text
    
    if (date_text == format(search_date, "%d")) {# Match the target date
      Sys.sleep(1)
      
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(date))# Ensure date is visible
      Sys.sleep(1)
      tryCatch({
        date$clickElement() # Click the date
        Sys.sleep(1)
      }, error = function(e) {
        remDr$executeScript("arguments[0].click();", list(date))# Use JavaScript click as a fallback
        Sys.sleep(1)
      })
      
      Sys.sleep(2)
      break  # Exit the loop once the correct date is clicked
    }
  }
}

# confirm date
accept_btn <- remDr$findElement(using = "xpath", "//button[contains(text(),'Aceptar')]")
accept_btn$clickElement()
Sys.sleep(2)  #Clicks the "Accept" button to confirm the date selection.

for (i in 1:10) { # Retry up to 10 times to ensure the date selection is registered
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-start-date')]")
  
  if (length(dates) > 0) {
    selected_date <- dates[[1]]
    selected_text <- selected_date$getElementText()[[1]]
    
    if (selected_text == format(search_date, "%d")) {
      break  # Exit the loop if the correct date is selected
    }
  }
  Sys.sleep(1)
}
# Click the "Search" button
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(12)
# Click again if necessary
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(10)

# extract trains data
train_data <- list()
trains <- remDr$findElements(using = "css selector", ".row.selectedTren")
#Finds all train elements on the results page.
for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  #The script loops through each train element found on the webpage.#It attempts to extract the text content from each train’s section.
  #If there’s an error (e.g., an element is missing), it returns NA instead of stopping the script.
  
  departure_time <- str_extract(raw_text, "^\\d{2}:\\d{2}")
  
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)  
  #Uses regular expressions (regex) to extract time values in the HH:MM format.
  #Departure time is the first HH:MM found in the text. Arrival time is the last HH:MM found in the text, ensuring we capture the correct arrival time.  
  duration <- str_extract(raw_text, "\\d+ horas? \\d+ minutos?")
  #Looks for a pattern like "3 horas 45 minutos" to get the total travel time.
  price <- tryCatch({
    price_element <- train$findElement(using = "css selector", ".precio-final")
    if (!is.null(price_element)) {
      price_text <- price_element$getElementText()[[1]]
      str_extract(price_text, "\\d+,\\d+ €")  
    } else {
      NA
    }
  }, error = function(e) NA)
  #The script tries to locate the price element using the CSS class .precio-final.
  #If the element exists, it extracts the price value (e.g., "45,99 €").If the price element is missing, it stores NA instead. 
  train_data <- append(train_data, list(data.frame(
    departure_time, departure_station, arrival_time,arrival_station, price,duration, stringsAsFactors = FALSE
  )))
}  #The extracted train details are stored as a data frame and appended to a list for further processing.


train_company_js <- remDr$executeScript("
  return Array.from(document.querySelectorAll('.trenes-enlaces img'))
         .map(img => img.getAttribute('alt'));
")#This JavaScript code runs inside the browser to find all train company logos.
#It extracts the "alt" text from <img> elements, which usually contains the train company name.
train_company_vector <- unlist(train_company_js)
train_company_vector <- gsub("Imagen de Tren. Tipo de tren ", "", train_company_vector)
#Removes extra text ("Imagen de Tren. Tipo de tren") from the extracted names to keep only the company name.
train_df <- do.call(rbind, train_data)
#Combines all individual train data frames into one large data frame.
if (length(train_company_vector) == nrow(train_df)) {
  train_df$`train_company` <- train_company_vector  
} else {
  print("Textraction failed!")
}
#Ensures that train company names align with the extracted train data.
#If the number of train companies doesn’t match the number of train entries, it prints an error message.
colnames(train_df) <- gsub("\\.", " ", colnames(train_df))

train_df <- train_df %>%
  mutate(
    duration_hour = as.numeric(str_extract(duration, "^\\d+")),
    duration_min = as.numeric(str_extract(duration, "\\d+(?=\\s*minutos?)")),
    duration_min_clean = duration_hour * 60 + ifelse(is.na(duration_min), 0, duration_min),
    duration_raw = paste0(duration_hour, "h", sprintf("%02d", ifelse(is.na(duration_min), 0, duration_min))),
    price = as.numeric(gsub("€", "", gsub(",", ".", as.character(price)))),
    departure_date = formatted_search_date,
    scrapping_date = as.Date(scraping_date, format = "%Y%m%d"),
    scrapping_web = "Renfe"
  ) %>%
  select(departure_time, departure_station, arrival_time, arrival_station,
         duration_raw, duration_min_clean, price, train_company,
         departure_date, scrapping_date, scrapping_web)
#Duration Processing: Extracts hours and minutes separately.Converts total duration into minutes (duration_min_clean).Creates a formatted version like "3h45" (duration_raw).
#Price Processing:Converts price from text to numeric format by removing € and replacing , with ..
#Adding Metadata:
#departure_date: The searched date.
#scrapping_date: The date when the data was scraped.
#scrapping_web: "Renfe", indicating the data source.

# create file path and file name
here::here()
dir.create(here::here("Scrapped data"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(here::here("Scrapped data"))) {
  print("Directory does not exist!")
} else {
  print("Directory exists.")
}
file_path <- here::here("Scrapped data", "Renfe.csv")#Constructs the file path using here(), which ensures the file is saved in the current project directory.

# save data
if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
} #If the file doesn’t exist, it creates a new CSV file. If the file already exists, it appends new data to it.

remDr$close() #Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.

###########################################################
###!!!The date is seven days later, and all other operations remain the same as for one day later.!!!

### Today+7
rm(list = ls())
library(RSelenium)
library(stringr)
library(dplyr)
library(readr)
library(here)
today <- Sys.Date()
scraping_date <- format(today, "%Y%m%d")  
search_date <- today + 7  
formatted_search_date <- format(search_date, "%Y-%m-%d")  

departure_station <- "Madrid"
arrival_station <- "Barcelona"

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "firefox"
)
remDr$open()
Sys.sleep(5)

remDr$navigate("https://www.renfe.com/")
Sys.sleep(5)

# accept Cookie
cookie_btn <- tryCatch(
  remDr$findElement(using = "id", value = "onetrust-accept-btn-handler"),
  error = function(e) NULL
)
if (!is.null(cookie_btn)) {
  cookie_btn$clickElement()
  Sys.sleep(2)
}

# origin station
origin_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu origen']")
origin_input$clickElement()
origin_input$clearElement()
origin_input$sendKeysToElement(list("Madrid"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("MADRID", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1)

# destination
destination_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu destino']")
destination_input$clickElement()
destination_input$clearElement()
destination_input$sendKeysToElement(list("Barcelona"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("BARCELONA", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1)

# single way
date_button <- remDr$findElement(using = "xpath", "//label[contains(text(),'Fecha ida')]")
date_button$clickElement()
Sys.sleep(2)

solo_ida_btn <- remDr$findElement(using = "xpath", "//*[contains(translate(text(),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'viaje solo ida')]")
solo_ida_btn$clickElement()
Sys.sleep(2)
# next month or not
select_date_smart <- function(target_date) {
  Sys.sleep(2)
  
  get_current_month_year <- function() {
    for (i in 1:5) {
      tryCatch({
        current_month <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-months")$getElementAttribute("value"))
        current_year <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-years")$getElementAttribute("value"))
        return(list(month = current_month, year = current_year))
      }, error = function(e) {
        Sys.sleep(2)
      })
    }
  }
  
  target_month <- as.numeric(format(target_date, "%m")) - 1
  target_year <- as.numeric(format(target_date, "%Y"))
  
  current_date_info <- get_current_month_year()
  current_month <- current_date_info$month
  current_year <- current_date_info$year
  
  while (current_year < target_year || (current_year == target_year && current_month < target_month)) {
    tryCatch({
      next_month_button <- remDr$findElement(using = "css selector", "button.lightpick__next-action")
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(next_month_button))
      Sys.sleep(1)
      next_month_button$clickElement()
      Sys.sleep(2)
    }, error = function(e) {
      break
    })
    
    current_date_info <- get_current_month_year()
    current_month <- current_date_info$month
    current_year <- current_date_info$year
    Sys.sleep(1)
  }
}

select_date_smart(search_date)


# choose date
for (i in 1:2) { 
  Sys.sleep(2)
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-available')]")
  
  for (date in dates) {
    date_text <- trimws(date$getElementText()[[1]])
    
    if (date_text == format(search_date, "%d")) {
      Sys.sleep(1)
      
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(date))
      Sys.sleep(1)
      tryCatch({
        date$clickElement()
        Sys.sleep(1)
      }, error = function(e) {
        remDr$executeScript("arguments[0].click();", list(date))
        Sys.sleep(1)
      })
      
      Sys.sleep(2)
      break  
    }
  }
}

# confirm date
accept_btn <- remDr$findElement(using = "xpath", "//button[contains(text(),'Aceptar')]")
accept_btn$clickElement()
Sys.sleep(2)

for (i in 1:10) {
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-start-date')]")
  
  if (length(dates) > 0) {
    selected_date <- dates[[1]]
    selected_text <- selected_date$getElementText()[[1]]
    
    if (selected_text == format(search_date, "%d")) {
      break  
    }
  }
  Sys.sleep(1)
}
# search 
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(12)
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(10)

# extract trains data
train_data <- list()
trains <- remDr$findElements(using = "css selector", ".row.selectedTren")

for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  departure_time <- str_extract(raw_text, "^\\d{2}:\\d{2}")
  
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)  
  
  duration <- str_extract(raw_text, "\\d+ horas? \\d+ minutos?")
  
  price <- tryCatch({
    price_element <- train$findElement(using = "css selector", ".precio-final")
    if (!is.null(price_element)) {
      price_text <- price_element$getElementText()[[1]]
      str_extract(price_text, "\\d+,\\d+ €")  
    } else {
      NA
    }
  }, error = function(e) NA)
  
  train_data <- append(train_data, list(data.frame(
    departure_time, departure_station, arrival_time,arrival_station, price,duration, stringsAsFactors = FALSE
  )))
}

train_company_js <- remDr$executeScript("
  return Array.from(document.querySelectorAll('.trenes-enlaces img'))
         .map(img => img.getAttribute('alt'));
")
train_company_vector <- unlist(train_company_js)
train_company_vector <- gsub("Imagen de Tren. Tipo de tren ", "", train_company_vector)

train_df <- do.call(rbind, train_data)

if (length(train_company_vector) == nrow(train_df)) {
  train_df$`train_company` <- train_company_vector  
} else {
  print("Textraction failed!")
}

colnames(train_df) <- gsub("\\.", " ", colnames(train_df))

train_df <- train_df %>%
  mutate(
    duration_hour = as.numeric(str_extract(duration, "^\\d+")),
    duration_min = as.numeric(str_extract(duration, "\\d+(?=\\s*minutos?)")),
    duration_min_clean = duration_hour * 60 + ifelse(is.na(duration_min), 0, duration_min),
    duration_raw = paste0(duration_hour, "h", sprintf("%02d", ifelse(is.na(duration_min), 0, duration_min))),
    price = as.numeric(gsub("€", "", gsub(",", ".", as.character(price)))),
    departure_date = formatted_search_date,
    scrapping_date = as.Date(scraping_date, format = "%Y%m%d"),
    scrapping_web = "Renfe"
  ) %>%
  select(departure_time, departure_station, arrival_time, arrival_station,
         duration_raw, duration_min_clean, price, train_company,
         departure_date, scrapping_date, scrapping_web)

# create file path and file name
here::here()
dir.create(here::here("Scrapped data"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(here::here("Scrapped data"))) {
  print("Directory does not exist!")
} else {
  print("Directory exists.")
}
file_path <- here::here("Scrapped data", "Renfe.csv")#Constructs the file path using here(), which ensures the file is saved in the current project directory.

# save data
if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
} #If the file doesn’t exist, it creates a new CSV file. If the file already exists, it appends new data to it.

remDr$close() #Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.


###########################################################
###!!!The date is 14 days later, and all other operations remain the same as for one day later.!!!

### Today+14
rm(list = ls())
library(RSelenium)
library(stringr)
library(dplyr)
library(readr)
library(here)
today <- Sys.Date()
scraping_date <- format(today, "%Y%m%d")  
search_date <- today + 14  
formatted_search_date <- format(search_date, "%Y-%m-%d")  

departure_station <- "Madrid"
arrival_station <- "Barcelona"

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "firefox"
)
remDr$open()
Sys.sleep(5)

remDr$navigate("https://www.renfe.com/")
Sys.sleep(5)

# accept Cookie
cookie_btn <- tryCatch(
  remDr$findElement(using = "id", value = "onetrust-accept-btn-handler"),
  error = function(e) NULL
)
if (!is.null(cookie_btn)) {
  cookie_btn$clickElement()
  Sys.sleep(2)
}

# origin station
origin_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu origen']")
origin_input$clickElement()
origin_input$clearElement()
origin_input$sendKeysToElement(list("Madrid"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("MADRID", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1)

# destination
destination_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu destino']")
destination_input$clickElement()
destination_input$clearElement()
destination_input$sendKeysToElement(list("Barcelona"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("BARCELONA", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1)

# single way
date_button <- remDr$findElement(using = "xpath", "//label[contains(text(),'Fecha ida')]")
date_button$clickElement()
Sys.sleep(2)

solo_ida_btn <- remDr$findElement(using = "xpath", "//*[contains(translate(text(),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'viaje solo ida')]")
solo_ida_btn$clickElement()
Sys.sleep(2)
# next month or not
select_date_smart <- function(target_date) {
  Sys.sleep(2)
  
  get_current_month_year <- function() {
    for (i in 1:5) {
      tryCatch({
        current_month <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-months")$getElementAttribute("value"))
        current_year <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-years")$getElementAttribute("value"))
        return(list(month = current_month, year = current_year))
      }, error = function(e) {
        Sys.sleep(2)
      })
    }
  }
  
  target_month <- as.numeric(format(target_date, "%m")) - 1
  target_year <- as.numeric(format(target_date, "%Y"))
  
  current_date_info <- get_current_month_year()
  current_month <- current_date_info$month
  current_year <- current_date_info$year
  
  while (current_year < target_year || (current_year == target_year && current_month < target_month)) {
    tryCatch({
      next_month_button <- remDr$findElement(using = "css selector", "button.lightpick__next-action")
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(next_month_button))
      Sys.sleep(1)
      next_month_button$clickElement()
      Sys.sleep(2)
    }, error = function(e) {
      break
    })
    
    current_date_info <- get_current_month_year()
    current_month <- current_date_info$month
    current_year <- current_date_info$year
    Sys.sleep(1)
  }
}

select_date_smart(search_date)


# choose date
for (i in 1:2) { 
  Sys.sleep(2)
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-available')]")
  
  for (date in dates) {
    date_text <- trimws(date$getElementText()[[1]])
    
    if (date_text == format(search_date, "%d")) {
      Sys.sleep(1)
      
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(date))
      Sys.sleep(1)
      tryCatch({
        date$clickElement()
        Sys.sleep(1)
      }, error = function(e) {
        remDr$executeScript("arguments[0].click();", list(date))
        Sys.sleep(1)
      })
      
      Sys.sleep(2)
      break  
    }
  }
}


# confirm date
accept_btn <- remDr$findElement(using = "xpath", "//button[contains(text(),'Aceptar')]")
accept_btn$clickElement()
Sys.sleep(2)

for (i in 1:10) {
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-start-date')]")
  
  if (length(dates) > 0) {
    selected_date <- dates[[1]]
    selected_text <- selected_date$getElementText()[[1]]
    
    if (selected_text == format(search_date, "%d")) {
      break  
    }
  }
  Sys.sleep(1)
}
# search 
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(12)
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(10)

# extract trains data
train_data <- list()
trains <- remDr$findElements(using = "css selector", ".row.selectedTren")

for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  departure_time <- str_extract(raw_text, "^\\d{2}:\\d{2}")
  
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)  
  
  duration <- str_extract(raw_text, "\\d+ horas? \\d+ minutos?")
  
  price <- tryCatch({
    price_element <- train$findElement(using = "css selector", ".precio-final")
    if (!is.null(price_element)) {
      price_text <- price_element$getElementText()[[1]]
      str_extract(price_text, "\\d+,\\d+ €")  
    } else {
      NA
    }
  }, error = function(e) NA)
  
  train_data <- append(train_data, list(data.frame(
    departure_time, departure_station, arrival_time,arrival_station, price,duration, stringsAsFactors = FALSE
  )))
}

train_company_js <- remDr$executeScript("
  return Array.from(document.querySelectorAll('.trenes-enlaces img'))
         .map(img => img.getAttribute('alt'));
")
train_company_vector <- unlist(train_company_js)
train_company_vector <- gsub("Imagen de Tren. Tipo de tren ", "", train_company_vector)

train_df <- do.call(rbind, train_data)

if (length(train_company_vector) == nrow(train_df)) {
  train_df$`train_company` <- train_company_vector  
} else {
  print("Textraction failed!")
}

colnames(train_df) <- gsub("\\.", " ", colnames(train_df))

train_df <- train_df %>%
  mutate(
    duration_hour = as.numeric(str_extract(duration, "^\\d+")),
    duration_min = as.numeric(str_extract(duration, "\\d+(?=\\s*minutos?)")),
    duration_min_clean = duration_hour * 60 + ifelse(is.na(duration_min), 0, duration_min),
    duration_raw = paste0(duration_hour, "h", sprintf("%02d", ifelse(is.na(duration_min), 0, duration_min))),
    price = as.numeric(gsub("€", "", gsub(",", ".", as.character(price)))),
    departure_date = formatted_search_date,
    scrapping_date = as.Date(scraping_date, format = "%Y%m%d"),
    scrapping_web = "Renfe"
  ) %>%
  select(departure_time, departure_station, arrival_time, arrival_station,
         duration_raw, duration_min_clean, price, train_company,
         departure_date, scrapping_date, scrapping_web)

# create file path and file name
here::here()
dir.create(here::here("Scrapped data"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(here::here("Scrapped data"))) {
  print("Directory does not exist!")
} else {
  print("Directory exists.")
}
file_path <- here::here("Scrapped data", "Renfe.csv")#Constructs the file path using here(), which ensures the file is saved in the current project directory.

# save data
if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
} #If the file doesn’t exist, it creates a new CSV file. If the file already exists, it appends new data to it.

remDr$close() #Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.


###########################################################
###!!!The date is 30 days later, and all other operations remain the same as for one day later.!!!

### Today+30
rm(list = ls())
library(RSelenium)
library(stringr)
library(dplyr)
library(readr)
library(here)
today <- Sys.Date()
scraping_date <- format(today, "%Y%m%d")  
search_date <- today + 30  

formatted_search_date <- format(search_date, "%Y-%m-%d")  

departure_station <- "Madrid"
arrival_station <- "Barcelona"

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "firefox"
)
remDr$open()
Sys.sleep(5)

remDr$navigate("https://www.renfe.com/")
Sys.sleep(5)

# accept Cookie*
cookie_btn <- tryCatch(
  remDr$findElement(using = "id", value = "onetrust-accept-btn-handler"),
  error = function(e) NULL
)
if (!is.null(cookie_btn)) {
  cookie_btn$clickElement()
  Sys.sleep(2)
}

# origin station
origin_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu origen']")
origin_input$clickElement()
origin_input$clearElement()
origin_input$sendKeysToElement(list("Madrid"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("MADRID", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1)

# destination
destination_input <- remDr$findElement(using = "css selector", "input[placeholder='Selecciona tu destino']")
destination_input$clickElement()
destination_input$clearElement()
destination_input$sendKeysToElement(list("Barcelona"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("BARCELONA", option$getElementText()[[1]])) {
    option$clickElement()
    break
  }
}
Sys.sleep(1)

# single way
date_button <- remDr$findElement(using = "xpath", "//label[contains(text(),'Fecha ida')]")
date_button$clickElement()
Sys.sleep(2)

solo_ida_btn <- remDr$findElement(using = "xpath", "//*[contains(translate(text(),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'viaje solo ida')]")
solo_ida_btn$clickElement()
Sys.sleep(2)
# next month or not
select_date_smart <- function(target_date) {
  Sys.sleep(2)
  
  get_current_month_year <- function() {
    for (i in 1:5) {
      tryCatch({
        current_month <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-months")$getElementAttribute("value"))
        current_year <- as.numeric(remDr$findElement(using = "css selector", ".lightpick__select-years")$getElementAttribute("value"))
        return(list(month = current_month, year = current_year))
      }, error = function(e) {
        Sys.sleep(2)
      })
    }
  }
  
  target_month <- as.numeric(format(target_date, "%m")) - 1
  target_year <- as.numeric(format(target_date, "%Y"))
  
  current_date_info <- get_current_month_year()
  current_month <- current_date_info$month
  current_year <- current_date_info$year
  
  while (current_year < target_year || (current_year == target_year && current_month < target_month)) {
    tryCatch({
      next_month_button <- remDr$findElement(using = "css selector", "button.lightpick__next-action")
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(next_month_button))
      Sys.sleep(1)
      next_month_button$clickElement()
      Sys.sleep(2)
    }, error = function(e) {
      break
    })
    
    current_date_info <- get_current_month_year()
    current_month <- current_date_info$month
    current_year <- current_date_info$year
    Sys.sleep(1)
  }
}

select_date_smart(search_date)

Sys.sleep(2)
# choose date  

for (i in 1:2) { 
  Sys.sleep(2)
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-available')]")
  
  for (date in dates) {
    date_text <- trimws(date$getElementText()[[1]])
    
    if (date_text == format(search_date, "%d")) {
      Sys.sleep(1)
      
      remDr$executeScript("arguments[0].scrollIntoView(true);", list(date))
      Sys.sleep(1)
      tryCatch({
        date$clickElement()
        Sys.sleep(1)
      }, error = function(e) {
        remDr$executeScript("arguments[0].click();", list(date))
        Sys.sleep(1)
      })
      
      Sys.sleep(2)
      break  
    }
  }
}


# confirm date
accept_btn <- remDr$findElement(using = "xpath", "//button[contains(text(),'Aceptar')]")
accept_btn$clickElement()

Sys.sleep(2)

for (i in 1:10) {
  dates <- remDr$findElements(using = "xpath", "//div[contains(@class, 'lightpick__day is-start-date')]")
  
  if (length(dates) > 0) {
    selected_date <- dates[[1]]
    selected_text <- selected_date$getElementText()[[1]]
    
    if (selected_text == format(search_date, "%d")) {
      break  
    }
  }
  Sys.sleep(1)
}

# search 
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(12)
search_button <- remDr$findElement(using = "css selector", ".sc-rf-button")
search_button$clickElement()
Sys.sleep(10)
# extract trains data
train_data <- list()
trains <- remDr$findElements(using = "css selector", ".row.selectedTren")

for (train in trains) {
  
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)
  
  departure_time <- str_extract(raw_text, "^\\d{2}:\\d{2}")
  
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)  
  
  duration <- str_extract(raw_text, "\\d+ horas? \\d+ minutos?")
  
  price <- tryCatch({
    price_element <- train$findElement(using = "css selector", ".precio-final")
    if (!is.null(price_element)) {
      price_text <- price_element$getElementText()[[1]]
      str_extract(price_text, "\\d+,\\d+ €")  
    } else {
      NA
    }
  }, error = function(e) NA)
  
  train_data <- append(train_data, list(data.frame(
    departure_time, departure_station, arrival_time,arrival_station, price,duration, stringsAsFactors = FALSE
  )))
}

train_company_js <- remDr$executeScript("
  return Array.from(document.querySelectorAll('.trenes-enlaces img'))
         .map(img => img.getAttribute('alt'));
")
train_company_vector <- unlist(train_company_js)
train_company_vector <- gsub("Imagen de Tren. Tipo de tren ", "", train_company_vector)

train_df <- do.call(rbind, train_data)

if (length(train_company_vector) == nrow(train_df)) {
  train_df$`train_company` <- train_company_vector  
} else {
  print("Textraction failed!")
}

colnames(train_df) <- gsub("\\.", " ", colnames(train_df))

train_df <- train_df %>%
  mutate(
    duration_hour = as.numeric(str_extract(duration, "^\\d+")),
    duration_min = as.numeric(str_extract(duration, "\\d+(?=\\s*minutos?)")),
    duration_min_clean = duration_hour * 60 + ifelse(is.na(duration_min), 0, duration_min),
    duration_raw = paste0(duration_hour, "h", sprintf("%02d", ifelse(is.na(duration_min), 0, duration_min))),
    price = as.numeric(gsub("€", "", gsub(",", ".", as.character(price)))),
    departure_date = formatted_search_date,
    scrapping_date = as.Date(scraping_date, format = "%Y%m%d"),
    scrapping_web = "Renfe"
  ) %>%
  select(departure_time, departure_station, arrival_time, arrival_station,
         duration_raw, duration_min_clean, price, train_company,
         departure_date, scrapping_date, scrapping_web)

# create file path and file name
here::here()
dir.create(here::here("Scrapped data"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(here::here("Scrapped data"))) {
  print("Directory does not exist!")
} else {
  print("Directory exists.")
}
file_path <- here::here("Scrapped data", "Renfe.csv")#Constructs the file path using here(), which ensures the file is saved in the current project directory.

# save data
if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
} #If the file doesn’t exist, it creates a new CSV file. If the file already exists, it appends new data to it.

remDr$close() #Closes the Selenium browser session, ensuring no unnecessary browser instances remain open. And ensure not be error.




