###Today+1

rm(list = ls())
library(RSelenium)
library(stringr)
library(dplyr)
library(readr)
library(here)
today <- Sys.Date()
scraping_date <- format(today, "%Y%m%d")  
search_date <- today + 1  
formatted_search_date <- format(search_date, "%Y-%m-%d")  

departure_station <- "Madrid"
arrival_station <- "Alicante"

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
destination_input$sendKeysToElement(list("Alicante"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("^ALICANTE/ALACANT$", option$getElementText()[[1]])) {
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

# save data
file_path <- file.path(here(), "Renfe.csv")

if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
}
remDr$close()

View(train_df)

###########################################################

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
arrival_station <- "Alicante"

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
destination_input$sendKeysToElement(list("Alicante"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("^ALICANTE/ALACANT$", option$getElementText()[[1]])) {
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

# save data
file_path <- file.path(here(), "Renfe.csv")

if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
}
remDr$close()

View(train_df)

###########################################################

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
arrival_station <- "Alicante"

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
destination_input$sendKeysToElement(list("Alicante"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("^ALICANTE/ALACANT$", option$getElementText()[[1]])) {
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

# save data
file_path <- file.path(here(), "Renfe.csv")

if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
}

remDr$close()

View(train_df)



###########################################################

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
arrival_station <- "Alicante"

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
destination_input$sendKeysToElement(list("Alicante"))
Sys.sleep(2)

suggestions <- remDr$findElements(using = "css selector", "ul li")
for (option in suggestions) {
  if (grepl("^ALICANTE/ALACANT$", option$getElementText()[[1]])) {
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

# save data
file_path <- file.path(here(), "Renfe.csv")

if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
}

remDr$close()

View(train_df)