# docker stop selenium
# docker rm selenium

#docker run -d --name selenium -p 4444:4444 -p 7900:7900 seleniarm/standalone-firefox:4.8.1


###########
rm(list = ls())
library(RSelenium)
library(here)
library(lubridate)
library(readr)
library(tidyverse)

# Calculate dates
today <- Sys.Date()
date_1 <- today + 1
date_7 <- today + 7
date_14 <- today + 14
date_30 <- today + 30


##########################################################
# name for csv which will be saved
scraping_date <- format(Sys.Date(), "%Y%m%d")

# set other variables
search_date <- date_7  
departure_city <- "Madrid Todas"
arrival_city <- "Zaragoza Todas"


# create file path and file name
here::here()
dir.create(here::here("Scrapped data"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(here::here("Scrapped data"))) {
  print("Directory does not exist!")
} else {
  print("Directory exists.")
}
file_path <- here::here("Scrapped data", "trainline.csv")


#########################################################


remDr <- remoteDriver(
  remoteServerAddr = "localhost",  # Mac 和 Windows 一样
  port = 4444,
  browserName = "firefox"
)
remDr$open()
Sys.sleep(10)



# Select the date
select_date_smart <- function(target_date) {
  # 点击时间选择框，展开日历
  # Click on the time selection input to open the calendar
  timeInput <- remDr$findElement(using = "id", value = "jsf-outbound-time-input-toggle")
  remDr$executeScript("arguments[0].click();", list(timeInput))
  Sys.sleep(2)  # 等待日历加载 / Ensure the calendar is fully loaded
  
  # 检测目标日期是否在下个月
  # Check if the target date is in the next month
  current_month <- as.numeric(format(Sys.Date(), "%m"))
  target_month <- as.numeric(format(target_date, "%m"))
  
  if (target_month > current_month) {
    # 如果是，就点击“下个月”按钮
    # If yes, click the "next month" button
    next_month_button <- remDr$findElement(using = "css selector", "button[data-testid='calendar-navigate-to-next-month']")
    remDr$executeScript("arguments[0].click();", list(next_month_button))
    Sys.sleep(2)  # 等待日历加载 / Wait for the calendar to update
  }
  
  # 选择目标日期
  # Select the target date
  day <- format(target_date, "%d")
  xpath <- paste0("//div[@class='Vvr0iqcCcX3JhCb7TaVp' and text()='", day, "']")
  dateElement <- remDr$findElement(using = "xpath", value = xpath)
  remDr$executeScript("arguments[0].scrollIntoView(); arguments[0].click();", list(dateElement))
  Sys.sleep(1)  # 等待选择完成 / Allow time for selection to take effect
}


##################### Open the web: TRAINLINE #########################

remDr$navigate("https://www.thetrainline.com/es")
Sys.sleep(5)

##################### Accept cookie policy ###########################
webElem <- remDr$findElement(using = "xpath", value = "//*[@id='onetrust-accept-btn-handler']")
remDr$executeScript("arguments[0].click();", list(webElem))
Sys.sleep(2)

###################### Departure city ##########################################  
# 1️⃣ 点击输入框，确保下拉列表出现
# 1️⃣ Click the input box to ensure the dropdown list appears
remDr$executeScript("document.getElementById('jsf-origin-input').click();")
Sys.sleep(2)
# 2️⃣ 触发 focus 事件，确保输入框激活
# 2️⃣ Trigger the focus event to ensure the input box is active
remDr$executeScript("document.getElementById('jsf-origin-input').focus();")
Sys.sleep(2)
# 3️⃣ 直接输入departure_city 并等待高亮
# 3️⃣ Directly input departure_city and wait for highlighting
origin_box <- remDr$findElement(using = "id", value = "jsf-origin-input")
origin_box$sendKeysToElement(list(departure_city))
Sys.sleep(1.5)  # 等待下拉列表加载

# 4️⃣ 直接按 Enter 键选中当前高亮项
# 4️⃣ Press the Enter key to select the currently highlighted item
origin_box$sendKeysToElement(list(key = "enter"))
Sys.sleep(2)

###################### Arrival city ########################################## 
destination_box <- remDr$findElement(using = "id", value = "jsf-destination-input")
remDr$executeScript("arguments[0].click();", list(destination_box))  # 强制点击
Sys.sleep(0.5)
destination_box$sendKeysToElement(list(arrival_city))
Sys.sleep(1.5)  
destination_box$sendKeysToElement(list(key = "enter"))
Sys.sleep(2)

##################### Avoid select "Buscar alojamientos en booking" #####################
remDr$executeScript("document.getElementById('bookingPromo').click();")
Sys.sleep(2)





###################### Select the date ########################################## 


#  Select the date 
select_date_smart(search_date)
Sys.sleep(2)

# Select departure hour 1am
select_departure_time <- function(hour = "01") {
  script <- "
        let select = document.getElementById('jsf-outbound-time-time-picker-hour');
        select.value = arguments[0].padStart(2, '0'); 
        select.dispatchEvent(new Event('change', { bubbles: true }));
    "
  remDr$executeScript(script, list(hour))
  Sys.sleep(1)
}

select_departure_time("01")

Sys.sleep(2)


# Search (Buscar billetes baratos)
remDr$executeScript("document.evaluate(\"//button[span[text()='Buscar billetes baratos']]\", document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click();")
Sys.sleep(10)

# Click on "Más tarde" until all trains appear
# We don't use "Más temprano" because it can lead to show trains in a day before
# And we set a early departure hour (like 1am) to solve this problem and avoiding missing early trains
click_until_disappear <- function(button_text) {
  repeat {
    # 尝试查找按钮
    # Try to find the button
    button <- tryCatch(
      remDr$findElement(using = "xpath", paste0("//span[text()='", button_text, "']/ancestor::button")),
      error = function(e) NULL  # 找不到按钮时返回 NULL / Return NULL if button is not found
    )
    
    # 如果按钮不存在，退出循环
    # If the button does not exist, exit the loop
    if (is.null(button)) {
      message("No more '", button_text, "' button found. Stopping...")
      break
    }
    
    # 使用 JavaScript 强制点击
    # Force-click using JavaScript
    remDr$executeScript("arguments[0].scrollIntoView(); arguments[0].click();", list(button))
    Sys.sleep(5)  # 等待加载新车次 / Wait for new train results to load
  }
}

click_until_disappear("Más tarde")



##########
library(rvest)
library(dplyr)
library(stringr)

page_source <- remDr$getPageSource()[[1]]
page <- read_html(page_source)

# 查找所有车次的 div（只取 wrapper）
train_rows <- page %>% html_nodes("div[data-test*='eu-journey-row'][data-test*='wrapper']")

# create df
train_data <- data.frame(
  Train_Number = character(),
  Departure_Time = character(),
  Arrival_Time = character(),
  Train_Company = character(),
  Price = character(),
  stringsAsFactors = FALSE
)

# go through all trains and scrape information
for (train in train_rows) {
  # departure_time
  departure_time <- train %>% html_nodes("time") %>% .[1] %>% html_text(trim = TRUE)
  if (length(departure_time) == 0) departure_time <- "N/A"
  
  # arrival_time
  arrival_time <- train %>% html_nodes("time") %>% .[2] %>% html_text(trim = TRUE)
  if (length(arrival_time) == 0) arrival_time <- "N/A"
  
  # train_company
  train_company <- train %>% html_node("svg title") %>% html_text(trim = TRUE)
  if (is.na(train_company) || train_company == "" || is.null(train_company)) {
    train_company <- "N/A"
  }
  
  # price: the frist appreared one, the standard class price
  price <- train %>% html_nodes("[data-test*='standard-ticket-price']") %>% html_text(trim = TRUE)
  if (length(price) == 0) price <- "N/A"
  
  # train_number
  train_number <- train %>% html_attr("data-test")
  if (is.null(train_number)) train_number <- "N/A"
  
  # df
  train_data <- rbind(train_data, data.frame(
    Train_Number = train_number,
    Departure_Time = departure_time,
    Arrival_Time = arrival_time,
    Train_Company = train_company,
    Price = price,
    stringsAsFactors = FALSE
  ))
}

# delete repeated trains
train_data_clean <- train_data %>%
  distinct(Train_Number, .keep_all = TRUE)

# clean the price format
train_data_clean <- train_data_clean  |> 
  mutate(Price = str_replace_all(Price, "[^0-9.,]", ""))  |>   
  mutate(Price = str_replace_all(Price, ",", "."))  |>   
  mutate(Price = na_if(Price, ""))  |>   
  mutate(Price = as.numeric(Price))  |>   
  drop_na(Price) |> 
  mutate(departure_date = search_date) |> 
  mutate(Price = as.numeric(Price)) |> 
  mutate(scrapping_date = today) |> 
  rename(departure_time = Departure_Time,
         arrival_time = Arrival_Time,
         train_company = Train_Company,
         price=Price) |> 
  dplyr::select(-1) |> 
  mutate(departure_station = departure_city,
         arrival_station = arrival_city) |> 
  mutate(departure_station = str_remove(departure_station, " Todas$"),
         arrival_station = str_remove(arrival_station, " Todas$"),
         duration_raw= as.numeric(
           hms(paste0(arrival_time, ":00")) - hms(paste0(departure_time, ":00"))
         ) / 60,
         duration_min_clean = duration_raw) |> 
  mutate(train_company = case_when(
    train_company=="Renfe Avlo" ~ "AVLO",
    train_company=="Renfe Ave" ~ "AVE",
    train_company=="Renfe" ~ "Alvia",
    TRUE ~ train_company
  )) |> 
  mutate(
    departure_time = as.character(departure_time),
    arrival_time = as.character(arrival_time)) |> 
  mutate(scrapping_web="Trainline") |> 
  select(departure_time,
         departure_station, 
         arrival_time,
         arrival_station,
         duration_raw,
         duration_min_clean,
         price,
         train_company,
         departure_date,
         scrapping_date,
         scrapping_web)


# review the table
print(train_data_clean)

#Write the data
if (!file.exists(file_path)) {
  write_csv(train_data_clean, file_path)
} else {
  write_csv(train_data_clean, file_path, append = TRUE)
}



remDr$close()

