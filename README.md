---
editor_options: 
  markdown: 
    wrap: sentence
---

# 🚆 Data-Harvesting-Train-prices-scrapping

## Project Overview

The scraper runs for different departure dates (1 day, 7 days, 14 days, and 30 days before departure), allowing a comparative analysis of pricing trends across multiple timeframes.
This dataset enables the exploration of key questions such as:

1.  How ticket prices fluctuate over time before departure

2.  Price variations between different train operators

3.  The best time of the day to purchase the cheapest tickets

4.  Which route offers the best price-to-duration ratio

In practice, each team member is responsible for scraping train information from a different railway company, with each company covering three designated routes.
Since we collect data for four different departure dates per route, each company performs 12 scrapes per day.
Although the data structure is largely similar across different companies, we choose one specific dataset as an example to illustrate our scraping process more clearly.

The three core railway routes covered in this project are:

Madrid - Barcelona: One of the busiest high-speed rail routes in Spain, connecting the capital with Catalonia’s economic hub.
The journey takes approximately 2.5 to 3 hours.

Madrid - Valencia: An important route to the Mediterranean coast, with a travel time of around 1.5 to 2 hours, frequently used for both business and tourism.

Barcelona - Valencia: A key connection between two coastal cities, with a journey time of about 3 hours, making it ideal for analyzing fare strategies among different operators.

For our analysis, we will use the Madrid - Barcelona route, specifically for the departure date 14 days in advance, as an example.
This choice not only represents our data collection process but also provides valuable insights into how ticket prices fluctuate over a longer timeframe.

## Install Docker Desktop and Debug the Image System

To ensure stable operation across different computer systems and browser versions, this project requires you to download Docker Desktop and use a specific version of the container image. When creating this project, we used the latest available version at that time: Docker Desktop 4.38.0 (181591). Please download this version to ensure repeatability and consistency.

Next, open your computer's terminal:
- On macOS and Linux, this is called "Terminal."
- On Windows, this is called "cmd."

If you have previously run another container, enter the following commands to stop and remove it:
```sh
docker stop selenium
docker rm selenium
```
If you haven't run any containers before, you can skip this step.

### Debugging the Image System
To load the same container image we used:

- **For macOS (Apple Silicon, M1/M2/M3) users**, enter the following command in the terminal:
  ```sh
  docker run -d --name selenium -p 4444:4444 -p 7900:7900 seleniarm/standalone-firefox:4.8.1
  ```

- **For Windows and Linux (x86_64) users**, enter the following command:
  ```sh
  docker run -d --name selenium -p 4444:4444 -p 7900:7900 selenium/standalone-firefox:4.8.1
  ```

If you want to monitor the browser interactions inside the container while running the code, you can open [http://localhost:7900](http://localhost:7900) in your browser. If login credentials are required, use the default username/password: `secret`.

The code to connect RStudio to RSelenium and the container image is already included in each scraper script. This ensures that every time the script runs, it reconnects to the container and disconnects after execution, maintaining a stable connection with the image system.

## Creating an R Project
We recommend creating an R project before running the script. Copy the files and scripts from GitHub into the same directory. This helps avoid issues where the working directory does not reset properly after switching from another project, ensuring that daily scraped data is appended correctly.


## Trainline Train Ticket Scraper

### Prerequisites
Before running the script, make sure you have the following installed:
- R (latest version)
- RSelenium and its dependencies
- Firefox and geckodriver
- tidyverse for data manipulation


### Daily Script Execution
Inside the project folder, you will find 12 R script files prefixed with "trainline_". To streamline execution, you can run them in batches using the following terminal commands (replace the directory path with your own):
```sh
Rscript "/Users/linyijia/Desktop/MCSC/Data Harvesting/Final project/Data Harvesting Final Project/trainline_date_1_MAD_ALI.R"
sleep 10

docker stop selenium
docker rm selenium
docker run -d --name selenium -p 4444:4444 -p 7900:7900 seleniarm/standalone-firefox:4.8.1
sleep 10

Rscript "/Users/linyijia/Desktop/MCSC/Data Harvesting/Final project/Data Harvesting Final Project/trainline_date_1_MAD_BCN.R"
sleep 10

# Repeat the process for other scripts...
```

### Code Structure
All 12 scripts follow the same structure:
1. **Lines 1-42**: Initial setup (loading packages, calculating scrape dates, defining routes and dates, setting CSV file storage location).
2. **Lines 44-55**: Connecting to RSelenium.
3. **Lines 56-84**: Creating a custom function to check if the departure date falls in the next month, determine if the calendar needs to be navigated, and select the correct date.
4. **Lines 87-133**: Opening the Trainline website, accepting cookie policies, and selecting departure and destination stations.
5. **Lines 134-156**: Using the custom function to select the date and setting the departure time to 1 AM to capture all available train schedules for the entire day.
6. **Lines 157-187**: Clicking the search button and continuously clicking the "Más tarde" button until no more results appear, ensuring all schedules for the day are visible.
7. **Lines 191-276**: Extracting HTML data and cleaning the dataset.
8. **Lines 297-305**: Saving the data as a CSV file. If an existing CSV with the same name is detected, the new data is appended to it.
9. **Line 309**: Closing the browser inside the container to prepare for the next scraping session.

By following this structure, the scripts ensure efficient and reliable data collection.


## Ouigo Train Ticket Scraper

### Prerequisites

Before running the script, ensure you have the following installed: R (latest version), RSelenium and its dependencies, Firefox and geckodriver, and tidyverse for data manipulation

### 1.1 Install Required Packages

``` r
library(RSelenium)
library(tidyverse)
library(xml2)
library(jsonlite)
library(here)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
```

### 1.2 Setting Up Selenium Server

Create a remote driver to control a Firefox browser using Selenium

``` r
rD <- rsDriver(browser = "firefox", port = 4444L, chromever = NULL)
remDr <- rD$client
remDr = remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()
remDr$navigate("https://www.ouigo.com/es/")
```

### 1.3 Get the target date

The code today \<- Sys.Date() assigns the current system date to the variable today.

Then date_14 is what we want

``` r
# Get today's date
today <- Sys.Date()

# Define future dates based on today
date_1 <- today + 1
date_7 <- today + 7
date_14 <- today + 14
date_30 <- today + 30
dates <- as.character(c(today + 1, today + 7, today + 14, today + 30))
```

### 2.1 Get origin station and destination

The script automates the selection of the origin station by interacting with the search input field on the Ouigo website.
It first locates the input box using its ID selector, clicks to activate it, clears any pre-existing text, and then types the station name ("Madrid - Todas las estaciones").

```         
departure = remDr$findElement(using = "id", "origin-station-input-field__input")
departure$clickElement()

departure$clearElement()
departure$sendKeysToElement(list("Madrid - Todas las estaciones"))
```

Since Ouigo provides autocomplete suggestions, the script pauses briefly (Sys.sleep(2)) to allow the dropdown to load.
It then extracts all available station suggestions from the dropdown list and prints them for verification.
After retrieving the list, the script searches for the correct station name within the suggestions and clicks on it once found.

```         
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
```

### 2.2 Search target date

The code automatically selects the departure date by first clicking the date input field, then locating and clicking the corresponding date_14 option using XPath, with delays added to ensure stable execution.

```         
date_input <- remDr$findElement(using = "css selector", "input[id='search-engine__inputfield__outbound-date__input']")
date_input$clickElement()
Sys.sleep(3)  

for (date in date_1) {
  xpath_query <- sprintf("//li[@data-date='%s']", date_1)
  date_element <- remDr$findElement(using = "xpath", xpath_query)
  date_element$clickElement()
  Sys.sleep(2)  
}
```

### 2.3 Click search_button

```         
search_button <- remDr$findElement(using = "css selector", "button[title='Actualizar el viaje']")
search_button$clickElement()
```

### 3.1 Finding Train Journey Elements

```         
trains <- remDr$findElements(using = "css selector", "div[role='button'][data-testid^='e2e_journey-results_journeys_journey-item']")
```

Each train listing contains multiple pieces of information, which are then extracted.

### 3.2 Extracting Train Data

For each train journey, the script retrieves:

1.  Departure time

2.  Arrival time

3.  Departure and arrival stations

4.  Journey duration

5.  Ticket price

```         
for (train in trains) {
  raw_text <- tryCatch({
    train$getElementText()[[1]]
  }, error = function(e) NA)

  departure_time <- str_extract(raw_text, "^\\d{2}:\\d{2}")
  times <- str_extract_all(raw_text, "\\d{2}:\\d{2}")[[1]]
  arrival_time <- ifelse(length(times) > 1, times[length(times)], NA)

  departure_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)

  split_text <- str_split(departure_station, "\n")[[1]]
  departure_station <- ifelse(length(split_text) >= 2, split_text[2], NA)

  arrival_station <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)

  split_text <- str_split(arrival_station, "\n")[[1]]
  arrival_station <- ifelse(length(split_text) >= 4, split_text[4], NA)

  duration <- str_extract(raw_text, "\\d+h\\d+")

  price <- tryCatch({
    train$findElement(using = "css selector", "span")$getElementText()[[1]]
  }, error = function(e) NA)

  split_text <- str_split(price, "\n")[[1]]
  price <- ifelse(length(split_text) >= 5, split_text[5], NA)

  # Store extracted data
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
```

### 3.3 Data Cleaning & Transformation

Once data is extracted, the script cleans and processes it:

a.  Removes unwanted characters from station names

b.  Converts ticket prices to numeric values

c.  Standardizes journey duration (converts "XhYmin" to total minutes)

```         
ouigo <- do.call(rbind, ouigo) |> 
  mutate(
    departure_station = str_replace(departure_station, "\\s*-.*", ""),  
    arrival_station = str_replace(arrival_station, "\\s*-.*", "")  
  ) |> 
  mutate(price = str_remove(price, "€") %>% as.numeric()) |> 
  rename(duration_raw = duration) |>  
  mutate(duration_min_clean = as.numeric(str_extract(duration_raw, "\\d+(?=h)")) * 60 +  
           as.numeric(str_extract(duration_raw, "(?<=h)\\d+"))) |> 
  relocate(duration_min_clean, .after = duration_raw)
```

### 4. Saving Extracted Data to CSV

The cleaned dataset is saved in a CSV file (ouigo.csv) inside the "Scrapped data" directory.

File Handling Behavior:

Creates a new file if ouigo.csv does not exist.

Appends new data if ouigo.csv already exists.

```         
file_path <- here::here("Scrapped data", "ouigo.csv")

if (!file.exists(file_path)) {
  write_csv(ouigo, file_path)  
} else {
  write_csv(ouigo, file_path, append = TRUE)  
}
```

### 5. Closing Selenium Browser

After extracting and saving data, the Selenium session is properly closed to avoid leaving unnecessary browser instances running:

```         
remDr$close()
```

## Renfe Train Ticket Scraper

### How the Scraping Works

Step 1: Setup & Initialization

-   Clears the environment.

-   Loads necessary Rpackages.

-   Defines the current date (today) and the target travel date (today + 14 days).

-   Sets the departure and arrival stations (Madrid → Barcelona).

-   Initializes Selenium WebDriver to automate interactions with the Renfe website.

Step 2: Website Navigation & Interaction

-   Accepts cookies if prompted.

-   Enters the departure and arrival stations in the search fields.

-   Selects a one-way trip option.

-   Opens the date picker and navigates to the correct month/year if needed.

-   Selects the travel date (14 days from today).

Step 3: Searching for Train Tickets

-   Clicks the search button and waits for the results to load.

-   Extracts train details including: Departure & arrival times ,Travel duration, Ticket prices ,Train company information

Step 4: Data Cleaning & Formatting

-   Converts extracted text into a structured dataset.

-   Cleans up travel durations, extracting hours and minutes separately.

-   Formats prices correctly (converting from €, to numerical values).

-   Saves the data to a CSV file (Renfe.csv).

Step 5: Saving & Appending Data

-   If Renfe.csv does not exist, it creates a new file.

-   Otherwise, it appends the new data to the existing file.

-   Closes the Selenium WebDriver.

### Advanced Code Features

1.  **Smart Date Selection**

The function select_date_smart(search_date) dynamically adjusts the calendar selection process:

-   If the search date is in a future month, it automatically clicks ‘Next Month’ until the correct month is displayed.

-   If the target month is already selected, it proceeds directly to the date selection.

-   This prevents errors when searching for dates across multiple months.

2.  **Intelligent Error Handling**

The script includes Try-Catch mechanisms to handle missing elements gracefully:

``` r
cookie_btn <- tryCatch(
  remDr$findElement(using = "id", value = "onetrust-accept-btn-handler"),
  error = function(e) NULL
)
```

If a cookie button is missing, the script continues without breaking.

3.  **Extracting Train Operator Logos**

Instead of relying on text data, the script extracts the train operator logos:

``` r
train_company_js <- remDr$executeScript("
  return Array.from(document.querySelectorAll('.trenes-enlaces img'))
         .map(img => img.getAttribute('alt'));
")
```

This ensures accurate company identification.

4.  **Automated Data Cleaning**

Converts travel duration into minutes:

``` r
duration_min_clean = duration_hour * 60 + ifelse(is.na(duration_min), 0, duration_min)
```

Converts price from text (€) to numeric format:

``` r
price = as.numeric(gsub("€", "", gsub(",", ".", as.character(price))))
```

This ensures consistency in the final dataset.

5.  **Data Persistence and Safety**

Automatic data backup by appending to an existing .csv file.
Ensuresseamless updates without overwriting existing data.

``` r
file_path <- file.path(here(), "Renfe.csv")

if (!file.exists(file_path)) {
  write_csv(train_df, file_path)  
} else {
  write_csv(train_df, file_path, append = TRUE)  
}
```

Reduces risk of data loss .Ensures incremental data growth for scalable analysis
