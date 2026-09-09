#API example ####

library(httr)
library(jsonlite)
library(ggplot2)

# Set coordinates for Barcelona
latitude <- 41.3851
longitude <- 2.1734

# Dates
start_date <- Sys.Date() - 30
end_date <- Sys.Date()

# Build URL
url <- "https://archive-api.open-meteo.com/v1/archive"

# Query parameters
params <- list(
  latitude = latitude,
  longitude = longitude,
  start_date = start_date,
  end_date = end_date,
  daily = "temperature_2m_mean",
  timezone = "Europe/Madrid"
)

# Make request
res <- GET(url, query = params)

# Parse response
weather_data <- fromJSON(content(res, "text", encoding = "UTF-8"))

df <- data.frame(
  date = as.Date(weather_data$daily$time),
  temp = weather_data$daily$temperature_2m_mean
)

ggplot(df, aes(x = date, y = temp)) +
  geom_line(color = "firebrick", size = 1.2) +
  labs(
    title = "Daily Mean Temperature in Barcelona (Last 30 Days)",
    x = "Date",
    y = "°C"
  ) +
  theme_minimal()

#API example 2 
CHECK HABS example




#pythorch example ####

# Set up a Python environment (you only need to do this once)
reticulate::virtualenv_create("r-pytorch")
reticulate::virtualenv_install("r-pytorch", packages = c("torch", "numpy"))
reticulate::use_virtualenv("r-pytorch", required = TRUE)

library(reticulate)

# Use Python in R
py_run_string("
import torch
import torch.nn as nn
import torch.optim as optim

# Create toy dataset
x = torch.tensor([[1.0], [2.0], [3.0], [4.0]])
y = torch.tensor([[2.0], [4.0], [6.0], [8.0]])

# Define simple linear model
model = nn.Linear(1, 1)

# Loss and optimizer
criterion = nn.MSELoss()
optimizer = optim.SGD(model.parameters(), lr=0.01)

# Training loop
for epoch in range(100):
    optimizer.zero_grad()
    outputs = model(x)
    loss = criterion(outputs, y)
    loss.backward()
    optimizer.step()
    if epoch % 10 == 0:
        print(f'Epoch {epoch}, Loss: {loss.item()}')

# Predict on a new value
with torch.no_grad():
    prediction = model(torch.tensor([[5.0]]))
    print(f'Prediction for x=5: {prediction.item()}')
")

library(reticulate)

# Activate or install Python environment with torch
reticulate::use_virtualenv("r-pytorch", required = TRUE)

# Python code as a string
py_code <- "
import torch
import torch.nn as nn

# Training data
x = torch.tensor([[1.0], [2.0], [3.0], [4.0]])
y = torch.tensor([[2.0], [4.0], [6.0], [8.0]])

# Define model
model = nn.Linear(1, 1)
criterion = nn.MSELoss()
optimizer = torch.optim.SGD(model.parameters(), lr=0.01)

# Train model
for epoch in range(200):
    optimizer.zero_grad()
    output = model(x)
    loss = criterion(output, y)
    loss.backward()
    optimizer.step()

# Save model
torch.save(model.state_dict(), 'linear_model.pt')
print('Model trained and saved.')
"

# Run the code
py_run_string(py_code)

torch <- import("torch")
nn <- import("torch.nn")

model <- nn$Linear(1L, 1L)
model$load_state_dict(torch$load("linear_model.pt"))
model$eval()

input <- torch$tensor(matrix(c(5.0, 6.0), ncol = 1), dtype = torch$float32)

with(torch$no_grad(), {
  preds <- model(input)
})

pred_values <- as.numeric(preds$numpy())

library(ggplot2)

# Create a simple data frame
input_values <- c(5, 6)
df <- data.frame(x = input_values, y_pred = pred_values)

# Plot
ggplot(df, aes(x = x, y = y_pred)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue") +
  labs(title = "Predictions from PyTorch Model in R",
       x = "Input x", y = "Predicted y") +
  theme_minimal()

# CALL LOCAL LLM (gemma3n) 

library(httr)
library(jsonlite)

# model <- "gemma3n"
# prompt <- "What is the role of phytoplankton in the ocean?"
# 
# # Use echo to send the prompt into Ollama
# cmd <- sprintf('echo "%s" | ollama run %s', prompt, model)
# 
# # Capture the output
# response <- system(cmd, intern = TRUE)
# 
# # Show the result
# cat(response, sep = "\n")

ask_ollama <- function(prompt, model = "gemma3n") {
  cmd <- sprintf('echo %s | ollama run %s', shQuote(prompt), model)
  response <- system(cmd, intern = TRUE)
  
  # Trim empty lines at the end
  response <- response[!grepl("^\\s*$", response)]
  
  # Print result cleanly
  cat(paste(response, collapse = "\n"), "\n")
  
  # Optionally also return the raw response (invisibly)
  invisible(response)
}

# Example use
ask_ollama("Explain ocean acidification.")



# prompt <- "Summarize the effects of red tide on fisheries."
# 
# response <- POST(
#   url = "http://localhost:11434/api/generate",
#   body = toJSON(list(
#     model = "gemma3n",  # or "mistral", "gemma"
#     prompt = prompt,
#     stream = FALSE
#   ), auto_unbox = TRUE),
#   encode = "json"
# )
# 
# result <- fromJSON(content(response, "text", encoding = "UTF-8"))
# cat(result$response)

# SQL ####

# install.packages("bigrquery")
library(bigrquery)
library(ggplot2)
library(dplyr)
library(lubridate)

# Your GCP project ID
project_id <- "test-468205"

# SQL to get annual average temperature (°C) in Barcelona
# Using a lat/lon around central Barcelona (41.38N, 2.17E)
sql_temp <- "
SELECT
  EXTRACT(YEAR FROM datetime) AS year,
  AVG(temperature_2m_above_ground) - 273.15 AS avg_temp_celsius
FROM
  `bigquery-public-data.noaa_gfs_seamless.gfs_1hr`
WHERE
  latitude BETWEEN 41.36 AND 41.40
  AND longitude BETWEEN 2.15 AND 2.19
  AND datetime BETWEEN '2000-01-01' AND '2023-12-31'
GROUP BY year
ORDER BY year
"

# Run the query
tb_temp <- bq_project_query(project_id, sql_temp)
barcelona_temp <- bq_table_download(tb_temp)

# Plot the result
ggplot(barcelona_temp, aes(x = year, y = avg_temp_celsius)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Annual Average Temperature in Barcelona (2000–2023)",
    x = "Year",
    y = "Average Temperature (°C)"
  ) +
  theme_minimal()



# Setup based on the user ####
if (Sys.info()['user']=='dvilasgonzalez') {
  out.dir <- 'C:/Users/dvilasgonzalez/Documents/WFS_DV2/WFS-FEM/'
  user.cop <- 'dvilas'
  pass.cop <- 'Vilgon;3113'
  depth.dir <- '/static drivers/'
} else {
  # .libPaths("C:/R/win-library")
  # user.cop <- 'dchagaris'
  # pass.cop <- 'Asdf1234!'
  # depth.dir <- '/maps/'
  out.dir<-"/Users/daniel/Work/WFS_DV2/WFS-FEM/"
  user.cop <- 'dvilas'
  pass.cop <- 'Vilgon;3113!'
  depth.dir <- '/static drivers'
}

# Load libraries
library(reticulate)
library(raster)
library(dplyr)
library(ggplot2)


# Setup Copernicus Marine Toolbox via Python
virtualenv_create(envname = "CopernicusMarine")
reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
cmt <- import("copernicusmarine")
cmt$login(user.cop, pass.cop)

# Load depth map to define bounding box (or use hardcoded Barcelona coords)
depth <- raster(paste0(out.dir, depth.dir, '/depth/depth 1min 330x390.asc'))
plot(depth)

# Define bounding box around Barcelona if depth raster doesn't cover it
# Barcelona: ~41.38N, 2.17E
bbox_barcelona <- c(
  xmin = 2.10, xmax = 2.25,
  ymin = 41.30, ymax = 41.45
)

datasets <- cmt$list_datasets()
View(datasets)

meta <- cmt$get_metadata("cmems_mod_glo_phy_my_0.083deg_P1M-m")
str(meta)

# PHYSICAL REANALYSIS DATA SUBSET: Near-surface temperature (thetao), upper ocean
cmt$subset(
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
  dataset_version = "202311",
  variables = list("thetao"),  # near-surface temperature
  minimum_longitude = bbox_barcelona["xmin"],
  maximum_longitude = bbox_barcelona["xmax"],
  minimum_latitude = bbox_barcelona["ymin"],
  maximum_latitude = bbox_barcelona["ymax"],
  start_datetime = "1993-01-01T00:00:00",
  end_datetime = "2024-06-01T00:00:00",
  minimum_depth = 0.5,  # near-surface layer
  maximum_depth = 1.0,
  disable_progress_bar = FALSE,
  output_directory = paste0(out.dir, '/GLORYS/')
)

library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)

#---------------------------------------------------
# 1. Open the NetCDF file
#---------------------------------------------------
# Replace this with the actual file path (adjust as needed)
nc_file <- list.files(paste0(out.dir, '/GLORYS/'), pattern = "\\.nc$", full.names = TRUE)[1]
nc <- nc_open(nc_file)

#---------------------------------------------------
# 2. Read variables and apply scale/offset
#---------------------------------------------------

# Read raw temperature
theta_raw <- ncvar_get(nc, "thetao")  # [lon, lat, depth, time]

# Read scale and offset attributes (if available)
scale  <- ncatt_get(nc, "thetao", "scale_factor")$value
offset <- ncatt_get(nc, "thetao", "add_offset")$value

# Apply scale and offset if present
if (!is.null(scale) && !is.null(offset)) {
  theta <- theta_raw * scale + offset
} else {
  theta <- theta_raw
}

#---------------------------------------------------
# 3. Convert time to calendar dates
#---------------------------------------------------

# Time is in hours since 1950-01-01
time <- ncvar_get(nc, "time")
origin <- as.POSIXct("1950-01-01", tz = "UTC")
dates <- origin + as.difftime(time, units = "hours")

#---------------------------------------------------
# 4. Flatten data to long format
#---------------------------------------------------

# Note: Assuming theta is [lon, lat, depth=1, time]
#       so we collapse lon & lat dimensions

theta_df <- data.frame(
  date = as.Date(rep(dates, each = dim(theta)[1] * dim(theta)[2])),
  temp = as.vector(theta)
)

#---------------------------------------------------
# 5. Aggregate to annual average
#---------------------------------------------------

annual_thetao <- theta_df %>%
  filter(!is.na(temp)) %>%
  group_by(year = year(date)) %>%
  summarize(avg_temp = mean(temp, na.rm = TRUE), .groups = "drop")

#---------------------------------------------------
# 6. Plot
#---------------------------------------------------

ggplot(annual_thetao, aes(x = year, y = avg_temp)) +
  geom_line(color = "darkorange") +
  geom_point() +
  labs(
    title = "Annual Near-Surface Temperature in Barcelona (1993–2021)",
    x = "Year", y = "Temperature (°C)"
  ) +
  theme_minimal()
