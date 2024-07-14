
#' Read HDI Data
#'
#' This function reads in HDI (Human Development Index) data from a specified file, 
#' assigns appropriate column types, and returns an object of class 'HDIdata'.
#'
#' @param file_name The name of the file containing HDI data.
#' @return An object of class 'HDIdata' with the data read from the file.
#' @import data.table
#' @examples
#' hdi_data <- read_HDI("path/to/your/hdi_data_file.csv")
#' @export
read_HDI <- function(file_name){
  # Define column types
  col_types <- c(
    country_code = "factor",  
    country_name = "character",  
    indicator_id = "factor",  
    indicator_name = "character",
    index_id = "factor",      
    index_name = "factor",    
    value = "numeric",
    year = "integer"
  )
  
  # read in data for Nigeria using data.table
  file = file_name

  # Skip the first two lines (header and the problematic second line) and specify column names manually
  df <- fread(file, 
           skip = 2, 
           header = FALSE, 
           sep = ",",
          #  col.names = c("country_code", "country_name", "indicator_id", "indicator_name", "index_id", "index_name", "value", "year"),
          col.names = names(col_types)
           )
  # Apply column types
  for (col in names(col_types)) {
    if (col_types[col] == "factor") {
      df[[col]] <- as.factor(df[[col]])
    } else if (col_types[col] == "character") {
      df[[col]] <- as.character(df[[col]])
    } else if (col_types[col] == "numeric") {
      df[[col]] <- as.numeric(df[[col]])
    } else if (col_types[col] == "integer") {
      df[[col]] <- as.integer(df[[col]])
    }
  }

  # # Read the header separately
  # header <- fread(file, nrows = 1, header = FALSE, sep = ",")

  # # Set the column names using the header
  # setnames(part2, names(header))

  # # Combine the two parts
  # df <- rbind(header, part2)

  # create and return an object of class 'HDIdata' while inheriting 
  # from original data.table
  class(df) <- c("HDIdata", class(df))

  return(df)
}

#' Print HDIdata 
#'
#' This method prints a summary of an HDIdata object, including the number of observations,
#' column names, country, indicators, and categories which the indicators belong to.
#'
#' @param x An object of class 'HDIdata'.
#' @return Prints a summary of the HDIdata object.
#' @import data.table
#' @examples
#' print(hdi_data)
#' @export
print.HDIdata <- function(x) {
  # convert HDIdata object to data.table to use data.table functions within the function
  # x = as.data.table(x)
  
  # extract the country_name
  country_name = x[1, country_name]
  # find the unique entries in indicator_name using data.table and not base R
  indicator_names = unique(x[, indicator_name])
  # find the categories which the indicator_names belong to
  indicator_categories = unique(x[, index_name])

  # print these values out in a neat format
  cat("HDIdata object with", nrow(x), "observations\n")
  cat("Column names:\n", paste(colnames(x), collapse = "\n"), "\n\n")
  cat("Country:\n", country_name, "\n\n")
  cat("Indicators:\n", paste(indicator_names, collapse = "\n"), "\n\n")
  cat("Indices which indicators belong to:\n",
      paste(indicator_categories, collapse = "\n"), "\n")
}


#' Summary of HDIdata
#'
#' Provides a comprehensive summary of an HDIdata object, including general overview statistics,
#' detailed summaries by indicator, and data completeness information. The general overview
#' includes the total number of observations, the number of unique indicators, and the range of years
#' covered. Detailed summaries for each indicator include mean, median, minimum, maximum, and the
#' number of missing values. Data completeness is presented as the percentage of non-NA values for
#' each column.
#'
#' @param object An object of class 'HDIdata'.
#' @return Prints a detailed summary of the HDIdata object, including statistics and data completeness.
#' @import data.table
#' @examples
#' summary(hdi_data)
#' @export
summary.HDIdata <- function(object) {
  # General overview
  cat("HDIdata Summary\n")
  cat("Observations:", nrow(object), "\n")
  cat("Indicators:", length(unique(object$indicator_name)), "\n")
  cat("Years:", range(object$year), "\n")
  
  # Detailed summary by indicator
  indicators <- unique(object$indicator_name)
  for (ind in indicators) {
    cat("\nSummary for", ind, ":\n")
    sub_data <- object[object$indicator_name == ind, ]
    stats <- summary(sub_data$value)
    cat("Mean:", stats["Mean"], "\n")
    cat("Median:", stats["Median"], "\n")
    cat("Min:", stats["Min."], "\n")
    cat("Max:", stats["Max."], "\n")
    cat("Years of data:", length(unique(sub_data$year)), "\n")
    cat("Missing Values:", sum(is.na(sub_data$value)), "\n")
  }

  # Data completeness
  completeness <- colSums(is.na(object)) / nrow(object) * 100
  cat("\nData Completeness (percentage of non-NA values):\n")
  print(100 - completeness)

  invisible(object)
}


#' Plot HDI Data
#' 
#' This function plots the trend of 4 indicators over time.
#' 
#' @param object An object of class 'HDIdata'.
#' @return A ggplot object showing the trend of the indicators over time.
#' @import ggplot2
#' @examples
#' plot.HDIdata(hdi_data)
#' @export
plot.HDIdata <- function(object){
  # Select indicators to plot
  indicators <- c("Life Expectancy at Birth (years)",
                  "Mean Years of Schooling (years)",
                  "Adolescent Birth Rate (births per 1,000 women ages 15-19)",
                  "Gross National Income Per Capita (2017 PPP$)")

  # Plot the trend of the first 4 indicators in a facet grid
  p <- ggplot(object[object$indicator_name %in% indicators, ], 
              aes(x = year, y = value, color = indicator_name)) +
    geom_line() +
    facet_wrap(~indicator_name, scales = "free_y") +
    labs(title = "Trend of Indicators Over Time",
         x = "Year",
         y = "Value") +
    theme_minimal() +
    theme(legend.position = "none")

  return(p)
}
        