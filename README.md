# R Programs for Data Cleaning and Visualization
Associated with INFO 5001 @Cornell University

Code not included for privacy reasons.

This repository contains R programs demonstrating effective data cleaning techniques and insightful visualizations. Each script includes methods and visualizations tailored to specific datasets and analytical goals.

## Contents

1. **Mass Shootings Data Cleaning and Visualization**
   - **Data Cleaning**: This script processes mass shooting data by renaming columns, transforming text, handling missing values, and extracting state information.
   - **Visualizations**:
     - **Boxplot**: Distribution of total victims by location type.
     - **Bar Plot**: Mass shooting incidents by state.
     - **Line Plot**: Trends in mean and median age of shooters over time.

2. **Supreme Court Voting Analysis**
   - **Data Cleaning**: Filters and prepares voting data from Supreme Court cases.
   - **Visualizations**:
     - **Line Plot**: Changes in percentage of one-vote margin cases over time.
     - **Bar Plot**: Conservative voting percentages of active justices across selected issues.
     - **Monthly Decision Trends**: Number of decisions by month, identifying annual patterns.

## Usage

Each script can be run independently. Ensure the required libraries (`tidyverse`, `dplyr`, `ggplot2`, etc.) are installed. These programs are structured for easy data loading, cleaning, and visualization generation.

## Dependencies

Install necessary packages:
```R
install.packages(c("tidyverse", "dplyr", "janitor", "ggplot2", "scales", "forcats"))

