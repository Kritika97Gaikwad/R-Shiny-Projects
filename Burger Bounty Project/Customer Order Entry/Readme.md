# Burger Bounty Shiny Dashboard

This Shiny app serves as a dashboard for summarizing and updating monthly sales data for a fictional burger business. The app allows users to input data related to customer visits, sales quantities, and pricing. The data is then visualized in an Excel file using the "shinydashboard" package in R.

## Features

- **Customer Order Entry Form:** Collects user information, date of data collection, town visited, time spent, weather conditions, and event/weekend status.

- **Sales Data Entry:** Allows the user to input the quantity sold for each type of burger.

- **Pricing Data Entry:** Lets the user set the selling price for each burger type.

- **Update Button:** Triggers the update of the Excel file with the entered data.

## Getting Started

### Prerequisites

- R installed on your machine
- Shiny package installed (`install.packages("shiny")`)
- Shinydashboard package installed (`install.packages("shinydashboard")`)
- Readxl package installed (`install.packages("readxl")`)

### Running the App

1. Open RStudio or your preferred R environment.
2. Open the `app.R` file.
3. Run the Shiny app.

## Usage

1. Fill in the customer order entry form with the required information.
2. Enter the quantity sold for each burger type.
3. Set the selling price for each burger type.
4. Click the "Update" button to update the Excel file with the entered data.

## Data Structure

- **Visits Sheet:** Contains data related to customer visits.
- **Prices Sheet:** Stores the selling prices for each burger type.
- **Sales Sheet:** Captures the quantity sold for each burger type.

## Contributing

Contributions are welcome! Feel free to submit issues or pull requests.

## License

This project is licensed under the [MIT License](LICENSE).

## Acknowledgments

- Thanks to the Shiny and Shinydashboard communities for providing powerful tools for interactive data visualization in R.
