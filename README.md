# MetricR: Cost Estimation App (R Shiny)

A small Shiny app that automates the *computo metrico* (the Italian bill of quantities used to estimate construction costs): it joins a list of material quantities with a price list, computes the cost of each line and of the whole project, and shows which materials weigh most.

The interface and the error messages are in Italian.

## What it does
- **Joins** quantities and unit prices on a shared material ID.
- **Reads** `.csv`, `.txt`, `.xlsx` and `.xls` files, and checks that the expected columns are present.
- **Calculates** the cost of each line (`quantity × unit price`) and the total project cost.
- **Shows** an interactive, searchable table and a bar chart of the **top 5 materials by cost**.
- **Exports** the result as a semicolon-separated CSV that opens correctly in Excel.

## Input format
| File | Required columns |
|---|---|
| Quantities | `id_materiale`, `descrizione`, `quantita` |
| Price list | `id_materiale`, `prezzo_unitario` |

Two example files are included: `sample_quantities.csv` and `sample_prices.csv` (5 materials, total cost € 1,849.50).

## How to run
1. Install R and RStudio.
2. Install the packages:
   ```r
   install.packages(c("shiny", "bslib", "dplyr", "readr", "readxl", "DT", "ggplot2"))
   ```
3. Open `estimator.R` in RStudio and click **Run App**, or run `shiny::runApp("estimator.R")`.
4. Upload the two sample files from the sidebar.

## Limitations
- A material with no price in the price list gets no cost, so it is left out of the total.
- Prices are not versioned or tied to a date, and there is no VAT or overhead calculation.
- The app has not been deployed online yet.
