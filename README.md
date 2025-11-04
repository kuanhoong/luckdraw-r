# Lucky Draw Wheel - Shiny App

This is a simple R Shiny app that creates a fun interactive Lucky Draw (Wheel of Fortune) from a CSV of names.

Features
- Upload a CSV with a single column of names.
- Visual wheel that displays current remaining participants.
- Spin the wheel to randomly select a winner; the winner is removed from the pool.
- Side panel lists winners in order; download winners as CSV; reset to original list.

Quick start

1. Install required R packages (run in R / RStudio):

```r
install.packages(c('shiny','shinyWidgets','readr','dplyr','jsonlite','shinyjs'))
```

2. Run the app (from this folder):

```r
library(shiny)
runApp('app.R')
```

CSV format
- A CSV file with a single column of names works best. Header is optional but recommended. Example:

```
name
Alice
Bob
Charlie
```

Notes
- The app uses a small embedded JavaScript canvas to draw and animate the wheel. The server picks the winner and instructs the client to animate to the chosen segment.
- Keep your names reasonably short so labels fit nicely on the wheel.

Have fun at your event! 🎉
