# COVID-19-Visualizations
An Rshiny app for visualizing and exploring COVID-19 Case and Death Data.

See the current published app at [alexocampo.shinyapps.io/shiny/](https://alexocampo.shinyapps.io/shiny/)

`Shiny2_Clean.R` loads and cleans data. Country specific data comesfrom the [Johns Hopkins University CSSE Team](https://github.com/CSSEGISandData/COVID-19) via the [tidycovid R package](https://github.com/joachim-gassen/tidycovid19). State and County specific data comes from [The New York Times times](https://github.com/nytimes/covid-19-data).

`app.R` performs some light data processing and contains the ui and server for the Rshiny app.

Code is the result of collaborative work between Harrison Reeder and Alex Ocampo.




