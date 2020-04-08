# Load Packages -----------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(ggthemes)
library(shiny)
library(dplyr)


# Map Packages
library(sf)
library(raster)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
# library(mapview) # for interactive maps

# County Views
library(urbnmapr)

# setwd("[something]")


# Load Data ---------------------------------------------------------------

#for now, assume that 'usa' and 'world' are loaded in already
load("usa.RData")
load("world.RData")

usa.today <- usa %>% filter(usa$Date == max(usa$Date))
world.today <- world %>% filter(world$Date == max(world$Date))

# Light Data Processing ---------------------------------------------------

try(usa <- usa %>% rename(Location=State))
try(world <- world %>% rename(Location=Country))
world$Location[world$Location=="Georgia"] <- "Georgia (Country)" #avoid collision of multiple locations
stopifnot(!any(usa$Location %in% world$Location))
overall <- rbind(usa,world)
#overall$Recovered <- as.numeric(overall$Recovered) #something was weird with this column

#create some reference vectors
locations <- c(sort(unique(usa$Location)),sort(unique(world$Location)))
time_endpoints <- c(min(overall$Date),max(overall$Date))
outcomes <- colnames(overall)[!(colnames(overall) %in% c("Date","Location"))]

# Clean Map Data ----------------------------------------------------------

###### USA
us_states$Confirmed <- usa.today$Confirmed[match(us_states$NAME,usa.today$State)]
us_states <- us_states[,-1]

###### World 
data("World") 
levels(World$name) <- c(levels(World$name),"USA") 
World$name[World$name=="United States"] <- "USA"

# Add Cases
World$Confirmed <- world.today$Confirmed[match(World$name,world.today$Country)]
World$Deaths <- world.today$Deaths[match(World$name,world.today$Country)]



# Begin Shiny App ---------------------------------------------------------

# Define UI for app ----
ui <- navbarPage("Visualizing COVID-19",
                 
                 # App title ----
                 tabPanel("Cases and Deaths",
                          
                          #Used to make divider between sliders and legend show up more clearly
                          #see https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar
                          tags$head(
                            tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                          ),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              selectInput(inputId = "select_location",
                                          label = "Countries/States:",
                                          choices=locations,
                                          selected=c("USA","Italy","China","New York","Iran",
                                                     "Switzerland","South Korea"),
                                          multiple=TRUE),
                              
                              actionButton(inputId = "clear_all",label = "Clear All Countries/States"),
                              br(" "),
                              
                              sliderInput(inputId = "select_timewindow",
                                          label = "Date Window:",
                                          min = time_endpoints[1],
                                          max = time_endpoints[2],
                                          value = c(as.Date("2020-03-01",format="%Y-%m-%d"),
                                                    max(overall$Date)),
                                          step = 1),
                              
                              radioButtons("select_outcome",
                                           "Outcome",
                                           choices=outcomes,
                                           selected=outcomes[1],
                                           inline = TRUE),
                              radioButtons("log_y",
                                           "Log y-axis",
                                           choices=c(Log=TRUE,Linear=FALSE),selected=F,inline = TRUE),
                              #Here we add in our desired written information on the sidebar
                              hr(), #horizontal bar
                              h5("Data"),
                              h6("International:",a("The Johns Hopkins University CSSE team", 
                                                    href = "https://github.com/CSSEGISandData/COVID-19")),
                              h6("US:",a("New York Times", 
                                         href = "https://github.com/nytimes/covid-19-data")),
                              hr(),
                              
                              h4("World Regions"),
                              selectInput(inputId = "region",
                                          label = "Regional Graph",
                                          choices = c(" ","South Asia", "Sub-Saharan Africa", 
                                                      "Europe & Central Asia","Middle East & North Africa",
                                                      "Latin America & Caribbean", "East Asia & Pacific", 
                                                      "North America"),
                                          selected=" "
                              ),
                              hr(),
                              
                              #h4("Other Custom Plots"),
                              
                              h4("US States"),
                              actionButton(inputId = "all_states",label = "Include All States"),
                              actionButton(inputId = "no_states",label = "Exclude All States")
                              #br(),
                              #actionButton(inputId = "midwest",label = "Midwest"),
                              
                              #h5("Africa"),
                              #actionButton(inputId = "ecowas",label = "ECOWAS")
                            ),
                            # Main panel for displaying outputs ----
                            mainPanel(
                              tabsetPanel(id="tabs", 
                                          tabPanel("Plot",
                                                   # h3("Estimated Power"),
                                                   plotOutput(outputId="plot_graph",height="600px"),
                                                   hr()
                                                   ),
                                          tabPanel("Table",
                                                   h3("Most Recent Data"),
                                                   tableOutput("data_raw"),
                                                   br()
                                          )
                              )
                            )
                          )
                 ), #Another tab panel can be added here if we want to expand to a second feature, e.g., modeling
                 tabPanel(title = "US Map",
                          leafletOutput("us_states_tmap")
                          ),
                 tabPanel(title = "World Map",
                          leafletOutput("world_tmap")
                 )
)



# Server ------------------------------------------------------------------

server <- function(input, output,session) {
  
  observeEvent(input$all_states,{
    new_choices <- unique(c(input$select_location,usa$Location))
    updateSelectizeInput(session,inputId = "select_location",choices=locations,selected=new_choices)
  })
  
  observeEvent(input$no_states,{
    new_choices <- unique(input$select_location[input$select_location %in% world$Location])
    print(new_choices)
    updateSelectizeInput(session,inputId = "select_location",choices=locations,selected=new_choices)
  })
  
  observeEvent(input$ecowas,{
    new_choices <- c("Ghana","Cape Verde","Guinea","Guinea-Bissau","Liberia","Mali","Senegal","Sierra Leone",
                     "Benin","Burkina Faso","Cote d'Ivoire","Niger","Nigeria","Togo")
    updateSelectizeInput(session,inputId = "select_location",choices=locations,selected=new_choices)
  })
  
  observeEvent(input$region,{
    
    if(input$region == " "){
      new_choices <- input$select_location
    }
    
    if(input$region == "South Asia"){
      new_choices <- c("Afghanistan", "Bangladesh", "Bhutan", "India", "Sri Lanka", 
                       "Maldives", "Nepal", "Pakistan")
    }
    if(input$region == "Sub-Saharan Africa"){
      new_choices <- c("Angola", "Burundi", "Benin", "Burkina Faso", "Botswana", "Central African Republic", 
                       "Cote d'Ivoire", "Cameroon", "Congo (Kinshasa)", "Congo (Brazzaville)", 
                       "Cabo Verde", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Guinea", 
                       "Gambia", "Guinea-Bissau", "Equatorial Guinea", "Kenya", "Liberia", 
                       "Madagascar", "Mali", "Mozambique", "Mauritania", "Mauritius", 
                       "Malawi", "Namibia", "Niger", "Nigeria", "Rwanda", "Sudan", "Senegal", 
                       "Sierra Leone", "Somalia", "South Sudan", "Eswatini", "Seychelles", 
                       "Chad", "Togo", "Tanzania", "Uganda", "South Africa", 
                       "Zambia", "Zimbabwe")
    }
    if(input$region == "Europe & Central Asia"){
      new_choices <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belgium", 
                       "Bulgaria", "Bosnia and Herzegovina", "Belarus", "Switzerland", 
                       "Cyprus", "Czechia", "Germany", "Denmark", "Spain", "Estonia", 
                       "Finland", "France", "United Kingdom", "Georgia", "Greece", "Croatia", 
                       "Hungary", "Ireland", "Iceland", "Italy", "Kazakhstan", "Kyrgyzstan", 
                       "Liechtenstein", "Lithuania", "Luxembourg", "Latvia", "Monaco", 
                       "Moldova", "North Macedonia", "Montenegro", "Netherlands", "Norway", 
                       "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", 
                       "Slovakia", "Slovenia", "Sweden", "Turkey", "Ukraine", "Uzbekistan")
    }
    if(input$region == "Middle East & North Africa"){
      new_choices <- c("United Arab Emirates", "Bahrain", "Djibouti", "Algeria", "Egypt", 
                       "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", 
                       "Libya", "Morocco", "Malta", "Oman", "West Bank and Gaza", "Qatar", 
                       "Saudi Arabia", "Syria", "Tunisia")
    }
    if(input$region == "Latin America & Caribbean"){
      new_choices <- c("Argentina", "Antigua and Barbuda", "Bahamas", "Belize", "Bolivia", 
                       "Brazil", "Barbados", "Chile", "Colombia", "Costa Rica", "Cuba", 
                       "Dominica", "Dominican Republic", "Ecuador", "Grenada", "Guatemala", 
                       "Guyana", "Honduras", "Haiti", "Jamaica", "Saint Kitts and Nevis", 
                       "Saint Lucia", "Mexico", "Nicaragua", "Panama", "Peru", "Paraguay", 
                       "El Salvador", "Suriname", "Trinidad and Tobago", "Uruguay", 
                       "Saint Vincent and the Grenadines", "Venezuela")
    }
    if(input$region == "East Asia & Pacific"){
      new_choices <- c("Australia", "Brunei", "China","Fiji", "Indonesia", "Japan", 
                       "Cambodia", "Korea, South","Laos", "Burma", "Mongolia", "Malaysia", 
                       "New Zealand", "Philippines", "Papua New Guinea", "Singapore", 
                       "Thailand", "Timor-Leste","Vietnam")
    }
    if(input$region == "North America"){
      new_choices <- c("Canada", "Mexico", "USA")
    }
    

    updateSelectizeInput(session,inputId = "select_location",choices=locations,selected=new_choices)
  })
  
  observeEvent(input$midwest,{
    new_choices <- c("Ohio","Michigan","Illinois","Wisconsin","Indiana","Iowa","Kansas","Minnesota","Missouri","Nebraska","North Dakota","South Dakota")
    updateSelectizeInput(session,inputId = "select_location",choices=locations,selected=new_choices)
  })
  
  observeEvent(input$clear_all,{
    new_choices <- c()
    updateSelectizeInput(session,inputId = "select_location",choices=locations,selected=new_choices)
  })
  
  plotdata <- reactive({
    req(input$select_location)
    plot_data <- overall %>% 
      filter(Location %in% input$select_location & 
               Date %in% seq(from = input$select_timewindow[1],to = input$select_timewindow[2],by = 1))
    plot_data
  })
  
  # daily_T_v <- reactive({as.numeric(input$T_v)})
  
  output$plot_graph= renderPlot({
    p <- ggplot(data=plotdata(),aes_string(y=input$select_outcome,x="Date")) +
      geom_line(aes(color = Location)) + 
      xlab("Date") + ylab(input$select_outcome) +
      theme_minimal() + 
      geom_text(data = plotdata() %>% filter(Date == last(Date)), aes_string(label = "Location", 
                                                                             x = "Date + 2", 
                                                                             y = input$select_outcome, 
                                                                             color = "Location")) +
      coord_cartesian(clip = 'off') +
      # geom_dl(aes(label=Location,color=Location),method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
      theme( axis.text = element_text( size = 14 ),
             axis.title = element_text( size = 18, face = "bold" )) + 
      theme(legend.position = "none",
            plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 
    if(input$log_y){
      p <- p + scale_y_log10()
    }
    
    p
  })
  
  output$data_raw <- renderTable({
    outtable <- plotdata() %>% filter(Date == last(Date)) %>% dplyr::arrange(Location,Date) 
    # criteria <- outtable[,"T_v"] %in% daily_T_v()
    # print(daily_T_v())
    # print(outtable[,"T_v"])
    # print(as.data.frame(sapply(outtable[criteria,], as.character)))
    as.data.frame(sapply(outtable, as.character))
    # as.data.frame(sapply(outtable[criteria,], as.character))
  },
  include.colnames=TRUE,
  include.rownames=FALSE,
  align="c",
  striped=T,
  spacing="xs")
  
  output$us_states_tmap = renderLeaflet({
    mybreaks <- c(0,500,1000,5000,10000,25000,100000,200000)
    tm <- tm_shape(us_states, projection = 2163) + tm_polygons("Confirmed",breaks=mybreaks) + tm_layout(frame = FALSE) 
    tmap_leaflet(tm)
  })
  
  # output$us_states_deaths_tmap = renderLeaflet({
  #   mybreaks <- c(0,500,1000,5000,10000,25000,100000,200000)
  #   tm <- tm_shape(us_states, projection = 2163) + tm_polygons("Confirmed",breaks=mybreaks) + tm_layout(frame = FALSE) 
  #   tmap_leaflet(tm)
  # })
  
  output$world_tmap = renderLeaflet({
    world.confirmed.breaks <- c(0,500,1000,5000,10000,25000,100000,200000,5e5)
    tm <- tm_shape(World) +
      tm_polygons("Confirmed",breaks=world.confirmed.breaks)
    tmap_leaflet(tm)
  })
  
}
# criteria <- CPD_power$tau %in% 1.1 & CPD_power$m %in% 2 & CPD_power$T %in% 22 & CPD_power$pchange %in% 0.5
# Create Shiny app ----
shinyApp(ui = ui, server = server)



