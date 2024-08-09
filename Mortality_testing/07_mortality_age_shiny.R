#As Ch4 relies on specific data setups, I'll just play around with Shiny instead

library(shiny)
library(tidyverse)
library(here)
library(readxl)
library(DT) #Assist with interactive data tables 
library(shinythemes)

#setwd to relevant area first... hopefully source file location

#### define directories ####
DIR_PROJECT <- paste0(getwd(),"/") #can also do paste0(here()) as long as the root folder of here is in the relevant position
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")
DIR_FIG_OUT  <- paste0(DIR_FIG,"007/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"007/")

# Function to process mortality data
process_mortality_data <- function(sheet_name, gender) {
  data <- read_excel("rawdata/007/mortality_by_age.xlsx", sheet = sheet_name, skip = 3) #sheet_name = name of sheet to read (ie number location), gender to label each sheet
  names(data) <- gsub("Deaths registered in (\\d{4})", "\\1", names(data)) #renaming columns based on given data
  pivot_data <- pivot_longer(data, cols = matches("^\\d{4}$"), names_to = 'Year_of_death', values_to = 'Registered_deaths')
  pivot_data$`Age in years` <- as.numeric(pivot_data$`Age in years`)
  pivot_data <- drop_na(pivot_data, `Age in years`)
  pivot_data$Year_of_death <- as.numeric(pivot_data$Year_of_death) #works same as pre-function, just no need to copy and paste everything for each sheet
  pivot_data$Gender <- gender #lower case gender 
  return(pivot_data)
}

# Process both datasets
male_data <- process_mortality_data(5, "Male") #Subsheet 5 has Male data
female_data <- process_mortality_data(6, "Female") #Subsheet 6 has female data
combined_data <- bind_rows(male_data, female_data) #Function adjusts data accordingly, now safe to merge

# Save example plots (optional)
ggplot(combined_data, aes(`Age in years`, Registered_deaths, color = Gender)) +
  geom_point() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(color = "Gender") +
  theme_minimal() 
  ggsave(file.path(DIR_FIG_OUT, "Registered_Deaths_per_age_and_year.png"), width = 16, height = 9)
#saves example graph of male vs female distributions
  
# Shiny app
ui <- fluidPage(
  theme = shinytheme("cerulean"), #trying out shinytheme package 
  titlePanel("Mortality by Age (1974-2020)"), #set title of the shiny page
  sidebarLayout(
    sidebarPanel( #these sidebar options allow me to place the slider on the side
      sliderInput("Years", "Select Year Range", 
                  min = min(combined_data$Year_of_death), 
                  max = max(combined_data$Year_of_death),
                  value = c(min(combined_data$Year_of_death), max(combined_data$Year_of_death)),
                  step = 1),
      selectInput("Gender", "Select Gender",  
                  choices = c("Male", "Female", "Both"), #Choice of which data to look at based on gender column
                  selected = "Both"),
      textOutput("deathsRange")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("deathPlot")),
        tabPanel("Data Table", DTOutput("dataTable")) #placement of plot and tables
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    if (input$Gender == "Both") {
      combined_data %>%
        filter(Year_of_death >= input$Years[1] & Year_of_death <= input$Years[2]) #filtering data ranges to be appropriate in choices
    } else {
      combined_data %>%
        filter(Year_of_death >= input$Years[1] & Year_of_death <= input$Years[2], Gender == input$Gender)
    }
  })
  
  output$deathsRange <- renderText({
    paste0("Displaying registered deaths by age (0-104) between ", 
           input$Years[1], " and ", input$Years[2], " for ", input$Gender, ".")
  })
  
  output$deathPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = `Age in years`, y = Registered_deaths, color = Gender)) +
      geom_point() +
      scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
      labs(color = "Gender", title = "Registered Deaths by Age and Year",
           x = "Age in Years", y = "Number of Registered Deaths") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}

shinyApp(ui, server)