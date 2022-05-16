library(DBI)
library("ggplot2")
library(dplyr)
library(ggplot2)
library(lemon)
library(tidyr)
library(shiny)
library(shinythemes)


# 1 connect + load in data
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "bio_db_project", 
                 host = "localhost",
                 user = "root",
                 password = "@ll@ert12")

tables <- dbListTables(con)
str(tables)

# pyramid data
pop_by_age_past <- dbReadTable(con, "pop_past")

pop_by_age_project <- dbReadTable(con, "pop_projections")

pop_by_age <- rbind(pop_by_age_past, pop_by_age_project)

# excess mortality

delta_mortality <- dbReadTable(con, "excess_mortality")

# life expectancy given age table 

life_table <- dbReadTable(con, "life_table")



# # plotting functions
# plot pop pyramid for data == pop_by_age for given year t and location loc
pyramid_plotter <- function(pop_data, loc, t){
  # data formatting
  pop_male <- pop_by_age[pop_by_age$Location== loc & pop_by_age$time == t ,c("Location", "AgeGrp", "PopMale")]
  pop_female <- pop_by_age[pop_by_age$Location== loc & pop_by_age$time == t,c("Location", "AgeGrp", "PopFemale")]
  if (!("Pop" %in% colnames(pop_male))){
    pop_male <- pop_male %>% mutate(sex = "Male")
    pop_female <- pop_female %>% mutate(sex = "Female")
    colnames(pop_male)[3] <- "Pop"
    colnames(pop_female)[3] <- "Pop"}
  pop <- rbind(pop_male , pop_female)
  pop$Pop <- pop$Pop *1000
  # actual plotting
  pyramid <- ggplot(data = pop, 
                    mapping = aes(x = ifelse(test = sex == "Male", yes = -Pop, no = Pop), 
                                  y = as.factor(AgeGrp), fill = sex)) +
    geom_col(width=1) +
    scale_x_symmetric(labels = abs) +
    labs(x = "Population") + ylab("age") + ggtitle("Population pyramid") + theme_classic() + theme(plot.title = element_text(size=22))
  return(pyramid + scale_y_discrete(guide = guide_axis(n.dodge=2)))
}



## life expactancy given age function

life_expectancy <- function(life_tab, loc,s, t) {
  plot_table <- life_tab %>% filter(Location == loc) %>% filter(Sex == s) %>% filter(time==t)
  plot <- ggplot(plot_table, aes(x=AgeGrpStart, y=ex+AgeGrpStart)) +
    geom_line() + geom_point() + 
    xlab("age") +
    ylab("life expectancy") + ggtitle("life expectancy given age") + theme_minimal() + theme(plot.title = element_text(size=22))
  return (plot)
}
# to get excess mortality



excess_mortality_plotter <- function(data, land, grouper=1){
  #skip cyclic week var
  if (!(land  %in% c(unique(life_table$Location)))){
    land <- "Belgium"
  }
  country_mortality <- data %>% filter(country==land)
  country_mortality$month <- seq(1,24)
  n_weeks <- length(country_mortality$month)
  
  grouping <- seq(1:n_weeks)
  counter <- 1
  for (i in 1:n_weeks){
    if (i %% grouper == 0)
    {
      grouping[i] <- counter 
      counter <- counter + 1
      
    }
    else{
      grouping[i] <- counter 
      
    }
  }
  country_mortality$grouper <- factor(grouping)
  
  mort <- country_mortality %>% group_by(grouper) %>% summarise(mort = mean(excess_mean))
  month <- country_mortality %>% group_by(grouper) %>% summarise(month = min(month))
  length(mort$mort)
  plot_frame <- data.frame(mort$mort, month$month)
  colnames(plot_frame)[1] <- "average_mortality"
  colnames(plot_frame)[2] <- "month"
  
  plot<- ggplot(plot_frame, aes(x=month, y=average_mortality)) +
    
    geom_line() + 
    geom_point() +
    xlab("month") + ylab("excess mortality") + ggtitle("Monthly Excess mortality 2020-2021") + theme_minimal() + theme(plot.title = element_text(size=22))
  return (plot)
}

# test 
excess_mortality_plotter(data=delta_mortality,land = "afafafa", grouper = 8) # works :)

### Now the actual app.



# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Explore mortality on a population level"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="location", label = "Choose a Country:", selected = "Belgium",
                  c(unique(life_table$Location))
      ),
      selectInput(inputId = "time", label = "choose a time period:", selected = "2000-2005",
                  c(unique(life_table$time))),
      selectInput(inputId = "sex", label = "For which sex group?", selected = "Male", 
                  c(unique(life_table$Sex))),
      sliderInput(inputId = "year",
                  label = "year slider",
                  min = 1950,
                  max = 2100,
                  value = 1950),
      sliderInput(inputId = "groups",
                  label = "Number of months to group excess mortality by",
                  min = 1,
                  max = 12,
                  value=1)
      
    ),
    
    
    # Main panel for displaying outputs ----
    
    # Output: Histogram ----
    mainPanel(plotOutput(outputId = "life_expectancy"),
              
              
              
              plotOutput(outputId = "pyramid"),
              plotOutput(outputId ="excess_mortality" )
              )))


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$life_expectancy <- renderPlot({

    life_expectancy(life_table,loc = input$location, s = input$sex, t = input$time)
    # copy each row 5 times and make year collumn?
    
  })
  output$pyramid <- renderPlot({
    pyramid_plotter(pop_by_age, loc = input$location, t = input$year)})
  output$excess_mortality <- renderPlot({
    excess_mortality_plotter(data=delta_mortality,land = input$location, grouper = input$groups)
  })
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
    
    
    
  })
  
  
}





# Create Shiny app ----
shinyApp(ui = ui, server = server)








## 