### Load helper packages ###
loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c("shiny", "knitr", "lattice", "grid", "qwraps2", "datasets", "maps", "colorspace", "scales", 
         "tidyverse", "usmap", "devtools", "rsconnect", "htmltools", "tools", "dplyr", "plyr", "stringr",
         "DT", "ggplot2")
loadlibs(libs)


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

# Loading in the datasets

# Read forest employment and earnings data from dataset
# Data source: U.S. Census Bureau's Annual Economic Surveys' data
# Download from https://data.census.gov/cedsci/advanced
# Topic: Employment
# NAICS: 113, 1142, 1153, 321, 322 (These are what we consider forest industries)
# The file is saved as forest_employment.csv which contains both earnings (pay) and employment (employee)

employ <- read.csv("./forest_employment.csv")

#---------------------------------------------------------------------------------

# Data source: U.S. Census Bureau Annual Economic Survey's data
# Download from https://data.census.gov/cedsci/advanced
# Topic: Employment
# NAICS: 00 (total for all sectors)
# The file is saved as total_employment.csv which contains both earnings (pay) and employment (employee)

#---------------------------------------------------------------------------------

total <- read.csv("./total_employment.csv")

# Since the datasets do not provide fips so we have to create fips by ourselves
# Create fips for forest_employment

fips_employ <- c()
for (i in 1:length(employ[,1]))
    fips_employ[i] <- fips(state = employ[i,2], county = employ[i,1])

employ <- data.frame(county = employ[,1], state = employ[,2], fips = fips_employ, pay_employ = employ[,3], employee_employ = employ[,4])

fips_total <- c()
for (i in 1:length(total[,1]))
    fips_total[i] <- fips(state = total[i,2], county = total[i,1])

total <- data.frame(county = total[,1], state = total[,2], fips = fips_total, pay_total = total[,3], employee_total = total[,4])

merged <- merge(employ,total,by=c('fips'),all.x=T)
merged <- select(merged,-c(county.y,state.y))

# convert the forest employment data into proportion
pct_pay <- c()
for (i in 1:length(merged[,1]))
    pct_pay[i] <- merged$pay_employ[i]/merged$pay_total[i]*100

pct_employee <- c()
for (i in 1:length(merged[,1]))
    pct_employee[i] <- merged$employee_employ[i]/merged$employee_total[i]*100

pct_employ <- data.frame(fips = merged[,1], county = merged[,2], state = merged[,3], pay_employ = merged[,4], employee_employ = merged[,5], pay_total = merged[,6], employee_total = merged[,7], pct_pay = pct_pay, pct_employee = pct_employee)

# save the percentage of forest employment data with fips
write.csv(pct_employ,'pct_employ.csv')

#---------------------------------------------------------------------------------

# Read indigenous data from dataset
# Data source: U.S. Census Bureau's American Community Survey
# Download from https://data.census.gov/cedsci/advanced
# Topic: Race and Ethnicity > American Indian and Alaska Native, Native Hawaiian and Pacific Islander
# The file is saved as indigenous.csv. The used column is named 'native'.
indigenous <- read.csv("./indigenous.csv")   # use this one instead
pop <- read.csv("./pop.csv")

# Create fips for indigenous data
fips_indigenous <- c()
for (i in 1:length(indigenous[,1]))
    fips_indigenous[i] <- fips(state = indigenous[i,2], county = indigenous[i,1])

indigenous <- data.frame(county = indigenous[,1], state = indigenous[,2], fips = fips_indigenous, indian_alaskan = indigenous[,3], hawaiian_pacific = indigenous[,4], indigenous = indigenous[,5])

fips_pop <- c()
for (i in 1:length(pop[,1]))
    fips_pop[i] <- fips(state = pop[i,2], county = pop[i,1])

pop <- data.frame(county = pop[,1], state = pop[,2], fips = fips_pop, pop = pop[,3])

# merge indigenous data with population
merged_native <- merge(indigenous, pop, by=c('fips'),all.x=T)

pct.indigenous <- merged_native$indigenous/merged_native$pop*100

merged_native <- data.frame(county = merged_native[,2], state = merged_native[,3], fips = merged_native[,1], indigenous = merged_native[,6], pop = merged_native[,9], pct.indigenous = round(pct.indigenous,2))

# save the percentage of indigenous data with fips
write.csv(merged_native,'merged_native.csv')

#---------------------------------------------------------------------------------

# Read forest land data from dataset
# Data source: USDA Forest Service, Forest Inventory and Analysis (FIA) data for 48 conterminous states and Hawaii, National Land Cover Data (NLCD) for Alaska
# Download from https://apps.fs.usda.gov/Evalidator/evalidator.jsp, https://www.mrlc.gov/national-land-cover-database-nlcd-2016 (for NLCD, we can ask for data in excel format)
# The file is saved as pct_forest.csv. The used column is named 'Forest area proportion'.
# The datasets provide fips
# Note that data from FIA for forest cover are based on inventories for the years 2014-2019.

forest <- read.csv("./pct_forest.csv")
forest <- data.frame(county = forest[,2], state = forest[,1], fips = forest[,5], forest.area = forest[,6], land.water = forest[,7], pct.forest = forest[,8], year = forest[,9])

# Write data frame into a .csv file
write.csv(forest,'forest.csv')

# Read all csv files we created
merged_native <- read.csv("./merged_native.csv")
forest <- read.csv("./forest.csv")
employ <- read.csv("./pct_employ.csv")

# Merge indigenous data with forest land data
native.forest <- merge(merged_native,forest,by=c('fips'),all.x=T)
native.forest.employ <- merge(native.forest, employ,by=c('fips'),all.x=T)
native.forest.employ <- select(native.forest.employ,-c(X.x, X.y, county.y, state.y, X, county, state))

native.forest.employ <- data.frame(county = native.forest.employ[,2], state = native.forest.employ[,3], fips = native.forest.employ[,1], indigenous = native.forest.employ[,4], pct.indigenous = native.forest.employ[,6], pop = native.forest.employ[,5], forest.area = native.forest.employ[,7], land.water = native.forest.employ[,8], pct.forest = native.forest.employ[,9], year = native.forest.employ[,10], pay.forest = native.forest.employ[,11], employee = native.forest.employ[,12], pay.total = native.forest.employ[,13], total.employ = native.forest.employ[,14], pct.pay = round(native.forest.employ[,15],2), pct.employ = round(native.forest.employ[,16],2))
native.forest.employ[is.na(native.forest.employ)] <- 0

# write the data frame into a csv file
write.csv(native.forest.employ,'final.merged.csv')

final.merged <- read.csv("./final.merged.csv")

#---------------------------------------------------------------------------------

#Read rural data from dataset
# Data source: U.S. Office of Management and Budget definitions of metro/non-metro counties
# Download from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx#.UYJuVEpZRvY
# Topic: Metro vs Non-Metro
# The file is saved as employment.ratio.csv. We use column 'Nonmetro'.
rural <- read.csv("./rural.csv")

rural <- data.frame(fips = rural[,1], county = rural[,3], state = rural[,2], pop.2010 = rural[,4], rucc.2013 = rural[,5], nonmetro = rural[,7], rural = rural[,8])

final.merged.rural <- merge(final.merged,rural,by=c('fips'),all.x=T)

# Drop irrelevant columns
drops <- c("X", "land.water", "county.y", "state.y", "pop.2010", "rucc.2013", "forest.area", "year","pay.total","total.employ", "indigenous", "population", "employee", "pay.forest")
final.merged.rural <- final.merged.rural[, !(names(final.merged.rural) %in% drops)]

# Rename columns
final.merged.rural <- final.merged.rural %>% dplyr::rename(county=county.x, state=state.x, pct.earnings=pct.pay, pct.employees=pct.employ)

# Rearrange columns
final.merged.rural <- final.merged.rural[c("fips", "state", "county", "pct.forest", "pct.employees", "pct.earnings", "pct.indigenous", "nonmetro", "rural")]

write.csv(final.merged.rural,'final.merged.rural.csv')

final.merged.rural <- read.csv("./final.merged.rural.csv")
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------



# Define UI for application that plots features of rawdata -----------
ui <- fluidPage(
    
    # Theme selector --------------------------------------------------
    theme = shinythemes::shinytheme("simplex"),
    
    # Application title -----------------------------------------------
    titlePanel("Where are Forest-Dependent Communities?"),
    
    # Sidebar layout with a input and output definitions --------------
    sidebarLayout(
        
        # Inputs: Select variables to plot ------------------------------
        sidebarPanel(
            
            # Select Thresholds ----------------------------------------------------
            # Forest land
            numericInput(inputId = "forest_land1", 
                         label = "Criteria 1: Spatial Relationship - Please put minimum forest land area (%)", 
                         min = 0, max = 100, 
                         value = 75),
            
            # Employees
            numericInput(inputId = "employees", 
                         label = "Criteria 2.1: Economic Dependence - Please put minimum employees in the forest sector (%)", 
                         min = 0, max = 100, 
                         value = 10),
            
            # Employees
            numericInput(inputId = "earnings", 
                         label = "Criteria 2.2: Economic Dependence - Please put minimum earnings from the forest sector (%)", 
                         min = 0, max = 100, 
                         value = 15),
            
            # forest land
            numericInput(inputId = "forest_land2", 
                         label = "Criteria 3.1: Cultural Connection - Please put minimum forest land area (%)", 
                         min = 0, max = 100, 
                         value = 30),
            
            # indigenous
            numericInput(inputId = "indigenous", 
                         label = "Criteria 3.2: Cultural Connection - Please put minimum indiginous population (%)", 
                         min = 0, max = 100, 
                         value = 5),
            
            # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            # Enter text for plot title ---------------------------------------------
            textInput(inputId = "plot_title1", 
                      label = "Title: Forest-Dependent Communities Map", 
                      placeholder = "Enter text to be used as plot title"),
            
            textInput(inputId = "plot_title2", 
                      label = "Title: Meeting the Criteria Map", 
                      placeholder = "Enter text to be used as plot title"),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
        ),
        
        # Output: -------------------------------------------------------
        mainPanel(
            
            # Show all the plots --------------------------------------------
            
            # plotOutput(outputId = "forest.dependent.map"),
            # br(),        
            # 
            # # Print number of obs plotted ---------------------------------
            # uiOutput(outputId = "text1"),
            # br(), br(),    # a little bit of visual separation
            # 
            # plotOutput(outputId = "criteria.map"),
            # br(),
            # 
            # # Print number of obs plotted ---------------------------------
            # uiOutput(outputId = "text2"),
            # br(), br(),    # a little bit of visual separation
            # 
            # # Show data table ---------------------------------------------
            # DT::dataTableOutput(outputId = "datatable1"),
            
            # Show download button ---------------------------------------------
            downloadButton("download1","Download data table as csv"),
            # Show data table ---------------------------------------------
            DT::dataTableOutput(outputId = "datatable2")
        )
    )
)

# Define server function required to create the plots ---------
server <- function(input, output, session) {
    
    # Prompt users to key thresholds ------
    # Forest land
    observe({
        updateNumericInput(session, 
                           inputId = "forest_land1",
                           value = min(75, 100),
                           max = 100
        )
    })
    
    # Employees
    observe({
        updateNumericInput(session, 
                           inputId = "employees",
                           value = min(10, 100),
                           max = 100
        )
    })
    
    # Earnings
    observe({
        updateNumericInput(session, 
                           inputId = "earnings",
                           value = min(15, 100),
                           max = 100
        )
    })
    
    # Forest land
    observe({
        updateNumericInput(session, 
                           inputId = "forest_land2",
                           value = min(30, 100),
                           max = 100
        )
    })
    
    # Indigenous
    observe({
        updateNumericInput(session, 
                           inputId = "indigenous",
                           value = min(5, 100),
                           max = 100
        )
    })

    
    forest.dependent <- reactive({
        ifelse(final.merged.rural$pct.forest > input$forest_land1 |
                   (final.merged.rural$pct.employees > input$employees | final.merged.rural$pct.earnings > input$earnings) |
                   (final.merged.rural$pct.forest > input$forest_land2 & final.merged.rural$pct.indigenous > input$indigenous), 1, 0)
    })
    
    
    other.rural <- reactive({
        ifelse(final.merged.rural$nonmetro == 1 & forest.dependent() == 0, 1,
               ifelse(is.na(final.merged.rural$nonmetro) | is.na(forest.dependent()), 0, 0))
    })
    
    non.forest.dependent <- reactive({
        ifelse(forest.dependent() == 0 & final.merged.rural$nonmetro == 0, 1,
               ifelse(is.na(forest.dependent()) | is.na(final.merged.rural$nonmetro), 0, 0))
    })
    
    df <- reactive({
        cbind(
            final.merged.rural,
            other.rural(),
            non.forest.dependent(),
            forest.dependent()
        )
    })
    
    #Assign 3 categories: Forest-dependent counties (1), Non-metro, non-forest counties (2), and Metro, non-forest counties (3).
    # category <- reactiveValues()
    # observe({
    #     for (i in 1:length(df()[,1]))
    #         if (is.na(forest.dependent()[i]) | is.na(other.rural()[i])){
    #             category$cat[i] <- 3
    #         }
    #     else if (forest.dependent()[i] == 1){
    #         category$cat[i] <- 1
    #     } else if (other.rural()[i] == 1) {
    #         category$cat[i] <- 2
    #     } else {
    #         category$cat[i] <- 3
    #     }
    # })
    # 
    # cat_recode <- reactive({
    #     recode_factor(category$cat,
    #                   `1` = "FDC",
    #                   `2` = "NMNFDC",
    #                   `3` = "MNFDC")
    # })
    # 
    # forest.dependent_recode <- reactive({
    #     recode_factor(forest.dependent(),
    #                   `1` = "FDC",
    #                   `0` = "NFDC")
    # })
    # 
    # newdf <- reactive({
    #     cbind(
    #         final.merged.rural[,!(names(final.merged.rural) %in% c("nonmetro", "rural"))],
    #         forest.dependent_recode(),
    #         cat_recode()
    #     )
    # })  
    # 
    # 
    # forest.dependent.sum <- reactive({
    #     sum(cat_recode() == "FDC")
    # })
    # 
    # nmnfdc.sum <- reactive({
    #     sum(cat_recode() == "NMNFDC")
    # })  
    # 
    # mnfdc.sum <- reactive({
    #     sum(cat_recode() == "MNFDC")
    # }) 
    # 
    # total <- reactive({
    #     forest.dependent.sum()+nmnfdc.sum()+mnfdc.sum()
    # }) 
    # 
    # #-----------------------------------------------------------------------------------
    # forest.first <- reactive({
    #     ifelse(final.merged.rural$pct.forest > input$forest_land1, 1, 0)
    # })
    # 
    # forest.second <- reactive({
    #     ifelse(final.merged.rural$pct.employees > input$employees | final.merged.rural$pct.earnings > input$earnings, 1, 0)
    # })
    # 
    # forest.third <- reactive({
    #     ifelse(final.merged.rural$pct.forest > input$forest_land2 & final.merged.rural$pct.indigenous > input$indigenous, 1, 0)
    # })
    # 
    # forest.first.second <- reactive({
    #     ifelse(final.merged.rural$pct.forest > input$forest_land1 & (final.merged.rural$pct.employees > input$employees | final.merged.rural$pct.earnings > input$earnings), 1, 0)
    # })
    # 
    # forest.first.third <- reactive({
    #     ifelse(final.merged.rural$pct.forest > input$forest_land1 & (final.merged.rural$pct.forest > input$forest_land2 & final.merged.rural$pct.indigenous > input$indigenous), 1, 0)
    # })
    # 
    # forest.second.third <- reactive({
    #     ifelse((final.merged.rural$pct.employees > input$employees | final.merged.rural$pct.earnings > input$earnings) & (final.merged.rural$pct.forest > input$forest_land2 & final.merged$pct.indigenous > input$indigenous), 1, 0)
    # })
    # 
    # forest.only.first <- reactive({
    #     ifelse(forest.first() == 1 & forest.second() == 0 & forest.third() == 0, 1, 0)
    # })
    # 
    # forest.only.second <- reactive({
    #     ifelse(forest.first() == 0 & forest.second() == 1 & forest.third() == 0, 1, 0)
    # })
    # 
    # forest.only.third <- reactive({
    #     ifelse(forest.first() == 0 & forest.second() == 0 & forest.third() == 1, 1, 0)
    # })  
    # 
    # forest.both.first.second <- reactive({
    #     ifelse(forest.first() == 1 & forest.second() == 1 & forest.third() == 0, 1, 0)
    # }) 
    # 
    # forest.both.first.third <- reactive({
    #     ifelse(forest.first() == 1 & forest.second() == 0 & forest.third() == 1, 1, 0)
    # }) 
    # 
    # forest.both.second.third <- reactive({
    #     ifelse(forest.first() == 0 & forest.second() == 1 & forest.third() == 1, 1, 0)
    # }) 
    # 
    # forest.all.three <- reactive({
    #     ifelse(forest.first() == 1 & forest.second() == 1 & forest.third() == 1, 1, 0)
    # })
    # 
    # newdf_criteria <- reactive({
    #     cbind(
    #         newdf(),
    #         forest.only.first(),
    #         forest.only.second(),
    #         forest.only.third(),
    #         forest.both.first.second(),
    #         forest.both.first.third(),
    #         forest.both.second.third(),
    #         forest.all.three()
    #     )
    # })
    # 
    # #Assign criteria categories
    # criteria <- reactiveValues()
    # observeEvent( eventExpr = df(),
    #          handlerExpr = {
    #     for (i in 1:length(newdf_criteria()[,1]))
    #         if (is.na(forest.dependent()[i]) | is.na(other.rural()[i])){
    #             criteria$cri[i] <- 0
    #         }
    #     else if (forest.only.first()[i] == 1){
    #         criteria$cri[i] <- 1
    #     } else if (forest.only.second()[i] == 1) {
    #         criteria$cri[i] <- 2
    #     } else if (forest.only.third()[i] == 1) {
    #         criteria$cri[i] <- 3
    #     } else if (forest.both.first.second()[i] == 1) {
    #         criteria$cri[i] <- 4
    #     } else if (forest.both.first.third()[i] == 1) {
    #         criteria$cri[i] <- 5
    #     } else if (forest.both.second.third()[i] == 1) {
    #         criteria$cri[i] <- 6
    #     } else if (forest.all.three()[i] == 1) {
    #         criteria$cri[i] <- 7
    #     } else {
    #         criteria$cri[i] <- 0
    #     }
    #   }
    # )
    # 
    # criteria_recode <- reactive({
    #     recode_factor(criteria$cri,
    #                   `1` = "Met criteria: 1",
    #                   `2` = "Met criteria: 2",
    #                   `3` = "Met criteria: 3",
    #                   `4` = "Met criteria: 1,2",
    #                   `5` = "Met criteria: 1,3",
    #                   `6` = "Met criteria: 2,3",
    #                   `7` = "Met criteria: 1,2,3",
    #                   `0` = "Met criteria: None")
    # })
    # 
    # newdf_criteria_recode <- reactive({
    #     cbind(
    #         newdf_criteria(),
    #         criteria_recode()
    #     )
    # })
    # 
    # first.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 1")
    # })
    # 
    # second.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 2")
    # })
    # 
    # 
    # third.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 3")
    # })
    # 
    # 
    # first.second.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 1,2")
    # })
    # 
    # 
    # first.third.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 1,3")
    # })
    # 
    # 
    # second.third.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 2,3")
    # })
    # 
    # 
    # all.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: 1,2,3")
    # })
    # 
    # 
    # none.criteria <- reactive({
    #     sum(criteria_recode() == "Met criteria: None")
    # })
    # 
    # 
    # total.criteria <- reactive({
    #     first.criteria()+second.criteria()+third.criteria()+first.second.criteria()+first.third.criteria()+second.third.criteria()+all.criteria()+none.criteria()
    # })
    # 
    # criteria.tbl <- reactive({
    #     data.frame("Criteria" = c("Only First Criteria", "Only Second Criteria", "Only Third Criteria", 
    #                               "Both First & Second Criteria", "Both First & Third Criteria", 
    #                               "Both Second & Third Criteria", "All Criteria", 
    #                               "Total Non-Forest Dependent Counties", 
    #                               "Total Counties"), "Counties" 
    #                = c(first.criteria(), second.criteria(), third.criteria(), first.second.criteria(), 
    #                    first.third.criteria(), second.third.criteria(), all.criteria(), 
    #                    none.criteria(), total.criteria()))
    # })
    
    # Print number of different types of counties  ----------------------------------
  #   output$text1 <- renderUI({
  #       
  #       HTML(paste("Figure 1 shows the counties identified as forest-dependent using the criteria in box 1 and 2018 data. 
  #              There are ", forest.dependent.sum(), "counties (", round(forest.dependent.sum()/total()*100,0)," percent ) that are categorized as forest-dependent. For comparison purposes, we grouped the remaining counties into metro,
  # non-forest-dependent (", nmnfdc.sum(), "counties,", round(nmnfdc.sum()/total()*100,0), " percent ) and non-metro,
  #              non-forest-dependent (", mnfdc.sum(), "counties,", round(mnfdc.sum()/total()*100,0), " percent ) counties.
  #              Forest-dependent counties include both metro and non-metro counties."))
  #   })
  #   
    
    # Create plot objects, the plotOutput function is expecting --
    
    # Create a map showing forest-dependent counties
    # states <- usmap::plot_usmap("states", color = "darkgreen", fill = alpha(0.01)) #this parameter is necessary to get counties to show on top of states
    # counties <- reactive(usmap::plot_usmap(data = newdf(), values = "forest.dependent_recode()", color = "grey"))
    # 
    # 
    # output$forest.dependent.map <- renderPlot({
    #     ggplot()+
    #         counties()$layers[[1]]+  #counties needs to be on top of states for this to work
    #         states$layers[[1]]+
    #         counties()$theme +
    #         coord_equal() +
    #         scale_fill_manual(values = c(`NFDC`="white", `FDC`="forestgreen"), name = "Community Type") +
    #         theme(legend.position = "bottom", panel.background = element_rect(colour = "black"))
    #     
    # })
    
    # Create a map showing forest-dependent counties under different criteria
    # counties_criteria <- reactive(usmap::plot_usmap(data = newdf_criteria_recode(), values = "criteria_recode()", color = "grey"))
    # 
    # output$criteria.map <- renderPlot({
    #     ggplot()+
    #         counties_criteria()$layers[[1]]+  #counties needs to be on top of states for this to work
    #         states$layers[[1]]+
    #         counties_criteria()$theme +
    #         coord_equal() +
    #         scale_fill_manual(values =
    #                               c(`Met criteria: None` = "white",
    #                                 `Met criteria: 1` = "gold",
    #                                 `Met criteria: 2` = "mediumpurple",
    #                                 `Met criteria: 3` = "salmon",
    #                                 `Met criteria: 1,2` = "turquoise",
    #                                 `Met criteria: 1,3` = "firebrick",
    #                                 `Met criteria: 2,3` = "cornflowerblue",
    #                                 `Met criteria: 1,2,3` = "forestgreen"),
    #                           name = "Community Type") +
    #         theme(legend.position = "bottom", panel.background = element_rect(colour = "black"))
    # })
    
    # Print data table if checked -------------------------------------
    # output$datatable1 <- DT::renderDataTable(
    #     DT::datatable(data = criteria.tbl(),
    #                   rownames = FALSE)
    # )
    
    # Print data table if checked -------------------------------------
    output$datatable2 <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = df(),
                          extensions = 'Buttons',
                          options = list(pageLength = 10, dom = 'B<"dwnld">frtip',
                                         buttons = list("copy")), 
                          rownames = FALSE)
        }
    )
    
    
    # Download data into a csv file if button is clicked -------------------------------------
    # output$download1 <- downloadHandler(
    #     filename = function() {
    #         paste("data-", Sys.Date(), ".csv", sep="")
    #     },
    #     content = function(file) {
    #         write.csv(newdf_criteria_recode(), file)
    #     }
    # )
    
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)