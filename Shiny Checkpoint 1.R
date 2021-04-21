# Loading packages
library(shiny)
library(shinythemes)



# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(
                  "Green House Gas Emissions - Analysis",
                  tags$h4("Green House Gas Emissions - Analysis Project"),
                  tags$h5("By Satish Gollu and Sri Surya Sameer Vaddhiparthy under the guidance of Professor Dr.Wassnaa Al-Mawee."),
                  
                  tabPanel("Introduction",
                           sidebarPanel(
                             submitButton("Click to refresh countries in the dataset", icon("refresh")),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("We have used the Greenhouse Gas (GHG) Inventory Data with the most recently submitted information, it has data from 1990 to the latest available year, it contains information on anthropogenic emissions (emissions from fossil fuels, deforestation, land use changes, livestock, fertilization) It consists data on unspecified mix of HFCs and PFCs, sulphur hexafluoride (SF6) and nitrogen triflouride (NF3)) that are not within the scope of Montreal Protocol, an international agreement in 1987, aimed at curbing the of ozone depleting substances."),
                             tags$label(h5('The below is the list of countries in the data')),
                             verbatimTextOutput("countries_list"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Overview of Emissions",
                           sidebarPanel(
                             tableOutput('table1')
                           ),
                           mainPanel(
                             plotOutput("plot1", click = "plot_click"),
                             "As in the First plot it's clearly showing that GHG is the highest emission around the world, followed by GHG_indirect_CO2 as second highest and next order is in CO2, CH4 and N2Os. These are the top five emissions from the year 1990 -2018 around the world.",
                             plotOutput("plot2", click = "plot_click"),
                             "As in the Second plot it's clearly showing that GHG is the highest emission around the world, followed by GHG_indirect_CO2 as second highest and next order is in CO2, CH4 and N2Os. These are the top five emissions from the year 1990 -2018 around the world.",
                             
                           )
                  ),
                  tabPanel("Gaswise Emissions Analysis",
                           sidebarPanel(
                             selectInput("dropdown1", "Choose the pollutant:", choices = c('GHG' ='GHG', 'GHG_indirect_CO2'='GHG_indirect_CO2','CO2'= 'CO2','CH4'='CH4')), 
                             submitButton("Visulaize"),
                             tableOutput("countries_observations2")
                           ),
                           mainPanel(
                             plotOutput("plot3", click = "plot_click"),
                             plotOutput("plot4", click = "plot_click"),
                           )
                  ),tabPanel("Conclusion", "This panel is intentionally left blank")
                ) # Navigation bar code ends
) # fluidPage code ends


# Define server function  
server <- function(input, output) {
  
  
  
  library(dplyr)
  library(ggplot2)
  
  dataset <- read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/4736/7235/greenhouse_gas_inventory_data_data.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210415%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210415T201933Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=2a2560a5f8cc5be63804145cbe38237942f892b3d9cd58107dbca01d5a19caeb3cb6cac87ecaa241f1bf385347ced99168a8ba447175492fa1cf9c542551e663efe111a27c604d0fe9a97bf35325359d4cb001c79ac0ffd46b37a52949e074598265f29b2cd7a9ab006f63d4ebd47cde18cdea0c539bceee6440fc84bfa77bf0a8c7c5161cfc98782cbc952af2f60e176e1c12f7e463dc6171c44ffaa2f55903f6072565c1687281fbf74fa744f322c5978f8c5308d33fcce42231d476b0e3f7274489187496510acd98c2b8c5f4510129430b5cd78cbf5f49fe4c4aaf4ba8ed780311bd3fa3f37326486bbb6f58c88e459fd8262aa2e56a14e95c0341a49505",sep=',',stringsAsFactors = FALSE)
  #dataset <- read.csv("greenhouse_gas_inventory_data_data.csv",sep=',',stringsAsFactors = FALSE) 
  
  dataset<-dataset %>% mutate(category=recode(category, 
                                              carbon_dioxide_co2_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='CO2',
                                              greenhouse_gas_ghgs_emissions_including_indirect_co2_without_lulucf_in_kilotonne_co2_equivalent='GHG_indirect_CO2',
                                              greenhouse_gas_ghgs_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='GHG',
                                              hydrofluorocarbons_hfcs_emissions_in_kilotonne_co2_equivalent='HFC',
                                              methane_ch4_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='CH4',
                                              nitrogen_trifluoride_nf3_emissions_in_kilotonne_co2_equivalent='HF3',
                                              nitrous_oxide_n2o_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='N2Os',
                                              perfluorocarbons_pfcs_emissions_in_kilotonne_co2_equivalent='PFCs',
                                              sulphur_hexafluoride_sf6_emissions_in_kilotonne_co2_equivalent='SF6',
                                              unspecified_mix_of_hydrofluorocarbons_hfcs_and_perfluorocarbons_pfcs_emissions_in_kilotonne_co2_equivalent='HFC-PFC-mix'))
  
  dataset <- dataset %>% rename( country = country_or_area)
  dataset$country <- tolower(dataset$country)
  dataset[dataset$country=='russian federation','country'] <-'russia'
  dataset[dataset$country=='united kingdom','country'] <-'uk'
  dataset[dataset$country=='united states of america','country'] <-'usa'
  
  
  # Sending list of countries to Intro page  
  output$countries_list <- renderPrint({ unique(dataset$country) 
  })
  
  dataset$value <- gsub(",","",dataset$value)
  dataset$value <- as.numeric(dataset$value)
  
  
  #**3.Data analysis**
  
  #Plot 1 
  total_emissions <- dataset %>% group_by(category) %>%summarise(sum(value))
  #arrange the data in descending order 
  colnames(total_emissions)[2] <- "total_value"#changing name of column
  total_emissions <- total_emissions %>% arrange(desc(`total_value`))#ordering the data in descending order
  total_emissions
  
  #table1
  output$table1 <- renderTable(total_emissions)
  
  plot_nr1 <- ggplot(total_emissions, aes(x=reorder(category,-total_value), y=total_value)) +
    geom_bar(stat="identity", fill="red", alpha=.8, width=.4) +
    xlab("Gas type") +
    ylab("In kilotons equivalent (log10)")+
    ggtitle("Total emissions by each gas from 1990-2018")+
    theme_bw()+
    coord_flip(expand = TRUE)#rotate the axis to avoid data overlapping
  output$plot1 <- renderPlot({plot_nr1})
  
  #Total contribution of all gases over the years.  
  
  # Plot 2 in "Summary tab"
  
  #group the data by category and year
  Total_contribution_per_year <- dataset %>% group_by(category,year) %>% 
    summarize(total_value = sum(value),.groups = 'drop')
  #now visualizing the grouped data 
  plot_nr2 <-  ggplot(Total_contribution_per_year, aes(x=factor(year),y=total_value,group=category)) + geom_line(aes(color=category),size=1.2) + 
    geom_point(size=.7,color='black') +
    scale_color_brewer(name='',palette='Paired') + 
    theme(legend.position='top') + 
    theme(axis.text.x = element_text(size=9,angle=45),
          legend.text=element_text(size=9)) + 
    labs(title='International Greenhouse gas emissions')+
    guides(color=guide_legend(ncol=3))
  
  
  output$plot2 <- renderPlot({plot_nr2})
  
  ### Gaswise emissions section:
  
  #1. Analysis of "GHG" gas emission
  
  #selecting a subset of data that only contains "GHG" gas emission
  GHG_data <- dataset[dataset$category=='GHG',]
  #grouping by each country to know which countries produced highest GHG
  GHG_by_country <- GHG_data%>% group_by(country) %>%summarise(sum(value))
  colnames(GHG_by_country)[2] <- "total_value"#changing name of column
  GHG_by_country <- GHG_by_country %>% arrange(desc(total_value))
  
  
  bar_ghg <- ggplot(GHG_by_country, aes(x= reorder(country, -total_value), y=total_value)) +
    geom_bar(stat="identity", fill="maroon2", alpha=1, width=.4) +
    xlab("Country") +
    ylab("Value")+
    ggtitle("Total emission of 'GHG' by each country from 1990-2018")+
    coord_flip(expand = TRUE)
  
  pie <- ggplot(GHG_by_country,aes(x="",y=total_value,fill = country))+
    geom_bar(stat = "identity",colour="gray")
  pie_ghg <-pie+coord_polar("y", start=0,direction = 1)+
    ggtitle("Total 'GHG' emission by Country",subtitle = "from the year 1990 -2018")
  
  # 2. Analysis of "GHG_indirect_CO2" gas emission
  
  #selecting a subset of data that only contains "GHG" gas emission
  GHG_indirect_CO2 <- dataset[dataset$category=='GHG_indirect_CO2',]
  #grouping by each country to know which countries produced highest GHG
  GHG_indirect_country <- GHG_indirect_CO2%>% group_by(country) %>%summarise(total_value = sum(value),.groups = 'drop')
  GHG_indirect_country <- GHG_indirect_country %>% arrange(desc(total_value))
  
  #visualising the GHG_indirect_CO2
  bar_GHG_indirect_CO2 <- ggplot(GHG_indirect_country, aes(x= reorder(country, -total_value), y=total_value)) +
    geom_bar(stat="identity", fill="blue", alpha=1, width=.4) +
    xlab("Country") +
    ylab("Value")+
    ggtitle("Total emission of 'GHG_indirect_CO2' by each country from 1990-2018")+
    coord_flip(expand = TRUE)
  
  pie <- ggplot(GHG_indirect_country,aes(x="",y=total_value,fill = country))+
    geom_bar(stat = "identity",colour="gray")
  pie_GHG_indirect_CO2 <- pie+coord_polar("y", start=0,direction = 1)+
    ggtitle("Total 'GHG_indirect_CO2' by Country",subtitle = "from the year 1990 -2018")
  
  ##**3.Analysis of CO2 emission**
  
  
  #selecting a subset of data that only contains "CO2" gas emission
  CO2_data <- dataset[dataset$category=='CO2',]
  #grouping by each country to know which countries produced highest GHG
  CO2_by_country <- CO2_data%>% group_by(country) %>%summarise(total_value = sum(value),.groups = 'drop')
  CO2_by_country <- CO2_by_country %>% arrange(desc(total_value))
  
  #visualising the GHG_indirect_CO2
  bar_CO2 <- ggplot(CO2_by_country, aes(x= reorder(country, -total_value), y=total_value)) +
    geom_bar(stat="identity", fill="darkgreen", alpha=1, width=.4) +
    xlab("Country") +
    ylab("Value")+
    ggtitle("Total emission of 'CO2' by each country from 1990-2018")+
    coord_flip(expand = TRUE)
  
  pie<- ggplot(CO2_by_country,aes(x="",y=total_value,fill = country))+
    geom_bar(stat = "identity",colour="gray")
  Pie_CO2 <- pie+coord_polar("y", start=0,direction = 1)+
    ggtitle("Total 'CO2' by Country",subtitle = "from the year 1990 -2018")
  
  #**analysis of 'CH4'**
  
  #selecting a subset of data that only contains "CO2" gas emission
  CH4_data <- dataset[dataset$category=='CH4',]
  #grouping by each country to know which countries produced highest GHG
  CH4_by_country <- CH4_data%>% group_by(country) %>%summarise(total_value = sum(value),.groups = 'drop')
  CH4_by_country <- CH4_by_country %>% arrange(desc(total_value))
  
  #visualising the GHG_indirect_CO2
  bar_ch4 <- ggplot(CH4_by_country, aes(x= reorder(country, -total_value), y=total_value)) +
    geom_bar(stat="identity", fill="cyan4", alpha=1, width=.4) +
    xlab("Country") +
    ylab("Value")+
    ggtitle("Total emission of 'CH4' by each country from 1990-2018")+
    coord_flip(expand = TRUE)
  
  pie <- ggplot(CH4_by_country,aes(x="",y=total_value,fill = country))+
    geom_bar(stat = "identity",colour="gray")
  pie_ch4 <- pie+coord_polar("y", start=0,direction = 1)+
    ggtitle("Total 'CH4' by Country",subtitle = "from the year 1990 -2018")+
    ylab("In kilotons equivalent")
  
  
  
  output$plot3 <- renderPlot({
    if(input$dropdown1 == "GHG") { bar_ghg    
      
      
    } else if(input$dropdown1 == 'GHG_indirect_CO2') {bar_GHG_indirect_CO2 
    } else if(input$dropdown1 =='CO2'){bar_CO2 
    } else if(input$dropdown1 == 'CH4') {bar_ch4 
    }
  })
  
  output$plot4 <- renderPlot({
    
    if(input$dropdown1 == "GHG") { pie_ghg    
    } else if(input$dropdown1 == 'GHG_indirect_CO2') {pie_GHG_indirect_CO2  
    } else if(input$dropdown1 =='CO2'){Pie_CO2 
    } else if(input$dropdown1 == 'CH4') {pie_ch4 
    }
  })
  
  output$countries_observations <- renderPrint({ 
    if(input$dropdown1 == "GHG") { 'countries-- usa,russia,japan,germany, canada ,uk and france are at top and all of these combine contributing a major chunk of GHG emission.'    
    } else if(input$dropdown1 == 'GHG_indirect_CO2') {'countries-- usa,russia,japan,germany, canada,france and italy are at top and all of these combine contributing a major chunk of GHG_indirect_CO2 emission.' 
    } else if(input$dropdown1 =='CO2'){'the Top CO2 emission countries are = usa,russia,japan,germany,canada,uk and italy' 
    } else if(input$dropdown1 == 'CH4') {'the Top CH4 emission countries are = usa,russia,ukraine,canada,australia,uk and germany' 
    }
  })
  
  # **USA emissions from 1990 to 2018** 
  #analysis of USA country emissions 
  usa_emissions <- dataset[dataset$country=='usa',] #selecting data for a single country
  u1 <- usa_emissions %>% group_by(category, year) %>% #grouping category and year together
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=year,y=total_value, col=category ))+#plotting a line plot
    geom_line(na.rm=TRUE, size=1)+
    geom_point(size=.5,alpha = .5,color = "black")+
    scale_color_brewer(name='',palette='Paired')+
    theme_minimal()+
    labs(title='USA Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')+
    xlab('year')+ylab('')
  
  #visualizing bar plot to know the portion of each emission gases and compare with each other
  u2 <- usa_emissions %>% group_by(category, year) %>% 
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=category, y=total_value,fill = category))+
    geom_col(width = .6)+
    theme(axis.text.x = element_text(size=10))+#increasing the length of x axis text
    xlab("Category")+
    ylab("In kilotons equivalent")+
    labs(title='USA Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')
  
  #lets find out the % of emission of each USA gases from 1990 to 2018
  usa_gas_wise_total <- usa_emissions%>% group_by(category) %>%summarise(total_value = sum(value),.groups = 'drop')
  #find the sum of total country gas emission
  usa_sum <- sum(usa_gas_wise_total$total_value)
  #create a column name 'emission_percentage and calculate the % of contribution'
  usa_percentage <- mutate(usa_gas_wise_total, usa_emission_percentage = (usa_gas_wise_total$total_value/usa_sum)*100)%>% mutate_at(vars(usa_emission_percentage), funs(round(., 2)))
  u3 <- usa_percentage
  
  u3
  ##**Russia emissions from 1990 to 2018**
  
  #analysis of Russia country emissions 
  russia_emissions <- dataset[dataset$country=='russia',] #selecting data for a single country
  r1 <-russia_emissions %>% group_by(category, year) %>% #grouping category and year together
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=year,y=total_value, col=category ))+#plotting a line plot
    geom_line(na.rm=TRUE, size=1)+
    geom_point(size=.5,alpha = .5,color = "black")+
    scale_color_brewer(name='',palette='Paired')+
    theme_minimal()+
    labs(title='Russia Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')+
    xlab('year')+ylab('')
  
  
  #visualizing bar plot to know the portion of each emission gases and compare with each other
  r2<- russia_emissions %>% group_by(category, year) %>% 
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=category, y=total_value,fill = category))+
    geom_col(width = .6)+
    theme(axis.text.x = element_text(size=10))+#increasing the length of x axis text
    xlab("Category")+
    ylab("In kilotons equivalent")+
    labs(title='Russia Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')
  
  
  #lets find out the % of emission of each Russia gases from 1990 to 2018
  russia_gas_wise_total <- russia_emissions%>% group_by(category) %>%summarise(total_value = sum(value),.groups = 'drop')
  #find the sum of total country gas emission
  russia_sum <- sum(russia_gas_wise_total$total_value)
  #create a column name 'emission_percentage and calculate the % of contribution'
  russia_percentage <- mutate(russia_gas_wise_total, russia_emission_percentage = (russia_gas_wise_total$total_value/russia_sum)*100)%>% mutate_at(vars(russia_emission_percentage), funs(round(., 2)))
  r3 <- russia_percentage
  
  #**Japan emissions from 1990 to 2018**
  
  #analysis of Japan country emissions 
  japan_emissions <- dataset[dataset$country=='japan',] #selecting data for a single country
  j1 <- japan_emissions %>% group_by(category, year) %>% #grouping category and year together
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=year,y=total_value, col=category ))+#plotting a line plot
    geom_line(na.rm=TRUE, size=1)+
    geom_point(size=.5,alpha = .5,color = "black")+
    scale_color_brewer(name='',palette='Paired')+
    theme_minimal()+
    labs(title='Japan Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')+
    xlab('year')+ylab('')
  
  
  #visualizing bar plot to know the portion of each emission gases and compare with each other
  j2 <- japan_emissions %>% group_by(category, year) %>% 
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=category, y=total_value,fill = category))+
    geom_col(width = .6)+
    theme(axis.text.x = element_text(size=10))+#increasing the length of x axis text
    xlab("Category")+
    ylab("In kilotons equivalent")+
    labs(title='Japan Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')
  
  #lets find out the % of emission of each Japan gases from 1990 to 2018
  japan_gas_wise_total <- japan_emissions%>% group_by(category) %>%summarise(total_value = sum(value),.groups = 'drop')
  #find the sum of total country gas emission
  japan_sum <- sum(japan_gas_wise_total$total_value)
  #create a column name 'emission_percentage and calculate the % of contribution'
  japan_percentage <- mutate(japan_gas_wise_total, japan_emission_percentage = (japan_gas_wise_total$total_value/japan_sum)*100)%>% mutate_at(vars(japan_emission_percentage), funs(round(., 2)))
  j3 <- japan_percentage
  
  
  #**Germany emissions from 1990 to 2018**
  #analysis of Germany country emissions 
  germany_emissions <- dataset[dataset$country=='germany',] #selecting data for a single country
  g1 <- germany_emissions %>% group_by(category, year) %>% #grouping category and year together
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=year,y=total_value, col=category ))+#plotting a line plot
    geom_line(na.rm=TRUE, size=1)+
    geom_point(size=.5,alpha = .5,color = "black")+
    scale_color_brewer(name='',palette='Paired')+
    theme_minimal()+
    labs(title='Germany Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')+
    xlab('year')+ylab('')
  
  #visualizing bar plot to know the portion of each emission gases and compare with each other
  g2 <- germany_emissions %>% group_by(category, year) %>% 
    summarise(total_value=sum(value),.groups = 'drop')%>%
    ggplot(aes(x=category, y=total_value,fill = category))+
    geom_col(width = .6)+
    theme(axis.text.x = element_text(size=10))+#increasing the length of x axis text
    xlab("Category")+
    ylab("In kilotons equivalent")+
    labs(title='Germany Gas Emissions',
         subtitle= 'in kilotons , from 1990-2018')
  
  #lets find out the % of emission of each Japan gases from 1990 to 2018
  germany_gas_wise_total <- germany_emissions%>% group_by(category) %>%summarise(total_value = sum(value),.groups = 'drop')
  #find the sum of total country gas emission
  germany_sum <- sum(germany_gas_wise_total$total_value)
  #create a column name 'emission_percentage and calculate the % of contribution'
  germany_percentage <- mutate(germany_gas_wise_total, germany_emission_percentage = (germany_gas_wise_total$total_value/germany_sum)*100)%>% mutate_at(vars(germany_emission_percentage), funs(round(., 2)))
  g3<-germany_percentage
  
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
