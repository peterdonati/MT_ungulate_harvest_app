#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Shiny app for MT ungulate harvest
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(rlang)
library(bslib)
library(tidyr)
source("helpers.R")

deer_dat <- read.csv(
  "https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/tidy_deer.csv",
  colClasses = c(
    "integer", "character", "character", "integer", "integer", "integer", 
    "integer", "integer", "integer", "integer", "double"
  )
)
elk_dat <- read.csv(
  "https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/tidy_elk.csv",
  colClasses = c(
    "integer", "character", "integer", "integer", "integer", "integer", 
    "integer", "integer", "integer", "double"
  )
)

# UI ===========================================================================
ui <- fluidPage(
  
  tags$head(includeHTML("google_analytics.html")),
  
  theme = bs_theme(
    base_font = list(font_google("Raleway"), "Segoe UI", "serif"), 
    bootswatch = "flatly"
  ),
  
  navbarPage(
    NULL,
    
    tabPanel(
      NULL, icon = icon("home"),
      h1("Montana Deer & Elk Harvest App"),
      p(
        "This app allows you to explore past deer and elk 
        harvest estimates made by Montana FWP since 2004.", 
        br(), 
        br(),
        "To create visuals and tables for specific hunting 
        districts, head to the", strong("Deer"), "or", 
        strong("Elk"), "tabs above.",
        br(),
        "Navigate to the", strong("Summary Tables"), "tab 
        to discover productive districts."
      ),
      br(),
      br(),
      div(style = "border-bottom: 5px solid #314859; padding-bottom: 50px")
    ),
    
    tabPanel(
      "Deer",
      plot_ui("d", deer_dat)
    ),
    tabPanel(
      "Elk",
      plot_ui("e", elk_dat)
    ),
    
    tabPanel(
      "Summary Tables",
      div(
        h5("Values in table are averages across chosen years."),
        style = "color: #314859"
      ),
      hr(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("st_ung", "Species", choices = c("Deer", "Elk")),
          uiOutput("st_deer_sp"),
          selectInput(
            "st_reg", 
            "Specific Region(s)", 
            choices = c("1", "2", "3", "4", "5", "6", "7"),
            multiple = TRUE
          ),
          sliderInput(
            inputId = "st_yr",
            label = "Years",
            min = 2004,
            max = 2020,
            value = c(2004, 2020),
            step = 1,
            ticks = FALSE,
            sep = ""
          ),
          downloadButton(
            "st_download", 
            ".csv download", 
            icon = icon("fas fa-file-download")
          )
        ),
        mainPanel(
          width = 9,
          div(
            DTOutput("st"), 
            style = "font-family:Arial, Helvetica, serif; font-size:12px;
            border-radius: 4px; border-style: solid; border-color: #c9c9c9;
            border-width: 1px 1px 3px 1px; padding: 2px"
          )
        )
      )
    ),
    
    tabPanel(
      "About",
      h1("About the project"),
      p(
        "All raw data on past harvest estimates comes",
        tags$a(href= "https://myfwp.mt.gov/fwpPub/harvestReports", "from FWP"),
        br(),
        "The exact data and source code used in this app can be accessed",
        tags$a(href= "https://github.com/peterdonati/MT_ungulate_harvest_app", "here at my GitHub"),
      ),
      h1("Contact"),
      p(
        "Send me your errors, suggestions, or questions!",
        br(),
        tags$i(class = "far fa-paper-plane"), "p.donati11@gmail.com",
        br(),
        tags$i(class = "fab fa-github"),
        tags$a(href = "https://github.com/peterdonati", "peterdonati")
      ),
      hr(),
      br(),
      h4("Updates"),
      p("July 2021:",
        tags$ul(
          tags$li("Added data for 2020 harvest estimates"),
          tags$li("Removed misleading success percentages
                  when looking at specific deer species"),
          tags$li("Added separate elk and deer tabs for side by side 
                  comparisons of plots"),
          tags$li("Added table outputs to see the data behind plots"),
          tags$li("Visual improvements"),
          style = "font-size: 12px"
        )
      ),
      div(style = "border-bottom: 5px solid #314859; padding-bottom: 50px")
    )
  )
)

# server =======================================================================
server <- function(input, output, session){
  
  # Deer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$d_split <- renderUI({
    if(input$d_yaxis == "total_harvest"){
      selectInput(
        inputId = "d_split",
        label = "Split harvest by",
        choices = c("Nothing", "Weapon", "Sex")
      )
    }
  })
  observeEvent(input$d_sp, {
    if (input$d_sp != "all_deer"){
      updateSelectInput(
        session, 
        "d_yaxis",
        choices = c(
          "Harvest" = "total_harvest",
          "Hunters" = "hunters"
        )
      )
    } else {
      updateSelectInput(
        session, 
        "d_yaxis",
        choices = c(
          "Harvest" = "total_harvest",
          "Hunters" = "hunters",
          "Success rate" = "success_rate_per_hunter"
        )
      )
      
    }
  })
  
  filt_deer <- reactive({
    d_years <- input$d_yr[[1]]:input$d_yr[[2]]
    d_out <- subset(
      deer_dat, 
      license_year %in% d_years &
        hunting_district %in% input$d_dist &
        deer_species %in% input$d_sp
    )
    if (input$d_sp != "all_deer"){
      d_out <- select(d_out, !success_rate_per_hunter)
    }
    return(d_out)
  })
  
  output$d_table <- renderDT(
    {filt_deer()}, 
    rownames = F,
    fillContainer = TRUE,
    options = list(
      scrollY = "500px",
      columnDefs = list(list(className = "dt-right", targets = "_all"))
    )
  )
  
  d_plot <- reactive({
    d_plot_dat <- filt_deer()
    
    if (!is.null(input$d_dist)){
      if (input$d_yaxis == "total_harvest"){
        if (input$d_split == "Nothing" || is.null(input$d_split)){
          d_total_harvest <- plot_total_harvest(d_plot_dat)
          return(d_total_harvest)
        } else if (input$d_split == "Sex") {
          d_plot_dat <- pivot_longer(
            d_plot_dat, 
            c("bucks", "does"),
            names_to = "sex",
            values_to = "s_harv"
          )
          d_s_harvest <- plot_sex_harvest(d_plot_dat)
          return(d_s_harvest)
        } else if (input$d_split == "Weapon") {
          d_plot_dat <- pivot_longer(
            d_plot_dat, 
            c("bow", "rifle"),
            names_to = "weapon",
            values_to = "w_harv"
          )
          d_w_harvest <- plot_weapon_harvest(d_plot_dat)
          return(d_w_harvest)
        }
      } else if(input$d_yaxis == "hunters"){
        d_plot_dat <- na.omit(d_plot_dat)
        d_hunters <- plot_hunters(d_plot_dat)
      return(d_hunters)
      } else if (input$d_yaxis == "success_rate_per_hunter"){
        d_plot_dat <- na.omit(d_plot_dat)
        d_success <- plot_success(d_plot_dat)
        return(d_success)
      } 
    } else {
      plot_empty()
    }
  })
  
  output$d_plot <- renderPlot({d_plot()}, width = 700, res = 96,
                              height = function() input$d_height)
  
  # Elk ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$e_split <- renderUI({
    if(input$e_yaxis == "total_harvest"){
      selectInput(
        inputId = "e_split",
        label = "Split harvest by",
        choices = c("Nothing", "Weapon", "Sex")
      )
    }
  })
  
  filt_elk <- reactive({
    e_years <- input$e_yr[[1]]:input$e_yr[[2]]
    e_out <- subset(
      elk_dat, 
      license_year %in% e_years &
        hunting_district %in% input$e_dist
    )
    return(e_out)
  })
  
  output$e_table <- renderDT(
    {filt_elk()}, 
    rownames = F,
    fillContainer = TRUE,
    options = list(
      scrollY = "500px",
      columnDefs = list(list(className = "dt-right", targets = "_all"))
    )
  )
  
  output$e_plot <- renderPlot({
    e_plot_dat <- filt_elk()
    
    if (!is.null(input$e_dist)){
      if (input$e_yaxis == "total_harvest"){
        if (input$e_split == "Nothing" || is.null(input$e_split)){
          e_total_harvest <- plot_total_harvest(e_plot_dat)
          return(e_total_harvest)
        } else if (input$e_split == "Sex") {
          e_plot_dat <- pivot_longer(
            e_plot_dat, 
            c("bulls", "cows"),
            names_to = "sex",
            values_to = "s_harv"
          )
          e_s_harvest <- plot_sex_harvest(e_plot_dat)
          return(e_s_harvest)
        } else if (input$e_split == "Weapon") {
          e_plot_dat <- pivot_longer(
            e_plot_dat, 
            c("bow", "rifle"),
            names_to = "weapon",
            values_to = "w_harv"
          )
          e_w_harvest <- plot_weapon_harvest(e_plot_dat)
          return(e_w_harvest)
        }
      } else if(input$e_yaxis == "hunters"){
        e_plot_dat <- na.omit(e_plot_dat)
        e_hunters <- plot_hunters(e_plot_dat)
        return(e_hunters)
      } else if (input$e_yaxis == "success_rate_per_hunter"){
        e_plot_dat <- na.omit(e_plot_dat)
        e_success <- plot_success(e_plot_dat)
        return(e_success)
      } 
    } else {
      plot_empty()
    }
  },
  width = 700,
  res = 96,
  height = function() input$e_height
  )
  
  # Summary Table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$st_deer_sp <- renderUI({
    if (input$st_ung == "Deer"){
      selectInput(
        inputId = "st_deer_sp",
        label = "Deer species",
        choices = c("Combined" = "all_deer", "Whitetail" = "wt", "Mule" = "md")
      )
    }
  })
  
  st <- reactive({
    # !is.null() avoids error in lag while rendering st_deer_sp ui above:
    if (input$st_ung == "Deer" && !is.null(input$st_deer_sp)){
      st_dat <- subset(deer_dat, deer_species %in% input$st_deer_sp)
      if (input$st_deer_sp != "all_deer"){
        st_dat <- select(st_dat, !success_rate_per_hunter) # All N/A's anyways
      }
    } else {
      st_dat <- elk_dat
    }
    region_expr <- paste0("^", input$st_reg, collapse = "|")
    st_dat <- st_dat[grep(region_expr, st_dat$hunting_district), ]
    st_yrs <- input$st_yr[1]:input$st_yr[2]
    st_dat <- subset(st_dat, license_year %in% st_yrs)
    st_dat <- select(st_dat, !license_year) # don't want to take average of this
    st_dat <- st_dat %>% 
      group_by(hunting_district) %>% 
      summarise(across(is.numeric, mean, na.rm = T)) %>% 
      mutate(
        across(is.numeric, round, 2),
        from = input$st_yr[1], 
        to = input$st_yr[2]
        ) %>% 
      select(hunting_district, from, to, everything())
      
    return(st_dat)
  })
  
  output$st <- renderDT(
    {st()},
    rownames = FALSE,
    fillContainer = TRUE,
    options = list(
      scrollY = "500px",
      paging = FALSE,
      columnDefs = list(list(className = "dt-right", targets = "_all"))
    )
  )
  
  output$st_download <- downloadHandler(
    filename = function(){
      if (input$st_ung == "Deer" && !is.null(input$st_deer_sp)){
        paste0(input$st_deer_sp, "_", input$st_yr[1], "-", input$st_yr[2], 
               ".csv")
      } else {
        paste0(input$st_ung, "_", input$st_yr[1], "-", input$st_yr[2], ".csv")
      }
    },
    content = function(file){
      write.csv(st(), file, row.names = F)
    }
  )
}

# The app ======================================================================
shinyApp(ui, server)
