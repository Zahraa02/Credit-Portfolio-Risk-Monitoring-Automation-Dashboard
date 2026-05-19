dashboardPage(
  skin = "purple",
  
  # ---------- Header ----------
  dashboardHeader(
    title = "Credit Portfolio Risk Monitoring & Automation Dashboard"
    
  ),
  
  
  # ---------- Sidebar ----------
  dashboardSidebar(
    
    sidebarMenu(
      
      # ---------- Tab 1: NPL Overview ----------
      menuItem("NPL Overview", tabName = "page1", icon = icon("house")),
      
      # ---------- Tab 2: DPD Overview ----------
      menuItem("DPD Overview", tabName = "page2", icon = icon("house")),
      
      # ---------- Tab 3: NPL Detail ----------
      menuItem("NPL Detail", tabName = "page3", icon = icon("layer-group")),
      
      # ---------- Tab 4: Dataset ----------
      menuItem("Dataset", tabName = "page4", icon = icon("layer-group"))
      
    )
    
  ),
  
  
  # ---------- Body ----------
  dashboardBody
  (
    tags$head
    (
      tags$style(HTML("
    .small-box {
      border-radius: 15px;  
      box-shadow: none;
    }
      
    h2 {
      font-size: 40px;
      font-weight: bold; 
    }
    
    h3 {
      font-size: 25px;
      font-weight: bold; 
    }
      "))
    ),
    
    
    tabItems
    (
      
      # ---------- Tab 1 ----------
      tabItem(
        tabName = "page1",
        h2("Portofolio Weekly Update"), 
        
        # ---------- Tab 1, Row 1 ----------
        fluidRow
        (
          box
          (
            width = 12, 
            dateInput
            (
              inputId= "input_cutoffdate", 
              label= "Select Saturday Cut Off Date", 
              min = "2024-09-07",
              max = max(c(
                max(sqllab_gross_clean$cut_off_date),
                max(sqllab_net_clean$cut_off_date),
                max(sqllab_external_clean$cut_off_date)
              )),
              value = max(c(
                max(sqllab_gross_clean$cut_off_date),
                max(sqllab_net_clean$cut_off_date),
                max(sqllab_external_clean$cut_off_date)
              )),
              format = "yyyy-mm-dd"
            )
          )
        ),
        
        # ---------- Tab 1, Row 2 ----------
        fluidRow
        (
          box(
            width = 6,
            plotlyOutput(outputId = "plot1a_bar")
          ),
          box(
            width = 6,
            plotlyOutput(outputId = "plot1b_bar")
          )
        ),
        
        # ---------- Tab 1, Row 3 ----------
        fluidRow
        (
          box(
            width = 6,
            plotlyOutput(outputId = "plot2a_bar")
          ),
          box(
            width = 6,
            plotlyOutput(outputId = "plot2b_bar")
          )
        ),
        
        # ---------- Tab 2, Row 4 ----------
        fluidRow
        (
          box(
            width = 6,
            plotlyOutput(outputId = "plot3a_bar")
          ),
          box(
            width = 6,
            plotlyOutput(outputId = "plot3b_bar")
          )
        ),
        
        # ---------- Tab 1, Row 5 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot4_bar")
          )
        ),
        
        # ---------- Tab 1, Row 6 ----------
        fluidRow
        (
          # ---------- Tab 1, Row 6, Col 1 ----------
          box(
            width = 6,
            plotlyOutput(outputId = "plot6_bar")
          ),
          
          # ---------- Tab 1, Row 6, Col 2 ----------
          column(
            width = 6,
            fluidRow
            (
              valueBoxOutput(outputId = "npl_weekly_diff",
                             width = 12)
            ),
            fluidRow
            (
              valueBoxOutput(outputId = "DPD90_amount_weekly_diff",
                             width = 12)
            ),
            p("(+) Increase", style = "font-size: 15px"),
            p("(-) Decrease", style = "font-size: 15px")
          )
        ),
        
        # ---------- Tab 1, Row 7 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot7_line")
          )
        ),
        
        # ---------- Tab 1, Row 8 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot8_line")
          )
        ),
        
        # ---------- Tab 1, Row 9 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot9_line")
          )
        )
      ), # closing tab 1
      
      
      
      # ---------- Tab 2 ----------
      tabItem(
        tabName = "page2",
        h2("DPD Bucket Overview"),
        
        # ---------- Tab 2, Row 1 ----------
        fluidRow
        (
          box
          (
            width = 12, 
            dateInput
            (
              inputId= "input_cutoffdate2", 
              label= "Select Saturday Cut Off Date", 
              min = "2024-09-07",
              max = max(c(
                max(sqllab_gross_clean$cut_off_date),
                max(sqllab_net_clean$cut_off_date),
                max(sqllab_external_clean$cut_off_date)
              )),
              value = max(c(
                max(sqllab_gross_clean$cut_off_date),
                max(sqllab_net_clean$cut_off_date),
                max(sqllab_external_clean$cut_off_date)
              )),
              format = "yyyy-mm-dd"
            )
          )
        ),
        
        # ---------- Tab 2, Row 2 ----------
        h3("DPD Bucket by Amount"),
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot7_bar")
          )
        ),
        
        # ---------- Tab 2, Row 3 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot8_bar")
          )
        ),
        
        # ---------- Tab 2, Row 4 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot9_bar")
          )
        ),
        
        # ---------- Tab 2, Row 5 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot10_bar")
          )
        ),
        
        # ---------- Tab 2, Row 6 ----------
        h3("DPD Bucket by Accounts"),
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot11_bar")
          )
        ),
        
        # ---------- Tab 2, Row 7 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot12_bar")
          )
        ),
        
        # ---------- Tab 2, Row 8 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot13_bar")
          )
        ),
        
        # ---------- Tab 2, Row 9 ----------
        fluidRow
        (
          box(
            width = 12,
            plotlyOutput(outputId = "plot14_bar")
          )
        )
      ), # closing tab 2
      
      
      
      # ---------- Tab 3 ----------
      tabItem(
        tabName = "page3",
        h2("Detail NPL Net: Island, Regional, Area, and Branch"),
        
        # ---------- Tab 3 Row 1 ----------
        fluidRow
        (
          box
          (
            width = 12, 
            dateInput
            (
              inputId= "input_cutoffdate3", 
              label= "Select Saturday Cut Off Date", 
              min = "2024-09-07",
              max = max(c(
                max(sqllab_gross_clean$cut_off_date),
                max(sqllab_net_clean$cut_off_date),
                max(sqllab_external_clean$cut_off_date)
              )),
              value = max(c(
                max(sqllab_gross_clean$cut_off_date),
                max(sqllab_net_clean$cut_off_date),
                max(sqllab_external_clean$cut_off_date)
              )), 
              format = "yyyy-mm-dd"
            )
          )
        ),
        
        
        # ---------- Tab 3 Row 2 ----------
        fluidRow
        (
          box
          (
            width = 6,
            selectInput
            (
              inputId = "input_island",
              label = "Select Island of Choice",
              choices = c("All", sort(as.character(unique(sqllab_net_clean$island)))),
              selected = "All"
            )
          ),
          box
          (
            width = 6,
            selectInput
            (
              inputId = "input_region",
              label = "Select Region of Choice",
              choices = c("All", sort(as.character(unique(sqllab_net_clean$region)))),
              selected = "All"
            )
          )
        ),
        
        # ---------- Tab 3 Row 3 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "Detail NPL Net: Island",
            DT::DTOutput(outputId = "table1")
          )
        ),
        
        # ---------- Tab 3 Row 4 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "Detail NPL Net: Region",
            DT::DTOutput(outputId = "table2")
          )
        ),
        
        
        # ---------- Tab 3 Row 5 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "Detail NPL Net: Area",
            DT:: DTOutput(outputId = "table3")
          )
        ),
        
        
        # ---------- Tab 3 Row 6 ----------
        fluidRow
        (
          box
          (
            width = 12,
            selectInput
            (
              inputId = "input_area",
              label = "Select Area of Choice",
              choices = c("All" ,sort(as.character(unique(sqllab_net_clean$area)))),
              selected = "All"
            )
          )
        ),
        
        # ---------- Tab 3 Row 7 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "Detail NPL Net: Branch",
            DT::DTOutput(outputId = "table4")
          )
        ) 
      ), # closing tab 3
      
      
      
      # ---------- Tab 4 ----------
      tabItem(
        tabName = "page4",
        h2("Dataset"),
        
        # ---------- Tab 4 Row 1 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "Gross Loan Portofolio Overview",
            DT::DTOutput(outputId= "dataset_gross" ) 
          )
        ),
        
        # ---------- Tab 4 Row 2 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "Net Loan Portofolio Overview",
            DT::DTOutput(outputId= "dataset_net" ) 
          )
        ),
        
        # ---------- Tab 4 Row 3 ----------
        fluidRow
        (
          box(
            width = 12,
            title = "External Loan Portofolio Overview",
            DT::DTOutput(outputId= "dataset_external" ) 
          )
        )
      ) # closing tab 4
      
    ) # tabItems
  ) # dashboardBody
) # dashboatdPage