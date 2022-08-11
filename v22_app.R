

# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(rsconnect)
library(DT) # Raw Table
library(ggh4x) # BS Plots
library(shinyjs) # Reset cutoff values


#rsconnect::setAccountInfo(name='chdeckerbyu', 
#                          token='177DF71B55792B3219C4940DF6033F18', 
#                          secret='tYODoboazjg842O0N43+raQMz21vxSIv1ENGVbnG')
#rsconnect::deployApp('path/to/your/app')
#rsconnect::showLogs()



### IDEAS ###
# Be able to enter your own data! (not this version)
# Submitting new Cutoffs will remove all output visualizations
# Final -> add in webpage link



######
# Version 15: 
# Added Overview Page
# Checked cutoffs manually (found 50505 results)
# Altered mouse hover color scheme
# Changed BS plot x axis label spacing
# Reversed Relative Power color mapping
# Fixed >3 theta null for Raw Table
# Corrected Type 1 -> Type I in several places
# Respacing "Current Cutoffs Used"
# Fixed Correction Method naming in tableSummary
# Changed BS plot x axis and table comment spacing

# Version 16:
# Altered variable input parameter user messages
# Added punctuation to all user messages
# Tau^2 estimator 'None' -> 'N/A (fixed-effect)'
# Removed INV pooling_method from ALL RESULTS
# Removed PM tau2_estimators from ALL RESULTS

# Version 17:
# Figures & Values reset when resubmitting new Cutoffs!
# Remove "refresh the Input Parameters after updating the Cutoff Values" message
# Changed loading bar message
# Working on new grey area for No_CC (partially completed)

# Version 18:
# MAJOR CHANGES!
# Removed the new grey area for No_CC (partially completed) features for now
# Changed the original dataset and optimal cutoffs
# Altered the final_data_set for use in the app
# Changed Tau^2 estimator -> 'N/A (common-effect)'
# Changed "Exclude SZ" to "Exclude-SZ-DZ" 
# Changed DGM abbreviation for Bhaumik from "B" to "pRB"
# Removed "for \u03B8" for Bias and MAE
# Altered spacing for "Current Cutoffs Used"
# Changed "100 to 1000" to "100 to 500"
# Added reactive grey area for MH-No-CC using geom_rect()!
  # one artifact -> the other theta graph will display even if no data is present

# Version 18 (continued):
# Debugging grey area code (make it robust)
# Reducing BS plot code count to 2 (CE and RE plots)
# Adding in geom_blank() to ensure x and y-axis ranges for BS plots

# Version 19:
# Made No_CC_values and blank_BS_values their own reactive variables
# Debug the server disconnecting error -> Done
# Changed 'Number of Occurrences' to 'Proportion of Occurrences'
# New error: If you load the Results page too early you will get a "CC_all not found" error

# Version 20:
# Made CC_all variable reactive to fix the errors
# Removed "Update" button in the Cutoffs page 
# Removed progress bar
# Streamlined the updating of the Cutoff parameter text 
# This version automagically updates itself while keeping the Results page inputs
# This is possible because of the speed at which the new CC_all dataset is created


# Version 21:
# Replaced old final_data_set.csv with new which included some INV results for tau2=0 cases
# Added INV method_comb_app option in Dataset
# Added INV in the geom_blank() function
# Changed the order of the BS plot datasets (No_CC, CSC, etc) by altering the facet_wrap code
# Changed count of Datasets to Proportion of possible datasets in Table Summary
# Proportions different for MH-No-CC dataset
# Changed the size of the grey area 
# Removed Total Observations
# Tried to correct the order of all y-axis in the BS plots (failed)


# Version 22: 
# Results title changed from to "Meta-Analysis Methods That Meet Selected Cutoffs by Scenario"
# Spelling correction for 'Methods that meet the cirteria set in "Select Cutoffs"' (criteria)
# Removed Results page subtitle
# Ran a spellcheck on the cutoffs and results page
# Changed Overview text 
# Altered grey area spacing for reordered y-axis BS plots
# Revised proportions based on changes
# Releveled factors in CC_all() and blank_BS_values()
# Reordered geom_blank() to fix errors arising in y-axis BS plot ordering

# Added in search features to the Raw Table (error when filtering for Correction Method)
# Added column lines in Raw Table




# To Do: 

# Move the INV up, bring the grey area down (order = MH, INV, SA, SSW)
# Change the heterogeneity estimator (order = DL, IPM, SJ)


# Raw Table:
# Everything filter option except results (Median Bias, MAE, Coverage, Width, T1Error)
# Remove sliders for the numerics (if its easier) and filter using drop down options for factors
# Move DGM to first column



# FALL:
# research what is posit, trying to work on switching 330 work from R and Rmarkdown





# &nbsp; = one space
# &ensp; = two spaces
# &emsp; = four spaces
######




# Necessary to put 'Theta: ' into the facet labels
plot_labels <- as_labeller(
  c(`1.5`  = "\u03B8 = 1.5", 
    `1`    = "\u03B8 = 1",
    `0.5`  = "\u03B8 = 0.5", 
    `0`    = "\u03B8 = 0",
    `-0.5` = "\u03B8 = -0.5", 
    `-1`   = "\u03B8 = -1",
    `-1.5` = "\u03B8 = -1.5",
    `CAC-CC` = "CAC-CC",
    `CAC-CC (k=0.005)` = "CAC-CC (k=0.005)",   
    `CAS-CC`      = "CAS-CC",
    `CZC-CC`      = "CZC-CC",
    `E-CC`        = "E-CC",
    `E-CC (m=0.01)`   = "E-CC (m=0.01)",
    `Exclude-SZ-DZ`  = "Exclude-SZ-DZ",
    `MH-No-CC`       = "MH-No-CC",
    `TA-CC`       = "TA-CC",
    `TA-CC (m=0.01)`  = "TA-CC (m=0.01)"))



# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),   # flatly, cerulean, spacelab, united, yeti
                
                tags$head(tags$style(HTML('.navbar-default .navbar-brand:hover{ color: white }',
                                          '.navbar-default:hover .navbar-nav>li>a:hover{ color: #66ccff }'))),

                
                navbarPage(title = HTML(paste0("Meta-Analysis Methods")), 

                           tabPanel("Overview",
                                    # tags$h2("Continuity Correction Website Tutorial"),
                                    # Nesting the HTML code
                                    # tags$h4("This webage was designed to compliment",
                                    #         tags$a(href="www.rstudio.com", '"The Impact of Continuity Corrections on Rare-Event Meta-Analysis" ', style = "color: darkred;"),
                                    #         "by Zabriskie et al."),
                                    fluidPage(
                                      # Left side!
                                      column(6, wellPanel(

                                        tags$h3("Overview"),
                                        tags$h5(HTML("This webpage displays simulation results from Zabriskie et al. (2022). While we provide some information here, we refer users to the paper for additional important details.")),

                                        br(),
                                        tags$h4("Step 1: Select Cutoffs"),
                                        tags$h5(HTML("In the “Select Cutoffs” tab, you have the option of choosing cutoff values to filter the meta-analysis methods to obtain “good” methods according to the chosen cutoffs. If no selection is made, the default values, as seen on that page, will be used. As different cutoff values are chosen, the data set will update automatically in the background. Clicking “Reset Cutoffs” returns the cutoffs to the default values.")),
                                        
                                        br(),
                                        tags$h4("Step 2: Visualize Results"),
                                        tags$h5(HTML("The “Results” tab displays the meta-analysis methods that meet the criteria chosen in the “Select Cutoffs” tab. A battleship plot will appear as you choose values for the four dataset parameters. These plots indicate methods that meet the chosen cutoffs using a square, colored by relative power.")),
                                        tags$h5(HTML("You may choose up to three values for the true log-odds ratio (θ), as used in the simulation study. If you have a specific data set you are trying to match with these simulation results, you may wish to first apply a standard meta-analysis method to obtain a rough estimate of the true log-odds ratio from your data set prior to using this webpage.")),
                                        tags$h5(HTML("Note that the baseline event probability reflects the average event rate in the control group (not the treatment group or the average of both groups).")),

                                      )),



                                      # Right side!
                                      column(6, wellPanel(
                                        
                                        tags$h3("Abbrevitions:"),
                                        tags$h4("Data Generating Models (DGM):"),
                                        tags$h5(HTML("pF = pCfixed (Pateras et al., 2017)", '<div>',
                                                     "pR = pRandom (Pateras et al., 2017)", '<div>',
                                                     "pRB = pRandomB (Bhaumik et al., 2012)", '<div>'
                                                     )),
                                        
                                        tags$h4("Pooling Methods:"),
                                        tags$h5(HTML("INV = Inverse Variance", '<div>',
                                                     "MH = Mantel-Haenszel", '<div>',
                                                     "SA = Simple Average", '<div>',
                                                     "SSW = Sample Size Weights", '<div>',
                                        )),
                                        
                                        tags$h4("Correction Methods:"),
                                        tags$h5(HTML("CC = Continuity Correction", '<div>',
                                                     "CAC-CC = Constant to All Cells CC", '<div>',
                                                     "CZC-CC = Constant to Zero Cells CC", '<div>',
                                                     "CAS-CC = Constant to All Studies CC", '<div>',
                                                     "TA-CC = Treatment Arm CC (Sweeting et al., 2004)", '<div>',
                                                     "E-CC = Empirical CC (Sweeting et al., 2004)", '<div>',
                                                     "Exclude-SZ-DZ = Exclude Single- and Double-Zero Studies", '<div>',
                                                     "MH-No-CC = MH Pooling Method with No CC", '<div>',
                                        )),


                                        tags$h4("Heterogenity Variance Estimators:"),
                                        tags$h5(HTML("DL = DerSimonian and Laird", '<div>',
                                                     "SJ = Sidik and Jonkman", '<div>',
                                                     "PM = Paule-Mandel", '<div>',
                                                     "IPM = Improved Paule-Mandel", '<div>',
                                        )),

                                        tags$h4("Other:"),
                                        tags$h5(HTML("MAE = Mean Absolute Error", '<div>',
                                                     "CI = Confidence Interval", '<div>',
                                                     ))
                                      )),
                                    ), #fluidPage (left and right sides)
                                    
                                    # Citations
                                    wellPanel(
                                      tags$h4(HTML('<b>', "Citations:", '</b>')),
                                      tags$h5("Bhaumik, D. K., Amatya, A., Normand, S.-L. T., Greenhouse, J., Kaizer, E., Neelon, B., and Gibbons, R. D. (2012). Meta-analysis of rare binary adverse event data. Journal of the American Statistical Association, 107(498):555–567."),
                                      tags$h5("Pateras, K., Nikolakopoulos, S., and Roes, K. (2017). Data-generating models of dichotomous outcomes: Heterogeneity in simulation studies for a random-effects meta-analysis. Statistics in Medicine, 37(7):1115–1124."),
                                      tags$h5("Sweeting, M., Sutton, A., and Lambert, P. (2004). What to add to nothing? use and avoidance of continuity corrections in meta-analysis of sparse data. Statistics in Medicine, 23(9):1351–1375."),
                                      tags$h5("Zabriskie, B. N., Cole, N., Baldauf, J., Decker, C. (2022). The Impact of Continuity Corrections on Rare-Event Meta-Analysis."),
                                      )
                                    
                                    
                           ), # tabPanel (Page 1)
                           
                           
                           
                           tabPanel("Select Cutoffs", 
                                    sidebarPanel(width = 5,
                                      tags$h3("Input Values"),
                                      br(),
                                      sliderInput(inputId = 'CRIT_prop_less_05_theta', label = "Type I Error Rate Cutoff", min = 0, max = 1, value = 0.055, step = 0.005),
                                      sliderInput(inputId = 'CRIT_mean_ci_coverage_theta', label = "Mean CI Coverage Cutoffs", min = 0.25, max = 1, value = c(0.945,0.98), step = 0.005, dragRange = TRUE),
                                      sliderInput(inputId = 'CRIT_median_ci_width', label = "Median CI Width Cutoff", min = 0, max = 23.5, value = 4, step = .5),
                                      sliderInput(inputId = 'CRIT_bias_median_theta', label = "Median Bias Cutoff", min = 0, max = 2.1, value = 0.3, step = 0.01),
                                      sliderInput(inputId = 'CRIT_MAE_theta', label = "MAE Cutoff", min = 0, max = 2.1, value = 0.6, step = 0.05),
                                      #actionButton(inputId = 'update_data', label = "Submit New Cutoffs"),
                                      
                                      # br(),
                                      # br(),
                                      
                                      actionButton(inputId = 'reset_data', label = "Reset Cutoffs"),
                                      tags$style(type="text/css", "#reset_data {color : #ffb3b3;}"),
                                      # useShinyjs() is to call for reset of sliderInput data
                                      useShinyjs()
                                      ), # sidebarPanel
                                    
                                    mainPanel(width = 7,
                                      h2("Select Cutoffs"),
                                      h4('The selected cutoffs will be used as cutoffs to filter the meta-analysis methods which will be displayed in the "Results" page. '),
                                      
                                      br(),
                                      
                                      # Output: Summary displaying the selected cutoffs
                                      htmlOutput('CRIT_user_text', placeholder = FALSE)
                                      
                                    ) # mainPanel
                                    
                           ), # tabPanel (Page 2)
                           
                           
                           
                           
                           
                           
                           
                           
                           tabPanel("Results", 
                                    
                                    h2("Meta-Analysis Methods That Meet Selected Cutoffs by Scenario"),

                                    # Inputs
                                    wellPanel(
                                      fluidRow(
                                        
                                        column(2, #offset = 1,
                                               # Input Title
                                               tags$h3("Input: Study Parameters")),
                                        
                                        column(2, selectInput(inputId = 'theta', label = "True Log-Odds Ratio (\u03B8)", multiple = TRUE,
                                                              c(1.5, 1.0, 0.5, 0.0, -0.5, -1.0, -1.5))),
                                        column(2, selectInput(inputId = 'tau2', label = "Heterogeneity Variance (\u03c4\u00b2)",
                                                              c(0, 0.2, 0.4, 0.8))),
                                        column(2, selectInput(inputId = 'p_ic_init', label = "Baseline Event probability",
                                                              list("1%" = 0.01, "5%" = 0.05, "10%" = 0.10))),
                                        column(2, selectInput(inputId = 'min_n', label = "Participants Per Study",
                                                              list("10 to 100" = 10, "100 to 500" = 100)))
                                      ),
                                      
                                      # Output: Error Messages (red)
                                      htmlOutput('user_text', placeholder = FALSE),
                                      htmlOutput('user_data', placeholder = FALSE),
                                      htmlOutput('user_result_cutoffs', placeholder = FALSE)
                                      #tags$style(type="text/css", "#user_text {color : red;}")
                                      
                                    ),
                                    
                                    
                                    # Testing
                                    verbatimTextOutput('test1', placeholder = FALSE),
                                    verbatimTextOutput('test2', placeholder = FALSE),
                                    
                                    # Output: Error Messages
                                    verbatimTextOutput('no_data', placeholder = FALSE),
                                    
                                    
                                    
                                    # Tables & Plots
                                    
                                    # Output: Summary & Battleship Plot
                                    #verbatimTextOutput('titlePlot', placeholder = FALSE),
                                    fluidRow(
                                      
                                      column(2, 
                                             # Output: Table #1 summarizing the values entered
                                             tableOutput('tableSummary'),
                                             #br(),
                                             #actionButton(inputId = 'export', label = "Export"),
                                             ),
                                      
                                      
                                      column(9, offset = 1, 
                                             # Output: Battleship Plot according to the parameters
                                             plotOutput('battleshipPlot', height = 500)
                                             )
                                      
                                    ),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    
                                    # Output: Exact/Raw Table
                                    # Output: Table #2 Displaying filtered data
                                    #tableOutput('tableRaw')
                                    DT::dataTableOutput("tableRaw")
                                    
                           ) # tabPanel (Page 3)
                           
                           
                           
                ) # navbarPage
) # fluidPage

                
                
                

# Define server function  
server <- function(input, output, session) {
  
  
  # Data Management
  
  #######################################################################################################################################
  
  
  # Import and Format the Raw Data! 
  CC_all <- reactive({

    final_data_set <<- read.csv("final_data_set.csv", stringsAsFactors = TRUE, header = TRUE)
    
    # Zabriskie
    good.t1e <- final_data_set %>% 
      filter(theta == 0,
             prop_less_05_theta <= input$CRIT_prop_less_05_theta) %>% 
      mutate(good.methods = paste0(tau2, "/",                   
                                   p_ic_init, "/",             
                                   method_comb, "/",         
                                   excludeDBZ, "/",            
                                   min_n, "/",             
                                   max_n, "/",             
                                   dgm, "/",             
                                   framework)) %>% 
      select(good.methods)
    good.res.t1e <- final_data_set %>%            # t1e = type 1 error rate
      mutate(methods = paste0(tau2, "/",                   
                              p_ic_init, "/",             
                              method_comb, "/",         
                              excludeDBZ, "/",            
                              min_n, "/",             
                              max_n, "/",               
                              dgm, "/",             
                              framework)) %>% 
      filter(methods %in% good.t1e$good.methods)
    res.good <- good.res.t1e %>% 
      filter((input$CRIT_mean_ci_coverage_theta[1] <= mean_ci_coverage_theta & mean_ci_coverage_theta <= input$CRIT_mean_ci_coverage_theta[2]),
             median_ci_width_theta <= input$CRIT_median_ci_width,
             MAE_theta <= input$CRIT_MAE_theta,
             bias_median_theta <= input$CRIT_bias_median_theta)
    
    

    
    # Tidy up the data for use in the program
    res.good <- res.good %>% 
      select(data_set, theta, tau2, p_ic_init, pooling_method, tau2_estimator, min_n, 
             max_n, dgm, framework, bias_median_theta, MAE_theta, mean_ci_coverage_theta, 
             median_ci_width_theta, prop_less_05_theta, method_comb) %>%
      mutate(theta = as.factor(theta),
             tau2 = as.factor(tau2),
             p_ic_init = as.factor(p_ic_init),
             min_n = as.factor(min_n),
             max_n = as.factor(max_n)) %>%
      rename(Datasets = data_set) %>%
      mutate(Datasets = case_when(Datasets == "cac_cc_1" ~ "CAC-CC",
                                  Datasets == "cac_cc_0.01" ~ "CAC-CC (k=0.005)",
                                  Datasets == "cas_cc_1" ~ "CAS-CC",
                                  Datasets == "czc_cc_1" ~ "CZC-CC",
                                  Datasets == "e_cc_1_fixed" ~ "E-CC",
                                  Datasets == "e_cc_0.01_fixed" ~ "E-CC (m=0.01)",
                                  Datasets == "exclude_zeros_cc" ~ "Exclude-SZ-DZ",
                                  Datasets == "original_data_set" ~ "MH-No-CC",
                                  Datasets == "ta_cc_1" ~ "TA-CC",
                                  Datasets == "ta_cc_0.01" ~ "TA-CC (m=0.01)")) %>%
      mutate(Datasets = as.factor(Datasets)) %>% 
      rename(DGM = dgm) %>%
      mutate(DGM = case_when(DGM == "b" ~ "pRB",
                             DGM == "pf" ~ "pF",
                             DGM == "pr" ~ "pR")) %>% 
      mutate(tau2_estimator = case_when(tau2_estimator == "DL" ~ "DL",
                                        tau2_estimator == "IPM" ~ "IPM",
                                        tau2_estimator == "SJ" ~ "SJ",
                                        is.na(tau2_estimator) ~ "N/A (common-effects)")) %>%
      mutate(method_comb_app = case_when(pooling_method == 'MH' & tau2_estimator == 'N/A (common-effects)' ~ 'MH',
                                         pooling_method == 'INV' & tau2_estimator == 'N/A (common-effects)' ~ 'INV',
                                         pooling_method == 'SA' & tau2_estimator == 'N/A (common-effects)' ~ 'SA',
                                         pooling_method == 'SSW' & tau2_estimator == 'N/A (common-effects)' ~ 'SSW',
                                         
                                         pooling_method == 'MH' & tau2_estimator == 'DL' ~ 'MH/DL',
                                         pooling_method == 'SA' & tau2_estimator == 'DL' ~ 'SA/DL',
                                         pooling_method == 'SSW' & tau2_estimator == 'DL' ~ 'SSW/DL',
                                         
                                         pooling_method == 'MH' & tau2_estimator == 'IPM' ~ 'MH/IPM',
                                         pooling_method == 'SA' & tau2_estimator == 'IPM' ~ 'SA/IPM',
                                         pooling_method == 'SSW' & tau2_estimator == 'IPM' ~ 'SSW/IPM',
                                         
                                         pooling_method == 'MH' & tau2_estimator == 'SJ' ~ 'MH/SJ',
                                         pooling_method == 'SA' & tau2_estimator == 'SJ' ~ 'SA/SJ',
                                         pooling_method == 'SSW' & tau2_estimator == 'SJ' ~ 'SSW/SJ')) %>%
      # CHANGES!!!
      # Reverse alphabetical order
      mutate(method_comb_app = factor(method_comb_app, levels = c('SSW','SA','INV','MH',
                                                                  'SSW/SJ','SSW/IPM','SSW/DL',
                                                                  'SA/SJ','SA/IPM','SA/DL',
                                                                  'MH/SJ','MH/IPM','MH/DL') ))
    # pooling_method (order = MH, INV, SA, SSW)
    # tau2_estimator (order = DL, IPM, SJ)

    return(res.good)
    
    
  })
  
  
  #output$test1 <- renderPrint({ summary(CC_subset()$method_comb_app) })
  #output$test2 <- renderPrint({ levels( blank_BS_values()$method_comb_app ) })
  
  
  
  # Reset button for sliderInput values
  observeEvent(input$reset_data, {
    shinyjs::reset('CRIT_prop_less_05_theta')
    shinyjs::reset('CRIT_mean_ci_coverage_theta')
    shinyjs::reset('CRIT_median_ci_width')
    shinyjs::reset('CRIT_bias_median_theta')
    shinyjs::reset('CRIT_MAE_theta')
  })
  
  
  
  
  
  # Notify users that the data has been updated with their cutoffs
  output$CRIT_user_text <- renderText({ paste0('<font size="+1">', "<h4><b>Current Cutoffs Used</b></h4>", '<div>',
                                                 "Type I Error Rate Cutoff: ",         '&emsp;','&emsp;','&nbsp;',  '<b>', input$CRIT_prop_less_05_theta,        '</b>',  '<div>',
                                                 "Mean CI Coverage Cutoffs: ",         '&emsp;','&nbsp;',           '<b>', input$CRIT_mean_ci_coverage_theta[1], '</b>', " & ", '<b>', input$CRIT_mean_ci_coverage_theta[2], '</b>', '<div>',
                                                 "Mean CI Width Cutoff: ",             '&emsp;','&emsp;','&emsp;',  '<b>', input$CRIT_median_ci_width,           '</b>', '<div>',
                                                 "Median Bias Cutoff: ",    '&emsp;','&emsp;','&emsp;','&emsp;','&nbsp;',           '<b>', input$CRIT_bias_median_theta,         '</b>', '<div>',
                                                 "MAE Cutoff: ",            '&emsp;','&emsp;','&emsp;','&emsp;','&emsp;','&emsp;','&emsp;','&ensp;',  '<b>', input$CRIT_MAE_theta, '</b>'
                                                 ) # paste0()
      }) # renderText({})
    
  
  
  #######################################################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  ### User Input Limitations
  # Scripts run anytime a reactive variable inside changes (observe())
  # For pIC of 0.1, Min_n can only be 10
  observe({
    if (input$p_ic_init == 0.1) {
      updateSelectInput(session, 'min_n', label = "Participants Per Study", choices = list("10 to 100" = 10))
      # Let the user know why Min_n = 10
      output$user_text <- renderText({"The chosen baseline event probability can be paired only with 10 to 100 participants per study."})}
    # Corrects the system when switching away from restricted parameters
    else if (input$p_ic_init == 0.01 && input$min_n == 10) {
      updateSelectInput(session, 'min_n', label = "Participants Per Study", choices = list("10 to 100" = 10, "100 to 500" = 100), selected = input$min_n)
      output$user_text <- renderText({""})} # Remove previous user_text
    # Corrects the system when switching away from restricted parameters
    else if (input$p_ic_init == 0.05 && input$min_n == 10) {
      updateSelectInput(session, 'min_n', label = "Participants Per Study", choices = list("10 to 100" = 10, "100 to 500" = 100), selected = input$min_n)
      output$user_text <- renderText({""})} # Remove previous user_text
  })
  # For Min_n of 100, pIC can only be 0.01 & 0.05
  observe({
    if (input$min_n == 100 && input$p_ic_init == 0.01) {
      updateSelectInput(session, 'p_ic_init', label = "Baseline Event Probability", choices = list("1%" = 0.01, "5%" = 0.05), selected = 0.01)
      output$user_text <- renderText({"The chosen number of participants per study can only be paired with event rates of 1% and 5%."})
    }
    else if (input$min_n == 100 && input$p_ic_init == 0.05) {
      updateSelectInput(session, 'p_ic_init', label = "Baseline Event Probability", choices = list("1%" = 0.01, "5%" = 0.05), selected = 0.05)
      output$user_text <- renderText({"The chosen number of participants per study can only be paired with event rates of 1% and 5%."})
    }
    else if (input$min_n == 100 && input$p_ic_init == 0.1) {
      updateSelectInput(session, 'p_ic_init', label = "Baseline Event Probability", choices = list("1%" = 0.01, "5%" = 0.05), selected = 0.01)
      output$user_text <- renderText({"The chosen number of participants per study can only be paired with event rates of 1% and 5%."})
    }
    else {
      updateSelectInput(session, 'p_ic_init', label = "Baseline Event Probability", choices = list("1%" = 0.01, "5%" = 0.05, "10%" = 0.1))
      output$user_text <- renderText({""})} # Remove previous user_text
  })
  
  
  
  
  
  ### Displays text if no data is present from query
  # fix this section, combine with observeEvent() -> isolate()
  # prevent auto-updating null seleciton
  observe({
    output$no_data <- renderText({
      if (is.null(input$theta)) {"Please select a value for \u03B8 (up to three)."}
      else if (length(unique(input$theta)) > 3) {"Please select no more than three values for \u03B8."}
      else if (count(unique(CC_subset())) == 0) {"No meta-analysis methods meet the cutoffs chosen to filter the data. Please select less strict cutoff values in the Select Cutoffs tab."}
      else {""} # remove the no_data message
    })
  })

  
  
  
  
  
  ### Filtering Raw Data Based on User Inputs
  # Reactive expression to create data frame of all input values ----
  # reactive creates a reactive variable which is rerun if invalidated
  # Creates and saves to specific output$
  
  CC_subset <- reactive({
    output$user_result_cutoffs <- renderText({""})
    if (is.null(input$theta)) {return()}
    else if (length(unique(input$theta)) == 1) {
      CC_all() %>% filter(theta == input$theta[1],
                        tau2 == input$tau2,
                        p_ic_init == input$p_ic_init,
                        min_n == input$min_n
                        ) 
    }
    else if (length(unique(input$theta)) == 2) {
      CC_all() %>% filter(theta == input$theta[1] | theta == input$theta[2],
                        tau2 == input$tau2,
                        p_ic_init == input$p_ic_init,
                        min_n == input$min_n
                        ) 
    }
    else if (length(unique(input$theta)) == 3) {
      CC_all() %>% filter(theta == input$theta[1] | theta == input$theta[2] | theta == input$theta[3],
                        tau2 == input$tau2,
                        p_ic_init == input$p_ic_init,
                        min_n == input$min_n
                        ) 

    }
    else {return()}
  })

  
    
 
  
  

  
  
  
  ### Create the Summary Table for Display
  # Proportions are calculated based on results / total available positions
  observe({
    output$tableSummary <<- renderTable({
      # Preventing error messages
      if (is.null(input$theta)) {return()}
      else if (count(unique(CC_subset())) != 0) {
        
        num_theta <<- length(unique(input$theta))
        
        # Take the number of thetas and multiply them by 9 (the number of available positions)
        if (input$tau2 == 0) {
          tableSummary <- CC_subset() %>% 
            count(Datasets) %>%
            mutate(n = case_when(Datasets == 'MH-No-CC' ~ round((n / (3 * as.numeric(num_theta) ) ) * 100),
                                 Datasets != 'MH-No-CC' ~ round((n / (12 * as.numeric(num_theta) ) ) * 100))) %>% 
            arrange(desc(n)) %>%
            mutate(n = paste0(n, "%")) %>% 
            rename( "Correction Method" = Datasets, "Proportion of Occurrences" = n)
          
          return(tableSummary)
        }
        
        # Take the number of thetas and multiply them by 9 (the number of available positions)
        else if (input$tau2 != 0) {
          tableSummary <- CC_subset() %>% 
            count(Datasets) %>%
            mutate(n = case_when(Datasets == 'MH-No-CC' ~ round((n / (9 * as.numeric(num_theta) ) ) * 100),
                                 Datasets != 'MH-No-CC' ~ round((n / (27 * as.numeric(num_theta) ) ) * 100))) %>% 
            arrange(desc(n)) %>%
            mutate(n = paste0(n, "%")) %>% 
            rename( "Correction Method" = Datasets, "Proportion of Occurrences" = n)
          
          return(tableSummary)
        }
        
        # Return nothing if more than THREE Thetas
        if (length(unique(input$theta)) >3) {return()}
        
          
      }
    })  
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ### Create the Raw Table for Display
  # Tailor datatable for aesthetics
  # Removed method_comb
  # Rounding: bias_median_theta, MAE_theta, median_ci_width_theta
  observe({
    # Preventing error messages
    if (is.null(input$theta)) {return()}
    else {
      output$tableRaw <<- DT::renderDataTable(DT::datatable(
        filter = 'top',                                             # CHECK: Errors when filtering for Correction Method
        class = 'cell-border stripe', 
        options = list(
          pageLength = 5
          #autoWidth = FALSE
          ),
        #colnames = c('<span style="color:red">Type I Error Rate or<br>Relative Power </span>' = "Type I Error Rate or Relative Power"),
        colnames = c('<span>Type I Error Rate or <br> Relative Power </span>' = "Type I Error Rate or Relative Power"),
        escape = FALSE, { 
        if (is.null(input$theta)) {return()}
        else if(length(unique(input$theta)) >3) {return()}
          
        
        CC_subset() %>%
          mutate(bias_median_theta     = sprintf("%.3f", round(bias_median_theta, digits = 3)),
                 MAE_theta             = sprintf("%.3f", round(MAE_theta, digits = 3)),
                 mean_ci_coverage_theta = sprintf("%.3f", round(mean_ci_coverage_theta, digits = 3)),
                 median_ci_width_theta = sprintf("%.3f", round(median_ci_width_theta, digits = 3)),
                 prop_less_05_theta = sprintf("%.3f", round(prop_less_05_theta, digits = 3))
                 ) %>%
          select(DGM, theta, Datasets, pooling_method, tau2_estimator, bias_median_theta, MAE_theta, 
                 mean_ci_coverage_theta, median_ci_width_theta, prop_less_05_theta) %>%
          mutate(bias_median_theta     =  as.numeric(bias_median_theta),
                 MAE_theta             =  as.numeric(MAE_theta),
                 mean_ci_coverage_theta = as.numeric(mean_ci_coverage_theta),
                 median_ci_width_theta =  as.numeric(median_ci_width_theta),
                 prop_less_05_theta =     as.numeric(prop_less_05_theta),
                 DGM = as.factor(DGM),
                 tau2_estimator = as.factor(tau2_estimator),
                 Datasets = as.character(Datasets),
                 theta = as.character(theta)) %>% 
          rename("\u03B8" = theta,
                 "Correction Method" = Datasets,
                 "Pooling Method" = pooling_method,
                 "\u03c4\u00b2 Estimator" = tau2_estimator,
                 "Median Bias" = bias_median_theta,
                 "MAE" = MAE_theta,
                 "Mean CI Coverage" = mean_ci_coverage_theta,
                 "Median CI Width" = median_ci_width_theta,
                 "Type I Error Rate or Relative Power" = prop_less_05_theta) 
        
      }) )
    }
  })
  
  
  
  
  

  
  
  
  
  
  
  ### Code TESTING
  # Use observeEvent() -> insolate() 
  #observeEvent(input$go, {output$test <- renderPrint({isolate(input$theta)})})
  #observeEvent(input$go, {output$test <- renderPrint({str(input$theta)})})
  #output$test <- renderPrint({CC_subset})
  #output$test <- renderPrint({summary(CC_all)})
  #output$test <- renderPrint({ head(CC_subset()) })
  #output$test <- renderPrint({input$update_data})
  #output$test <- renderPrint({ levels(CC_subset()$Datasets) })
  #output$test <- renderPrint({ unique(CC_subset()$Datasets) })
  #output$test <- renderPrint({ paste0("\u03B8:", input$theta, ", ", "\u03c4\u00b2:", input$tau2, ", ", "EventProb:", input$p_ic_init, ", ", "Min_n:", input$min_n) })
  #output$test <- renderPrint({ as.factor(input$theta) })
  #output$test <- renderPrint({ num_theta })
  
  # Export Button
   # observeEvent(input$export, {
   #   write_csv(CC_subset(), "/Users/craigdecker/Downloads/CC_Subset_Output.csv")
   #   })
  
  
  
  
  
  

  
  
  
  ### Necessary for building Common-Effect vs Random-Effect BS plot boundaries
  # Specify the coordinates used when we are dealing with a large plot vs small plot
  blank_BS_values <- reactive({
    # This code runs only when we have only 3 rows total in the BS plot (tau2 == 0) (Common-Effect)
    if (any(CC_subset()$tau2 == 0)) {
      pooling_method <- as.factor(c('MH','INV','SA','SSW'))
      Datasets <- as.factor(c("CAC-CC","CAC-CC (k=0.005)","CAS-CC","CZC-CC","E-CC","E-CC (m=0.01)","Exclude-SZ-DZ","MH-No-CC","TA-CC","TA-CC (m=0.01)"))
      DGM <- as.factor(c('pF','pR','pRB'))
      grid.values.CE <- expand.grid(pooling_method, Datasets, DGM)
      grid.values.CE <- grid.values.CE %>% rename(pooling_method = Var1, 
                                                  Datasets = Var2, 
                                                  DGM = Var3)
      # CHANGES!!!
      grid.values.CE <- grid.values.CE %>% mutate(method_comb_app = as.factor(paste0(pooling_method)))
      grid.values.CE <- grid.values.CE %>% mutate(method_comb_app = factor(method_comb_app, levels = c('SSW','SA','INV','MH') ))
      
      return(grid.values.CE)
    }
    
    # This code runs when we have 9 rows total in the BS plot (tau2 == 0.2 to 0.8) (Random-Effect)
    else {  #(any(CC_subset()$tau2 == 0.2 | 0.4 | 0.8))
      pooling_method <- as.factor(c('MH','SA','SSW'))
      tau2_estimator <- as.factor(c('DL','IPM','SJ'))
      Datasets <- as.factor(c("CAC-CC","CAC-CC (k=0.005)","CAS-CC","CZC-CC","E-CC","E-CC (m=0.01)","Exclude-SZ-DZ","MH-No-CC","TA-CC","TA-CC (m=0.01)"))
      DGM <- as.factor(c('pF','pR','pRB'))
      grid.values.RE <- expand.grid(pooling_method, tau2_estimator, Datasets, DGM)
      grid.values.RE <- grid.values.RE %>% rename(pooling_method = Var1, 
                                                  tau2_estimator = Var2, 
                                                  Datasets = Var3, 
                                                  DGM = Var4)
      # CHANGES!!!
      grid.values.RE <- grid.values.RE %>% mutate(method_comb_app = as.factor(paste0(pooling_method, '/', tau2_estimator)))
      grid.values.RE <- grid.values.RE %>% mutate(method_comb_app = factor(method_comb_app, levels = c('SSW/SJ','SSW/IPM','SSW/DL',
                                                                                                       'SA/SJ','SA/IPM','SA/DL',
                                                                                                       'MH/SJ','MH/IPM','MH/DL') ))
      
      return(grid.values.RE)
    }

  })
  
  
  
  ### Necessary for grey area in MH-No-CC BS plots
  # Sets coordinates for the grey area
  No_CC_Grey <- reactive({
    # This code runs only when we have only 3 rows total in the BS plot (tau2 == 0) (Common-Effect)
    # CHANGES!!!
    if (any(CC_subset()$tau2 == 0)) {
      data.frame(Datasets = 'MH-No-CC', xmin = 0.35, xmax = 3.65, ymin = 0, ymax = 3.25)
    }
    # This code runs when we have 9 rows total in the BS plot (tau2 == 0.2 to 0.8) (Random-Effect)
    # CHANGES!!!
    else {  #(any(CC_subset()$tau2 == 0.2 | 0.4 | 0.8))
      data.frame(Datasets = 'MH-No-CC', xmin = 0.35, xmax = 3.65, ymin = 0, ymax = 6.25)
    }
      
  })
  
  
  
  
  
  
  
  
  
  ### Building the Battleship Plots
  # Two battleship plots are required because of single and multiple theta coding differences
  # geom_blank() can display the graphs which do not contain information. This allows me to control x and y axis ranges.
  
  observe({
    output$battleshipPlot <<- renderPlot({
      # If statement checks for data before making graphs, preventing error messages
      if (is.null(input$theta)) {return()}                  # No theta selected
      else if (length(unique(input$theta)) > 3) {return()}  # Too many thetas selected
      else if (count(unique(CC_subset())) == 0) {return()}  # No data at all in CC_subset # CHECK! # (should we just try to build a blank plot?)
      else if (length(unique(input$theta)) == 1) {                    # This code runs if only 1 theta is selected
        
          ###########################
          # Plot code for ONE theta value
          ###########################
        CC_subset() %>% ggplot(aes(x = DGM, y = method_comb_app)) +
          # CHANGES!!!
          geom_blank(data = blank_BS_values()) + 
          geom_point(aes(color = prop_less_05_theta), pch = 15, size = 4) +
          scale_color_viridis_c(option = "D", #A, D (https://bids.github.io/colormap/)
                                na.value = "#00000000",
                                limits = c(min(CC_subset()$prop_less_05_theta), max(CC_subset()$prop_less_05_theta)),
                                name = "Relative\nPower",
                                direction = -1) +
          theme_bw(base_size = 5) +
          theme(text = element_text(size = 15), panel.spacing = unit(0.5, "lines")) +
          xlab("\nDGM") + ylab("Method Combination") +
          facet_nested_wrap(factor(Datasets, levels = c('CAC-CC', 'CZC-CC', 'CAS-CC', 'TA-CC', 'E-CC', 
                                                        'CAC-CC (k=0.005)', 'TA-CC (m=0.01)', 'E-CC (m=0.01)', 'Exclude-SZ-DZ', 'MH-No-CC')) ~ .
                            , nrow = 2, scales = 'fixed', labeller = plot_labels) + 
          geom_rect(data=No_CC_Grey(), aes(x = NULL,y = NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                    alpha=0.4, fill="grey", inherit.aes = FALSE) 
        
      } # 1 theta statement

      
      
      else if (length(unique(input$theta)) == 2 | 3) {                # This code runs if 2 or 3 thetas are selected
        
        ###########################
        # Plot code for TWO OR MORE theta values
        ###########################
        CC_subset() %>% ggplot(aes(x = DGM, y = method_comb_app)) + 
          # CHANGES!!!
          geom_blank(data = blank_BS_values()) + 
          geom_point(aes(color = prop_less_05_theta), pch = 15, size = 4) +
          scale_color_viridis_c(option = "D", #A, D (https://bids.github.io/colormap/)
                                na.value = "#00000000", 
                                limits = c(min(CC_subset()$prop_less_05_theta), max(CC_subset()$prop_less_05_theta)),
                                name = "Relative\nPower",
                                direction = -1) +
          theme_bw(base_size = 5) +
          theme(text = element_text(size = 15), panel.spacing = unit(0.5, "lines")) +
          xlab("\nDGM") + ylab("Method Combination") +
          facet_nested_wrap(. ~ factor(Datasets, levels = c('CAC-CC', 'CZC-CC', 'CAS-CC', 'TA-CC', 'E-CC', 'CAC-CC (k=0.005)', 'TA-CC (m=0.01)', 'E-CC (m=0.01)', 'Exclude-SZ-DZ', 'MH-No-CC'))
                            + theta, nrow = 2, scales = 'fixed', labeller = plot_labels) + 
          geom_rect(data=No_CC_Grey(), aes(x = NULL,y = NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                    alpha=0.4, fill="grey", inherit.aes = FALSE)
        
      } # 2-3 theta statement
        
      
      else {return()}
      
    }) # battleshipPlot
    
  })
  
  
  
  
  
} # server


# Remove Error Scripts in App (optional)
#options(shiny.sanitize.errors = TRUE)

# Create Shiny object
shinyApp(ui = ui, server = server)



