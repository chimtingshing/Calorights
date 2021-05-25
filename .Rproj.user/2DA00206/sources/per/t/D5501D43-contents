library(readxl)
library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)

#load nutrition list file into the app
met_list <- read_excel("MET.xlsx")
nutrition_list <- data.table(read_excel("Nutrient Values.xlsx", sheet = "FNDDS Nutrient Values", skip = 1))
food_portionList <- data.table(read_excel("Portions and Weights.xlsx", sheet = "Portions and Weights", skip = 1, guess_max = 17000))
food_portionList <- food_portionList[!grepl("specified", food_portionList$`Portion description`, ignore.case = T)]
daily_nutrientList <- data.table(read_excel("Daily Nutrients.xlsx"))

#use navbarPage instead of fluidPage for multiple tabs
ui <- navbarPage(title = "Calorights",
                 theme = bs_theme(bootswatch = "flatly"),
                 
  #first app (tab) is food nutrition calculator
  tabPanel("Food Nutrition Calculator", 
           icon = icon("calculator"),
                          
    fluidRow(column(2,
    
    #this is the input to enter food
    selectizeInput(inputId = "food", 
                   label = "Enter food",
                   choices = nutrition_list$`Main food description`,
                   options = list(placeholder = "Type to choose a food", 
                                  onInitialize = I('function() { this.setValue(""); }'))
                   ),
                                          
    #once food is selected, an input to select serving size is displayed
    #choices will be updated in server according to food entered
    conditionalPanel('!input.food == ""',
      selectizeInput(inputId = "serving_size",
                     label = "Choose serving size",
                     choices = "")
      ),
                                          
    #once serving size is chosen, input to select number of serving is displayed
    conditionalPanel('!input.serving_size == ""',
      numericInput(inputId = "number_of_serving",
                   label = "Choose number of serving(s)",
                   min = 1,
                   max = 10000,
                   value = 1
                   ),
                                                           
    #action button to add ingredient
    actionButton(inputId = "add_ingredient",
                 label = "Add ingredient/food",
                 width = "219px",
                 icon = icon("plus", class = "fab-fa", lib = "font-awesome")
                 ), 
    br(),
    br()),
                                          
    #action button to remove all ingredients
    actionButton(inputId = "remove_ingredient",
                 label = "Remove all ingredient/food",
                 icon = icon("minus", class = "fab-fa", lib = "font-awesome")
                 )
    ), #closes column align
    
    column(10,
    tabsetPanel(type = "pills",
                tabPanel("Ingredient/Food table", icon = icon("utensils"),
                         br(),
                         DT::DTOutput(outputId = "ingredient_df"),
                         br(),
                         verbatimTextOutput(outputId = "calories")
                         ),
  
                tabPanel( "Nutrition table", icon = icon("table"),
                          br(),
                          DT::DTOutput(outputId = "nutrition_df"),
                          ),
                
                navbarMenu("Graphs", icon = icon("chart-bar", class = "far fa"),
                           tabPanel("Vitamin A breakdown graph",
                                    br(),
                                    plotlyOutput(outputId = "vitaminA_df")
                           ),
                           tabPanel("Fat breakdown graph",
                                    br(),
                                    plotlyOutput(outputId = "fat_df")
                           )
                           
                ),
                tabPanel("Definition", icon = icon("readme"),
                         h3("What are daily values?", align = "center"),
                         tags$p("According the U.S. Food and Drug Administration, Daily Values (DVs) consists of two sets of reference
                         values for reporting nutritions in nutrition labels. These sets are Daily Reference Values (DRVs)
                         and Reference Daily Intakes (RDIs). To limit consumer confusion, the single term Daily Value is used 
                         to designate both DRVs and RDIs. DVs are essentially the daily intake of a nutrient that is considered
                         to meet the requirements of healthy individuals. Our nutrition calculator calculates the DVs based 
                         on the assumption that the user is at least of age 4 and above and is not pregnant or lactating.", align = "justify"),
                         br(),
                         h3("Breakdown of vitamin A", align = "center"),
                         tags$p("Vitamin A can be broken down into two forms which consits of preformed vitamin A (retinols) and 
                         provitamin A carotenoids (alpha carotene, beta carotene and beta cryptoxanthin). The human body
                         is considered inefficient. For example, the Food and Nutrition board has listed the estimated effieciency
                         factor for the conversion of beta carotene to vitamin A as 12:1. Thus, do not be surprised if the 
                         sums of the breakdown of vitamin A are not equal to the total amount of vitamin A in the tables.", align = "justify"),
                         br(),
                         h3("Breakdown of fats", align = "center"),
                         tags$p("Individual fatty acids can be broken down into 3 types of fatty acids which consists of 
                         saturated fatty acids, monounsaturated fatty acids and also polyunsaturated fatty acids. Here are
                         the list of individual fatty acids which can be found in foods.", align = "justify"),
                         tags$img(src = "FattyAcids.png", style="display: block; margin-left: auto; margin-right: auto;"),
                         tags$p(em("Source: 2017-2018 Food and Nutrient Database for Dietary Studies Factsheet"), align = "center"),
                ),
                navbarMenu("Tables", icon = icon("table"),
                           tabPanel("Daily Reference values (DRVs)",
                                    br(),
                                    tags$img(src = "DRV.png", style="display: block; margin-left: auto; margin-right: auto;"),
                                    tags$p(em("*Based on the reference caloric intake of 2,000 calories for adults and children
                                              aged 4 years and older, and for pregnant women and lactating women."), align = "center"),
                                    tags$p(em("*Based on the reference caloric intake of 1,000 calories for children 1 through 
                                              3 years of age. "), align = "center"),
                                    tags$p(em("Source: https://s3.amazonaws.com/public-inspection.federalregister.gov/2016-11867.pdf
                                              (page 905)"), align = "center")
                           ),
                           tabPanel("Reference Daily Intake (RDIs)",
                                    br(),
                                    tags$img(src = "RDI.png", style = "display: block; margin-left: auto; margin-right: auto;"),
                                    tags$p(em("*RAE = Retinol Activity Equivalents; 1 microgram RAE=1 microgram retinol, 2 microgram 
                                              supplemental β-carotene, 12 micrograms β-carotene, or 24 micrograms α-carotene,
                                              or 24 micrograms β-cryptoxanthin."), align = "center"),
                                    tags$p(em("*Based on the reference caloric intake of 2,000 calories for adults and children
                                              aged 4 years and older, and for pregnant women and lactating women."), align = "center"),
                                    tags$p(em("*Based on the reference caloric intake of 1,000 calories for children 1 through 
                                              3 years of age. "), align = "center"),
                                    tags$p(em("Source: https://s3.amazonaws.com/public-inspection.federalregister.gov/2016-11867.pdf
                                              (pages 903-904))"), align = "center")
                           )
                )#closes navbar menu
    )#closes tabSetPanel
    ) #closes column
    ) #closes fluidrow
  ), #close tab panel for nutrition calculator
  
  tabPanel("MET calculator", icon = icon("calculator"),
           sidebarLayout(
             sidebarPanel(
               numericInput(inputId = "weight_MET", 
                            label = "Weight (Kg)", 
                            value = "0",
                            min = 0),
               
               #Ask user to select one activity from the list provided
               selectizeInput(inputId = "activity_MET", 
                              label = "Select one activity", 
                              choices = met_list$`SPECIFIC MOTION`,
                              options = list(placeholder = "Type to chooose an activity", 
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               
               
               #Ask user to select the time in minutes
               sliderInput(inputId = "time_MET", 
                           label = "Duration of time doing the activity (minutes)", 
                           min = 0,max = 240,
                           value = 30, 
                           step = 5),
               
               selectizeInput(inputId = "food_MET",
                              label = "Choose one food to compare the calories",
                              choices = nutrition_list$`Main food description`,
                              options = list(
                                placeholder = "Type to choose a food",
                                onInitialize = I('function() { this.setValue(""); }'))
               ),
               
               #once food is selected, an input to select serving size is displayed
               #choices will be updated in server according to food entered
               conditionalPanel('!input.food_MET == ""',
                                selectizeInput(inputId = "serving_size_MET",
                                               label = "Choose serving size",
                                               choices = ""
                                )
               ),
               
               #once serving size is chosen, input to select number of serving is displayed
               conditionalPanel('!input.serving_size_MET == ""',
                                numericInput(inputId = "number_of_serving_MET",
                                             label = "Choose number of serving(s)",
                                             min = 1,
                                             max = 10000,
                                             value = 1
                                )),
               
               #submit button to update inputs
               actionButton(inputId = "update_MET", label = "Update", class = "btn-lg btn-success")
             ),
             mainPanel(
               tabsetPanel(type = "pills",
                           tabPanel("Calories burned", icon = icon("fire-alt"),
                                    fluidRow(
                                      img(src = "Exercise.png")
                                    ),
                                    fluidRow(column(7,
                                                    h5("Calculations based on weight, time and activity"),
                                                    verbatimTextOutput(outputId = "calories_MET"))
                                    ),
                                    fluidRow(column(7, 
                                                    h5("Comparison with the food chosen"),
                                                    verbatimTextOutput(outputId = "compareFood_MET"))
                                    )
                           ),
                           tabPanel("Definition", icon = icon("readme"),
                                    h3("What is MET?", align = "center"),
                                    p("According to the Compendium of Physical Activities, MET (Metabolic Equivalent) is the ratio of the work
                                    metabolic rate to the resting metabolic rate. One MET is defined as 1 kcal/kg/hour and is roughly equivalent
                                    to the energy cost of sitting quietly. A MET can also be defined as oxygen intake in ml/kg/min with 1 MET
                                    being equivalent to the oxygen cost of sitting quietly, which is 3.5 ml/kg/min", align = "justify"),
                                    br(),
                                    h3("Limitations of using MET", align = "center"),
                                    p("According to the Conpendium of Physical Activities MET is only the estimate to the energy cost of energies. 
                                      It was not developed to determine the precise energy cost of physical activities, but rather to provide
                                      a classication system that standardizes the MET intensities of physical activities. MET does not account
                                      for differences in body mass, age, sex, efficiency of movement, geographic and environmental conditions. Thus 
                                      the energy expenditure may differ greatly from the calculated values in the MET calculator.", align = "justify")
                           ),
                           tabPanel("Formula", icon = icon("list-alt"),
                                    h3("Calculate calories burned", align = "center"),
                                    p("The formula for calculating the calories burned from an exercise is"),
                                    tags$ul(tags$li("Calories burned = MET value of an activity * 3.5 * weight in kg / 200 * time"))
                           )#closes tabPanel (formula)
               )#closes tabsetpanel
             )#closes main panel
           )#close sidebarlayout
  ),#close tabpanel for MET calculator
  
  tabPanel(
    "BFP/BMI Calculator", icon = icon("calculator"),
    sidebarLayout(
      # sidebarPanel for user input
      sidebarPanel(
        #choose type of calculator
        selectInput(
          "type", "Select calculator",
          choices = c("", "Body Fat Percentage", "Body Mass Index")),
        
        # if BFP is chosen, display the inputs needed
        conditionalPanel(
          condition = "input.type == 'Body Fat Percentage'",
          #input gender
          radioButtons(inputId = "gender_BFP",
                       label = "Gender",
                       choices = c("Male" = "male", "Female" = "female")),
          #input age
          numericInput(inputId = "age_BFP",
                       label = "Age",
                       value = 0),
          #input weight
          numericInput(inputId = "weight_BFP",
                       label = "Weight (kg)",
                       value = 0),
          #input height
          numericInput(inputId = "height_BFP",
                       label = "Height (cm)",
                       value = 0),
          
          #input a calculate button
          actionButton(inputId = "calcBFP",
                       label = "Calculate", 
                       class = "btn-lg btn-success"),
        ), 
        
        # if BMI is chosen, display the inputs needed
        conditionalPanel(
          condition = "input.type == 'Body Mass Index'",
          #input weight
          numericInput(inputId = "weight_BMI",
                       label = "Weight (kg)",
                       value = 0),
          #input height
          numericInput(inputId = "height_BMI",
                       label = "Height (cm)",
                       value = 0),
          #input a calculate button
          actionButton(inputId = "calcBMI",
                       label = "Calculate", 
                       class = "btn-lg btn-success"),
        ), 
        
        hr(),
        #Show the output
        verbatimTextOutput("result"),
        
        #small note to user
        em("*Note: BMI and BFP do not necessarily apply to athletes or people 
            with high muscle mass", align = "justify")
      ),
      
      #Descriptions, tables, other information
      mainPanel(
        tabsetPanel( type = "pills",
                     
                     #tab panel "Definition"
                     tabPanel( "Definition", icon = icon("readme"),
                               h3("What is Body Mass Index?", align = "center"),
                               p("Body Mass Index (BMI) is a measurement of a person's weight
            with respect to his or her height. It is more of an indicator
            than a direct measurement of a person's total body fat.
            World Health Organization (WHO) defines an adult who has BMI 
            between 25 and 29.9 as overweight, an adult who has a BMI of 30 or 
            higher is considered obese. Meanwhile, a BMI below 18.5 is considered
            underweight, and between 18.5 to 24.9 a healthy weight.",
                                 align = "justify"),
                               p(em("(What is body mass index. "),
                                 "Retrieved from
             https://www.news-medical.net/health/What-is-Body-Mass-Index-(BMI).aspx)"),
                               br(),
                               h3("What is Body Fat?", align = "center"),
                               p("There is a misconception that all body fat is bad. In fact,
             body fat is needed to protect a person's health as well as to
             provide a source of energy for performing various bodily
             functions. Generally, the overall weight of body fat can be
             split into three separate categories:", align = "justify"),
                               p(""),
                               p(strong("Essential Body Fat")),
                               p("The amount of body fat needed to protect the body from
             infectious diseases and bruising damage to the internal organs.",
                                 align = "justify"),
                               p(strong("Reserve Body Fat")),
                               p("The amount of additional body fat that serves as reservoir of fuel
          for the body. It does not pose any health risks.", align = "justify"),
                               p(strong("Excess Body Fat")),
                               p("The amount of body fat that is not part of the essential body fat 
          and reserve body fat. Excess body fat puts people at risk for severe 
          health problems such as heart attacks, strokes and diabetes.", align = "justify"),
                               p( em("(What is body fat. "),
                                  "Retrieved from https://www.futrex.com/support/what-is-body-fat/)")
                     ),
                     
                     #tab panel "Formula"
                     tabPanel(
                       "Formula", icon = icon("list-alt"),
                       h3("Calculate BMI", align = "center"),
                       p("The formula of calculating BMI is weight in kilograms divided 
            by height in metres squared"),
                       tags$ul(tags$li("BMI = weight / (height x height)")),
                       br(), 
                       h3("Calculate Body Fat", align = "center"),
                       p("Input the data into the calculator. According to a
             study published in the British Journal of Nutrition in 1991,
             using the following gender-based formulas in combination with
             the Body Mass Index (BMI), the percentage of body fat can be
             measured as accurately as skin-fold measurements
             and bioelectrical tests for adults.", align = "justify"),
                       p("Men: "),
                       tags$ul(tags$li(
                         "Body Fat Percentage (%) = (1.20 x BMI) + (0.23 x Age) - 5.4")),
                       p("Women: "),
                       tags$ul(tags$li(
                         "Body Fat Percentage (%) = (1.20 x BMI) + (0.23 x Age) - 16.2 ")),
                       p(""),
                       p("Take note that the result is a rough estimation and does
            not reflect on the exact body fat percentage. It is a
            guideline to help individuals have a grasp on how much is
            their body fat.", align = "justify") 
                     ), #close tabPanel (formula)
                     
                     # Menu "Tables"
                     navbarMenu( 
                       "Tables", icon = icon("table"),
                       
                       #tab panel "Body Fat Percentage Table"
                       tabPanel( 
                         "Body Fat Percentage Table",
                         h3("Body Fat Percentage Table", align = "center"),
                         p("After calculating the body fat percentage, compare the result
              with the following tables. Various organisations have different
              tables as they depict the data in different aspects.
              When comparing the results to these numbers, you should have
              a good indication of how close or how far you are from your
              ideal body fat percentage.", align = "justify"),
                         img( src = "table1.png", height = 300, width = 350),
                         p(em(
                           "Source: https://www.gaiam.com/blogs/discover/how-to-calculate-your-ideal-body-fat-percentage"
                         )),
                         br(),
                         strong("American Journal of Clinical Nutrition"),
                         br(),
                         img( src = "table2.png", width = 360, height = 75),
                         p(em("Source: https://www.calculator.net/body-fat-calculator.html")),
                         br(),
                         p("Based on the tables, women naturally have a higher body fat to lean
              tissue ratio than men, and body fat naturally increases with age.")
                       ),
                       
                       #tab panel "Body Mass Index Table"
                       tabPanel( 
                         "Body Mass Index Table",
                         h3("Body Mass Index Table", align = "center"),
                         br(),
                         p("Compare your calculated body mass index (BMI) with the table below"),
                         img(src = "bmi table.jpg"),
                         p(em("Source: https://www.bodymassindex.co/")),
                         br(),
                         p("You can also refer to the chart below", align = "justify"),
                         img(src = "bmi chart.jpg"),
                         p(em("Source: https://www.businessinsider.com/"))
                       )
                     ) #closes navbarMenu (Tables)
        ) #close tabset panel
      ) #close mainPanel
    ) #close sidebarLayout
  ), #close tabPanel (BMI calculator)
  
  tabPanel(
    "Calories Calculator", icon = icon("calculator"),
    
    #Allows user to enter input on the left side of ui
    sidebarLayout(
      sidebarPanel(
        #Ask user to enter weight in kg
        numericInput(inputId= "weight_BMR",
                     label = "Weight (kg)",
                     value = 0),
        
        #Ask user to select gender
        selectInput(inputId = "gender_BMR",
                    label = "Gender",
                    choices = c("","Male", "Female")),
        
        #Let user to choose lean factor multiplier based o body fat percentage
        p("Table 1: Lean factor multiplier"),
        img(src = "LeanFactorMultipler.png", height = 250, width = 250),
        p(em("Use the 'body fat percentage calculator' under 'BFP/BMI Calculator'
           tab to get body fat percentage")),
        
        numericInput(inputId = "lean_factor_multiplier", 
                     label = "Lean factor multiplier (refer Table 1)",
                     value = 0, min = 0, max = 1.0),
        
        #Ask user to choose daily activity level
        selectInput(
          inputId = "daily_activity_level",
          label = "Daily activity level (refer Table 2)",
          choices = c("","Very Light", "Light", "Moderate", "Heavy", "Very Heavy"),
          selected = ""), 
        
        #submit button to update inputs
        actionButton(inputId = "update_BMR", 
                     label = "Calculate",
                     class = "btn-lg btn-success")
      ),
      
      #Show output on the right side of interface
      mainPanel(
        tabsetPanel( type = "pills",
                     # tab panel Main
                     tabPanel("Main" , icon = icon("home"), 
                              h5("This calorie calculator can help you to estimate the recommended 
                              amount of calories you need to consume daily to maintain your body weight.",  align = "justify"),
                              br(),
                              
                              h5("Table 2 : Daily activity level"),
                              img(src = "table activity.png"),
                              p(em("Source: https://diabetesstrong.com")),
                              
                              #Show selections of the user
                              #Show weight entered
                              h5(strong("Your weight in kilogram:")),
                              verbatimTextOutput(outputId = "selected_weight"),
                              
                              #Show gender selected
                              h5(strong("Your gender: ")),
                              verbatimTextOutput(outputId = "selected_gender"),
                              
                              #Show lean factor multiplier selected
                              h5(strong("Your Lean Factor Multiplier: ")),
                              verbatimTextOutput("selected_lean_factor_multiplier"),
                              
                              #Show selected daily activity level 
                              h5(strong("Your selected daily activity level: ")),
                              verbatimTextOutput(outputId = "selected_activity"),
                              
                              br(),
                              #Show the result
                              h4(strong("The amount of daily calories intake to maintain your body weight is: ")),
                              verbatimTextOutput(outputId = "recommended_calories") ),
                     
                     # tab panel Description
                     tabPanel("Description", icon = icon("book"),
                              h3("Daily calorie intake recommendation", align = "center"),
                              HTML('<center><img src="Healthy_Eating.png" height = "250" width = "400"></center>'),
                              br(),
                              p("Daily calorie intake need to be customized to each individual.
            Since every individual has different build and carry out daily 
            activities of different intensities, this calculator take into 
            account these factors and provide the estimated calorie intake to 
            maintain weight.", align = "justify"),
                              h3("How to reduce calorie intake", align = "center"),
                              p("Many people struggle with their daily calorie intake exceeding the
           recommended amount. Therefore, here are some tips on how to reduce 
           calorie intake.", align = "justify"),
                              tags$ol(
                                tags$li("Eat more protein"),
                                p("Protein raises your metabolic rate while also helping to curb 
               your appetite, according to studies. Protein takes energy to 
               metabolize, so eating a high-protein diet will help you burn an 
               extra 80–100 calories per day.", align = "justify"),
                                tags$li("Drink more water"),
                                p("Some studies have shown that drinking water may boost metabolism.
               It will help you eat less calories if you drink it half an hour 
               before meals.", align = "justify" ),
                                tags$li("Reduce carbohydrate and sugar intake"),
                                p("Cutting carbohydrates is a great way to lose weight because it 
              suppresses your appetite and causes you to consume less calories.
              According to studies, eating a low carb diet until fullness 
              will help you lose two to three times as much weight as a 
               calorie-restricted, low fat diet.", align = "justify")
                              ),
                              p("Source: https://www.healthline.com/nutrition/how-many-calories-per-day")
                     ),

                     tabPanel("Formula", icon = icon("list-alt"),
                              h3("Calculate daily calorie intake to maintain body weight", align = "center"),
                              p("The formula used to calculate the daily calorie needed:", align = "justify"),
                              p("Men: "),
                              tags$ul(tags$li(
                                "Calorie = weight x 1.0 x 24 x lean factor multiplier x activity level multiplier")),
                              p("Women: "),
                              tags$ul(tags$li(
                                "Calorie = weight x 0.9 x 24 x lean factor multiplier x activity level multiplier")),
                              p("Refer to Table 1 for the lean factor multiplier"),
                              p("Refer table below to obtain the activity level multiplier."),
                              img(src = "table activity.png"),
                              p(""),
                              br()
                     )#close tabPanel (formula)
        ) #close tabsetPanel
      ) #close mainPanel
    ) #close sidebarLayout
  ), #close tabPanel (BMR calculator)
  tabPanel("Documentation", icon = icon("book-open"),
           p("This shiny application, called Calorights has multiple functions which include a nutrition calculator which 
             calculates the exact amount of nutrient in foods, a BMI/BFP calculator which calculates the BMI or body fat
             percentage, a MET calculator which calculates the amount of calories burned during an exercise and also a calorie 
             calculator which calculates the recommended daily calorie intake to maintain your body weight.", align = "justify"),
           br(),
           p("Firstly, for the food nutrition calculator, choose a food, it's serving size and it's number of servings.
             Then press the add button to add it into a list. The list will be displayed at the ingredient/food table tab.
             The exact amount of nutrients and their percentage daily value will be displayed in table form in the nutrition 
             table tab. There are 2 graphs which are the vitamin A and fat breakdown graph which can be found in the graph tab. 
             There are definitions and description of related terms in the definition tab and tables for the Daily Reference
             Values (DRVs) and Reference Daily Intakes (RDIs) are also shown.", align = "justify"),
           br(),
           p("Next, for the MET calculator, enter your weight, select an activity and the duration of the activity and press the
             update button to calculate the amount of calories burned during the activity. You can enter a food, it's serving size
             and number of servings to obtain the ratio of calories burned and the calories in that food. There are definitions
             and description of related terms in the definition tab and also formula of calculation in the formula tab.", align = "justify"),
           br(),
           p("Moving on, for the BFP/BMI calculator, select a calculator  (Body Fat Percentage/Body Mass Index) and enter the 
             required information to calculate your BMI or your Body Fat Percentage. There are definitions and description for
             related terms in the definition tab. Body Fat Percentage table and BMI table can be accessed in the tables tab and 
             formulas can be found in the formula tab.", align = "justify"),
           br(),
           p("Lastly, for the Calories Calculator, enter your weight, gender, lean factor multiplier based on Table 1 and daily 
             activity level based on Table 2. Then press calculate to calculate the amount of daily calorie intake needed to 
             maintain your body weight. Descriptions about daily calorie intake and tips to reduce daily calorie intake can be 
             found in the description tab. The  formula used for calculation can be found in the formula tab.", align = "justify")
  )# close tabPanel (Documentation)
) #close navbarPage

server <- function(input, output){
  
  #creates a container to store reactive values
  ingredient_df <- reactiveValues()
  
  #summary_df is the data table containing summary of all food/ingredients that are chosen
  ingredient_df$summary_df <- data.table("Name" = character(), 
                                         "Serving size" = character(),
                                         "Number of serving(s)" = numeric(),
                                         "Calories" = numeric())
  
  #chosen_df is the data table which stores all info of chosen food (69 columns)
  ingredient_df$chosen_df <- setNames(data.table(matrix(nrow = 0, ncol = ncol(nutrition_list) + 1)),
                                      c(colnames(nutrition_list), "Food Weight"))
  
  #nutrition table is the data table which stores all info of nutrients
  ingredient_df$nutritionTable <- data.table(Nutrients = daily_nutrientList$`Food Component`,
                                             Unit = daily_nutrientList$`Unit of measurement`,
                                             "Recommended daily intake" = daily_nutrientList$`Daily Intake`,
                                             "Daily intake taken" = 0,
                                             "Daily value (%)" = 0
  )
  
  #food choice is the subset of nutrition list which contains the input food
  food_choice <- eventReactive(input$food,{
    food_choice <-nutrition_list %>%
      filter(nutrition_list$`Main food description` == input$food)
    food_choice
  })
  
  #food portion is the subset of food portion list which contains the input food
  food_portion <- eventReactive(input$food,{
    food_portion <- food_portionList[food_portionList$`Food code` %in% food_choice()$`Food code`]
    food_portion
  })
  
  #code to run once food is chosen
  observeEvent( input$food, {
    updateSelectizeInput(inputId = "serving_size", label = "Choose serving size",
                         choices = c(food_portion()$`Portion description`, "1 g"),
                         options = list(
                           placeholder = "Type to choose serving size",
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
  })
  
  #calculate food weight based on food serving size and number of portion
  #it is only called when the inputs it depends on changes (which are number of serving as it is the last input)
  food_weight <- reactive({
    if (input$serving_size == "1 g"){
      food_weight <- input$number_of_serving
    }else{
      food_weight <- food_portion()[`Portion description` == input$serving_size]$`Portion weight (g)` * input$number_of_serving}
  })
  
  #it is only called when the inputs it depends on changes (which are number of serving as it is the last input)
  #calculate the calories based on serving size
  calorie <- reactive({
    calorie <- food_choice()$`Energy (kcal)` / 100 * food_weight()
    calorie
  })
  
  #It is only called when the input it depends on changes (which is the ingredient table, each time ingredient is added),
  #totalCalorie is called to update the total calories
  totalCalorie <- reactive({
    sum(ingredient_df$summary_df$Calories)
  })
  
  #updates the ingredient of data table  
  observeEvent(input$add_ingredient, {
    df_ingredient_chosen <- data.table(
      "Name" = input$food,
      "Serving size" = input$serving_size,
      "Number of serving(s)" = input$number_of_serving,
      "Calories" = calorie()
    )
    ingredient_df$summary_df <- rbind(ingredient_df$summary_df, df_ingredient_chosen)
    
    #updates table for the list of all ingredients
    #use temp as a temporary storage 
    temp <- food_choice()
    temp$`Food Weight` <- food_weight()
    ingredient_df$chosen_df <- rbind(ingredient_df$chosen_df, temp, fill = TRUE)
    
    #updates the nutrition table for all nutrients (updates done singly)
    ingredient_df$nutritionTable[grepl(78, `Recommended daily intake`)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Total Fat (g)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Saturated Fat", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Fatty acids, total saturated (g)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Cholesterol", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Cholesterol (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("carbohydrate", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Carbohydrate (g)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Fibre", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Fiber, total dietary (g)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Protein", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Protein (g)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Sugar", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Sugars, total (g)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin A", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin A, RAE (mcg_RAE)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Thiamin", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Thiamin (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Riboflavin", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Riboflavin (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Niacin", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Niacin (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Folate", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Folate, total (mcg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin B6", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin B-6 (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin B12", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin B-12 (mcg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Choline", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Choline, total (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin C", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin C (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin D", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin D (D2 + D3) (mcg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin E", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin E (alpha-tocopherol) (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Vitamin K", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Vitamin K (phylloquinone) (mcg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Calcium", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Calcium (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Phosphorus", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Phosphorus (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Magnesium", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Magnesium (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Iron", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Iron\r\n(mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Zinc", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Zinc\r\n(mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Copper", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Copper (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Selenium", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Selenium (mcg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Potassium", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Potassium (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    ingredient_df$nutritionTable[grepl("Sodium", Nutrients)]$`Daily intake taken` <- sum(ingredient_df$chosen_df$`Sodium (mg)` / 100 * ingredient_df$chosen_df$`Food Weight`)
    
    ingredient_df$nutritionTable$`Daily value (%)` <- round(ingredient_df$nutritionTable$`Daily intake taken` / ingredient_df$nutritionTable$`Recommended daily intake` * 100, digits = 1)
  })
  
  #clears all data tables
  observeEvent(input$remove_ingredient, {
    ingredient_df$summary_df <- ingredient_df$summary_df <- data.table("Name" = character(), 
                                                                       "Serving size" = character(),
                                                                       "Number of serving(s)" = numeric(),
                                                                       "Calories" = numeric())
    ingredient_df$chosen_df <- setNames(data.table(matrix(nrow = 0, ncol = ncol(nutrition_list) + 1)),
                                        c(colnames(nutrition_list), "Food Weight"))
    ingredient_df$nutritionTable <- data.table(Nutrients = daily_nutrientList$`Food Component`,
                                               Unit = daily_nutrientList$`Unit of measurement`,
                                               "Recommended daily intake" = daily_nutrientList$`Daily Intake`,
                                               "Daily intake taken" = 0,
                                               "Daily value (%)" = 0
    )
  })
  
  #prints the total calories by calling the function totalCalorie
  output$calories <- renderPrint({
    cat("Total calories is", totalCalorie())
  })
  
  #renders the ingredient data table using DT library for server side processing
  output$ingredient_df <- DT::renderDT(ingredient_df$summary_df)
  
  #renders the nutrition data table using DT library for server side processing
  output$nutrition_df <- DT::renderDT(ingredient_df$nutritionTable)
  
  #plots graph of breakdown of fats 
  output$fat_df <- renderPlotly({
    #select fat columns from chosen_df
    fat_df <- ingredient_df$chosen_df %>%
      select(`4:0\r\n(g)`:`22:6 n-3\r\n(g)`)
    
    #for loop to calculate amount of nutrient in each row by weight
    for (i in 1:nrow(fat_df)){
      fat_df[i, ] <- fat_df[i, ] / 100 * ingredient_df$chosen_df$`Food Weight`[i]
    }
    
    #update fat_df to have 2 columns consisting of names and value
    fat_df <- data.table(colnames(fat_df), colSums(fat_df))
    
    #this is to prevent error at the start where there is only 1 column of names because chosen_df is empty
    if (ncol(fat_df) == 1){
      fat_df$newCol <- 0
    }
    
    #set the names of the column
    setnames(fat_df, c("Fats", "Value (g)"))
    
    #plot fat_df
    fat_plot <- ggplot(fat_df) +
      geom_col(aes(x = `Fats`, y = `Value (g)`, fill = `Fats`)) +
      labs(x = "\nTypes of fat", y = "Value (g)\n") +
      ylim(0, NA) +
      geom_hline(yintercept = 78) +
      ggtitle("Graph of breakdown of fats") +
      theme(legend.position = "none", axis.title.x = element_text(size = 11), panel.background = element_rect(fill = "snow1", colour = "snow1"), plot.title = element_text(size = 20, hjust = 0.5))
    
    #use ggplotly to produce interactive web graph
    ggplotly(fat_plot)
  })
  
  output$vitaminA_df <- renderPlotly({
    #select vitamin A columns from chosen_df 
    vitaminA_df <- ingredient_df$chosen_df %>%
      select(`Retinol (mcg)`:`Cryptoxanthin, beta (mcg)`, - `Vitamin A, RAE (mcg_RAE)`)
    
    #for loop to calculate amount of nutrient in each row by weight
    for (i in 1:nrow(vitaminA_df)){
      vitaminA_df[i, ] <- vitaminA_df[i, ] / 100 * ingredient_df$chosen_df$`Food Weight`[i]
    }
    
    #update vitaminA_df to have 2 columns consisting of names and value
    vitaminA_df <- data.table(colnames(vitaminA_df), colSums(vitaminA_df))
    
    #this is to prevent error at the start where there is only 1 column of names because chosen_df is empty
    if (ncol(vitaminA_df) == 1){
      vitaminA_df$newCol <- 0
    }
    
    #set the names of the column
    setnames(vitaminA_df, c("Vitamin A", "Value (mcg)"))
    
    #plot vitaminA_df    
    vitaminA_plot <- ggplot(vitaminA_df) +
      geom_col(aes(x = `Vitamin A`, y = `Value (mcg)`, fill = `Vitamin A`)) +
      labs(x = "\nTypes of vitamin A", y = "Value (mcg)\n") +
      ylim(0, NA) +
      geom_hline(yintercept = 900) +
      ggtitle("Graph of breakdown of vitamin A") +
      theme(legend.position = "none", axis.text.x = element_text(size = 11), panel.background = element_rect(fill = "snow1", colour = "snow1"), plot.title = element_text(size = 20, hjust = 0.5))
    ggplotly(vitaminA_plot)
  })
  
  #list of reactive values for the MET calculator
  MET <- reactiveValues()
  MET$value <- 0
  MET$calories <- 0
  MET$compare <- 0
  MET$food_calorie <- 0
  
  #when update button is pressed, update all the MET reactive values
  observeEvent(input$update_MET,{
    MET$value <- subset(met_list, `SPECIFIC MOTION` == input$activity_MET)$`METs`
    MET$calories <-(MET$value * 3.5 * input$weight_MET / 200) * input$time_MET
    MET$compare <- compare_MET()
    MET$food_calorie <- food_calorie_MET()
  })
  
  #food choice is the subset of nutrition list which contains the input food
  food_choice_MET <- eventReactive(input$food_MET,{
    food_choice_MET <-nutrition_list %>%
      filter(nutrition_list$`Main food description` == input$food_MET)
    food_choice_MET
  })
  
  #food portion is the subset of food portion list which contains the input food
  food_portion_MET <- eventReactive(input$food_MET,{
    food_portion_MET <- food_portionList[food_portionList$`Food code` %in% food_choice_MET()$`Food code`]
    food_portion_MET
  })
  
  #code to run once food is chosen
  observeEvent(input$food_MET, {
    updateSelectizeInput(inputId = "serving_size_MET", label = "Choose serving size",
                         choices = c(food_portion_MET()$`Portion description`, "1 g"),
                         options = list(
                           placeholder = "Type to choose serving size",
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
  })
  
  #calculate the food weight of the MET calculator
  food_weight_MET <- reactive({
    if (input$serving_size_MET == "1 g"){
      food_weight_MET <- input$number_of_serving_MET
    }else{
      food_weight_MET <- food_portion_MET()[`Portion description` == input$serving_size_MET]$`Portion weight (g)` * input$number_of_serving_MET}
  })
  
  #calculate the food calories for the chosen food in the MET calculator
  food_calorie_MET <- reactive({
    calorie_MET <- food_choice_MET()$`Energy (kcal)` / 100 * food_weight_MET()
  })
  
  #gets the ratio of calories burned to calories of the food in the MET calculator
  compare_MET <- reactive({
    compare_MET <- round(MET$calories / food_calorie_MET(), digits = 1)
  })
  
  #prints the MET value of chosen activity and calories burned
  output$calories_MET <- renderPrint({
    cat("The MET value of the chosen activity is", MET$value)
    cat("\nApproximate total calories burned from the activity is", MET$calories)
  })
  
  #prints the calories of 1 serving size of the food and the ratio of calories burned to the calorie of the food
  output$compareFood_MET <- renderPrint({
    cat("The calories of the selected food is", MET$food_calorie)
    cat("\nThe ratio of calories burned from the activity to calories in the food is", MET$compare)
  })
  
  # Update the input based on selectInput
  observeEvent(input$type, {
    updateTabsetPanel(inputId = "params", selected = input$type)
  })
  
  ### BMI calculator
  
  #Calculate body mass index (BMI)
  calculatedBMI <- eventReactive(input$calcBMI, {
    calculatedBMI = input$weight_BMI / ((input$height_BMI / 100) ^ 2)
  })
  
  #Outputs the BMI and the health status
  BMI <- renderText({
    if (calculatedBMI() < 18.5) {
      status_BMI <- "Underweight"
    } else if (calculatedBMI() < 25) {
      status_BMI <- "Normal"
    } else if (calculatedBMI() < 30) {
      status_BMI <- "Overweight"
    } else if (calculatedBMI() < 35) {
      status_BMI <- "Obesity class 1"
    } else if (calculatedBMI() < 40) {
      status_BMI <- "Obesity class 2"
    } else {
      status_BMI <- "Obesity class 3 / Extremely obese"
    }
  })
  
  ### BFP calculator
  
  # Calculate body fat percentage (BFP)
  calculatedBFP <- eventReactive(input$calcBFP, {
    bmi = input$weight_BFP / ((input$height_BFP / 100) ^ 2)
    
    if (input$gender_BFP == "male") {
      calculatedBFP = 1.2 * bmi + (0.23 * input$age_BFP) - 16.2
    } else {
      calculatedBFP = 1.2 * bmi + (0.23 * input$age_BFP) - 5.4
    }
  })
  
  #Determine the health status based on gender, age and calculated BFP
  BFP <- renderText({
    if (input$gender_BFP == "female") {
      
      if (input$age_BFP > 19 && input$age_BFP < 40) {
        if (calculatedBFP() < 21) {
          status_BFP <- "Low fat"
        } else if (calculatedBFP() < 33) {
          status_BFP <- "Healthy"
        } else {
          status_BFP <- "Obese"
        }
      } else if (input$age_BFP < 60) {
        if (calculatedBFP() < 23) {
          status_BFP <- "Low fat"
        } else if (calculatedBFP() < 35) {
          status_BFP <- "Healthy"
        } else {
          status_BFP <- "Obese"
        }
      } else if (input$age_BFP < 80) {
        if (calculatedBFP() < 24) {
          status_BFP <- "Low fat"
        } else if (calculatedBFP() < 36) {
          status_BFP <- "Healthy"
        } else {
          status_BFP <- "Obese"
        }
      }
      
    } else if (input$gender_BFP == "male") {
      
      if (input$age_BFP > 19 && input$age_BFP < 40) {
        if (calculatedBFP() < 8) {
          status_BFP <- "Low fat"
        } else if (calculatedBFP() < 19) {
          status_BFP <- "Healthy"
        } else {
          status_BFP <- "Obese"
        }
      } else if (input$age_BFP < 60) {
        if (calculatedBFP() < 11) {
          status_BFP <- "Low fat"
        } else if (calculatedBFP() < 22) {
          status_BFP <- "Healthy"
        } else {
          status_BFP <- "Obese"
        }
      } else if (input$age_BFP < 80) {
        if (calculatedBFP() < 13) {
          status_BFP <- "Low fat"
        } else if (calculatedBFP() < 25) {
          status_BFP <- "Healthy"
        } else {
          status_BFP <- "Obese"
        }
      }
    }
  })
  
  #Display final output for BMI/BFP
  output$result <- renderPrint({
    if (input$type == "Body Mass Index") {
      cat("Body mass index (BMI):", format(round(calculatedBMI(), 2)))
      cat("\nStatus: ", BMI(), sep = "")
    } else if (input$type == "Body Fat Percentage") {
      bodyFatWeight = calculatedBFP() / 100 * input$weight_BFP
      cat("Body fat percentage: ", format(round(calculatedBFP(), 2)), "%", sep = "")
      cat("\nBody fat weight: ", format(round(bodyFatWeight, 2)), "kg", sep = "")
      cat("\nStatus: ", BFP(), sep = "")
    }
  })
  
  ### BMR calculator
  
  #Show selection chose by the user when calculate button is clicked
  #Show weight
  show_weight <- eventReactive(input$update_BMR, {
    input$weight_BMR
  })
  
  output$selected_weight <- renderPrint({
    cat(show_weight(), "kg")
  })
  
  #Show gender
  show_gender <- eventReactive(input$update_BMR, {
    input$gender_BMR
  })
  
  output$selected_gender <- renderPrint({
    cat(show_gender())
  })
  
  #Show lean factor multiplier
  show_lean_factor_multiplier <- eventReactive(input$update_BMR, {
    input$lean_factor_multiplier
  })
  
  output$selected_lean_factor_multiplier <- renderPrint({
    cat(show_lean_factor_multiplier())
  })
  
  #Show selected activity level
  show_selected_activity <- eventReactive(input$update_BMR, {
    input$daily_activity_level
  })
  
  output$selected_activity <- renderPrint({
    cat(show_selected_activity())
  })
  
  # calculate the recommended calories intake
  get_calories <- eventReactive(input$update_BMR, {
    if (input$gender_BMR == "Male"){
      if (input$daily_activity_level == "Very Light") {
        calories <- input$weight_BMR * 1.0 * 24 * input$lean_factor_multiplier * 1.3
      } else if (input$daily_activity_level == "Light") {
        calories <- input$weight_BMR * 1.0 * 24 * input$lean_factor_multiplier * 1.55
      } else if (input$daily_activity_level == "Moderate") {
        calories <- input$weight_BMR * 1.0 * 24 * input$lean_factor_multiplier * 1.65
      } else if (input$daily_activity_level == "Heavy"){
        calories <- input$weight_BMR * 1.0 * 24 * input$lean_factor_multiplier * 1.8
      } else if (input$daily_activity_level == "Very Heavy") {
        calories <- input$weight_BMR * 1.0 * 24 * input$lean_factor_multiplier * 2.0
      }
      
    }
    
    else if (input$gender_BMR == "Female") {
      if(input$daily_activity_level == "Very Light") {
        calories <- input$weight_BMR * 0.9 * 24 * input$lean_factor_multiplier * 1.3
      } else if (input$daily_activity_level == "Light") {
        calories <- input$weight_BMR * 0.9 * 24 * input$lean_factor_multiplier * 1.55
      } else if (input$daily_activity_level == "Moderate") {
        calories <- input$weight_BMR * 0.9 * 24 * input$lean_factor_multiplier * 1.65
      } else if (input$daily_activity_level == "Heavy") {
        calories <- input$weight_BMR * 0.9 * 24 * input$lean_factor_multiplier * 1.8
      } else if (input$daily_activity_level == "Very Heavy") {
        calories <- input$weight_BMR * 0.9 * 24 * input$lean_factor_multiplier * 2.0
      }
    }
  })
  
  output$recommended_calories <- renderPrint({
    cat(round(get_calories(), digits = 0), "kcal")
    cat("\n* Note that the result is only an estimated value")
  })
}

shinyApp(ui = ui, server = server)