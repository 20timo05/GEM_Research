library(shiny)
library(dplyr)
library(ggplot2) # Required for the plots

# 1. LOAD DESIGN SYSTEM
source("ui_utils.R")

# 2. PRE-FLIGHT CHECK (Original Logic)
required_files <- c(
  "output_production/final_model.rds",
  "output_production/final_threshold.rds",
  "output_production/shap_background.rds"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  msg <- paste0(
    "\n\n====================================================================\n",
    "CRITICAL ERROR: Missing Production Artifacts\n",
    "====================================================================\n",
    "The following required files are missing from 'output_production/':\n",
    paste0(" - ", missing_files, collapse = "\n"),
    "\n\n",
    "ACTION REQUIRED:\n",
    "Please run the training script to generate these files before starting the GUI.\n",
    "Run: source('scripts_production/02_train_final_model.R')\n",
    "====================================================================\n"
  )
  stop(msg)
}

cat("--- Pre-flight check passed: All model artifacts found. ---\n")

# 3. LOAD ARTIFACTS (Original Logic)
final_model <- readRDS("output_production/final_model.rds")
shap_bg     <- readRDS("output_production/shap_background.rds") 
source("scripts_production/00_utils_shap.R")

# 4. HELPER FUNCTIONS (Original Logic)

# Likert Levels (Matches Training Data Schema)
likert_levels <- c("Strongly Disagree", "Somewhat Disagree", "Neither", 
                   "Somewhat Agree", "Strongly Agree", "Refused_Answer", "Not_Applicable")

# Recommendation Logic
get_recommendations <- function(shap_res) {
  if (is.null(shap_res)) {
    return("No data available for recommendations.")
  }
  
  lowest_var <- shap_res %>%
    arrange(shap_value) %>%
    slice(1) %>%
    pull(feature)
  
  recs <- list(
    "FRFAILyy" = c("1. View failure as a learning opportunity.", "2. Start with small, calculated risks.", "3. Talk to experienced mentors about their setbacks."),
    "SUSKILyy" = c("1. Use online courses for targeted upskilling.", "2. Look for a co-founder with complementary skills.", "3. Practice pitching your idea to friends."),
    "OPPORTyy" = c("1. Network more actively in your local startup scene.", "2. Investigate current market trends and niches.", "3. Brainstorm solutions for everyday problems that annoy you."),
    "KNOWENyy" = c("1. Attend startup events and meetups.", "2. Connect specifically with founders on LinkedIn.", "3. Join online founder communities."),
    "EASYSTyy" = c("1. Inform yourself at local authorities about founding steps.", "2. Look for simplifications or digital founding paths.", "3. Get support from chambers of commerce or incubation centers."),
    "GEMHHINC" = c("1. Create a solid financial plan.", "2. Check possibilities for part-time founding.", "3. Look for cost-effective resources (bootstrapping)."),
    "GEMEDUC" = c("1. Use specialized training for founders.", "2. Seek mentors with industry experience.", "3. Learn by doing through small projects."),
    "cphhinc" = c("1. Focus on cost-efficient business models.", "2. Use government aid for founders.", "3. Rebuild financial reserves slowly."),
    "OPPISMyy" = c("1. Train your eye for positive opportunities.", "2. Read success stories for inspiration.", "3. Surround yourself with optimistic people."),
    "PROACTyy" = c("1. Set daily small, achievable goals.", "2. Establish a 'do it now' mentality.", "3. Break large tasks into small steps."),
    "CREATIVyy" = c("1. Hold brainstorming sessions in the team.", "2. Change perspectives more often when solving problems.", "3. Use creativity techniques like Design Thinking."),
    "VISIONyy" = c("1. Define your long-term goals in writing.", "2. Create a vision board for your company.", "3. Reflect on your personal values."),
    "age" = c("1. Use your life experience as a strength.", "2. Stay curious and open to new technologies.", "3. Network with founders of other generations."),
    "gender" = c("1. Use specific support programs (e.g., for female founders).", "2. Look for role models in your peer group.", "3. Build a diverse network."),
    "hhsize" = c("1. Organize your time management strictly.", "2. Involve family/partner in your plans.", "3. Create fixed working hours and spaces."),
    "ctryalp" = c("1. Investigate the local startup ecosystem closely.", "2. Network internationally.", "3. Use local advantages and subsidies.")
  )
  
  default_rec <- c("1. Review your business plan for weaknesses.", "2. Get general feedback from potential customers.", "3. Analyze the market again for needs.")
  
  selected_recs <- if (lowest_var %in% names(recs)) recs[[lowest_var]] else default_rec
  
  HTML(paste0(
    "<strong>The strongest negative influence factor was '", lowest_var, "'.</strong>",
    "We recommend the following steps:<br>",
    "<ul style='width: fit-content; list-style-type: none;'>", paste0("<li>", selected_recs, "</li>", collapse = ""), "</ul>"
  ))
}

# --- 5. UI DEFINITION ---

ui <- fluidPage(
  # Inject the CSS and JS from ui_utils.R
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
  ),
  
  div(class = "container",
      tabsetPanel(
        id = "wizard",
        type = "hidden",
        
        tabPanel("landing", ui_landing_page("btn_start_test")),
        
        # --- SLIDE 1: DEMOGRAPHICS ---
        tabPanel("p1",
                 ui_page_template(
                   id = "p1", 
                   title = "Section A: Demographics",
                   step_now = 1, total_steps = 4,
                   prev_id = NULL, next_id = "btn_p1_next",
                   content_ui = tagList(
                     p("Please provide some information about yourself."),
                     
                     # ctryalp: Using standard input because list is long
                     ui_standard_input(
                       selectInput(inputId= "ctryalp", label = NULL, width = "100%",
                                   choices = list("Germany (DE)" = "Germany (DE)", "Spain (ES)" = "Spain (ES)", "Poland (PL)" = "Poland (PL)", "Chile (CL)" = "Chile (CL)", "France (FR)" = "France (FR)",  "Saudi Arabia (SA)" = "Saudi Arabia (SA)", "(Other)" = "Other")),
                       "In which country do you reside?"
                     ),
                     
                     # gender: Using segmented control
                     ui_segmented_control(
                       inputId = "gender", 
                       label = "Please indicate your gender:", 
                       choices = c("Male" = "Male", "Female" = "Female")
                     ),
                     
                     # age: Standard numeric input
                     ui_standard_input(
                       numericInput(inputId= "age", label = NULL, value = NULL, min = 18, max = 64, width = "100px"),
                       "What is your age in years?"
                     ),
                     
                     # hhsize: Standard numeric input
                     ui_standard_input(
                       numericInput(inputId= "hhsize", label = NULL, value = 2, min = 1, max = 20, width = "100px"),
                       "Including yourself, how many people live in your household?"
                     )
                   )
                 )
        ),
        
        # --- SLIDE 2: PERCEPTIONS PART 1 ---
        tabPanel("p2",
                 ui_page_template(
                   id = "p2", 
                   title = "Section B: Your Work Situation and Household Income",
                   step_now = 2, total_steps = 4,
                   prev_id = "btn_p2_back", next_id = "btn_p2_next",
                   content_ui = tagList(
                     
                     # GEMHHINC: Segmented
                     ui_segmented_control(
                       inputId = "GEMHHINC", 
                       label = "Into which of the following categories does your total annual household income fall?", 
                       choices = c("Lowest Third" = "Lowest Third", "Middle Third" = "Middle Third", "Upper Third" = "Upper Third") # @TODO allow no choice (and then map that to Unknown Category)
                     ),
                     
                     # GEMEDUC: Standard Input (List is too long for buttons)
                     ui_standard_input(
                       selectInput(inputId = "GEMEDUC", label = NULL, width = "100%", selected = character(0),
                                   choices = list("None" = "None", "Some Secondary" = "Some Secondary", "Secondary Degree" = "Secondary Degree", "Post-Secondary" = "Post-Secondary", "Graduate Experience" = "Graduate Experience", "Other/Unknown" = "Other/Unknown")), # @TODO don't allow no choice, put default None
                       "What is the highest level of education you have completed?"
                     ),
                     
                     # cphhinc: Segmented (5 options fits okay on wide screens, or stacks)
                     ui_segmented_control(
                       inputId = "cphhinc", 
                       label = "How has the coronavirus pandemic affected your household income in 2020?", 
                       choices = c("Strongly Decrease" = "Strongly Decrease", "Somewhat Decrease" = "Somewhat Decrease", "No Change" = "No Change", "Somewhat Increase" = "Somewhat Increase", "Strongly Increase" = "Strongly Increase")
                     )
                   )
                 )
        ),
        
        # --- SLIDE 3: PERCEPTIONS PART 2 ---
        tabPanel("p3",
                 ui_page_template(
                   id = "p3", 
                   title = "Section C: Your Entrepreneurial Perceptions",
                   step_now = 3, total_steps = 4,
                   prev_id = "btn_p3_back", next_id = "btn_p3_next",
                   content_ui = tagList(
                     p("Your attitude towards risk and success."),
                     
                     ui_segmented_control(
                       "KNOWENyy", 
                       "Do you personally know anyone who has started a business in the past two years?",
                       c("None" = "None", "At least one" = "At least one")
                     ),
                     
                     ui_segmented_control(
                       "OPPORTyy", 
                       '"In the next six months, there will be good opportunities for starting a business in the area where you live."',
                       c("Disagree" = "Disagree", "Agree" = "Agree")
                     ),
                     
                     ui_segmented_control(
                       "SUSKILyy", 
                       '"You have the knowledge, skill, and experience required to start a new business."',
                       c("Disagree" = "Disagree", "Agree" = "Agree")
                     ),
                     
                     ui_segmented_control(
                       "FRFAILyy", 
                       '"Fear of failure would prevent you from starting a business."',
                       c("Disagree" = "Disagree", "Agree" = "Agree") 
                     ),
                     
                     ui_segmented_control(
                       "EASYSTyy", 
                       '"In your country, it is easy to start a business."',
                       c("Disagree" = "Disagree", "Agree" = "Agree")
                     )
                   )
                 )
        ),
        
        # --- SLIDE 4: MINDSET ---
        tabPanel("p4",
                 ui_page_template(
                   id = "p4", 
                   title = "Section D: Your Personal Views",
                   step_now = 4, total_steps = 4,
                   prev_id = "btn_p4_back", next_id = "btn_finish",
                   content_ui = tagList(
                     p("How would you describe your personality?"),
                     
                     # Note: Using the exact choice strings from original app
                     ui_segmented_control(
                       "OPPISMyy", 
                       '"You rarely see business opportunities, even if you are very knowledgeable in the area."',
                       c("Strongly Disagree" = "Strongly Disagree", "Somewhat Disagree" = "Somewhat Disagree", "Neither" = "Neither", "Somewhat Agree" = "Somewhat Agree", "Strongly Agree" = "Strongly Agree")
                     ),
                     
                     ui_segmented_control(
                       "PROACTyy", 
                       '"Even when you spot a profitable opportunity, you rarely act on it."',
                       c("Strongly Disagree" = "Strongly Disagree", "Somewhat Disagree" = "Somewhat Disagree", "Neither" = "Neither", "Somewhat Agree" = "Somewhat Agree", "Strongly Agree" = "Strongly Agree")
                     ),
                     
                     ui_segmented_control(
                       "CREATIVyy", 
                       '"Other people think you are highly innovative."',
                       c("Strongly Disagree" = "Strongly Disagree", "Somewhat Disagree" = "Somewhat Disagree", "Neither" = "Neither", "Somewhat Agree" = "Somewhat Agree", "Strongly Agree" = "Strongly Agree")
                     ),
                     
                     ui_segmented_control(
                       "VISIONyy", 
                       '"Every decision you make is part of your long-term career plan."',
                       c("Strongly Disagree" = "Strongly Disagree", "Somewhat Disagree" = "Somewhat Disagree", "Neither" = "Neither", "Somewhat Agree" = "Somewhat Agree", "Strongly Agree" = "Strongly Agree")
                     )
                   )
                 )
        ),
        
        # --- SLIDE 5: RESULTS ---
        tabPanel("results",
                 div(class = "container", style = "text-align: center;",
                     h2("Your Result"),
                     p("Based on your answers, we have identified the following key factors", style = "color: #666;"),
                     hr(),
                     
                     fluidRow(
                       column(6, 
                              h4("Positive Factors (Boosters)", style = "color: #2ca02c;"),
                              plotOutput("plot_positive", height = "300px")
                       ),
                       column(6,
                              h4("Negative Factors (Barriers)", style = "color: #d62728;"),
                              plotOutput("plot_negative", height = "300px")
                       )
                     ),
                     
                     hr(),
                     h4("Recommendations for Action"),
                     wellPanel(
                       htmlOutput("recommendation_text", style = "display: flex; flex-direction: column; align-items: center; gap: 10px;")
                     ),
                     
                     br(),
                     actionButton("btn_restart", "Restart", class = "btn-start", style = "margin-left: auto; margin-right: auto;")
                 )
        )
      )
  )
)

# --- 6. SERVER LOGIC (Original Logic) ---

server <- function(input, output, session) {
  addResourcePath("assets", "www")
  
  # Navigation Logic
  observeEvent(input$btn_start_test, { updateTabsetPanel(session, "wizard", selected = "p1") })
  observeEvent(input$btn_p2_back, { updateTabsetPanel(session, "wizard", selected = "p1") })
  observeEvent(input$btn_p3_back, { updateTabsetPanel(session, "wizard", selected = "p2") })
  observeEvent(input$btn_p4_back, { updateTabsetPanel(session, "wizard", selected = "p3") })
  observeEvent(input$btn_p1_next, { 
    has_error <- FALSE
    
    # 1. Validate Gender (Mandatory)
    if (is.null(input$gender)) {
      session$sendCustomMessage("validate_input", list(id = "gender", valid = FALSE))
      has_error <- TRUE
    } else {
      session$sendCustomMessage("validate_input", list(id = "gender", valid = TRUE))
    }
    
    # 2. Validate Age (Optional, but strict interval IF entered)
    val_age <- input$age
    is_age_valid <- TRUE
    
    # numericInput returns NA if empty. If NOT NA, check bounds.
    if (!is.na(val_age)) {
      if (val_age < 18 || val_age > 64) {
        is_age_valid <- FALSE
      }
    }
    
    session$sendCustomMessage("validate_input", list(id = "age", valid = is_age_valid))
    if (!is_age_valid) has_error <- TRUE
    
    if (!has_error) updateTabsetPanel(session, "wizard", selected = "p2") 
  })
  observeEvent(input$btn_p2_next, { 
    # Validate Page 2 Inputs
    req_ids <- c("GEMHHINC", "GEMEDUC", "cphhinc")
    has_error <- FALSE
    
    for(id in req_ids) {
      val <- input[[id]]
      if (is.null(val) || val == "") {
        session$sendCustomMessage("validate_input", list(id = id, valid = FALSE))
        has_error <- TRUE
      } else {
        session$sendCustomMessage("validate_input", list(id = id, valid = TRUE))
      }
    }
    
    if (!has_error) updateTabsetPanel(session, "wizard", selected = "p3") 
  })
  observeEvent(input$btn_p3_next, { 
    # Validate Page 3 Inputs
    req_ids <- c("KNOWENyy", "OPPORTyy", "SUSKILyy", "FRFAILyy", "EASYSTyy")
    has_error <- FALSE
    
    for(id in req_ids) {
      val <- input[[id]]
      if (is.null(val) || val == "") {
        session$sendCustomMessage("validate_input", list(id = id, valid = FALSE))
        has_error <- TRUE
      } else {
        session$sendCustomMessage("validate_input", list(id = id, valid = TRUE))
      }
    }
    
    if (!has_error) updateTabsetPanel(session, "wizard", selected = "p4") 
  })
  
  
  observeEvent(input$btn_finish, { 
    # Validate Page 4 Inputs (Mindset)
    req_ids <- c("OPPISMyy", "PROACTyy", "CREATIVyy", "VISIONyy")
    has_error <- FALSE
    
    for(id in req_ids) {
      val <- input[[id]]
      if (is.null(val) || val == "") {
        session$sendCustomMessage("validate_input", list(id = id, valid = FALSE))
        has_error <- TRUE
      } else {
        session$sendCustomMessage("validate_input", list(id = id, valid = TRUE))
      }
    }
    
    if (!has_error) {
      updateTabsetPanel(session, "wizard", selected = "results") 
    }
  })
  
  observeEvent(input$btn_restart, {
    updateTabsetPanel(session, "wizard", selected = "landing")
    session$reload() # Reload to clear inputs cleanly
  })
  
  observeEvent(input$btn_close, {
    updateTabsetPanel(session, "wizard", selected = "landing")
    session$reload() # Reload to clear inputs cleanly
  })
  
  # Reactive Data Construction
  current_data <- reactive({
    # 1. Map Country Input to WBINC Level
    wbinc_lookup <- c(
      "Germany (DE)"      = "High",
      "Spain (ES)"        = "High",
      "Poland (PL)"       = "High",
      "Chile (CL)"        = "High",
      "France (FR)"       = "High",
      "Saudi Arabia (SA)" = "High",
      "Other"             = "High" # Default fallback
    )
    
    # Get level based on input, default to "High" if something goes wrong
    selected_wbinc <- wbinc_lookup[input$ctryalp]
    if(is.na(selected_wbinc)) selected_wbinc <- "High"
    
    if (is.na(input$age)) {
      val_age  <- 21    # Fallback/Imputation value (prevent model crash on NA)
      val_miss <- "Yes"
    } else {
      val_age  <- as.numeric(input$age)
      val_miss <- "No"
    }
    
    data.frame(
      # Demographics
      WBINC = factor(selected_wbinc, levels = c("Low", "Lower Middle", "Upper Middle", "High")),
      gender = factor(input$gender, levels = c("Male", "Female")),
      age = val_age,
      hhsize = as.numeric(input$hhsize),
      GEMHHINC = factor(input$GEMHHINC, levels = c("Lowest Third", "Middle Third", "Upper Third", "Unknown")),
      GEMEDUC = factor(input$GEMEDUC, levels = c("None", "Some Secondary", "Secondary Degree", "Post-Secondary", "Graduate Experience", "Other/Unknown")),
      cphhinc = factor(input$cphhinc, levels = c("Strongly Decrease", "Somewhat Decrease", "No Change", "Somewhat Increase", "Strongly Increase"), ordered = TRUE),
      
      # Perceptions
      KNOWENyy = factor(input$KNOWENyy, levels = c("None", "At least one")),
      OPPORTyy = factor(input$OPPORTyy, levels = c("Disagree", "Agree", "Unknown")),
      SUSKILyy = factor(input$SUSKILyy, levels = c("Disagree", "Agree", "Unknown")),
      FRFAILyy = factor(input$FRFAILyy, levels = c("Disagree", "Agree")),
      EASYSTyy = factor(input$EASYSTyy, levels = c("Disagree", "Agree", "Refused", "Unknown")),
      
      # Mindset
      OPPISMyy = factor(input$OPPISMyy, levels = likert_levels, ordered = TRUE),
      PROACTyy = factor(input$PROACTyy, levels = likert_levels, ordered = TRUE),
      CREATIVyy = factor(input$CREATIVyy, levels = likert_levels, ordered = TRUE),
      VISIONyy = factor(input$VISIONyy, levels = likert_levels, ordered = TRUE),
      
      # Engineering Flags
      Mindset_Asked = factor("Asked", levels = c("Asked", "Not_Asked")),
      age_is_missing = factor(val_miss, levels = c("No", "Yes"))
    )
  })
  
  # --- Model Calculation ---
  predicted_score <- eventReactive(input$btn_finish, {
    req(final_model)
    req(current_data())
    
    # Predict probability using the workflow saved in final_model.rds
    pred_prob <- predict(final_model, current_data(), type = "prob")
    
    # Return the probability for the "Yes" class
    return(pred_prob$.pred_Yes) 
  })
  
  # SHAP Calculation
  shap_results <- eventReactive(input$btn_finish, {
    req(final_model)
    tryCatch({
      res <- compute_single_shap(final_model, shap_bg, current_data())
      res
    }, error = function(e) {
      showNotification(paste("Error in model:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # --- Plotting ---
  output$plot_positive <- renderPlot({
    validate(need(shap_results(), "Analysis starting... Please wait."))
    plots <- plot_shap_contribution(shap_results(), top_n = 3)
    plots$positive + 
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_blank())
  })
  
  output$plot_negative <- renderPlot({
    validate(need(shap_results(), "Analysis starting... Please wait."))
    plots <- plot_shap_contribution(shap_results(), top_n = 3)
    plots$negative + 
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_blank())
  })
  
  # --- Recommendations ---
  output$recommendation_text <- renderUI({
    req(shap_results())
    get_recommendations(shap_results())
  })
  
}

shinyApp(ui, server)