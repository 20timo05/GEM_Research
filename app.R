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
    return("Keine Daten für Empfehlungen verfügbar.")
  }
  
  lowest_var <- shap_res %>%
    arrange(shap_value) %>%
    slice(1) %>%
    pull(feature)
  
  recs <- list(
    "FRFAILyy" = c("1. Betrachte Fehler als wertvolle Lernchance.", "2. Starte mit kleinen, kalkulierbaren Risiken.", "3. Sprich mit erfahrenen Mentoren über ihre Rückschläge."),
    "SUSKILyy" = c("1. Nutze Online-Kurse zur gezielten Weiterbildung.", "2. Suche nach einem Mitgründer mit ergänzenden Skills.", "3. Übe das Pitchen deiner Idee vor Freunden."),
    "OPPORTyy" = c("1. Netzwerke aktiver in deiner lokalen Startup-Szene.", "2. Untersuche aktuelle Marktrends und Nischen.", "3. Brainstorme Lösungen für Alltagsprobleme, die dich stören."),
    "KNOWENyy" = c("1. Besuche Startup-Events und Meetups.", "2. Vernetze dich gezielt auf LinkedIn mit Gründern.", "3. Tritt Online-Gründer-Communities bei."),
    "EASYSTyy" = c("1. Informiere dich bei lokalen Behörden über Gründungsschritte.", "2. Such nach Vereinfachungen oder digitalen Gründungswegen.", "3. Hol dir Unterstützung bei der IHK oder Gründungszentren."),
    "GEMHHINC" = c("1. Erstelle einen soliden Finanzplan.", "2. Prüfe Möglichkeiten für Nebenberufliche Gründungen.", "3. Suche nach kostengünstigen Ressourcen (Bootstrapping)."),
    "GEMEDUC" = c("1. Nutze spezialisierte Weiterbildungen für Gründer.", "2. Suche Mentoren mit Branchenerfahrung.", "3. Lerne 'Learning by Doing' durch kleine Projekte."),
    "cphhinc" = c("1. Fokussiere auf kosteneffiziente Geschäftsmodelle.", "2. Nutze staatliche Hilfen für Gründer.", "3. Baue finanzielle Reserven langsam wieder auf."),
    "OPPISMyy" = c("1. Trainiere deinen Blick für positive Chancen.", "2. Lies Erfolgsgeschichten zur Inspiration.", "3. Umgib dich mit optimistischen Menschen."),
    "PROACTyy" = c("1. Setze dir täglich kleine, erreichbare Ziele.", "2. Etabliere eine 'Tu es jetzt'-Mentalität.", "3. Zerlege große Aufgaben in kleine Schritte."),
    "CREATIVyy" = c("1. Mache Brainstorming-Sessions im Team.", "2. Wechsle öfter die Perspektive bei Problemen.", "3. Nutze Kreativitätstechniken wie Design Thinking."),
    "VISIONyy" = c("1. Definiere deine langfristigen Ziele schriftlich.", "2. Erstelle ein Vision Board für dein Unternehmen.", "3. Reflektiere deine persönlichen Werte."),
    "age" = c("1. Nutze deine Lebenserfahrung als Stärke.", "2. Bleibe neugierig und offen für neue Technologien.", "3. Vernetze dich mit Gründern anderer Generationen."),
    "gender" = c("1. Nutze spezifische Förderprogramme (z.B. für Gründerinnen).", "2. Suche dir Vorbilder in deiner Peer-Group.", "3. Baue ein diverses Netzwerk auf."),
    "hhsize" = c("1. Organisiere dein Zeitmanagement strikt.", "2. Involviere Familie/Partner in deine Pläne.", "3. Schaffe dir feste Arbeitszeiten und -räume."),
    "ctryalp" = c("1. Untersuche das lokale Startup-Ökosystem genau.", "2. Vernetze dich international.", "3. Nutze lokale Vorteile und Förderungen.")
  )
  
  default_rec <- c("1. Überprüfe deinen Businessplan auf Schwachstellen.", "2. Hole dir allgemeines Feedback von potenziellen Kunden ein.", "3. Analysiere den Markt erneut auf Bedürfnisse.")
  
  selected_recs <- if (lowest_var %in% names(recs)) recs[[lowest_var]] else default_rec
  
  HTML(paste0(
    "<strong>Der stärkste negative Einflussfaktor war '", lowest_var, "'.</strong>",
    "Wir empfehlen dir folgende Schritte:<br>",
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
                   title = "Teil 1: Demografische Daten",
                   step_now = 1, total_steps = 4,
                   prev_id = NULL, next_id = "btn_p1_next",
                   content_ui = tagList(
                     p("Bitte gib uns ein paar Informationen zu deiner Person."),
                     
                     # ctryalp: Using standard input because list is long
                     ui_standard_input(
                       selectInput(inputId= "ctryalp", label = NULL, width = "100%",
                                   choices = list("Deutschland" = "Germany (DE)", "Spanien" = "Spain (ES)", "Polen" = "Poland (PL)", "Chile" = "Chile (CL)", "Frankreich" = "France (FR)",  "Saudi Arabien" = "Saudi Arabia (SA)", "Andere" = "Other")),
                       "In welchem Land lebst du?"
                     ),
                     
                     # gender: Using segmented control
                     ui_segmented_control(
                       inputId = "gender", 
                       label = "Was ist dein Geschlecht?", 
                       choices = c("Männlich" = "Male", "Weiblich" = "Female")
                     ),
                     
                     # age: Standard numeric input
                     ui_standard_input(
                       numericInput(inputId= "age", label = NULL, value = NULL, min = 18, max = 64, width = "100px"),
                       "Wie alt bist du?"
                     ),
                     
                     # hhsize: Standard numeric input
                     ui_standard_input(
                       numericInput(inputId= "hhsize", label = NULL, value = 2, min = 1, max = 20, width = "100px"),
                       "Wie viele Personen leben in deinem Haushalt (mit dir eingeschlossen):"
                     )
                   )
                 )
        ),
        
        # --- SLIDE 2: PERCEPTIONS PART 1 ---
        tabPanel("p2",
                 ui_page_template(
                   id = "p2", 
                   title = "Teil 2: Dein Arbeitsstatus & Einkommen",
                   step_now = 2, total_steps = 4,
                   prev_id = "btn_p2_back", next_id = "btn_p2_next",
                   content_ui = tagList(
                     
                     # GEMHHINC: Segmented
                     ui_segmented_control(
                       inputId = "GEMHHINC", 
                       label = "In welche der folgenden Kategorien fällt dein jährliches Gesamthaushaltseinkommen?", 
                       choices = c("Unteres Drittel" = "Lowest Third", "Mittleres Drittel" = "Middle Third", "Oberes Drittel" = "Upper Third") # @TODO allow no choice (and then map that to Unknown Category)
                     ),
                     
                     # GEMEDUC: Standard Input (List is too long for buttons)
                     ui_standard_input(
                       selectInput(inputId = "GEMEDUC", label = NULL, width = "100%", selected = character(0),
                                   choices = list("Kein Abschluss" = "None", "Schule ohne Abschluss" = "Some Secondary", "Mittlere Reife / Abitur" = "Secondary Degree", "Berufsausbildung / Lehre" = "Post-Secondary", "Hochschulabschluss" = "Graduate Experience")), # @TODO don't allow no choice, put default None
                       "Was ist deine höchste schulische Bildung?"
                     ),
                     
                     # cphhinc: Segmented (5 options fits okay on wide screens, or stacks)
                     ui_segmented_control(
                       inputId = "cphhinc", 
                       label = "Wie hat die Corona Pandemie dein Haushaltseinkommen im Jahr 2020 verringert?", 
                       choices = c("Stark verringert" = "Strongly Decrease", "Etwas verringert" = "Somewhat Decrease", "Keine Veränderung" = "No Change", "Etwas erhöht" = "Somewhat Increase", "Stark erhöht" = "Strongly Increase")
                     )
                   )
                 )
        ),
        
        # --- SLIDE 3: PERCEPTIONS PART 2 ---
        tabPanel("p3",
                 ui_page_template(
                   id = "p3", 
                   title = "Teil 3: Unternehmerische Wahrnehmung",
                   step_now = 3, total_steps = 4,
                   prev_id = "btn_p3_back", next_id = "btn_p3_next",
                   content_ui = tagList(
                     p("Deine Einstellung zu Risiko und Erfolg."),
                     
                     ui_segmented_control(
                       "KNOWENyy", 
                       "Kennst du jemanden, der in den letzten 2 Jahren ein Unternehmen gegründet hat?",
                       c("Nein" = "None", "Ja" = "At least one")
                     ),
                     
                     ui_segmented_control(
                       "OPPORTyy", 
                       '"In den nächsten sechs Monaten wird es in der Gegend, in der du lebst, gute Gelegenheiten geben, ein Unternehmen zu gründen."',
                       c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree") # @TODO don't allow no answer (unsure if unknown is not answered or survey design)
                     ),
                     
                     ui_segmented_control(
                       "SUSKILyy", 
                       '"Ich habe das Wissen, die Fähigkeiten und die Erfahrung, um ein Unternehmen zu gründen."',
                       c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree")  # @TODO don't allow no answer 
                     ),
                     
                     ui_segmented_control(
                       "FRFAILyy", 
                       "Die Angst vor dem Scheitern würde mich davon abhalten, ein Unternehmen zu gründen.",
                       c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree")  # @TODO don't allow no answer 
                     ),
                     
                     ui_segmented_control(
                       "EASYSTyy", 
                       "Es ist leicht in meinem Land ein Unternehmen zu gründen.",
                       c("Stimme nicht zu" = "Disagree", "Stimme zu" = "Agree")  # @TODO don't allow no answer 
                     )
                   )
                 )
        ),
        
        # --- SLIDE 4: MINDSET ---
        tabPanel("p4",
                 ui_page_template(
                   id = "p4", 
                   title = "Teil 4: Deine Einstellung",
                   step_now = 4, total_steps = 4,
                   prev_id = "btn_p4_back", next_id = "btn_finish",
                   content_ui = tagList(
                     p("Wie würdest du deine Persönlichkeit beschreiben?"),
                     
                     # Note: Using the exact choice strings from original app
                     ui_segmented_control(
                       "OPPISMyy", 
                       "Selbst wenn ich mich in einem Bereich sehr gut auskenne, sehe ich selten Geschäftsmöglichkeiten.",
                       c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")
                     ),
                     
                     ui_segmented_control(
                       "PROACTyy", 
                       "Selbst wenn ich eine gewinnbringende Gelegenheit erkenne, werde ich selten aktiv.",
                       c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")
                     ),
                     
                     ui_segmented_control(
                       "CREATIVyy", 
                       "Andere denken über mich, dass ich innovativ bin.",
                       c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")
                     ),
                     
                     ui_segmented_control(
                       "VISIONyy", 
                       "Jede Entscheidung, die ich treffe, ist Teil meines langfristigen Karriereplans.",
                       c("Stimme gar nicht zu" = "Strongly Disagree", "Stimme weniger zu" = "Somewhat Disagree", "Weder noch" = "Neither", "Stimme zu" = "Somewhat Agree", "Stimme gar zu" = "Strongly Agree")
                     )
                   )
                 )
        ),
        
        # --- SLIDE 5: RESULTS ---
        tabPanel("results",
                 div(class = "container", style = "text-align: center;",
                     h2("Dein Ergebnis"),
                     p("Basierend auf deinen Antworten haben wir folgende Schlüsselfaktoren identifiziert", style = "color: #666;"),
                     hr(),
                     
                     fluidRow(
                       column(6, 
                              h4("Positive Faktoren (Booster)", style = "color: #2ca02c;"),
                              plotOutput("plot_positive", height = "300px")
                       ),
                       column(6,
                              h4("Negative Faktoren (Hemmnisse)", style = "color: #d62728;"),
                              plotOutput("plot_negative", height = "300px")
                       )
                     ),
                     
                     hr(),
                     h4("Handlungsempfehlungen"),
                     wellPanel(
                       htmlOutput("recommendation_text", style = "display: flex; flex-direction: column; align-items: center; gap: 10px;")
                     ),
                     
                     br(),
                     actionButton("btn_restart", "Neu Starten", class = "btn-start", style = "margin-left: auto; margin-right: auto;")
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
  observeEvent(input$btn_p1_next, { updateTabsetPanel(session, "wizard", selected = "p2") })
  observeEvent(input$btn_p2_back, { updateTabsetPanel(session, "wizard", selected = "p1") })
  observeEvent(input$btn_p3_back, { updateTabsetPanel(session, "wizard", selected = "p2") })
  observeEvent(input$btn_p4_back, { updateTabsetPanel(session, "wizard", selected = "p3") })
  observeEvent(input$btn_p2_next, { 
    # Validate Page 2 Inputs
    req_ids <- c("GEMHHINC", "GEMEDUC")
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
    req_ids <- c("OPPORTyy", "SUSKILyy", "FRFAILyy", "EASYSTyy")
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
    updateTabsetPanel(session, "wizard", selected = "results") 
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
    validate(need(shap_results(), "Die Analyse wird gestartet... Bitte warten."))
    plots <- plot_shap_contribution(shap_results(), top_n = 3)
    plots$positive + 
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_blank())
  })
  
  output$plot_negative <- renderPlot({
    validate(need(shap_results(), "Die Analyse wird gestartet... Bitte warten."))
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