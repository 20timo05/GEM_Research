app_css <- '
  :root {
      --primary-black: #000000;
      --secondary-gray: #cccccc;
      --accent-turquoise: #8fdbd4;
      --bg-color: #ffffff;
      --font-main: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
    }

    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }

    body {
      font-family: var(--font-main);
      background-color: var(--bg-color);
      color: var(--primary-black);
      /* Flex centered layout handled by fluidPage usually, 
         but we override body slightly to match sketch */
      display: flex;
      justify-content: center;
      min-height: 100vh;
    }
    
    .landing-header h1 { font-size: 8rem; } 
    .landing-header .logo-section img { scale: 3; translate: 70px; }
    
    .landing-content { 
      width: 100%; 
      text-align: center; 
      display: flex; 
      flex-direction: column; 
      align-items: center; 
      justify-content: center; 
      margin-top: 50px; 
    }
    
     .form-control:focus {
        border-color: var(--primary-black) !important; /* Make border black instead of blue */
        box-shadow: none !important;                  /* Remove the blue glow */
        outline: none !important;
      }
    
      /* 2. Select/Dropdown Inputs (Selectize) */
      .selectize-input.focus {
        border-color: var(--primary-black) !important;
        box-shadow: none !important;
      }
    
    .landing-main-text { font-size: 3rem; font-weight: bold; margin-bottom: 10px; }
    .landing-sub-text { font-size: 1.5rem; color: #666; margin-bottom: 40px; }
    
    .btn-start { 
      font-size: 2rem; 
      padding: 15px 60px; 
      background-color: var(--accent-turquoise); 
      border: 2px solid black; 
      border-radius: 50px; 
      cursor: pointer; 
      transition: transform 0.2s; 
    }
    .btn-start:hover { transform: scale(1.05); }
    
    .container-fluid,
    .container {
      width: 100%;
      max-width: 900px;
      background: white;
      padding: 20px;
      position: relative;
      margin: 20px auto;
      display: flex;
      flex-direction: column;
    }
    
    .wrapper {
      display: flex;
      flex-direction: column;
    }
    
    /* --- Header --- */
    header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-bottom: 20px;
      padding-bottom: 10px;
      border-bottom: 1px solid black;
    }

    .logo-section {
      display: flex;
      align-items: center;
      width: 20%;
    }

    .logo-section img {
      /* CSS from your request */
      scale: 2; 
      translate: 50px;
      width: 80px;
      height: auto;
    }

    .title-section {
      flex-grow: 1;
      text-align: center;
    }

    h1 {
      font-weight: 400;
      font-size: 6rem;
      letter-spacing: 1px;
      font-family: "Brush Script MT", cursive, var(--font-main);
      text-align: center;
      width: 100%;
    }

    .user-section {
      width: 20%;
      text-align: right;
      font-size: 1.5rem;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      cursor: pointer;
    }
    
    .close-icon {
      font-size: 3rem;
      margin-bottom: 5px;
      line-height: 1;
    }

    /* --- Progress Bar --- */
    .progress-bar {
      display: flex;
      gap: 15px;
      margin-bottom: 40px;
      width: 100%;
      background: none;
    }

    .progress-step {
      height: 12px;
      flex: 1;
      border: 1px solid var(--primary-black);
    }

    .progress-step.filled {
      background-color: var(--accent-turquoise);
    }

    /* --- Main Content --- */
    main {
      display: flex;
      flex-direction: row;
      justify-content: space-around;
      align-items: center;
      gap: 20px;
    }

    h2 {
      font-weight: 600;
      margin-bottom: 40px;
      font-size: 2.3rem;
    }

    .question-block {
      margin-bottom: 40px;
    }

    .question-label {
      display: block;
      margin-bottom: 15px;
      font-size: 1.5rem;
      font-style: italic;
    }

    /* --- Counter --- */
    .counter-wrapper {
      width: fit-content;
    }
    
    /* Target the internal input field of the numericInput */
    .counter-wrapper input.form-control {
      border: 1px solid var(--secondary-gray);
      border-radius: 5px;
      box-shadow: none;
      text-align: center;
      font-weight: bold;
      font-size: 1.2rem;
      height: 50px;
      width: 80px;           /* Adjust width as needed */
      color: var(--primary-black);
    }

    /* --- Segmented Control (Likert) --- */
    .segmented-control {
      display: inline-flex;
      border: 1px solid var(--secondary-gray);
      width: 100%;
      max-width: 700px;
      border-radius: 5px;
    }
    
    /* Identify the buttons as custom-input-btn for JS logic */
    .segmented-option {
      flex: 1;
      background: none;
      border: none;
      border-right: 1px solid var(--secondary-gray);
      padding: 12px 5px;
      cursor: pointer;
      font-size: 1.3rem;
      transition: background-color 0.2s;
      text-align: center;
    }

    .segmented-option:last-child {
      border-right: none;
    }

    .segmented-option:hover {
      background-color: #f0f0f0;
    }

    .segmented-option.active {
      background-color: var(--accent-turquoise);
    }

    .segmented-control.small {
      max-width: 350px;
    }

    .navigation-btn {
      background: none;
      border: none;
      cursor: pointer;
      width: 60px;
      height: 60px;
      border: 1px solid var(--primary-black);
      border-radius: 50%;
      font-size: 2.5rem;
      display: flex;
      align-items: center;
      justify-content: center;
      box-shadow: 2px 2px 0px rgba(0, 0, 0, 0.1);
      min-width: 60px; /* prevent shrinking */
      transition: 0.1s ease-in-out;
    }
    
    .navigation-btn:hover {
      background: #aee6e0 !important;
    }
    
    .input-error {
      border: 2px solid #d62728 !important; /* Red Border */
      box-shadow: 0 0 8px rgba(214, 39, 40, 0.4) !important; /* Red Glow */
    }
    
    /* Hide Standard Shiny Notifications for cleaner look if desired */
    /* #shiny-notification-panel { display: none; } */
'

app_js <- "
  $(document).on('click', '.segmented-option', function() {
    // 1. Visual update
    $(this).siblings().removeClass('active');
    $(this).addClass('active');
    
    // 2. Send value to Shiny
    // We find the parent container's ID and send input$[parentID] = value
    var inputId = $(this).parent().attr('id');
    var val = $(this).attr('data-value');
    Shiny.setInputValue(inputId, val);
  });
  
   Shiny.addCustomMessageHandler('validate_input', function(data) {
    var id = data.id;
    var isValid = data.valid;
    var $el = $('#' + id);
    
    // Helper to toggle class
    var toggle = function($target) {
      if (!isValid) $target.addClass('input-error');
      else $target.removeClass('input-error');
    };
 
    // 1. Check if it is our custom Segmented Control
    if ($el.hasClass('segmented-control')) {
      toggle($el);
    } 
    // 2. Check if it is a Shiny SelectInput (Selectize)
    // The ID is on the hidden select, we style the sibling .selectize-control .selectize-input
    else {
      var $selectize = $el.next('.selectize-control').find('.selectize-input');
      if ($selectize.length > 0) toggle($selectize);
    }
  });
"

# 2. UI HELPER FUNCTION: The Page Wrapper
# This wraps every page so they all look identical without copying HTML
ui_page_template <- function(id, title, step_now, total_steps, 
                             prev_id = NULL, next_id = NULL, 
                             content_ui) {
  
  # Generate Progress Bars
  progress_html <- lapply(1:total_steps, function(i) {
    cls <- if(i <= step_now) "progress-step filled" else "progress-step"
    div(class = cls)
  })
  
  # Navigation Buttons
  btn_prev <- if(!is.null(prev_id)) actionButton(prev_id, "<", class = "navigation-btn") else div(style="width: 50px;")
  btn_next <- if(!is.null(next_id)) actionButton(next_id, ">", class = "navigation-btn") else div(style="width: 50px;")
  
  tags$div(
    class="wrapper",
    
    tags$header(
      div(class="logo-section", img(src = "assets/startupXboosterLogo.png", alt="Logo")),
      tags$h1("Startup Booster"),
      div(class="user-section", actionLink("btn_close", "✕", class="close-icon", style = "color: inherit !important; text-decoration: none !important;"))
    ),
    
    div(class = "progress-bar", progress_html),
    
    tags$main(
      btn_prev,
      div(class = "content-area",
          h2(title),
          content_ui
      ),
      btn_next
    )
  )
  
}

# 3. UI HELPER: Custom Likert/Select Input
# Automatically generates the button group
ui_segmented_control <- function(inputId, label, choices) {
  # choices should be a named list/vector: c("Label" = "value")
  
  options_html <- lapply(seq_along(choices), function(i) {
    lbl <- names(choices)[i]
    val <- choices[[i]]
    # Default to first option active or none (logic can be improved)
    cls <- "segmented-option" 
    tags$button(class = cls, `data-value` = val, lbl)
  })
  
  div(class = "question-block",
      tags$label(class = "question-label", paste("•", label)),
      div(id = inputId, class = "segmented-control", options_html)
  )
}

# 4. UI HELPER: Styled Standard Input
ui_standard_input <- function(input_obj, label) {
  div(class = "question-block",
      tags$label(class = "question-label", paste("•", label)),
      div(class = "custom-input-wrapper", input_obj)
  )
}

# 5. UI HELPER: Landing Page
ui_landing_page <- function(btn_id) {
  tagList(
    # Header with specific class for larger sizing
    tags$header(class = "landing-header",
                div(class="logo-section", 
                    img(src = "assets/startupXboosterLogo.png", alt="Rocket Logo")
                ),
                tags$h1("Startup Booster")
    ),
    
    # Main content centered
    tags$main(
      div(class = "landing-content",
          div(class = "landing-main-text", "Test your Potential Now"),
          div(class = "landing-sub-text", "What is holding you back?"),
          actionButton(btn_id, "Test", class = "btn-start")
      )
    )
  )
}
