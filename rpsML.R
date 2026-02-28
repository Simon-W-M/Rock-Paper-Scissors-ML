library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Rock Paper Scissors - ML Edition",
  sidebar = sidebar(
    h4("Make Your Choice"),
    actionButton("rock", "Rock 🪨", class = "btn-primary w-100 mb-2"),
    actionButton("paper", "Paper 📄", class = "btn-primary w-100 mb-2"),
    actionButton("scissors", "Scissors ✂️", class = "btn-primary w-100 mb-2"),
    hr(),
    actionButton("reset", "Reset Game", class = "btn-secondary w-100"),
    hr(),
    card(
      card_header("AI Strategy Info"),
      verbatimTextOutput("ml_info")
    )
  ),
  card(
    card_header("Game Result"),
    uiOutput("result_display")
  ),
  card(
    card_header("Score"),
    layout_columns(
      value_box(
        title = "Your Wins",
        value = textOutput("player_score"),
        showcase = icon("trophy"),
        theme = "success"
      ),
      value_box(
        title = "Ties",
        value = textOutput("tie_score"),
        showcase = icon("handshake"),
        theme = "info"
      ),
      value_box(
        title = "AI Wins",
        value = textOutput("computer_score"),
        showcase = icon("robot"),
        theme = "danger"
      )
    )
  ),
  card(
    card_header("Your Move Patterns - AI Learning"),
    plotOutput("prediction_plot", height = "300px")
  )
)

server <- function(input, output, session) {
  game_state <- reactiveVal(list(
    player_choice = NULL,
    computer_choice = NULL,
    result = NULL,
    player_wins = 0,
    computer_wins = 0,
    ties = 0,
    history = list(),
    transition_counts = matrix(0, nrow = 3, ncol = 3, 
                               dimnames = list(c("rock", "paper", "scissors"),
                                               c("rock", "paper", "scissors")))
  ))
  
  determine_winner <- function(player, computer) {
    if (player == computer) {
      return("tie")
    } else if (
      (player == "rock" && computer == "scissors") ||
      (player == "paper" && computer == "rock") ||
      (player == "scissors" && computer == "paper")
    ) {
      return("player")
    } else {
      return("computer")
    }
  }
  
  get_emoji <- function(choice) {
    switch(choice, "rock" = "🪨", "paper" = "📄", "scissors" = "✂️")
  }
  
  what_beats <- function(move) {
    switch(move, "rock" = "paper", "paper" = "scissors", "scissors" = "rock")
  }
  
  update_markov_model <- function(state, current_move) {
    history <- state$history
    if (length(history) >= 1) {
      last_move <- history[[length(history)]]$player_choice
      state$transition_counts[last_move, current_move] <- 
        state$transition_counts[last_move, current_move] + 1
    }
    return(state)
  }
  
  ml_predict_move <- function(state) {
    history <- state$history
    
    # Not enough data
    if (length(history) == 0) {
      return(list(move = "paper", confidence = "Low", method = "Opening"))
    }
    
    # Build predictions only if we have enough history
    predictions <- list()
    weights <- c()
    
    # 1. Markov Chain - requires at least 1 previous move
    if (length(history) >= 1) {
      last_move <- history[[length(history)]]$player_choice
      transition_probs <- state$transition_counts[last_move, ]
      if (sum(transition_probs) > 0) {
        transition_probs <- transition_probs / sum(transition_probs)
        markov_pred <- names(which.max(transition_probs))
        predictions$markov <- what_beats(markov_pred)
        weights <- c(weights, 0.35)
      }
    }
    
    # 2. Pattern Recognition - requires at least 4 moves
    if (length(history) >= 4) {
      recent_pattern <- paste(sapply(tail(history, 3), function(x) x$player_choice), collapse = "-")
      pattern_matches <- c()
      
      # Safely iterate through history
      for (i in 4:length(history)) {
        hist_pattern <- paste(sapply(history[(i-3):(i-1)], function(x) x$player_choice), collapse = "-")
        if (hist_pattern == recent_pattern && i < length(history)) {
          pattern_matches <- c(pattern_matches, history[[i]]$player_choice)
        }
      }
      
      if (length(pattern_matches) > 0) {
        pattern_pred <- names(sort(table(pattern_matches), decreasing = TRUE)[1])
        predictions$pattern <- what_beats(pattern_pred)
        weights <- c(weights, 0.25)
      }
    }
    
    # 3. Frequency Analysis - requires at least 1 move
    if (length(history) >= 1) {
      recent_moves <- sapply(tail(history, min(10, length(history))), function(x) x$player_choice)
      move_counts <- table(factor(recent_moves, levels = c("rock", "paper", "scissors")))
      most_frequent <- names(which.max(move_counts))
      predictions$frequency <- what_beats(most_frequent)
      weights <- c(weights, 0.20)
    }
    
    # 4. Win-Stay Lose-Shift Psychology - requires at least 1 move
    if (length(history) >= 1) {
      last_result <- history[[length(history)]]$result
      last_player_move <- history[[length(history)]]$player_choice
      
      if (last_result == "player") {
        predictions$psychology <- what_beats(last_player_move)
      } else if (last_result == "computer") {
        remaining_moves <- setdiff(c("rock", "paper", "scissors"), last_player_move)
        predictions$psychology <- sample(sapply(remaining_moves, what_beats), 1)
      } else {
        # Tie case
        predictions$psychology <- what_beats(what_beats(last_player_move))
      }
      weights <- c(weights, 0.20)
    }
    
    # Ensemble voting
    if (length(predictions) > 0) {
      all_preds <- unlist(predictions)
      vote_scores <- c(rock = 0, paper = 0, scissors = 0)
      for (i in seq_along(all_preds)) {
        vote_scores[all_preds[i]] <- vote_scores[all_preds[i]] + weights[i]
      }
      final_prediction <- names(which.max(vote_scores))
      confidence <- ifelse(length(history) < 10, "Medium", "High")
      return(list(move = final_prediction, confidence = confidence, method = "ML Ensemble"))
    }
    
    # Fallback
    return(list(move = "paper", confidence = "Low", method = "Default"))
  }
  
  play_game <- function(player_choice) {
    current_state <- game_state()
    
    # Get prediction BEFORE updating history
    prediction <- ml_predict_move(current_state)
    computer_choice <- prediction$move
    winner <- determine_winner(player_choice, computer_choice)
    
    # Update scores
    if (winner == "player") {
      current_state$player_wins <- current_state$player_wins + 1
    } else if (winner == "computer") {
      current_state$computer_wins <- current_state$computer_wins + 1
    } else {
      current_state$ties <- current_state$ties + 1
    }
    
    # Update Markov model
    current_state <- update_markov_model(current_state, player_choice)
    
    # Add to history AFTER prediction
    current_state$history <- c(current_state$history, list(list(
      player_choice = player_choice, 
      computer_choice = computer_choice,
      result = winner, 
      prediction_method = prediction$method, 
      confidence = prediction$confidence
    )))
    
    current_state$player_choice <- player_choice
    current_state$computer_choice <- computer_choice
    current_state$result <- winner
    current_state$last_prediction <- prediction
    
    game_state(current_state)
  }
  
  observeEvent(input$rock, { play_game("rock") })
  observeEvent(input$paper, { play_game("paper") })
  observeEvent(input$scissors, { play_game("scissors") })
  
  observeEvent(input$reset, {
    game_state(list(
      player_choice = NULL, computer_choice = NULL, result = NULL,
      player_wins = 0, computer_wins = 0, ties = 0, history = list(),
      transition_counts = matrix(0, nrow = 3, ncol = 3, 
                                 dimnames = list(c("rock", "paper", "scissors"),
                                                 c("rock", "paper", "scissors")))
    ))
  })
  
  output$result_display <- renderUI({
    state <- game_state()
    if (is.null(state$player_choice)) {
      return(div(style = "text-align: center; padding: 40px;",
                 h3("Choose rock, paper, or scissors to start playing!"),
                 p(style = "color: #666; margin-top: 20px;", 
                   "The AI uses machine learning to adapt to your playing patterns!")
      ))
    }
    
    result_text <- switch(state$result, "player" = "You Win! 🎉",
                          "computer" = "AI Wins! 🤖", "tie" = "It's a Tie! 🤝")
    result_color <- switch(state$result, "player" = "success", 
                           "computer" = "danger", "tie" = "info")
    
    div(style = "text-align: center; padding: 20px;",
        h2(class = paste0("text-", result_color), result_text), hr(),
        div(style = "font-size: 3em; margin: 20px 0;",
            span(get_emoji(state$player_choice)),
            span(" vs ", style = "font-size: 0.5em; vertical-align: middle;"),
            span(get_emoji(state$computer_choice))
        ),
        p(style = "font-size: 1.2em;",
          sprintf("You chose %s, AI chose %s", toupper(state$player_choice), toupper(state$computer_choice))
        )
    )
  })
  
  output$ml_info <- renderText({
    state <- game_state()
    if (length(state$history) == 0) {
      return("AI learning status:\nNo games played yet")
    }
    
    sprintf("Games played: %d\nConfidence: %s\nMethod: %s",
            length(state$history),
            if (!is.null(state$last_prediction)) state$last_prediction$confidence else "N/A",
            if (!is.null(state$last_prediction)) state$last_prediction$method else "N/A"
    )
  })
  
  output$player_score <- renderText({ game_state()$player_wins })
  output$computer_score <- renderText({ game_state()$computer_wins })
  output$tie_score <- renderText({ game_state()$ties })
  
  output$prediction_plot <- renderPlot({
    state <- game_state()
    if (length(state$history) < 3) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Not enough data yet")
      text(1, 1, "Play at least 3 games to see AI learning", cex = 1.2)
      return()
    }
    
    recent_moves <- sapply(tail(state$history, min(20, length(state$history))), 
                           function(x) x$player_choice)
    move_freq <- table(factor(recent_moves, levels = c("rock", "paper", "scissors")))
    
    # Create bar plot with colors
    colors <- c("rock" = "#ff6b6b", "paper" = "#4ecdc4", "scissors" = "#95e1d3")
    barplot(move_freq, 
            main = "Your Recent Move Distribution",
            ylab = "Frequency",
            xlab = "Move",
            col = colors,
            border = "white",
            las = 1)
    
    # Add Markov transition matrix visualization
    if (sum(state$transition_counts) > 0) {
      # Add text showing AI's learned patterns
      mtext(sprintf("Games analyzed: %d | AI adapting to your patterns", 
                    length(state$history)), 
            side = 3, line = 0.5, cex = 0.8, col = "gray40")
    }
  })
}

shinyApp(ui = ui, server = server)
