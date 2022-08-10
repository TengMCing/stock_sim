library(shiny)
library(tidyverse)

options(shiny.reactlog = TRUE)

# remotes::install_github("TengMCing/bandicoot")

# game_logic --------------------------------------------------------------

CLASS_GAME_INFO <- function(env = new.env(parent = parent.frame())) {

  bandicoot::new_class(env = env, class_name = "GAME_INFO")

  env$buy_warning <- FALSE
  env$sell_warning <- FALSE

  init_ <- function(init_day, init_balance, init_price) {
    self$day <- init_day
    self$balance <- init_balance
    self$init_history(init_price)
  }

  init_history_ <- function(init_price) {
    self$price <- init_price
    self$history <- self$price
    self$update_price(times = 30)
    self$deal_history <- tibble()
    self$amount <- 0
  }

  update_price_ <- function(times = 1) {
    for (i in 1:times) {
      self$price <- exp(rnorm(1, log(self$price) + 0.07/365, sd = 0.01))
      self$history <- c(self$history, self$price)
    }
  }

  reset_warnings_ <- function() {
    self$buy_warning <- FALSE
    self$sell_warning <- FALSE
  }

  buy_ <- function(amount = 100) {
    if (self$balance < self$price * amount) {self$buy_warning <- TRUE; return();}

    self$balance <- self$balance - self$price * amount
    self$amount <- self$amount + amount
    self$deal_history <- self$deal_history %>%
      bind_rows(tibble(day = self$day, mode = "buy", price = self$price, amount = amount, balance = self$balance))
    self$summarise_deal()
    self$reset_warnings()
  }

  sell_ <- function(amount = 100) {
    if (self$amount < amount) {self$sell_warning <- TRUE; return();}

    self$balance <- self$balance + self$price * amount
    self$amount <- self$amount - amount
    self$deal_history <- self$deal_history %>%
      bind_rows(tibble(day = self$day, mode = "sell", price = self$price, amount = amount, balance = self$balance))
    self$summarise_deal()
    self$reset_warnings()
  }

  summarise_deal_ <- function() {
    self$deal_overview <- self$deal_history %>%
      mutate(net_buy = ifelse(mode == "buy", amount, -amount)) %>%
      group_by(day) %>%
      summarise(price = first(price), amount = sum(net_buy), balance = last(balance)) %>%
      filter(amount != 0) %>%
      mutate(mode = ifelse(amount > 0, "buy", "sell")) %>%
      mutate(amount = abs(amount)) %>%
      arrange(desc(day))
  }

  move_ <- function(times = 1) {
    self$update_price(times = times)
    self$day <- self$day + times
  }

  bandicoot::register_method(env,
                             ..init.. = init_,
                             init_history = init_history_,
                             update_price = update_price_,
                             summarise_deal = summarise_deal_,
                             reset_warnings = reset_warnings_,
                             buy = buy_,
                             sell = sell_,
                             move = move_)

  return(env)
}


INIT_BALANCE <- 50000
INIT_PRICE <- 100
GAME_INFO <- CLASS_GAME_INFO()
game_info <- GAME_INFO$instantiate(init_day = 30, init_balance = INIT_BALANCE, init_price = INIT_PRICE)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(3,
           h1("Stock simulation", style = "color: #fcba03"),
           actionButton("buy", "BUY 100 STOCKS", width = 300, style = "background-color:#fd8d3c; color:white"),
           actionButton("sell", "SELL 100 STOCKS", width = 300, style = "background-color:#008CBA; color:white"),
           br(),
           br(),
           actionButton("move", "MOVE 1 DAY", width = 300, style = "background-color:#41ab5d; color:white"),
           actionButton("move7", "MOVE 7 DAYS", width = 300, style = "background-color:#238443; color:white"),
           actionButton("move28", "MOVE 28 DAYS", width = 300, style = "background-color:#005a32; color:white"),
           h3("Price: $", textOutput("price", inline = TRUE)),
           h3("Gain: $", textOutput("gain", inline = TRUE)),
           h3("Balance: $", textOutput("balance", inline = TRUE)),
           h3("Stocks: ", textOutput("amount", inline = TRUE)),
           br(),
           br(),
           h1(textOutput("warning", inline = TRUE), style = "color: red")
    ),
    column(9, plotOutput("history"))
  ),
  fluidRow(column(2), column(8, htmlOutput("record", inline = FALSE)), column(2))
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {

  event_buy <- reactive({input$buy; isolate(if (input$buy) game_info$buy())})
  event_sell <- reactive({input$sell; isolate(if (input$sell) game_info$sell())})

  event_move <- reactive({input$move; isolate(if (input$move) game_info$move())})
  event_move7 <- reactive({input$move7; isolate(if (input$move7) game_info$move(7))})
  event_move28 <- reactive({input$move28; isolate(if (input$move28) game_info$move(28))})

  output$price <- renderText({event_move(); event_move7(); event_move28()
    format(game_info$price, scientific = FALSE, digits = 6)})

  output$gain <- renderText({event_move(); event_move7(); event_move28()
    paste0(format(game_info$balance + game_info$amount * game_info$price - INIT_BALANCE, scientific = FALSE, digits = 2),
           "   (",
           format(((game_info$balance + game_info$amount * game_info$price)/INIT_BALANCE - 1)*100, scientific = FALSE, digits = 2),
           "%)")})

  output$balance <- renderText({event_buy(); event_sell()
    format(game_info$balance, scientific = FALSE)})

  output$amount <- renderText({event_buy(); event_sell()
    format(game_info$amount, scientific = FALSE)})

  output$warning <- renderText({
    event_buy(); event_sell()
    warning_out <- ""
    if (game_info$buy_warning) warning_out <- "Not enough money!"
    if (game_info$sell_warning) warning_out <- "Not enough stock!"
    warning_out
    })

  output$history <- renderPlot({
    event_move(); event_move7(); event_move28()

    p <- ggplot() +
      geom_line(aes(1:length(game_info$history), game_info$history)) +
      xlab("Day") +
      ylab("Price") +
      theme_light() +
      ggtitle(paste0("Day: ", game_info$day)) +
      scale_color_manual(values = c("buy" = "#fd8d3c", "sell" = "#008CBA"))

    if (!is.null(game_info$deal_overview) && nrow(game_info$deal_overview) > 0)
      p +
      geom_vline(data = game_info$deal_overview, aes(xintercept = day, col = mode), linetype = 2) +
      geom_text(data = game_info$deal_overview, aes(day, max(game_info$history), label = amount, col = mode))
    else
      p
  }, res = 96)

  output$record <- reactive({event_buy(); event_sell()
    if (!is.null(game_info$deal_overview) && nrow(game_info$deal_overview) > 0) {
      kableExtra::kable(game_info$deal_overview, format = "html") %>%
      kableExtra::kable_styling(bootstrap_options = c("hover", "bordered")) %>%
      kableExtra::row_spec(which(game_info$deal_overview$mode == "sell"), color = "white", background = "#008CBA") %>%
      kableExtra::row_spec(which(game_info$deal_overview$mode == "buy"), color = "white", background = "#fd8d3c")
      }
  })

}


# app ---------------------------------------------------------------------

shinyApp(ui, server)
