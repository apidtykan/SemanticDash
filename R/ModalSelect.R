accordion_content <- list(
  list(title = "How do we measure duration", content = shiny.semantic::multiple_radio(input_id = "un", label = "duration", choices = c("secs", "mins", "hours","days", "weeks"),
                                                                      selected = "secs", position = "inline")),
  list(title = "What is the duration", content = numericInput(inputId = "up", label = "before:", 30, min = 1, max = 100))
)

ShipSelctUI <- function(id, selectype) {
  ns <- NS(id)
  tagList(
    div(class="ui grid",
        div(class="three column row",
          div(class="left floated column",
              div(class = "ui raised segment",
                  div(a(class="ui green ribbon label", "Ship"),
                      div(class="ui grid",
                          div(class="two column row",
                              div(class = "column",
                                  dropdown_input(ns("typ"), choices = selectype, 
                                                 default_text = "select ship type",  
                                                 type = "search selection")),
                              div(class = "column",
                                  dropdown_input(ns("nam"), choices = NULL,
                                                 default_text = "select ship name",
                                                 type = "search selection")
                                  )
                              ))))),
          div(class="right floated column",
              accordion(accordion_content , fluid = F, 
                        active_title = "How do we measure duration?", custom_style = "background: #fffddb;")
              )
        ))
  )
}

ShipSelct <- function(input, output, session, date) {
  
  observeEvent(input[['typ']], {
    update_dropdown_input(session, "nam", 
                          choices = date[ship_type == input[["typ"]], unique(SHIPNAME)], 
                          value = date[ship_type == input[["typ"]], unique(SHIPNAME)][1])
  })
  
  selec <- reactiveValues()
  selec$typ <- reactive(input[['typ']])
  selec$nam <- reactive(input[['nam']])
  
  return(selec)
}
  