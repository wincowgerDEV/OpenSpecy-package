library(shiny)
library(shinyjs)

if (!dir.exists('www/')) {
    dir.create('www')
}

download.file(
    url = 'https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js',
    destfile = 'www/js.cookie.js'
)

addResourcePath("js", "www")

# This would usually come from your user database. But we want to keep it simple.
password_hash <- bcrypt::hashpw('secret123') # Never store passwords as clear text
sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo" # Our not so random sessionid


jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'

server <- function(input, output) {
    
    status <- reactiveVal(value = NULL)
    # check if a cookie is present and matching our super random sessionid  
    observe({
        js$getcookie()
        if (!is.null(input$jscookie) && input$jscookie == sessionid) {
            status(paste0('in with sessionid ', input$jscookie))
        }
        else {
            status('out')
        }
    })
    
    observeEvent(input$login, {
        if (input$username == 'admin' & bcrypt::checkpw(input$password, hash = password_hash)) {
            # generate a sessionid and store it in your database, but we keep it simple in this example...
            # sessionid <- paste(collapse = '', sample(x = c(letters, LETTERS, 0:9), size = 64, replace = TRUE))
            js$setcookie(sessionid)
        } else {
            status('out, cause you don\'t know the password secret123 for user admin.')
        }
    })
    
    observeEvent(input$logout, {
        status('out')
        js$rmcookie()
    })
    
    output$output <- renderText({
        paste0('You are logged ', status())}
    )
}

ui <- fluidPage(
    tags$head(
        # you must copy https://raw.githubusercontent.com/js-cookie/js-cookie/master/src/js.cookie.js to www/
        tags$script(src = "js/js.cookie.js")
    ),
    useShinyjs(),
    extendShinyjs(text = jsCode),
    sidebarLayout(
        sidebarPanel(
            textInput('username', 'User', placeholder = 'admin'),
            passwordInput('password', 'Password', placeholder = 'secret123'),
            actionButton('login', 'Login'),
            actionButton('logout', 'Logout')
        ),
        mainPanel(
            verbatimTextOutput('output')
        )
    )
)

shinyApp(ui = ui, server = server)
