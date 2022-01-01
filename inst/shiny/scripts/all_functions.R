# Data Related Functions ----
load_data <- function() {
    costs <- fread("data/costs.csv")
    donations <- fread("data/donations.csv")
    testdata <- raman_hdpe
    
    tweets <- c("https://twitter.com/EnviroMichaela/status/1471622640183959555",
                "https://twitter.com/OpenSpecy/status/1472361269093023744",
                "https://twitter.com/DSemensatto/status/1461038613903380484",
                "https://twitter.com/SETAC_plastics/status/1460738878101356544",
                "https://twitter.com/AliciaMateos_/status/1460197329760313344",
                "https://twitter.com/Irreverent_KUP/status/1454418069036568578",
                "https://twitter.com/PeterPuskic/status/1454267818166210561",
                "https://twitter.com/JannesJegminat/status/1427257468384681985",
                "https://twitter.com/pnwmicroplastic/status/1415730821730734080",
                "https://twitter.com/OpenSpecy/status/1408391168745000961",
                "https://twitter.com/ToMExApp/status/1399859256615079936",
                "https://twitter.com/kat_lasdin/status/1399576094622175241",
                "https://twitter.com/an_chem/status/1397621113421803521",
                "https://twitter.com/WarrierAnish/status/1395245636967014401",
                "https://twitter.com/EnviroMichaela/status/1395199312645300233",
                "https://twitter.com/SocAppSpec/status/1392883693027430400",
                "https://twitter.com/zsteinmetz_/status/1387677422028480512",
                "https://twitter.com/OpenSpecy/status/1382820319635775488",
                "https://twitter.com/zsteinmetz_/status/1377222029250822146",
                "https://twitter.com/OpenSpecy/status/1318214558549372928",
                "https://twitter.com/YokotaLimnoLab/status/1311069417892184065")
    
    goals <- tibble(
        Status =      c("Revolutionizing", 
                        "Thriving", 
                        "Maintaining", 
                        "Supporting", 
                        "Saving"),
        Description = c("A paid team that is pushing Open Specy closer to the ultimate goal of 100% accurate spectral identification and deep spectral diagnostics with a single click",
                        "A single paid staff person working to update and build the community and the tool",
                        "Maintenance costs and minor ad-hoc updates and bug fixes",
                        "Keeping the app online and essential maintenance",
                        "Long term storage only"),
        'Annual Need'  = c(">100,000$",
                           "10,000–100,000$",
                           "1,000–10,000$",
                           "100–1,000$",
                           "<100$")
    )
    # Check if spectral library is present and load
    test_lib <- class(tryCatch(check_lib(path = conf$library_path),
                               warning = function(w) {w}))
    
    if(any(test_lib == "warning")) get_lib(path = conf$library_path)
    
    spec_lib <- load_lib(path = conf$library_path)
    
    if(droptoken) {
        drop_auth(rdstoken = "data/droptoken.rds")
    }

    
    # Inject variables into the parent environment
    invisible(list2env(as.list(environment()), parent.frame()))
}

#UI related functions ----

render_tweet <- function(x){renderUI({
    div(style = "width: 100%",
        
        tagList(
            tags$blockquote(class = "twitter-tweet", 
                            tags$a(href = x)),
            tags$script('twttr.widgets.load(document.getElementById("tweet"));')
            )
        )
    })
}
    
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

inputUserid <- function(inputId, value="") {
    #   print(paste(inputId, "=", value))
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js",
                                        type="text/javascript"))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js",
                                        type="text/javascript"))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "userid", value=as.character(value),
                   type = "text", style = "display:none;")
    )
}

inputIp <- function(inputId, value=""){
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js",
                                        type="text/javascript"))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js",
                                        type="text/javascript"))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "ipaddr", value=as.character(value),
                   type = "text", style = "display:none;")
    )
}

css <- HTML(
    "body {
    color: #fff;
  }
  .nav-tabs > li[class=active] > a,
  .nav-tabs > li[class=active] > a:focus,
  .nav-tabs > li[class=active] > a:hover
  {
    background-color: #000;
  }"
)

# CSS for star
appCSS <-
    ".mandatory_star { color: red; }
    #loading_overlay {
      position: absolute;
      margin-top: 10%;
      background: #000000;
      opacity: 0.9;
      z-index: 100;
      left: 0;
      right: 0;
      height: 100%;
      text-align: center;
      color: #f7f7f9;
    }"

containerfunction <- function(...) {
    div(
        style = "padding:5rem",
        div(class = "jumbotron jumbotron-fluid",
            style = "border:solid #f7f7f9;background-color:rgba(0, 0, 0, 0.5)",
            align = "justify", ... ))
}

plotcontainerfunction <- function(...) {
    div(
        #style = "padding:0.1rem",
        div(class = "jumbotron jumbotron-fluid",
            style = "border:solid #f7f7f9;background-color:rgba(0, 0, 0, 0.5);padding:1rem",
            align = "justify",
            ...)
    )
}

columnformat <- function() {
    # 'background-color:rgba(0, 0, 0, 0.5);
    # padding-bottom: 2rem'
}

bodyformat <- function() {
    # 'background-color:rgba(0, 0, 0, 0.5);
    # padding-bottom: 2rem'
}

#linefunction <- function(...){
#  hr(style = "color:#f7f7f9", ...)
#}

version <- paste0("Open Specy v", packageVersion("OpenSpecy"))

citation <- HTML(
    paste(
        "Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De",
        "Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral",
        "Classification Needs an Open Source Community: Open Specy to the Rescue!”",
        "<i>Analytical Chemistry</i>, <b>93</b>(21), 7543–7548. doi:",
        "<a href='https://doi.org/10.1021/acs.analchem.1c00123'>10.1021/acs.analchem.1c00123</a>."
    )
)