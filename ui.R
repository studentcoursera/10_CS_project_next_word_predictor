# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
library(markdown)

shinyUI(navbarPage(
    theme = "bootstrap.css",
    # app icon =================================================================
    HTML(
        " <div align='center'> <img src='nwp3.jpg', align='left', width=240, height=36></img></div>"
    ),
    
    tabPanel(
        "App",
        #tags$head(tags$script(src = "message-handler.js")),
        singleton(tags$head(tags$script(src = "message-handler.js"))),
        sidebarPanel(
            "",
            # help text ========================================================
            helpText("Enter a phrase and click Predict;", br(),
                     "to get the next word."),
            
            # style tags =======================================================
            tags$style(
                type = 'text/css',
                '#theword {background-color: rgba(255,255,0,0.10); color: green;}'
            ),
            tags$style(type = 'text/css', '#firstword, #secword, #thirdword { color: gray; font-size:12px}'),
            tags$style(type = 'text/css', '#pipe1, #pipe2 { color: orange;}'),
            tags$style(type = 'text/css', '{ font-size: 10;}'),
            
            # alert ============================================================
            bsAlert("alert_anchor"),
            
            HTML(
                "<span style='color:brown;'> [ options: twitter & google; for fun ]</span>"
            ),
            
            # default/twitter/google ===========================================
            radioButtons(
                "inRadio",
                "source :",
                inline = T,
                c(
                    "default" = "default",
                    "twitter" = 0,
                    "google"  = 1
                )
            ),
            
            # phrase & predict =================================================
            HTML("<center>"),
            h3("Phrase ..."),
            tags$textarea(
                id = "srch_text",
                rows = 5,
                cols = 27,
                maxlength = "150",
                ""
            ),
            HTML("<br><sup>[NOTE: punctuations will be ignored]</sup>"),
            submitButton("Predict", icon = icon("lightbulb-o", "fa-2x fa-pull-left")),
            #actionButton(
            #    "Predict", "Predict",
            #    icon = icon("lightbulb-o", "fa-2x fa-pull-left")
            #),
            HTML("</center><hr></hr>"),
            
            # outputs ==========================================================
            #verbatimTextOutput("theword"), ## this does not wraps text in chrome and firefox
            ## so resorting to htmlOutput ...
            htmlOutput("theword"),
            br(),
            column(3, textOutput("firstword")),
            column(1, textOutput("pipe1")),
            column(3, textOutput("secword")),
            column(1, textOutput("pipe2")),
            column(3, textOutput("thirdword")),
            
            br()
        ),
        
        mainPanel(
            # slider ===========================================================
            sliderInput(
                "words",
                label = "Number of words on the plot",
                min = 1,
                max = 50,
                value = 30
            ),
            
            # plot =============================================================
            textOutput("text1"),
            plotOutput("distPlot")
            
        ),
        column(12, hr(),
        column(6,
        h4("5 Tweets:"),
        "1. Avalanna Routh, a 6-year-old super belieber passes away from a rare form of ", br(),
        "2. Paul Walker’s death is confirmed through his own official ", br(),
        "3. After Cory Monteith’s untimely death (Glee), Lea Michele tweeted in memory of her ", br(),
        "4. Shortly after being re-elected to Presidency for his second term, Obama leaves a mark on social ", br(), 
        "5. Ellen brakes the record (by far) during the Oscars ceremony, with a world-renowned"),
        column(6,h4("5 news:"),
        "1. The senator says he’ll back his party’s presumptive ", br(),
        "2. Memorial Day on the streets, and the violence that has engulfed families and  ", br(),
        "3. European governments say the war in Afghanistan is over, but Afghans are leaving in record  ", br(),
        "4. A hero to some and a villain to others, Francesca Immacolata Chaouqui stands accused with two others of leaking  ", br(),
        "5. 'The President,' a reality show, allowed Palestinians to choose leader, which they have not had the chance to do in more than a ")
)


    ),
    tabPanel(
        "Info",
        sidebarPanel(
            HTML(
                "<h2>About <img src='icon_nwp3.jpg', align='right',
                width=100, height=50></img></h2>"
            ),
            hr(),
            # about ============================================================
            includeMarkdown("about_nwp.md")
            #,HTML("<img src='main.jpg'align='left', width=200></img>")
            ),
        mainPanel(# more info ========================================================
                  includeMarkdown("info_nwp.md"))
    ),
    tabPanel(
        "More Details",
        sidebarPanel(# slides ===========================================================
                     includeMarkdown("slide_more_info_nwp.md")),
        mainPanel(# more info ========================================================
                  includeMarkdown("more_details_nwp.md"))
    )
))
