library(shiny)

sidebarPanel <- function (...) 
{
  div(class = "span3", tags$form(class = "well", ...))
}

shinyUI(pageWithSidebar(
  headerPanel("StochastiWalk"),
  sidebarPanel(
    gsub("label class=\"radio\"", "label class=\"radio inline\"",
         radioButtons('d',
                      'Order of Integration',
                      choices = list(
                        'None' = 0,
                        '1' = 1,
                        '2' = 2,
                        '3' = 3),
                      selected = 'None')),
    gsub("label class=\"radio\"", "label class=\"radio inline\"",
         radioButtons('p',
                      'Autoregressive Order',
                      choices = list(
                        'None' = 0,
                        '1' = 1,
                        '2' = 2,
                        '3' = 3),
                      selected = 'None')),
    conditionalPanel('input.p > 0',
                     sliderInput('ar1',
                                 'AR1',
                                 min = -2,
                                 max = 2,
                                 step = .1,
                                 value = 0)),
    conditionalPanel('input.p > 1',
                     sliderInput('ar2',
                                 'AR2',
                                 min = -2,
                                 max = 2,
                                 step = .1,
                                 value = 0)),
    conditionalPanel('input.p > 2',
                     sliderInput('ar3',
                                 'AR3',
                                 min = -2,
                                 max = 2,
                                 step = .1,
                                 value = 0)),
    gsub("label class=\"radio\"", "label class=\"radio inline\"",
         radioButtons('q',
                      'Moving Average Order',
                      choices = list(
                        'None' = 0,
                        '1' = 1,
                        '2' = 2,
                        '3' = 3),
                      selected = 'None')),
    conditionalPanel('input.q > 0',
                     sliderInput('ma1',
                                 'MA1',
                                 min = -2,
                                 max = 2,
                                 step = .1,
                                 value = 0)),
    conditionalPanel('input.q > 1',
                     sliderInput('ma2',
                                 'MA1',
                                 min = -2,
                                 max = 2,
                                 step = .1,
                                 value = 0)),
    conditionalPanel('input.q > 2',
                     sliderInput('ma3',
                                 'MA3',
                                 min = -2,
                                 max = 2,
                                 step = .1,
                                 value = 0))
  ),
  mainPanel(
    plotOutput('ts', height = 275),
    #gsub("class=\"tabbable\"", "class=\"tabbable tabs-left\"",
    tabsetPanel(
      tabPanel('Unit Roots',
               div(class = 'row-fluid',
                   div(class = 'span6',
                       plotOutput('ur.ar', height = 375, width = 375)
                   ),
                   div(class = 'span6',
                       plotOutput('ur.ma', height = 375, width = 375)
                   )
               )),
      tabPanel('Autocorrelation',
               div(class = 'row-fluid',
                   div(class = 'span6',
                       plotOutput('acf.raw')),
                   div(class = 'span6',
                       plotOutput('acf.compare'))
               )),
      tabPanel("About",
               p(
                 div(tags$p('Created by Spencer Boucher, MS candidate in data analytics.')),
                 div(tags$a(href = 'http://spencerboucher.com', 'spencerboucher.com'))))
  
    )#)
  )
))