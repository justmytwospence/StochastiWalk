library(shiny)
library(ggplot2)
library(forecast)

circleFun <- function(center = c(0, 0), diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

shinyServer(function(input, output) {
  
  timeseries <- reactive({
    ar <- c()
    if (input$p > 0) {
      ar <- c(ar, input$ar1)
    }
    if (input$p > 1) {
      ar <- c(ar, input$ar2)
    }
    if (input$p > 2) {
      ar <- c(ar, input$ar3)
    }
    ma <- c()
    if (input$q > 0) {
      ma <- c(ma, input$ma1)
    }
    if (input$q > 1) {
      ma <- c(ma, input$ma2)
    }
    if (input$q > 2) {
      ma <- c(ma, input$ma3)
    }
    
    innov <- rnorm(500, sd = 2)
    if (input$shock) {
      innov[250] <- 10
    }
    
    timeseries <- arima.sim(list(order = c(length(ar), as.numeric(input$d), length(ma)),
                                 ar = ar,
                                 ma = ma),
                            innov = innov,
                            n = 500)
  })
  
  model <- reactive({
    forecast(timeseries(), h = input$h)
  })
  
  output$ts <- renderPlot({
    if ((input$ar1 != 0 | input$ar2 != 0 | input$ar3 != 0) & 
          (input$ma1 != 0 | input$ma2 != 0 | input$ma3 != 0)) {
      color <- '#aa66cc'
    }
    else if (input$ar1 != 0 | input$ar2 != 0 | input$ar3 != 0) {
      color <- '#ff4444'
    } else if (input$ma1 != 0 | input$ma2 != 0 | input$ma3 != 0) {
      color <- '#0099cc'
    } else {
      color <- 'black'
    }
    
    df <- data.frame('Time' = as.numeric(time(timeseries())),
                     'Value' = as.numeric(timeseries()),
                     'Upper' = NA,
                     'Lower' = NA,
                     'Type' = 'Data')
    if (input$showmodel) {
      df.model <- data.frame('Time' = as.numeric(time(timeseries())),
                             'Value' = as.numeric(model()$fitted),
                             'Upper' = NA,
                             'Lower' = NA,
                             'Type' = 'Model')
      df <- rbind(df, df.model)
    }
    if (input$showforecast) {
      df.forecast <- data.frame('Time' = as.numeric(row.names(print(model()))),
                                'Value' = model()$mean,
                                'Upper' = model()$upper[,2],
                                'Lower' = model()$lower[,2],
                                'Type' = 'Forecast')
      df <- rbind(df, df.forecast)      
    }
    
    p <- ggplot(data = df,
                aes(x = Time,
                    y = Value,
                    group = Type)) + 
      geom_line(aes(colour = Type)) +
      xlab('Time') + 
      ylab(NULL) + 
      scale_colour_manual(values = c('Data' = 'black',
                                     'Model' = color,
                                     'Forecast' = '#33cc66')) +
      guides(colour = FALSE) +
      theme_bw()
    if (input$showforecast) {
      p <- p +
        geom_ribbon(aes(x = Time,
                        ymin = Lower,
                        ymax = Upper),
                    fill = '#33cc66',
                    alpha = .5)
    }
    
    print(p)
  })
  
  output$ur.ar <- renderPlot({
    if (input$d == 0) {
      roots <- polyroot(c(1 , -input$ar1, -input$ar2, -input$ar3))
    }
    if (input$d == 1) {
      roots <- polyroot(c(1, -(1 + input$ar1), -input$ar2, -input$ar3))
    }
    if (input$d == 2) {
      roots <- polyroot(c(1, -(1 + input$ar1), -(1 + input$ar2), -input$ar3))
    }
    roots.real <- sapply(roots, Re)
    roots.imaginary <- sapply(roots, Im)
    roots.df <- data.frame('Real' = roots.real,
                           'Imaginary' = roots.imaginary)
    dat <- circleFun(npoints = 100)
    p <- ggplot() + 
      geom_path(data = dat,
                aes(x, y),
                colour = '#ff4444') +
      xlim(c(-3, 3)) +
      ylim(c(-3, 3)) +
      ggtitle('AR Roots') +
      xlab('Real') +
      ylab('Imaginary') +
      theme_bw()
    if (input$ar1 != 0 | input$ar2 != 0 | input$ar3 != 0) {
      p <- p +
        geom_point(data = roots.df,
                   aes(Real,
                       Imaginary),
                   size = 3,
                   colour = '#ff4444')
    }
    print(p)
  })
  
  output$ur.ma <- renderPlot({
    if (input$d == 0) {
      roots <- polyroot(c(1 , input$ma1, input$ma2, input$ma3))
    }
    if (input$d == 1) {
      roots <- polyroot(c(1, (1 + input$ma1), input$ma2, input$ma3))
    }
    if (input$d == 2) {
      roots <- polyroot(c(1, (1 + input$ar1), (1 + input$ar2), input$ar3))
    }
    roots.real <- sapply(roots, Re)
    roots.imaginary <- sapply(roots, Im)
    roots.df <- data.frame('Real' = roots.real,
                           'Imaginary' = roots.imaginary)
    dat <- circleFun(npoints = 100)
    q <- ggplot() + 
      geom_path(data = dat,
                aes(x, y),
                colour = '#0099cc') +
      xlim(c(-3, 3)) +
      ylim(c(-3, 3)) +
      ggtitle('MA Roots') +
      xlab('Real') +
      ylab('Imaginary') +
      theme_bw()
    if (input$ma1 != 0 | input$ma2 != 0 | input$ma3 != 0) {
      q <- q +
        geom_point(data = roots.df,
                   aes(Real,
                       Imaginary),
                   size = 3,
                   colour = '#0099cc')
    }
    print(q)
  })
  
  output$acf.raw <- renderPlot({
    acf <- acf(timeseries())
    acf <- data.frame('Type' = 'Raw',
                      'Correlation' = acf$acf[-1],
                      'Lag' = acf$lag[-1])
    pacf <- pacf(timeseries())
    pacf <- data.frame('Type' = 'Partial',
                       'Correlation' = pacf$acf,
                       'Lag' = pacf$lag)
    corr.df <- rbind(acf, pacf)
    p <- ggplot(data = corr.df) + 
      geom_bar(aes(x = Lag,
                   y = Correlation,
                   fill = Type), 
               stat = 'identity',
               position = 'dodge') +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = c('#0099cc', '#ff4444')) +
      theme_bw() +
      theme(legend.position = 'bottom')
    print(p)
  })
  
  output$acf.compare <- renderPlot({
    acf <- acf(timeseries())
    acf <- data.frame('Type' = 'Raw',
                      'Correlation' = abs(acf$acf[-1]),
                      'Lag' = acf$lag[-1])
    pacf <- pacf(timeseries())
    pacf <- data.frame('Type' = 'Partial',
                       'Correlation' =  abs(pacf$acf),
                       'Lag' = pacf$lag)
    corr.df <- rbind(acf, pacf)
    p <- ggplot(data = corr.df) +
      geom_line(aes(x = Lag,
                    y = Correlation,
                    group = Type,
                    colour = Type),
                size = 2,
                alpha = .75) +
      scale_colour_manual(values = c('#0099cc', '#ff4444')) +
      ylab('|Correlation|') +
      theme_bw() +
      theme(legend.position = 'bottom')
    print(p)
  })
  
})