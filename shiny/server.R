
# Load data
datBio <- read.csv("data/ewe_bio_results_SD_corrected_2502.csv", row.names = NULL, sep = ";")
datCth <- read.csv("data/ewe_catch_results_SD_corrected_2502.csv", row.names = NULL, sep = ";")
datNov <- read.csv("data/novelty_incl_total.csv", row.names = NULL, sep = ";", stringsAsFactors = FALSE)
datOpt <- read.csv("data/optimize_2401.csv", row.names = NULL, sep = ";")


# Define the server function
server <- function(input, output, session) {
  
  observe_helpers(help_dir = "data")
  
  ##### "ABOUT" TAB #####
  
  output$about_shortly <- renderUI({
    box(includeMarkdown("data/about_intro_short.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$about_continued <- renderUI({
    box(includeMarkdown("data/about_intro_continued.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$modelText <- renderUI({
    box(includeMarkdown("data/model_info.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$modelText2 <- renderUI({
    box(includeMarkdown("data/model_info2.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$mapText <- renderUI({
    box(includeMarkdown("data/map_info.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$about_bluewebs <- renderUI({
    box(includeMarkdown("data/about_bluewebs.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$aknowledgement <- renderUI({
    box(includeMarkdown("data/aknowledgement.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$intro <- renderImage({
    return(list(src = "data/intro.png", contentType = "image/png", width = 700))
  }, deleteFile = FALSE)
  
  output$model <- renderImage({
    return(list(src = "data/dss.png", contentType = "image/png", width = 700))
  }, deleteFile = FALSE)
  
  output$map <- renderImage({
    return(list(src = "data/map.png", contentType = "image/png"))
  }, deleteFile = FALSE)
  
  output$logos <- renderImage({
    return(list(src = "data/logos.png", contentType = "image/png", width = 725))
  }, deleteFile = FALSE)
  
  
  ##### "PREDICTED TIME SERIES" TAB ######
  
  output$timeseries_info <- renderUI({
    includeMarkdown("data/timeseries_info.md")
  })
  
  observeEvent(input$time_series_link, {
    toggle("timeseries_info2")
    output$timeseries_info2 <- renderUI({
      includeMarkdown("data/timeseries_info_hidden.md")
    })
    
    if (input$time_series_link %% 2 == 1) {
      newlabel <- "Show less."
    } else {
      newlabel <- "Show more."
    }
    updateActionButton(session, "time_series_link", label = newlabel)
  })
  
  ## BIOMASS VARIABLES
  
  # subsetting data for bio plots
  bio_subset <- reactive({
    a <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),] 
    return(a)
  })
  
  # Cod
  output$plotCod <- renderPlot(
    {
      if("plotCod" %in% input$bioVars){
        
        ggplot(bio_subset(), aes(x = Year, y = Cod)) +
          scale_y_continuous(limits=c(0,275), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             # increases expansion constant so that all the tick labels fit
                             expand = c(0.02, 0)) +
          ggtitle("Biomass of cod") +
          xlab("\nYear") +
          ylab("Spawning-stock biomass (*1000 tonnes)\n") +
          geom_hline(aes(yintercept = 96.5, col = "red")) +
          annotate("text", x = 2050, y = 106, label = "GES above this line", col = "red", size = 5) +
          geom_vline(xintercept=2014, col = "blue") +
          annotate(geom = "text", x = 2011, y = 60, label = "Calibration period", size = 4,
                   angle = 90) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                             title = element_text(size = 14),
                             axis.title.y = element_text(size = 13),
                             axis.text.y = element_text(size = 11),
                             axis.title.x = element_text(size = 13),
                             axis.text.x = element_text(size = 11),
                             legend.position = "none"
          ) +
          geom_line(stat = "identity") +
          geom_ribbon(aes(ymin = bio_subset()[,"Cod"] - bio_subset()[,"CodSD"], ymax = bio_subset()[,"Cod"] + bio_subset()[,"CodSD"]), 
                      linetype = 2, alpha = 0.3) +
          geom_ribbon(aes(ymin = bio_subset()[,"Cod"] - bio_subset()[,"CodSD_95"], ymax = bio_subset()[,"Cod"] + bio_subset()[,"CodSD_95"]), 
                      linetype = 2, alpha = 0.2)
      }
    }
  )  
  
  # Herring
  output$plotHer <- renderPlot(
    {
      if("plotHer" %in% input$bioVars){
        
        ggplot(bio_subset(), aes(x = Year, y = Herring)) +
          scale_y_continuous(limits=c(0,800), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             expand = c(0.02, 0)) +
          ggtitle("Biomass of herring") +
          xlab("\nYear") +
          ylab("Spawning-stock biomass (*1000 tonnes)\n") +
          geom_hline(aes(yintercept = 430, col = "red")) +
          annotate("text", x = 2040, y = 460, label = "GES above this line", col = "red", size = 5) +
          geom_vline(xintercept=2014, col = "blue") +
          annotate(geom = "text", x = 2011, y = 200, label = "Calibration period", size = 4,
                   angle = 90) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                             title = element_text(size = 14),
                             axis.title.y = element_text(size = 13),
                             axis.text.y = element_text(size = 11),
                             axis.title.x = element_text(size = 13),
                             axis.text.x = element_text(size = 11),
                             legend.position = "none"
          ) +
          geom_line(stat = "identity") +
          geom_ribbon(aes(ymin = bio_subset()[,"Herring"] - bio_subset()[,"HerringSD"], ymax = bio_subset()[,"Herring"] + bio_subset()[,"HerringSD"]), 
                      linetype = 2, alpha = 0.3) +
          geom_ribbon(aes(ymin = bio_subset()[,"Herring"] - bio_subset()[,"HerringSD_95"], ymax = bio_subset()[,"Herring"] + bio_subset()[,"HerringSD_95"]), 
                      linetype = 2, alpha = 0.2)
      }
    }
  )
  
  # Sprat
  output$plotSpr <- renderPlot(
    {
      if("plotSpr" %in% input$bioVars){
        
        ggplot(bio_subset(), aes(x = Year, y = Sprat)) +
          scale_y_continuous(limits=c(0,1150), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             expand = c(0.02, 0)) +
          ggtitle("Biomass of sprat") +
          xlab("\nYear") +
          ylab("Spawning-stock biomass (*1000 tonnes)\n") +
          geom_hline(aes(yintercept = 410, col = "red")) +
          annotate("text", x = 2040, y = 450, label = "GES above this line", col = "red", size = 5) +
          geom_vline(xintercept=2014, col = "blue") +
          annotate(geom = "text", x = 2011, y = 300, label = "Calibration period", size = 4,
                   angle = 90) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                             title = element_text(size = 14),
                             axis.title.y = element_text(size = 13),
                             axis.text.y = element_text(size = 11),
                             axis.title.x = element_text(size = 13),
                             axis.text.x = element_text(size = 11),
                             legend.position = "none"
          ) +
          geom_line(stat = "identity") +
          geom_ribbon(aes(ymin = bio_subset()[,"Sprat"] - bio_subset()[,"SpratDS"], ymax = bio_subset()[,"Sprat"] + bio_subset()[,"SpratDS"]), 
                      linetype = 2, alpha = 0.3) +
          geom_ribbon(aes(ymin = bio_subset()[,"Sprat"] - bio_subset()[,"SpratDS_95"], ymax = bio_subset()[,"Sprat"] + bio_subset()[,"SpratDS_95"]), 
                      linetype = 2, alpha = 0.2)
      }
    }
  )
  
  ## CATCH VARIABLES
  
  # subsetting data for catch plots
  cth_subset <- reactive({
    a <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),] 
    return(a)
  })
  
  
  # Cod
  output$plotCodCatch <- renderPlot(
    {
      if("plotCodCatch" %in% input$catchVars){
        
        ggplot(cth_subset(), aes(x = Year, y = CodCatch)) +
          scale_y_continuous(limits=c(0,85), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             # increases expansion constant so that all the tick labels fit
                             expand = c(0.02, 0)) +
          ggtitle("Cod catch") +
          xlab("\nYear") +
          ylab("Catch (*1000 tonnes)\n") +
          geom_vline(xintercept=2014, col = "blue") +
          annotate(geom = "text", x = 2011, y = 20, label = "Calibration period", size = 4,
                   angle = 90) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                             title = element_text(size = 14),
                             axis.title.y = element_text(size = 13),
                             axis.text.y = element_text(size = 11),
                             axis.title.x = element_text(size = 13),
                             axis.text.x = element_text(size = 11),
                             legend.position = "none"
          ) +
          geom_line(stat = "identity") +
          geom_ribbon(aes(ymin = cth_subset()[,"CodCatch"] - cth_subset()[,"CodSD"], ymax = cth_subset()[,"CodCatch"] + cth_subset()[,"CodSD"]), 
                      linetype = 2, alpha = 0.3) +
          geom_ribbon(aes(ymin = cth_subset()[,"CodCatch"] - cth_subset()[,"CodSD_95"], ymax = cth_subset()[,"CodCatch"] + cth_subset()[,"CodSD_95"]), 
                      linetype = 2, alpha = 0.2) 
      }
    }
  )  
  
  # Herring
  output$plotHerCatch <- renderPlot(
    {
      if("plotHerCatch" %in% input$catchVars){
        
        ggplot(cth_subset(), aes(x = Year, y = HerringCatch)) +
          scale_y_continuous(limits=c(0,250), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             # increases expansion constant so that all the tick labels fit
                             expand = c(0.02, 0)) +
          ggtitle("Herring catch") +
          xlab("\nYear") +
          ylab("Catch (*1000 tonnes)\n") +
          geom_vline(xintercept=2014, col = "blue") +
          annotate(geom = "text", x = 2011, y = 60, label = "Calibration period", size = 4,
                   angle = 90) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                             title = element_text(size = 14),
                             axis.title.y = element_text(size = 13),
                             axis.text.y = element_text(size = 11),
                             axis.title.x = element_text(size = 13),
                             axis.text.x = element_text(size = 11),
                             legend.position = "none"
          ) +
          geom_line(stat = "identity") +
          geom_ribbon(aes(ymin = cth_subset()[,"HerringCatch"] - cth_subset()[,"HerringSD"], ymax = cth_subset()[,"HerringCatch"] + cth_subset()[,"HerringSD"]), 
                      linetype = 2, alpha = 0.3) +
          geom_ribbon(aes(ymin = cth_subset()[,"HerringCatch"] - cth_subset()[,"HerringSD_95"], ymax = cth_subset()[,"HerringCatch"] + cth_subset()[,"HerringSD_95"]), 
                      linetype = 2, alpha = 0.2)
      }
    }
  )
  
  # Sprat
  output$plotSprCatch <- renderPlot(
    {
      if("plotSprCatch" %in% input$catchVars){
        
        ggplot(cth_subset(), aes(x = Year, y = SpratCatch)) +
          scale_y_continuous(limits=c(0,400), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             # increases expansion constant so that all the tick labels fit
                             expand = c(0.02, 0)) +
          ggtitle("Sprat catch") +
          xlab("\nYear") +
          ylab("Catch (*1000 tonnes)\n") +
          geom_vline(xintercept=2014, col = "blue") +
          annotate(geom = "text", x = 2011, y = 100, label = "Calibration period", size = 4,
                   angle = 90) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                             title = element_text(size = 14),
                             axis.title.y = element_text(size = 13),
                             axis.text.y = element_text(size = 11),
                             axis.title.x = element_text(size = 13),
                             axis.text.x = element_text(size = 11),
                             legend.position = "none"
          ) +
          geom_line(stat = "identity") +
          geom_ribbon(aes(ymin = cth_subset()[,"SpratCatch"] - cth_subset()[,"SpratDS"], ymax = cth_subset()[,"SpratCatch"] + cth_subset()[,"SpratDS"]), 
                      linetype = 2, alpha = 0.3) +
          geom_ribbon(aes(ymin = cth_subset()[,"SpratCatch"] - cth_subset()[,"SpratDS_95"], ymax = cth_subset()[,"SpratCatch"] + cth_subset()[,"SpratDS_95"]), 
                      linetype = 2, alpha = 0.2)
      }
    }
  )
  
  # Render Biomass variable time-series
  output$bio_plot_list <- renderUI(
    {
      bio_plot_output_list <- lapply(
        input$bioVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13, solidHeader = TRUE))
        }
      )
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, bio_plot_output_list)
    }
  )
  
  # Render Catch variable time-series  
  output$catch_plot_list <- renderUI(
    {
      catch_plot_output_list <- lapply(
        input$catchVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13, solidHeader = TRUE))
        }
      )
      do.call(tagList, catch_plot_output_list)
    }
  )
  
  
  ##### "NOVELTY TAB" #####
  
  output$novel_info <- renderUI({
      box(includeMarkdown("data/novelty_info.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  # subset the data for novelty plots
  nov_subset <- reactive({
    a <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),]
    return(a)
  })
  
    nov_var <- reactive({
      melted <- melt(nov_subset(), id.vars = "Year", measure.vars = c("codRV", "T_050_MarchMay", "Aug060mT", "notHypoxicA"))
      a <- melted[melted$variable %in% input$novelVars,]
      return(a)
    }) 
    
    output$novel_plot <- renderPlot({
      
      ggplot(nov_var(), aes(x = Year, y = value, col = variable)) +
        scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) +
        scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5),
                           expand = c(0.02, 0)) +
        ggtitle("Novelty variables") +
        xlab("\nYear") +
        ylab("Novelty\n") +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                                 title = element_text(size = 14),
                                 axis.title.y = element_text(size = 13),
                                 axis.text.y = element_text(size = 11),
                                 axis.title.x = element_text(size = 13),
                                 axis.text.x = element_text(size = 11),
                                 legend.text=element_text(size=11),
                                 legend.position = c(0.15,0.75)
        ) +
        geom_line(stat = "identity", size = 1) + 
        scale_colour_manual(name="Variables",
                            labels=c("codRV" = "Cod reproductive volume","T_050_MarchMay" = "Spring temperature","Aug060mT" = "Summer temperature","notHypoxicA" = "Inverse hypoxic area"),
                            values = c("codRV" = "navyblue", "T_050_MarchMay" = "darkgreen", "Aug060mT" = "darkmagenta", "notHypoxicA" = "orangered3"))
    })
  
  output$plotTotal <- renderPlot(
    {
      ggplot(nov_subset(), aes(x = Year, y = Abiotic_novelty)) +
        scale_y_continuous(limits=c(0,2.7), breaks = scales::pretty_breaks(n = 5)) +
        scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5),
                           expand = c(0.02, 0)) +
        ggtitle("Total abiotic novelty") +
        xlab("\nYear") +
        ylab("Novelty\n") +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                           title = element_text(size = 14),
                           axis.title.y = element_text(size = 13),
                           axis.text.y = element_text(size = 11),
                           axis.title.x = element_text(size = 13),
                           axis.text.x = element_text(size = 11)
        ) +
        geom_line(stat = "identity", size = 1)
    }
  )

  output$novel_plot_total <- renderUI({
    # only make room for this plot in the app if it's selected
    if(input$novelTotal == TRUE) {
      # wrapping elements inside a fluidRow function extends the white space of the main panel accordingly
          box(plotOutput("plotTotal", height = 300), width = 12, solidHeader = TRUE)
    }
  })
  
  output$aboutNovelty <- renderUI({
    box(includeMarkdown("data/about_novelty.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$aboutNovelty_fig <- renderUI({
    box(includeMarkdown("data/about_novelty_fig.md"), width = 12, solidHeader = TRUE, status = "primary")
  })
  
  output$noveltyci <- renderImage({
    return(list(src = "data/novelty.png", contentType = "image/png", width = 400))
  }, deleteFile = FALSE)
  
  ##### "OPTIMIZE" TAB #####

  # subsetting data for the optimizing plots
  opt_subset <- reactive({
    a <- datOpt[(datOpt$F_GES == input$F_GES & datOpt$Nutr_GES == input$Nutr_GES & datOpt$Min_acc_profit == input$Profit & datOpt$Novelty == input$Novelty & datOpt$Ref_point == input$Ref_point),]
    return(a)
  })
  
  # generate dynamic text
  output$opt_scens <- renderUI({

    # extract row with highest probability fishery scenario
    fish <- opt_subset()[which.max(opt_subset()$F_scen),]

    # format fishery policy
    fish_s <- ""

    if (fish$F_labels == "Sus") {
      fish_s <- "Sustainable"
    }
    else if (fish$F_labels == "Pel") {
      fish_s <- "Pelagics-Focused"
    } else {
      fish_s <- "Open Access"
    }

    # format the probability of fishery policy
    fish_p <- percent(fish$F_scen)
    
    title <- "<h3>How to reach your goal?</h3> <br>"

    a <- "To reach your management goal, the "
    b <- " fishery policy should be applied as it has the highest chance ("
    c <- ") of being the best option. <br><br>"

    section1 <- HTML(paste0(title, a, "<b>", fish_s, "</b>", b, "<b>", fish_p, "</b>", c))

    # extract row with highest probability nutrient scenario
    nutr <- opt_subset()[which.max(opt_subset()$Nutr_scen),]

    # format nutrient policy
    nutr_s <- ""

    if (nutr$Nutr_labels == "BSAP") {
      nutr_s <- "Baltic Sea Action Plan (BSAP)"
    }
    else {
      nutr_s <- "Reference conditions"
    }

    # format the probability of nutrient policy
    nutr_p <- percent(nutr$Nutr_scen)

    d <- "The nutrient loading policy according to the "
    e <- " ("
    f <- ") should be applied and combined with the fishery policy. <br><br>"

    section2 <- HTML(paste0(d, "<b>", nutr_s, "</b>", e, "<b>", nutr_p, "</b>", f))

    # extract row with highest probability climate scenario
    clim <- opt_subset()[which.max(opt_subset()$Clim_scen),]

    # format climate scenario
    clim_s <- gsub("RCP", "RCP ", clim$Clim_labels)

    # format the probability of climate scenario
    clim_p <- percent(clim$Clim_scen)

    g <- "Applying these management policies will most likely lead to the preferred outcome given that the climate change scenario of "
    h <- " is also in effect ("
    i <- "). <br><br>"

    section3 <- HTML(paste0(g, "<b>", clim_s, "</b>", h, "<b>", clim_p, "</b>", i))

    # extract row with highest probability decade
    dec <- opt_subset()[which.max(opt_subset()$Decade),]

    # format decade
    dec_s <- gsub("_", "-", dec$Dec_labels)

    # format the probability of decade
    dec_p <- percent(dec$Decade)

    j <- "The decision support system covers four decades. With the combination of management and climate change scenarios outlined above, the preferred outcome would most likely be reached in "
    k <- " ("
    l <- "). <br><br>"

    post <- "Take a closer look at the probability distributions of different management and climate change scenario options below."

    section4 <- HTML(paste0(j, "<b>", dec_s, "</b>", k, "<b>", dec_p, "</b>", l, post))

    wholeThing <- HTML(paste(section1, section2, section3, section4))
    box(wholeThing, width = 12, solidHeader = TRUE, status = "primary")
  })
  
  # draw pie charts
  opt_fish <- reactive({	
    p <- ggplot(opt_subset()[1:3,], aes(x = 1, y = F_scen, fill = F_labels)) +	
      ggtitle("Fishery Policy Scenario") +
      scale_fill_discrete("Fishery policy", labels = c("Open Access", "Pelagics-Focused", "Sustainable")) +
      theme_void() + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                           title = element_text(size = 14)) +
      geom_bar(stat = "identity", color = "white")	 +
      geom_text(data=subset(opt_subset()[1:3,], percent(round(F_scen, 2)) != "0%" & percent(round(F_scen, 2)) != "0.0%"),
                aes(label = percent(signif(F_scen, 2))), position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") 
    
    return(p)
  })
  
  opt_clim <- reactive({	
    p <- ggplot(opt_subset()[1:2,], aes(x = 1, y = Clim_scen, fill = Clim_labels)) +
      scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) + 	
      ggtitle("Climate Change Scenario") +	
      scale_fill_discrete("Climate change scenario", labels = c("RCP 4.5", "RCP 8.5")) +
      theme_void() + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                           title = element_text(size = 14)) +
      geom_bar(stat = "identity", color = "white")	 +
      geom_text(data=subset(opt_subset()[1:2,], percent(round(Clim_scen, 2)) != "0%" & percent(round(Clim_scen, 2)) != "0.0%"),
                aes(label = percent(signif(Clim_scen, 2))), position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") 
    
    return(p)
  })
  
  opt_nutr <- reactive({	
    p <- ggplot(opt_subset()[1:2,], aes(x = 1, y = Nutr_scen, fill = Nutr_labels)) + 	
      ggtitle("Nutrient Loading Policy Scenario") +	
      scale_fill_discrete("Nutrient loading policy", labels = c("Baltic Sea Action Plan", "Reference conditions")) +
      theme_void() + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                           title = element_text(size = 14)) +
      geom_bar(stat = "identity", color = "white")	 +
      geom_text(data=subset(opt_subset()[1:2,], percent(round(Nutr_scen, 2)) != "0%" & percent(round(Nutr_scen, 2)) != "0.0%"),
                aes(label = percent(signif(Nutr_scen, 2))), position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") 
    
    return(p)
  })
  
  opt_dec <- reactive({	
    # format legend labels
    dec_labels <- gsub("_", "-", opt_subset()$Dec_labels)
    
    p <- ggplot(opt_subset(), aes(x = 1, y = Decade, fill = Dec_labels)) + 	
      ggtitle("Decade") +
      scale_fill_discrete("Decade", labels = dec_labels) +
      theme_void() + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14),
                           title = element_text(size = 14)) +
      geom_bar(stat = "identity", color = "white")	 +
      geom_text(data=subset(opt_subset(), percent(round(Decade, 2)) != "0%" & percent(round(Decade, 2)) != "0.0%"),
                aes(label = percent(signif(Decade, 2))), position = position_stack(vjust = 0.5))+
      coord_polar(theta = "y") 
    
    return (p)
  })

  output$opt_plots <- renderPlot({
      p1 <- opt_fish()
      p2 <- opt_nutr()
      p3 <- opt_clim()
      p4 <- opt_dec()
      wrap_plots(p1, p2, p3, p4)
  })
 
}