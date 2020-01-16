
# Load data
datBio <- read.csv("D:/Github/shinyapp/EwE_bio_results_corrected0412.csv", row.names = NULL, sep = ";")
datCth <- read.csv("D:/Github/shinyapp/EwE_catch_results_corrected0412.csv", row.names = NULL, sep = ";")
datNov <- read.csv("D:/Github/shinyapp/Novelty_incl_total.csv", row.names = NULL, sep = ";", stringsAsFactors = FALSE)
datOpt <- read.csv("D:/Github/shinyapp/DSS_Bottom_up_3x3.csv", row.names = NULL, sep = ";")


# Define the server function
server <- function(input, output) {
  
  ##### "ABOUT" TAB #####
  
  output$aboutText <- renderUI({
    rawText <- readLines("D:/Github/shinyapp/about.txt")
    # split the text into a list of character vectors containing one line
    splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    # wrap a paragraph tag around each element in the list
    replacedText <- lapply(splitText, p)
    return(replacedText)
  })
  
  output$modelText <- renderUI({
    rawText <- readLines("D:/Github/shinyapp/model.txt")
    splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    replacedText <- lapply(splitText, p)
    return(replacedText)
  })
  
  output$modelText2 <- renderUI({
    rawText <- readLines("D:/Github/shinyapp/model2.txt")
    splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    replacedText <- lapply(splitText, p)
    return(replacedText)
  })
  
  output$mapText <- renderUI({
    rawText <- readLines("D:/Github/shinyapp/map.txt")
    splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    replacedText <- lapply(splitText, p)
    return(replacedText)
  })
  
  output$model <- renderImage({
    return(list(src = "D:/Github/shinyapp/model.PNG", contentType = "image/png"))
  }, deleteFile = FALSE)
  
  output$DSS <- renderImage({
    return(list(src = "D:/Github/shinyapp/map.PNG", contentType = "image/png"))
  }, deleteFile = FALSE)
  
  
  
  ##### "PREDICTED TIME SERIES" TAB ######
  
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
          scale_y_continuous(limits=c(0,250), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             # increases expansion constant so that all the tick labels fit
                             expand = c(0.07, 0)) +
          ggtitle("Biomass of cod") +
          xlab("\nYear") +
          ylab("Spawning-stock biomass\n") +
          geom_hline(aes(yintercept = 96.5, col = "red")) +
          annotate("text", x = 2040, y = 104, label = "GES above this line", col = "red", size = 5) +
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
                             expand = c(0.07, 0)) +
          ggtitle("Biomass of herring") +
          xlab("\nYear") +
          ylab("Spawning-stock biomass\n") +
          geom_hline(aes(yintercept = 430, col = "red")) +
          annotate("text", x = 2040, y = 460, label = "GES above this line", col = "red", size = 5) +
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
                             expand = c(0.07, 0)) +
          ggtitle("Biomass of sprat") +
          xlab("\nYear") +
          ylab("Spawning-stock biomass\n") +
          geom_hline(aes(yintercept = 410, col = "red")) +
          annotate("text", x = 2040, y = 445, label = "GES above this line", col = "red", size = 5) +
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
                      linetype = 2, alpha = 0.2)
      }
    }
  )
  
  # Zooplankton
  output$plotZoo <- renderPlot(
    {
      if("plotZoo" %in% input$bioVars){
        
        ggplot(bio_subset(), aes(x = Year, y = Zooplankton)) +
          scale_y_continuous(limits=c(0,12.5), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             expand = c(0.07, 0)) +
          ggtitle("Biomass of zooplankton") +
          xlab("\nYear") +
          ylab("Biomass\n") +
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
          geom_ribbon(aes(ymin = bio_subset()[,"Zooplankton"] - bio_subset()[,"ZooplanktonSD"], ymax = bio_subset()[,"Zooplankton"] + bio_subset()[,"ZooplanktonSD"]), 
                      linetype = 2, alpha = 0.2)
      }
    }
  )
  
  #Phytoplankton
  output$plotPhy <- renderPlot(
    {
      if("plotPhy" %in% input$bioVars){
        
        ggplot(bio_subset(), aes(x = Year, y = Phytoplankton)) +
          scale_y_continuous(limits=c(0,13), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             expand = c(0.07, 0)) +
          ggtitle("Biomass of phytoplankton") +
          xlab("\nYear") +
          ylab("Biomass\n") +
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
          geom_ribbon(aes(ymin = bio_subset()[,"Phytoplankton"] - bio_subset()[,"PhytoplanktonSD"], ymax = bio_subset()[,"Phytoplankton"] + bio_subset()[,"PhytoplanktonSD"]), 
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
          scale_y_continuous(limits=c(0,80), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2096), breaks = scales::pretty_breaks(n = 5),
                             # increases expansion constant so that all the tick labels fit
                             expand = c(0.07, 0)) +
          ggtitle("Cod catch") +
          xlab("\nYear") +
          ylab("Catch\n") +
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
                             expand = c(0.07, 0)) +
          ggtitle("Herring catch") +
          xlab("\nYear") +
          ylab("Catch\n") +
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
                             expand = c(0.07, 0)) +
          ggtitle("Sprat catch") +
          xlab("\nYear") +
          ylab("Catch\n") +
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
      rawText <- readLines("D:/Github/shinyapp/novelty.txt")
      # split the text into a list of character vectors containing one line
      splitText <- stringi::stri_split(str = rawText, regex = '\\n')
      # wrap a paragraph tag around each element in the list
      replacedText <- lapply(splitText, p)
      box(replacedText, width = 12, solidHeader = TRUE)
  })

  # subset the data for novelty plots
  nov_subset <- reactive({
    a <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),]
    return(a)
  })
  
  output$plotTotal <- renderPlot(
    {
      ggplot(nov_subset(), aes(x = Year, y = Abiotic_novelty)) +
        scale_y_continuous(limits=c(0,2.7), breaks = scales::pretty_breaks(n = 5)) +
        scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5)) +
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
        geom_line(stat = "identity")
    }
  )
  
  output$plotRv <- renderPlot(
    {
      if("plotRv" %in% input$novelVars){
        
        ggplot(nov_subset(), aes(x = Year, y = codRV)) +
          scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5)) +
          ggtitle("Novelty for cod reproductive volume") +
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
          geom_line(stat = "identity")
      }
    }
  )
  
  output$plotTemp1 <- renderPlot(
    {
      if("plotTemp1" %in% input$novelVars){
        
        ggplot(nov_subset(), aes(x = Year, y = T_050_MarchMay)) +
          scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5)) +
          ggtitle("Novelty for 0-50m water temperature") +
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
          geom_line(stat = "identity")
      }
    }
  )
  
  output$plotTemp2 <- renderPlot(
    {
      if("plotTemp2" %in% input$novelVars){
        
        ggplot(nov_subset(), aes(x = Year, y = Aug060mT)) +
          scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5)) +
          ggtitle("Novelty for 0-60m water temperature") +
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
          geom_line(stat = "identity")
      }
    }
  )
  
  output$plotHyp <- renderPlot(
    {
      if("plotHyp" %in% input$novelVars){
        
        ggplot(nov_subset(), aes(x = Year, y = notHypoxicA)) +
          scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) +
          scale_x_continuous(limits=c(2004,2098), breaks = scales::pretty_breaks(n = 5)) +
          ggtitle("Novelty for inverse hypoxic area") +
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
          geom_line(stat = "identity")
      }
    }
  )
  
  # Render Novelty variable time-series
  output$novel_plot_list <- renderUI(
    {
      novel_plot_output_list <- lapply(
        input$novelVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13, solidHeader = TRUE))
        }
      )
      fluidRow(
        lapply(
          X = split(novel_plot_output_list, f = rep(c(1, 2), length.out = length(novel_plot_output_list))),
          FUN = column, width = 6, style='padding:0px'
        )
      )
    }
  )
  
  output$novel_plot_total <- renderUI({
    # only make room for this plot in the app if it's selected
    if(input$novelTotal == TRUE) {
      # wrapping elements inside a fluidRow function extends the white space of the main panel accordingly
      fluidRow(
          box(plotOutput("plotTotal", height = 300), width = 12, solidHeader = TRUE)
        )
    }
  })
  
  
  ##### "OPTIMIZE" TAB #####
  
  output$opt_info <- renderUI({
    rawText <- readLines("D:/Github/shinyapp/optimize.txt")
    splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    replacedText <- lapply(splitText, p)
    box(replacedText, width = 12, solidHeader = TRUE)
  })

  # subsetting data for the optimizing plots
  opt_subset <- reactive({
    a <- datOpt[(datOpt$F_GES == input$F_GES & datOpt$Nutr_GES == input$Nutr_GES & datOpt$Min_acc_profit == input$Profit),]
    return(a)
  })

  output$opt_fish <- renderPlot({
    ggplot(opt_subset()[1:3,], aes(x = F_labels, y = F_scen)) +
      scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) +
      ggtitle("Fishery Policy Scenario") +
      xlab("\nFishery policy") +
      ylab("Probability\n") + 
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                         title = element_text(size = 14),
                         axis.title.y = element_text(size = 13),
                         axis.text.y = element_text(size = 11),
                         axis.title.x = element_text(size = 13),
                         axis.text.x = element_text(size = 11)
      ) +
      geom_bar(stat = "identity")
    
  })
  
  output$opt_clim <- renderPlot({
    ggplot(opt_subset()[1:2,], aes(x = Clim_labels, y = Clim_scen)) +
      scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) + 
      ggtitle("Climate Scenario") +
      xlab("\nClimate scenario") +
      ylab("Probability\n") + 
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                         title = element_text(size = 14),
                         axis.title.y = element_text(size = 13),
                         axis.text.y = element_text(size = 11),
                         axis.title.x = element_text(size = 13),
                         axis.text.x = element_text(size = 11)
      ) +
      geom_bar(stat = "identity")
  })
  
  output$opt_nutr <- renderPlot({
    ggplot(opt_subset()[1:2,], aes(x = Nutr_labels, y = Nutr_scen)) +
      scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) + 
      ggtitle("Nutrient Loading Policy Scenario") +
      xlab("\nNutrient loading policy") +
      ylab("Probability\n") + 
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                         title = element_text(size = 14),
                         axis.title.y = element_text(size = 13),
                         axis.text.y = element_text(size = 11),
                         axis.title.x = element_text(size = 13),
                         axis.text.x = element_text(size = 11)
      ) +
      geom_bar(stat = "identity")
  })
  
  output$opt_dec <- renderPlot({
    ggplot(opt_subset(), aes(x = Dec_labels, y = Decade)) +
      scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 5)) + 
      ggtitle("Decade") +
      xlab("\nDecade") +
      ylab("Probability\n") + 
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
                         title = element_text(size = 14),
                         axis.title.y = element_text(size = 13),
                         axis.text.y = element_text(size = 11),
                         axis.title.x = element_text(size = 13),
                         axis.text.x = element_text(size = 11)
      ) +
      geom_bar(stat = "identity")
  })
  
  
  output$opt_plots <- renderUI(
    {
      opt_plot_output_list <- lapply(
        input$optVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13, solidHeader = TRUE))
        }
      )
      fluidRow(
        lapply(
          X = split(opt_plot_output_list, f = rep(c(1, 2), length.out = length(opt_plot_output_list))),
          FUN = column, width = 6, style='padding:0px'
        )
      )
    }
  )
 
}