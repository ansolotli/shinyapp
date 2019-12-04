
# Load data
datBio <- read.csv("D:/Github/shinyapp/EwE_bio_results_corrected0412.csv", row.names = NULL, sep = ";")
datCth <- read.csv("D:/Github/shinyapp/EwE_catch_results_corrected0412.csv", row.names = NULL, sep = ";")
datNov <- read.csv("D:/Github/shinyapp/Novelty_incl_total.csv", row.names = NULL, sep = ";")


# Define the server function
server <- function(input, output) {
  
  # "ABOUT" TAB
  
  output$aboutText <- renderText({
    HTML("This decision support tool has been created as part of the BONUS BLUEWEBS project. 
         It aims to visualize and explain the key results of various environmental modelling scenarios.",
         "<br>",
         "<br>",
         "The model behind this application has been used to forecast the impacts of different climatic futures,
         nutrient load management schemes and fisheries management options in the open Baltic Sea marine ecosystem
         during the 21st century.")
  })
  
  output$DSS <- renderImage({
    return(list(src = "D:/Github/shinyapp/DSS_simplified.PNG", contentType = "image/png"))
  }, deleteFile = FALSE)
  
  
  
  # "PREDICTED TIME SERIES" TAB
  
  ## BIOMASS VARIABLES
  
  # Cod
  output$plotCod <- renderPlot(
    {
      if("plotCod" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),] 
        plot(x=tmp$Year, y=tmp$Cod,  xlab="Year", ylab="Biomass", ylim=c(0,250), xlim=c(2004,2096), type = 'n', main = "Biomass of cod")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Cod - tmp$CodSD), rev(tmp$Cod + tmp$CodSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Cod, col="black")
        abline(h=96.5, col = "red")
        text(2007, 68, "GES above this line", col="red", pos=4)
      }
    }
  )  
  
  # Herring
  output$plotHer <- renderPlot(
    {
      if("plotHer" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Herring,  xlab="Year", ylab="Biomass", ylim=c(0,800), xlim=c(2004,2096), type = 'n', main = "Biomass of Herring")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Herring - tmp$HerringSD), rev(tmp$Herring + tmp$HerringSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Herring, col="black")
        abline(h=430, col = "red")
        text(2007, 450, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Sprat
  output$plotSpr <- renderPlot(
    {
      if("plotSpr" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Sprat,  xlab="Year", ylab="Biomass", ylim=c(0,1150), xlim=c(2004,2096), type = 'n', main = "Biomass of Sprat")
        # !!!! NOTE In data file there is SpratDS instead of SpratSD
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Sprat - tmp$SpratDS), rev(tmp$Sprat + tmp$SpratDS)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Sprat, col="black")
        abline(h=410, col = "red")
        text(2007, 425, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Zooplankton
  output$plotZoo <- renderPlot(
    {
      if("plotZoo" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Zooplankton,  xlab="Year", ylab="Biomass", ylim=c(0,12), xlim=c(2004,2096), type = 'n', main = "Biomass of Zooplankton")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Zooplankton - tmp$ZooplanktonSD), rev(tmp$Zooplankton + tmp$ZooplanktonSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Zooplankton, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  #Phytoplankton
  output$plotPhy <- renderPlot(
    {
      if("plotPhy" %in% input$bioVars){
        tmp <- datBio[(datBio$F == input$F & datBio$Nutr_scen == input$Nutr_scen & datBio$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$Phytoplankton,  xlab="Year", ylab="Biomass", ylim=c(0,12), xlim=c(2004,2096), type = 'n', main = "Biomass of Phytoplankton")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$Phytoplankton - tmp$PhytoplanktonSD), rev(tmp$Phytoplankton + tmp$PhytoplanktonSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Phytoplankton, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  ## CATCH VARIABLES
  
  # Cod
  output$plotCodCatch <- renderPlot(
    {
      if("plotCodCatch" %in% input$catchVars){
        tmp <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),] 
        plot(x=tmp$Year, y=tmp$CodCatch,  xlab="Year", ylab="Catch", ylim=c(0,80), xlim=c(2004,2096), type = 'n', main = "Cod Catch")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$CodCatch - tmp$CodSD), rev(tmp$CodCatch + tmp$CodSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$CodCatch, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )  
  
  # Herring
  output$plotHerCatch <- renderPlot(
    {
      if("plotHerCatch" %in% input$catchVars){
        tmp <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$HerringCatch,  xlab="Year", ylab="Catch", ylim=c(0,250), xlim=c(2004,2096), type = 'n', main = "Herring Catch")
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$HerringCatch - tmp$HerringSD), rev(tmp$HerringCatch + tmp$HerringSD)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$HerringCatch, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Sprat
  output$plotSprCatch <- renderPlot(
    {
      if("plotSprCatch" %in% input$catchVars){
        tmp <- datCth[(datCth$F == input$F & datCth$Nutr_scen == input$Nutr_scen & datCth$Climate == input$Climate),]  
        plot(x=tmp$Year, y=tmp$SpratCatch,  xlab="Year", ylab="Catch", ylim=c(0,400), xlim=c(2004,2096), type = 'n', main = "Sprat Catch")
        # !!!! NOTE In data file there is SpratDS instead of SpratSD
        polygon(c(tmp$Year, rev(tmp$Year)), c((tmp$SpratCatch - tmp$SpratDS), rev(tmp$SpratCatch + tmp$SpratDS)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$SpratCatch, col="black")
        #abline(h=0.8, col = "red")
        #text(2007, 0.85, "GES above this line", col="red", pos=4)
      }
    }
  )
  
  # Render Biomass variable time-series
  output$bio_plot_list <- renderUI(
    {
      bio_plot_output_list <- lapply(
        input$bioVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13))
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
          column(width=12, box(plotOutput(plotname, height = 300), width = 13))
        }
      )
      do.call(tagList, catch_plot_output_list)
    }
  )
  
  
  # "NOVELTY TAB"
  
  output$novel_info <- renderUI({
    box(
      title = "About novelty", width = 12,
      HTML("Novel conditions are conditions that have not been observed in the past. This page shows the result were analysed based on the past 'observed range'
           calculating the confidence interval (CI) from the four food web (EwE) forcing variables (0-50m March-May water temperature,
           0-60m August water temperature, inverse hypoxic area and the cod reproductive volume) using the biogeochemical (RCO-Scobi)
           model for the period 1974-2004. For each of the four variables the future novelty is calculated and defined as the distance outside the CI as the level of novelty.")
      )
  })
  
  output$plotRv <- renderPlot(
    {
      if("plotRv" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),] 
        plot(x=tmp$Year, y=tmp$codRV,  xlab="Year", ylab="Cod reproductive volume", ylim=c(0,1), xlim=c(2004,2096), 
             type = 'n', main = "Novelty for cod reproductive volume")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$codRV, col="black")
      }
    }
  )
  
  output$plotTemp1 <- renderPlot(
    {
      if("plotTemp1" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),] 
        plot(x=tmp$Year, y=tmp$T_050_MarchMay,  xlab="Year", ylab="Water remperature", ylim=c(0, 1), xlim=c(2004,2096), 
             type = 'n', main = "Novelty for 0-50m water temperature")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$T_050_MarchMay, col="black")
      }
    }
  )
  
  output$plotTemp2 <- renderPlot(
    {
      if("plotTemp2" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),] 
        plot(x=tmp$Year, y=tmp$Aug060mT,  xlab="Year", ylab="Water temperature", ylim=c(0, 1), xlim=c(2004,2096), 
             type = 'n', main = "Novelty for 0-60m water temperature")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$Aug060mT, col="black")
      }
    }
  )
  
  output$plotHyp <- renderPlot(
    {
      if("plotHyp" %in% input$novelVars){
        tmp <- datNov[(datNov$Nutr_scen == input$Nutr_scen_nov & datNov$Clim_scen == input$Climate_nov),]
        plot(x=tmp$Year, y=tmp$notHypoxicA,  xlab="Year", ylab="Inverse hypoxic area", ylim=c(0,1), xlim=c(2004,2096), 
             type = 'n', main = "Novelty for inverse hypoxic area")
        #polygon(c(tmp$Year, rev(tmp$Year)), col = 'grey80', border = NA)
        lines(x=tmp$Year, y=tmp$notHypoxicA, col="black")
      }
    }
  )
  
  # Render Novelty variable time-series
  output$novel_plot_list <- renderUI(
    {
      novel_plot_output_list <- lapply(
        input$novelVars, function(plotname) {
          column(width=12, box(plotOutput(plotname, height = 300), width = 13))
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
  
}