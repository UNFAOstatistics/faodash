library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {

  output$country <- renderUI({
    country_names <- unique(FAOcountryProfile$FAO_TABLE_NAME)
    opts <- selectInput("country_name", "Pick a Country:",choices = country_names, selected=country_names[1])
    list(opts)
  })


  output$group <- renderUI({
    groupNames <- groupTable[["groupName"]]
    opts <- selectInput("gc_name", "Which Group are you looking for:",choices = groupNames, selected=groupNames[2])
    list(opts)
  })

  output$domain <- renderUI({

    gc <- groupTable[groupTable$groupName == input$gc_name,]$groupCode
    #     x <- groupNames[1]
    #     gc <- groupTable[groupTable$groupName == x,]$groupCode
    subdomainTable <- domainTable[domainTable$groupCode == gc,]
    domainNames <- subdomainTable[["domainName"]]
    opts <- selectInput("domain_name", "Which Domain are you looking for:",
                        choices = domainNames, selected=domainNames[1])
    list(opts)
  })

  output$indOrAgg <- renderUI({
    ind_or_agg <- data.frame(name = c("(0) Individual item (e.g. Apples, Wheat)",
                                      "(1) Aggregated item (e.g. Total cereals, Total meat"),
                             code = c(0,1),
                             stringsAsFactors = FALSE
    )
    values <- ind_or_agg$name
    opts <- selectInput("ind_or_agg", "Are you looking for individual item or aggregated item:",
                        choices = values, selected=values[1])
    list(opts)
  })

  output$item <- renderUI({

    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    #    agg <- ind_or_agg[ind_or_agg$name == input$ind_or_agg,]$code
    #     x <- values[2]
    #     agg <- ind_or_agg[ind_or_agg$name == x,]$code
    #if (agg == 1) {
    #  subitemTable = itemAggTable[itemAggTable$domainCode == dc,]
    #} else {
    subitemTable = itemTable[itemTable$domainCode == dc,]
    #}
    values <- subitemTable$itemName
    opts <- selectInput("item_name", "Which Item are you looking for?",
                        choices = values, selected=values[1])
    list(opts)
  })

  output$element <- renderUI({


    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    subelementTable = elementTable[elementTable$domainCode == dc,]
    values <- as.character(subelementTable$elementName)
    opts <- selectInput("element_name", "Which Element are you looking for?",
                        choices = values, selected=values[1])
    list(opts)
  })

  fao_data <- reactive({

    dc <- domainTable[domainTable$domainName == input$domain_name,]$domainCode
    ec <- elementTable[elementTable$elementName == input$element_name & elementTable$domainCode == dc,]$elementCode
    ic <- itemTable[itemTable$itemName == input$item_name & itemTable$domainCode == dc,]$itemCode

    dat <- getFAOtoSYB(domainCode = dc,
                       elementCode = ec,
                       itemCode = ic)
    #dat[["entity"]]
    dat[["aggregates"]]

  })

  output$yearRange <- renderUI({

    data <- fao_data()
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_range", "Select year range", min = minim, max = maxim, value = c(minim,maxim), step = 1)
    list(opts)
  })

  fao_data_filtered <- reactive({

    data <- fao_data()
    data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      arrange(FAOST_CODE)
  })


  output$mytable = renderDataTable({
    fao_data_filtered()
  },options = list(pageLength = 100))

# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------

# Population

# ------------------------------------------------------------
# ------------------------------------------------------------


  output$populationBox <- renderInfoBox({

      # download population data from FAOSTAT
      pop <- read.csv("./data/population.csv")
      ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
      value <- pop[pop$AreaCode %in% ccode,]$Value / 1000
      value <- round(value,1)

    # Define the box
      infoBox(title="Population",value=paste0(value," million"),icon = icon("users"),color = "purple",width=6)
    })

  output$populationGrowth <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- syb.df[syb.df$FAOST_CODE %in% ccode & syb.df$Year == 2015,]$OA.TPBS.POP.PPL.GR10
    value <- round(value,1)

    # Define the box
    infoBox("Population growth",value=paste0(value," %"),icon = icon("line-chart"),width=6)
  })


  output$lifeExpectancy <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- syb.df[syb.df$FAOST_CODE %in% ccode & syb.df$Year == 2013,]$SP.DYN.LE00.IN
    value <- round(value,1)

    # Define the box
    infoBox("Life expectancy",value=paste0(value, " years"),icon = icon("wheelchair"),width=6)
  })

# RANKS


output$populationBox_rank <- renderPlot({

    # download population data from FAOSTAT
    pop <- read.csv("./data/population.csv")
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    #pop <- pop[pop$Value >= 15000,]
    pop$Value <- pop$Value / 1000
    pop$barfill <- "not"
    pop$barfill[pop$AreaCode == ccode] <- "selected"

    pop <- arrange(pop, -Value)
    rank <- which(pop[,"AreaCode"] == ccode)
  # Define the plot
    p <- ggplot(pop, aes(x=reorder(AreaCode,-Value),y=Value,fill=barfill))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + theme_minimal()
    p <- p + labs(title=NULL, x=NULL, y=NULL)
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p <- p + theme(legend.position = "null")
    p <- p + scale_fill_manual(values = c("grey80","orange"))
    p <- p + geom_vline(xintercept=rank, linetype="dashed", color="orange")
    p
  })

output$populationGrowth_rank <- renderPlot({

  # download population data from FAOSTAT
  ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
  plot <- syb.df[syb.df$Year == 2015,c("FAOST_CODE","OA.TPBS.POP.PPL.GR10")]
  names(plot) <- c("FAOST_CODE","Value")

  plot$barfill <- "not"
  plot$barfill[plot$FAOST_CODE == ccode] <- "selected"

  plot <- arrange(plot, -Value)
  rank <- which(plot[,"FAOST_CODE"] == ccode)

# Define the plot
  p <- ggplot(plot, aes(x=reorder(FAOST_CODE,-Value),y=Value,fill=barfill))
  p <- p + geom_bar(stat="identity",position="dodge")
  p <- p + theme_minimal()
  p <- p + labs(title=NULL, x=NULL, y=NULL)
  p <- p + theme(axis.text.x = element_blank())
  p <- p + theme(axis.ticks = element_blank())
  p <- p + theme(legend.position = "null")
  p <- p + scale_fill_manual(values = c("grey80","orange"))
  p <- p + geom_vline(xintercept=rank, linetype="dashed", color="orange")
  p
})


output$lifeExpectancy_rank <- renderPlot({

  # download population data from FAOSTAT
  ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
  plot <- syb.df[syb.df$Year == 2013,c("FAOST_CODE","SP.DYN.LE00.IN")]
  names(plot) <- c("FAOST_CODE","Value")


  plot$barfill <- "not"
  plot$barfill[plot$FAOST_CODE == ccode] <- "selected"

  plot <- arrange(plot, -Value)
  rank <- which(plot[,"FAOST_CODE"] == ccode)

# Define the plot
  p <- ggplot(plot, aes(x=reorder(FAOST_CODE,-Value),y=Value,fill=barfill))
  p <- p + geom_bar(stat="identity",position="dodge")
  p <- p + theme_minimal()
  p <- p + labs(title=NULL, x=NULL, y=NULL)
  p <- p + theme(axis.text.x = element_blank())
  p <- p + theme(axis.ticks = element_blank())
  p <- p + theme(legend.position = "null")
  p <- p + scale_fill_manual(values = c("grey80","orange"))
  p <- p + geom_vline(xintercept=rank, linetype="dashed", color="orange")
  p

})



  # ------------------------------------------------------------
  # ------------------------------------------------------------
  # ------------------------------------------------------------

  # Economy

  # ------------------------------------------------------------
  # ------------------------------------------------------------

  output$gdpPerCapita <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- syb.df[syb.df$FAOST_CODE %in% ccode & syb.df$Year == 2012,]$NY.GNP.PCAP.CD
    value <- round(value,1)

    # Define the box
    infoBox("GDP per capita",value=paste0(value, " US$"),icon = icon("money"),width=6)
  })

  output$valueAddedWorker <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- syb.df[syb.df$FAOST_CODE %in% ccode & syb.df$Year == 2012,]$EA.PRD.AGRI.KD
    value <- round(value,1)

    # Define the box
    infoBox("Agr value added per worker",value=paste0(value, " US$"),icon = icon("money"),width=6)
  })

  output$valueAddedAgriculture <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- syb.df[syb.df$FAOST_CODE %in% ccode & syb.df$Year == 2012,]$NV.AGR.TOTL.ZS
    value <- round(value,1)

    # Define the box
    infoBox("Agr value added",value=paste0(value, " % of GDP"),icon = icon("money"),width=6)
  })



  output$gdpPerCapita_rank <- renderPlot({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE

    plot <- syb.df[syb.df$Year == 2012,c("FAOST_CODE","EA.PRD.AGRI.KD")]
    names(plot) <- c("FAOST_CODE","Value")

    plot$barfill <- "not"
    plot$barfill[plot$FAOST_CODE == ccode] <- "selected"

    plot <- arrange(plot, -Value)
    rank <- which(plot[,"FAOST_CODE"] == ccode)

  # Define the plot
    p <- ggplot(plot, aes(x=reorder(FAOST_CODE,-Value),y=Value,fill=barfill))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + theme_minimal()
    p <- p + labs(title=NULL, x=NULL, y=NULL)
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p <- p + theme(legend.position = "null")
    p <- p + scale_fill_manual(values = c("grey80","orange"))
    p <- p + geom_vline(xintercept=rank, linetype="dashed", color="orange")
    p

  })

  output$valueAddedWorker_rank <- renderPlot({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE

    plot <- syb.df[syb.df$Year == 2012,c("FAOST_CODE","NY.GNP.PCAP.CD")]
    names(plot) <- c("FAOST_CODE","Value")

    plot$barfill <- "not"
    plot$barfill[plot$FAOST_CODE == ccode] <- "selected"

    plot <- arrange(plot, -Value)
    rank <- which(plot[,"FAOST_CODE"] == ccode)

  # Define the plot
    p <- ggplot(plot, aes(x=reorder(FAOST_CODE,-Value),y=Value,fill=barfill))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + theme_minimal()
    p <- p + labs(title=NULL, x=NULL, y=NULL)
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p <- p + theme(legend.position = "null")
    p <- p + scale_fill_manual(values = c("grey80","orange"))
    p <- p + geom_vline(xintercept=rank, linetype="dashed", color="orange")
    p

  })

  output$valueAddedAgriculture_rank <- renderPlot({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE

    plot <- syb.df[syb.df$Year == 2012,c("FAOST_CODE","NV.AGR.TOTL.ZS")]
    names(plot) <- c("FAOST_CODE","Value")

    plot$barfill <- "not"
    plot$barfill[plot$FAOST_CODE == ccode] <- "selected"

    plot <- arrange(plot, -Value)
    rank <- which(plot[,"FAOST_CODE"] == ccode)

  # Define the plot
    p <- ggplot(plot, aes(x=reorder(FAOST_CODE,-Value),y=Value,fill=barfill))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + theme_minimal()
    p <- p + labs(title=NULL, x=NULL, y=NULL)
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p <- p + theme(legend.position = "null")
    p <- p + scale_fill_manual(values = c("grey80","orange"))
    p <- p + geom_vline(xintercept=rank, linetype="dashed", color="orange")
    p

  })



  # DES

  # ------------------------------------------------------------
  # ------------------------------------------------------------

  output$dietaryEnergySupply <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- fsi[fsi$FAOST_CODE %in% ccode & fsi$Year == 2012,]$FBS.PCS.PDES.KCD3D
    value <- round(value,1)

    # Define the box
    infoBox("Dietary energy supply",value=paste0(value, " kcal/cap/day"),icon = icon("glass"),width=6)
  })

  output$undernourishment <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    # fsi$FS.OA.NOU.P3D1 <- as.factor(fsi$FS.OA.NOU.P3D1)
    # fsi$FS.OA.NOU.P3D1 <- as.numeric(levels(fsi$FS.OA.NOU.P3D1))[fsi$FS.OA.NOU.P3D1]
    fsi$FS.OA.POU.PCT3D1[fsi$FS.OA.POU.PCT3D1 == "<5.0"] <- 0.1
    fsi$FS.OA.POU.PCT3D1 <- as.factor(fsi$FS.OA.POU.PCT3D1)
    fsi$FS.OA.POU.PCT3D1 <- as.numeric(levels(fsi$FS.OA.POU.PCT3D1))[fsi$FS.OA.POU.PCT3D1]
    value <- fsi[fsi$FAOST_CODE %in% ccode & fsi$Year == 2012,]$FS.OA.POU.PCT3D1
    value <- round(value,1)

    # Define the box
    infoBox("Prevalence of undernourishment",value=paste0(value, " %"),icon = icon("leaf"),width=6)
  })

  output$rootsAndTubers <- renderInfoBox({

    # download population data from FAOSTAT
    ccode <- FAOcountryProfile[FAOcountryProfile$FAO_TABLE_NAME %in% input$country_name,]$FAOST_CODE
    value <- fsi[fsi$FAOST_CODE %in% ccode & fsi$Year == 2010,]$FBS.PCSS.CSR.PCT3D

    value <- round(value,1)

    # Define the box
    infoBox("DES, cereals/roots/tubers",value=paste0(value, " %"),icon = icon("pie-chart"),width=6)
  })



})
