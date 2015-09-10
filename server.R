library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

options(scipen=999) # disable scientific number formatting

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
  
  output$indicator <- renderUI({
    
    if (input$trade_production %in% "Production"){
      
      fao_meta2 <- fao_meta %>% filter(!(Element %in% c("Export Quantity","Export Value",
                                                        "Import Quantity","Import Value")))
      items <- unique(fao_meta2$Item)
      items <- items[c(-125)]
    } 
    if (input$trade_production %in% "Trade"){
      fao_meta2 <- fao_meta %>% filter(Element %in% c("Export Quantity","Export Value",
                                                      "Import Quantity","Import Value"))
      items <- unique(fao_meta2$Item)
      items <- items[c(-76,-287,-390)]
    }
    
    opts <- selectInput("indicator_name", "Pick an item:",choices = items, selected = items[2])
    list(opts)
  })
  
  #item_to_pick <- "Bananas"
  
  
  data_trade <- reactive({
    
    fao_meta3 <- fao_meta %>% filter(Element %in% c("Export Quantity","Export Value","Import Quantity","Import Value"), 
                                     Item %in% input$indicator_name)
    file_name <- str_replace_all(unique(fao_meta3$file_name), ".RData", "")
    ids <- unique(fao_meta3$id)
    dat <- get(file_name)
    dat2 <- dat[c("CountryCode","Year",ids)]
    
    dat3 <- gather(dat2, var, value, 3:ncol(dat2))
    dat3$var <- as.character(dat3$var)
    vars <- as.character(unique(dat3$var))
    for (i in vars){
      dat3$var[dat3$var %in% i] <- fao_meta[fao_meta$id %in% i,]$Element
    }
    names(dat3)[names(dat3)=="CountryCode"] <- "FAOST_CODE"
    dat3 <- left_join(dat3,FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME","SHORT_NAME")])
    dat3[dat3$FAOST_CODE != 41,]
    
  })
  
  data_production <- reactive({
    
    fao_meta3 <- fao_meta %>% filter(!(Element %in% c("Export Quantity","Export Value","Import Quantity","Import Value")), 
                                     Item %in% input$indicator_name)
    file_name <- str_replace_all(unique(fao_meta3$file_name), ".RData", "")
    ids <- unique(fao_meta3$id)
    dat <- get(file_name)
    dat2 <- dat[c("CountryCode","Year",ids)]
    
    dat3 <- gather(dat2, var, value, 3:ncol(dat2))
    dat3$var <- as.character(dat3$var)
    vars <- as.character(unique(dat3$var))
    for (i in vars){
      dat3$var[dat3$var %in% i] <- fao_meta[fao_meta$id %in% i,]$Element
    }
    names(dat3)[names(dat3)=="CountryCode"] <- "FAOST_CODE"
    dat3 <- left_join(dat3,FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME","SHORT_NAME")])
    dat3[dat3$FAOST_CODE != 41,]
    
  })
  
  output$yearYear <- renderUI({
    
    if (input$trade_production %in% "Trade") data <- data_trade()
    if (input$trade_production %in% "Production") data <- data_production()
    
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_year", "Select year", min = minim, max = maxim, value = maxim, step = 1,animate = TRUE)
    list(opts)
  })
  
  
  
  output$yearRange <- renderUI({
    
    if (input$trade_production %in% "Trade") data <- data_trade()
    if (input$trade_production %in% "Production") data <- data_production()
    
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_range", "Select year range", min = minim, max = maxim, value = c(minim,maxim), step = 1)
    list(opts)
  })
  
  
  
  
  
  output$mytable = renderDataTable({
    #fao_data()
    #d <- data_trade()
    d <- data_production()
    d %>% filter(Year > input$year_range[1], Year < input$year_range[2])
    d %>% filter(Year == input$year_year)
  },options = list(pageLength = 100))
  
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  
  # Define the boxes
  
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  
  output$page_title <- renderUI({
    
    #paste(input$trade_production,"of",input$item_name)
    input$indicator_name
  })
  
  
  
  output$box_1_1 <- renderValueBox({
    
    
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      #dd <- dat3
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Production",]$value / 1000000,1)
      #value <- round(dd[dd$Year == 2000 & dd$FAOST_CODE == 5000 & dd$var %in% "Production",]$value / 1000000,1)
      
      valueBox(subtitle=paste("Production quantity in",input$year_year),
              value=paste0(value," million tonnes"),
              icon = icon("th"),
              color = "green")
      
      
    } else {
      
      dd <- data_trade()
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Export Value",]$value / 1000000,1)
      valueBox(subtitle="Total export value",
              value=paste(value,"million US$"),
              icon = icon("fa fa-plane"),
              color = "red")
    }
  })
  
  output$box_1_2 <- renderValueBox({
    
    # Define the box
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Area harvested",]$value / 1000000,1)
      valueBox(subtitle=paste("Area harvested in ",input$year_year),
              value=paste0(value,"million hectares"),
              icon = icon("globe"),
              color = "green")
    } else {

      dd <- data_trade()
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Import Value",]$value / 1000000,1)
      valueBox(subtitle="Total import value",value=paste(value,"million US$"),
              icon = icon("fa fa-plane fa-rotate-180"),
              color = "red")
    }
    
  })
  
  
  output$box_1_3 <- renderInfoBox({
    
    
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      #dd <- dat3
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Yield",]$value,1)
      #value <- round(dd[dd$Year == 2000 & dd$FAOST_CODE == 5000 & dd$var %in% "Production",]$value / 1000000,1)
      
      infoBox(title=paste("Yield in",input$year_year),
              value=paste0(value," unit here"),
              icon = icon("signal"),
              color = "aqua")
    } else {
      
      dd <- data_trade()
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Export Quantity",]$value / 1000000,1)
      infoBox(title="Total export quantity",
              value=paste(value,"million tonnes"),
              icon = icon("fa fa-plane"),
              color = "yellow")
    }
  })
  
  
  output$box_1_4 <- renderInfoBox({
    
    
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      #dd <- dat3
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Seed",]$value,1)
      
      infoBox(title=paste("Seed in",input$year_year),value=paste0(value," unit here"),icon = icon("cubes"),color = "aqua",width=6)
    } else {
      
      dd <- data_trade()
      value <- round(dd[dd$Year == input$year_year & dd$FAOST_CODE == 5000 & dd$var %in% "Import Quantity",]$value / 1000000,1)
      infoBox(title="Total import quantity",
              value=paste(value,"million tonnes"),
              icon = icon("fa fa-plane fa-rotate-180"),
              color = "yellow")
    }
  })
  
  
  
  output$sub_title1 <- renderUI({
    
    if (input$trade_production %in% "Production") subtitle <- paste("Top 10 countries in",input$year_year)
    if (input$trade_production %in% "Trade")      subtitle <- paste("Top 10 countries in",input$year_year)
    subtitle
  })
  
  
  # RANKS
  
  
  output$box_2_1 <- renderPlot({
    
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      dd <- dd[!is.na(dd$SHORT_NAME),]
      top10 <- dd %>% filter(Year == input$year_year, FAOST_CODE < 5000, var %in% "Production") %>% arrange(-value) %>% slice(1:10)
      
      p <- ggplot(top10, aes(x=reorder(SHORT_NAME,value),y=value,fill=factor(SHORT_NAME)))
      p <- p + geom_bar(stat="identity",position="dodge")
      p <- p + labs(title = "Production quantity")
      p <- p + theme_minimal()
      p <- p + coord_flip()
      p <- p + theme(legend.position="none")
      p <- p + theme(axis.text.x = element_text(angle=90, color = "dim grey"))
      p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey"))
      p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.ticks = element_blank())
      
    } 
    if (input$trade_production %in% "Trade"){
      
      dd <- data_trade()
      dd <- dd[!is.na(dd$SHORT_NAME),]
      top10 <- dd %>% filter(Year == input$year_year, FAOST_CODE < 5000, var %in% "Export Value") %>% arrange(-value) %>% slice(1:10)
      
      p <- ggplot(top10, aes(x=reorder(SHORT_NAME,value),y=value,fill=factor(SHORT_NAME)))
      p <- p + geom_bar(stat="identity",position="dodge")
      p <- p + labs(title = "Export Value")
      p <- p + theme_minimal()
      p <- p + coord_flip()
      p <- p + theme(legend.position="none")
      p <- p + theme(axis.text.x = element_text(angle=90, color = "dim grey"))
      p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey"))
      p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.ticks = element_blank())
      
      
    }
    p
  })
  
  output$box_2_2 <- renderPlot({
    
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      dd <- dd[!is.na(dd$SHORT_NAME),]
      top10 <- dd %>% filter(Year == input$year_year, FAOST_CODE < 5000, var %in% "Area harvested") %>% arrange(-value) %>% slice(1:10)
      
      p <- ggplot(top10, aes(x=reorder(SHORT_NAME,value),y=value,fill=factor(SHORT_NAME)))
      p <- p + geom_bar(stat="identity",position="dodge")
      p <- p + labs(title = "Area harvested")
      p <- p + theme_minimal()
      p <- p + coord_flip()
      p <- p + theme(legend.position="none")
      p <- p + theme(axis.text.x = element_text(angle=90, color = "dim grey"))
      p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey" ))
      p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.ticks = element_blank())
      p
      
    } 
    if (input$trade_production %in% "Trade"){
      
      dd <- data_trade()
      dd <- dd <- dd[!is.na(dd$SHORT_NAME),]
      top10 <- dd %>% filter(Year == input$year_year, FAOST_CODE < 5000, var %in% "Import Value") %>% arrange(-value) %>% slice(1:10)
      
      
      p <- ggplot(top10, aes(x=reorder(SHORT_NAME,value),y=value,fill=factor(SHORT_NAME)))
      p <- p + geom_bar(stat="identity",position="dodge")
      p <- p + labs(title = "Import value")
      p <- p + theme_minimal()
      p <- p + coord_flip()
      p <- p + theme(legend.position="none")
      p <- p + theme(axis.text.x = element_text(angle=90, color = "dim grey"))
      p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey"))
      p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.ticks = element_blank())
      
      
    }
    p
  })
  
  output$sub_title2 <- renderUI({
    
    if (input$trade_production %in% "Production") subtitle <- "Growth"
    if (input$trade_production %in% "Trade")      subtitle <- "Growth"
    subtitle
  })
  
  
  output$box_3_1 <- renderPlot({
    
    if (input$trade_production %in% "Production"){
      
      dd <- data_production()
      top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000, var %in% "Production") %>% arrange(-value) %>% slice(1:10)
      dat_plot <- dd[dd$FAOST_CODE %in% top10$FAOST_CODE,]
      dat_plot <- dat_plot %>% filter(var %in% "Production", Year >= input$year_range[1], Year <= input$year_range[2])
      
      
      p <- ggplot(dat_plot, aes(x=Year,y=value,color=factor(SHORT_NAME)))
      p <- p + geom_text(data=dat_plot[dat_plot$Year == input$year_range[2],], 
                         aes(x=Year,y=value,label=SHORT_NAME,color=SHORT_NAME), 
                         size=4, hjust=-0.2, family="Ubuntu")
      p <- p + geom_point() + geom_line()
      p <- p + labs(title = "Production quantity")
      p <- p + theme_minimal()
      #p <- p + coord_flip()
      p <- p + theme(legend.position="none")
      p <- p + theme(axis.text.y = element_text(color = "dim grey"))
      #p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey" ))
      p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = .5))
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.ticks = element_blank())
      p <- p + coord_cartesian(xlim=c(input$year_range[1],input$year_range[2]+((input$year_range[2]-input$year_range[1])*.2)))
      p <- p + scale_x_continuous(breaks=c(input$year_range[1]:input$year_range[2]))
      p <- p + theme(axis.text.x = element_text(angle = 90, color = "dim grey"))
      
    } 
    if (input$trade_production %in% "Trade"){
      
      dd <- data_trade()
      top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000, var %in% "Import Value") %>% arrange(-value) %>% slice(1:10)
      dat_plot <- dd[dd$FAOST_CODE %in% top10$FAOST_CODE,]
      dat_plot <- dat_plot %>% filter(var %in% "Import Value", Year >= input$year_range[1], Year <= input$year_range[2])
      
      
      p <- ggplot(dat_plot, aes(x=Year,y=value,color=factor(SHORT_NAME), group=factor(SHORT_NAME)))
      p <- p + geom_text(data=dat_plot[dat_plot$Year == input$year_range[2],], 
                         aes(x=Year,y=value,label=SHORT_NAME,color=SHORT_NAME), 
                         size=4, hjust=-0.2, family="Ubuntu")
      #p <- p + geom_bar(stat="identity",position="dodge")
      p <- p + geom_line()
      p <- p + labs(title = "Import value")
      p <- p + theme_minimal()
      #p <- p + coord_flip()
      p <- p + theme(legend.position="none")
      p <- p + theme(axis.text.y = element_text(color = "dim grey"))
      #p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey" ))
      p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = .5))
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.ticks = element_blank())
      p <- p + coord_cartesian(xlim=c(input$year_range[1],input$year_range[2]+((input$year_range[2]-input$year_range[1])*.2)))
      p <- p + scale_x_continuous(breaks=c(input$year_range[1]:input$year_range[2]))
      p <- p + theme(axis.text.x = element_text(angle = 90, color = "dim grey"))
      #p <- p + coord_cartesian(xlim=c(min(dat_plot$Year),max(dat_plot$Year*1.1)))
    }
    p
  })
  
  
  output$box_3_2 <- renderTable({
    mtcars$names <- rownames(mtcars)
    mtcars %>% slice(1:11) %>% select(names,disp)
  
    })
  
  
  
  
  
})
