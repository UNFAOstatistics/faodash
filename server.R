library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

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
    
    opts <- selectInput("indicator_name", "Pick an item:",choices = items)
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
    left_join(dat3,FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME")])
    
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
    left_join(dat3,FAOcountryProfile[c("FAOST_CODE","FAO_TABLE_NAME")])
    
  })
  
  

  output$yearRange <- renderUI({

    
    
    if (input$trade_production %in% "Trade") data <- data_trade()
    if (input$trade_production %in% "Production") data <- data_production()
    
    maxim <- max(data$Year)
    minim <- min(data$Year)
    opts <- sliderInput("year_range", "Select year range", min = minim, max = maxim, value = c(minim,maxim), step = 1,animate = TRUE)
    list(opts)
  })


  output$mytable = renderDataTable({
    #fao_data()
    trade_value()
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
  
  

  output$box_1_1 <- renderInfoBox({
    
      
      if (input$trade_production %in% "Production"){
        
        dd <- fao_data_quantity()
        
        d <- dd %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>% arrange(FAOST_CODE,-Year)
        
        value <- d[d$Year == input$year_range[2] & d$FAOST_CODE == 5000,]$Value
        

        # Define the box
        infoBox(title=paste("Production quantity in",input$year_range[2]),value=paste0(value," million tonnes"),icon = icon("th"),color = "blue",width=6)
      } else {
        value <- 12345
        infoBox(title="Total export value",value=paste(value,"million US$"),icon = icon("fa fa-plane"),color = "purple")
      }
    })

  output$box_1_2 <- renderInfoBox({

    # Define the box
    if (input$trade_production %in% "Production"){
      dd <- fao_data_area_harvested()
      
      d <- dd %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>% arrange(FAOST_CODE,-Year)
      
      value <- d[d$Year == input$year_range[2] & d$FAOST_CODE == 5000,]$Value
      
      infoBox(title=paste("Area harvested in ",input$year_range[2]),value=paste0(value," hectares"),icon = icon("globe"),color = "green",width=6)
  } else {
    value <- 12345
    infoBox(title="Total import value",value=paste(value,"million US$"),icon = icon("fa fa-plane fa-rotate-180"),color = "purple")
  }
  
  })


  output$sub_title1 <- renderUI({
    
    if (input$trade_production %in% "Production") subtitle <- paste("Top 10 countries in",input$year_range[2])
    if (input$trade_production %in% "Trade")      subtitle <- paste("Top 10 countries in",input$year_range[2])
    subtitle
  })
  

# RANKS


output$box_2_1 <- renderPlot({

  if (input$trade_production %in% "Production"){
    
    dd <- fao_data_quantity_entity()
    
    top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000) %>% arrange(-Value) %>% slice(1:10)
    
    p <- ggplot(top10, aes(x=reorder(FAO_TABLE_NAME,Value),y=Value,fill=factor(FAOST_CODE)))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + labs(title = "Production quantity")
    p <- p + theme_minimal()
    p <- p + coord_flip()
    p <- p + theme(legend.position="none")
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey"))
    p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    
  } 
  if (input$trade_production %in% "Trade"){
    
    dd <- trade_value()

    top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000) %>% arrange(-Value) %>% slice(1:10)

    p <- ggplot(top10, aes(x=reorder(FAOST_CODE,Value),y=Value,fill=factor(FAOST_CODE)))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + labs(title = "Export Value")
    p <- p + theme_minimal()
    p <- p + coord_flip()
    p <- p + theme(legend.position="none")
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey"))
    p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    
    
  }
  p
  })

output$box_2_2 <- renderPlot({

  if (input$trade_production %in% "Production"){
    
    dd <- fao_data_area_harvested_entity()
    
    top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000) %>% arrange(-Value) %>% slice(1:10)
    
    p <- ggplot(top10, aes(x=reorder(FAO_TABLE_NAME,Value),y=Value,fill=factor(FAOST_CODE)))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + labs(title = "Area harvested")
    p <- p + theme_minimal()
    p <- p + coord_flip()
    p <- p + theme(legend.position="none")
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey" ))
    p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p
    
  } 
  if (input$trade_production %in% "Trade"){
    
    dd <- trade_value()
    
    top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000) %>% arrange(-Value) %>% slice(1:10)
    
    p <- ggplot(top10, aes(x=reorder(FAOST_CODE,Value),y=Value,fill=factor(FAOST_CODE)))
    p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + labs(title = "Import value")
    p <- p + theme_minimal()
    p <- p + coord_flip()
    p <- p + theme(legend.position="none")
    p <- p + theme(axis.text.x = element_blank())
    p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey"))
    p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = 0))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    
    
  }
  p
})

output$sub_title2 <- renderUI({
  
  if (input$trade_production %in% "Production") subtitle <- "Top 10 growing countries"
  if (input$trade_production %in% "Trade")      subtitle <- "Top 10 growing countries"
  subtitle
})


output$box_3_1 <- renderPlot({
  
  if (input$trade_production %in% "Production"){
    
    dd <- fao_data_quantity_entity()
    
    top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000) %>% arrange(-Value) %>% slice(1:10)
    dat_plot <- dd[dd$FAOST_CODE %in% top10$FAOST_CODE,]
    
    p <- ggplot(dat_plot, aes(x=Year,y=Value,color=factor(FAO_TABLE_NAME)))
    p <- p + geom_text(data=dat_plot[dat_plot$Year == input$year_range[2],], 
                       aes(x=Year,y=Value,label=FAO_TABLE_NAME,color=FAO_TABLE_NAME), 
                       size=4, hjust=-0.2, family="Ubuntu")
    #p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + geom_point() + geom_line()
    p <- p + labs(title = "Production quantity")
    p <- p + theme_minimal()
    #p <- p + coord_flip()
    p <- p + theme(legend.position="none")
    p <- p + theme(axis.text.y = element_blank())
    #p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey" ))
    p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = .5))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p <- p + coord_cartesian(xlim=c(input$year_range[1],input$year_range[2]+((input$year_range[2]-input$year_range[1])*.2)))
    
  } 
  if (input$trade_production %in% "Trade"){
    
    dd <- trade_value()
    
    top10 <- dd %>% filter(Year == input$year_range[2], FAOST_CODE < 5000) %>% arrange(-Value) %>% slice(1:10)
    dat_plot <- dd[dd$FAOST_CODE %in% top10$FAOST_CODE,]
    
    p <- ggplot(dat_plot, aes(x=Year,y=Value,color=factor(FAO_TABLE_NAME)))
    p <- p + geom_text(data=dat_plot[dat_plot$Year == input$year_range[2],], 
                       aes(x=Year,y=Value,label=FAO_TABLE_NAME,color=FAO_TABLE_NAME), 
                       size=4, hjust=-0.2, family="Ubuntu")
    #p <- p + geom_bar(stat="identity",position="dodge")
    p <- p + geom_point() + geom_line()
    p <- p + labs(title = "Import value")
    p <- p + theme_minimal()
    #p <- p + coord_flip()
    p <- p + theme(legend.position="none")
    p <- p + theme(axis.text.y = element_blank())
    #p <- p + theme(axis.text.y = element_text(size=16, family = "Ubuntu", color="dim grey" ))
    p <- p + theme(title = element_text(size=18, family = "Ubuntu", color="dim grey",hjust = .5))
    p <- p + theme(axis.title = element_blank())
    p <- p + theme(axis.ticks = element_blank())
    p <- p + coord_cartesian(xlim=c(input$year_range[1],input$year_range[2]+((input$year_range[2]-input$year_range[1])*.2)))
    
  }
  p
})

  






})
