library(dplyr)

function(input, output, session) {

  ## Data Explorer ###########################################

  observe({
    province <- if (is.null(input$country)) character(0) else {
      filter(cleantable, Country %in% input$country) %>%
        `$`('Province') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$province[input$province %in% province])
    updateSelectInput(session, "province", choices = province,
      selected = stillSelected)
  })

  observe({
    region_1 <- if (is.null(input$country)) character(0) else {
      cleantable %>%
        filter(Country %in% input$country,
          is.null(input$province) | Province %in% input$province) %>%
        `$`('Region1') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$region_1[input$region_1 %in% region_1])
    updateSelectInput(session, "region_1", choices = region_1,
      selected = stillSelected)
  })

  observe({
    region_2 <- if (is.null(input$country)) character(0) else {
      cleantable %>%
        filter(Country %in% input$country,
          is.null(input$region_1) | Region1 %in% input$region_1) %>%
        `$`('Region2') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$region_2[input$region_2 %in% region_2])
    updateSelectInput(session, "region_2", choices = region_2,
      selected = stillSelected)
  })

  observe({
    winery <- if (is.null(input$country)) character(0) else {
      cleantable %>%
        filter(Province %in% input$province,
          is.null(input$region_2) | Region2 %in% input$region_2) %>%
        `$`('Winery') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$winery[input$winery %in% winery])
    updateSelectInput(session, "winery", choices = winery,
      selected = stillSelected)
  })

  output$winetable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Points >= input$rangePoints[1],
        Points <= input$rangePoints[2],
        Price >= input$rangePrice[1],
        Price <= input$rangePrice[2],
        is.null(input$country) | Country %in% input$country,
        is.null(input$province) | Province %in% input$province,
        is.null(input$region_1) | Region1 %in% input$region_1,
        is.null(input$region_2) | Region2 %in% input$region_2,
        is.null(input$winery) | Winery %in% input$winery
      )

    DT::datatable(df, options = list(lengthMenu = c(5, 30, 50), escape = FALSE))
  })
  
  #### visuals page
  observe({
    histProvince <- if (is.null(input$histCountry)) character(0) else {
      filter(cleantable, Country %in% input$histCountry) %>%
        `$`('Province') %>%
        unique() %>%
        sort()
    }
    
    stillSelected <- isolate(input$histProvince[input$histProvince %in% histProvince])
    updateSelectInput(session, "histProvince", choices = histProvince,
                      selected = stillSelected)
  })
  
  observe({
    histWinery <- if (is.null(input$histCountry)) character(0) else {
      cleantable %>%
        filter(Country %in% input$histCountry,
               is.null(input$histProvince) | Province %in% input$histProvince) %>%
        `$`('Winery') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$histWinery[input$histWinery %in% histWinery])
    updateSelectInput(session, "histWinery", choices = histWinery,
                      selected = stillSelected)
  })
  
  output$hist <- renderPlot({
    df2 = cleantable %>%
      filter(
        is.null(input$histCountry) | Country %in% input$histCountry,
        is.null(input$histProvince) | Province %in% input$histProvince,
        is.null(input$histWinery) | Winery %in% input$histWinery
      )
    x = df2$Points
    bins <- seq(min(x), max(x), length.out = 10)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Points",
         main = "Histogram of Wine Points")
  })
  
  output$priceGraph <- renderPlot({
    df3 = cleantable %>%
      filter(
        is.null(input$histCountry) | Country %in% input$histCountry,
        is.null(input$histProvince) | Province %in% input$histProvince,
        is.null(input$histWinery) | Winery %in% input$histWinery
      )
    wine.by.year = df3 %>% na.omit() %>% group_by(VintageYear) %>% summarise(avg.price = mean(Price))
    wine.by.year = wine.by.year[order(wine.by.year$VintageYear),]
    ggplot(data = wine.by.year, aes(x = VintageYear, y = avg.price)) + geom_line()
  })
  
  output$wordCloud <- renderPlot({
    df4 = cleantable %>%
      filter(
        is.null(input$histCountry) | Country %in% input$histCountry,
        is.null(input$histProvince) | Province %in% input$histProvince,
        is.null(input$histWinery) | Winery %in% input$histWinery
      )
    
    v <- getTermMatrix(df4$Description)
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 10, max.words=20,
                  colors=brewer.pal(8, "Dark2"))
  })
}