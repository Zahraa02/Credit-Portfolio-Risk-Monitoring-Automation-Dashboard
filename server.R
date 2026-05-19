function(input, output, session) {

  # Tab 1: NPL Overview
  output$plot1a_bar <- renderPlotly({
    
    # Data Wrangling
    # Data Wrangling
    gross_total_outstanding <- sqllab_gross_clean %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>% 
      head(14) # this and last week
    
    # tooltip
    gross_total_outstanding <- gross_total_outstanding %>% 
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(total_outstanding_amount, big.mark = '.', decimal.mark = ',', accuracy = 1)}
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.1 a: Weekly Gross Total Outstanding (Current Cut-Off)
    plot1.1.a <- ggplot(gross_total_outstanding, aes(x = island, 
                                                     y = total_outstanding_amount,
                                                     text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B",
                                               accuracy = 1)) +
      geom_text(aes(y = total_outstanding_amount + 1.5e10,
                    label = format(floor(total_outstanding_amount/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Date),
                position = position_dodge(width = 1),
                size = 2.8) +
      scale_fill_manual(values= c("lavender","purple")) +
      labs(title = "Weekly Gross Total Outstanding",
           subtitle = "in billion Rupiah", 
           x = "Island",
           y = "Total Outstanding",
           fill = "Date") +
      theme_minimal() +
      theme(legend.position = "bottom") 
    
    ggplotly(plot1.1.a, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "Weekly Gross Total Outstanding",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",  
                      y = -0.3)
      )
  })
  
  output$plot1b_bar <- renderPlotly({
    
    # Data Wrangling
    # Total amount DPD 90+
    gross_DPD_90 <- sqllab_gross_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    gross_total_outstanding2 <- sqllab_gross_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    gross_NPL_Current <- gross_DPD_90 %>% 
      inner_join(gross_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    # tooltip
    gross_NPL_Current <- gross_NPL_Current %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.1.b: NPL Comparison by Island
    plot1.1.b <- ggplot(gross_NPL_Current, aes(x = island, 
                                               y = NPL,
                                               text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      geom_text(aes(y = NPL + 0.8,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL),
                    group = Date),
                position = position_dodge(width = 1),
                size = 3.0) +
      scale_fill_manual(values= c("lavender", "purple")) +
      labs(title = "Weekly NPL Gross Comparison by Island",
           x = "Island",
           y = "NPL (%)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.1.b, tooltip = "text") %>% 
      layout(legend = list(orientation = "h",
                           y = -0.3)
      )
  })
  
  output$plot2a_bar <- renderPlotly({
    # Data Wrangling
    net_total_outstanding <- sqllab_net_clean %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>% 
      head(14) # this and last week
    
    # tooltip
    net_total_outstanding <- net_total_outstanding %>% 
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(total_outstanding_amount, big.mark = '.', decimal.mark = ',', accuracy = 1)}
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.2.a: Weekly Net Total Outstanding (Current Cut-Off)
    plot1.2.a <- ggplot(net_total_outstanding, aes(x = island, 
                                                   y = total_outstanding_amount,
                                                   text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B", 
                                               accuracy = 1)) +
      geom_text(aes(y = total_outstanding_amount + 1.2e10,
                    label = format(floor(total_outstanding_amount/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Date),
                position = position_dodge(width = 1),
                size = 2.8) +
      scale_fill_manual(values= c("lavender","purple")) +
      labs(title = "Weekly Net Total Outstanding",
           subtitle = "in billion Rupiah", 
           x = "Island",
           y = "Total Outstanding",
           fill = "Date") +
      theme_minimal() +
      theme(legend.position = "bottom") 
    
    ggplotly(plot1.2.a, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "Weekly Net Total Outstanding",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",  
                      y = -0.3)
      )
  })
  
  output$plot2b_bar <- renderPlotly({
    # Data Wrangling
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    net_NPL_Current <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    # tooltip
    net_NPL_Current <- net_NPL_Current %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.2.b: NPL Comparison by Island
    plot1.2.b <- ggplot(net_NPL_Current, aes(x = island, 
                                             y = NPL,
                                             text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      geom_text(aes(y = NPL + 0.5,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL),
                    group = Date),
                position = position_dodge(width = 1),
                size = 3.0) +
      scale_fill_manual(values= c("lavender", "purple")) +
      labs(title = "Weekly NPL Net Comparison by Island",
           x = "Island",
           y = "NPL (%)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.2.b, tooltip = "text") %>% 
      layout(legend = list(orientation = "h",
                           y = -0.3)
      )
  })
  
  output$plot3a_bar <- renderPlotly({
    # Data Wrangling
    external_total_outstanding <- sqllab_external_clean %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>% 
      head(14) # this and last week
    
    # tooltip
    external_total_outstanding <- external_total_outstanding %>% 
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(total_outstanding_amount, big.mark = '.', decimal.mark = ',', accuracy = 1)}
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.3 a: Weekly Gross Total Outstanding (Current Cut-Off)
    plot1.3.a <- ggplot(external_total_outstanding, aes(x = island, 
                                                        y = total_outstanding_amount,
                                                        text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B", 
                                               accuracy = 1)) +
      geom_text(aes(y = total_outstanding_amount + 1e10,
                    label = format(floor(total_outstanding_amount/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Date),
                position = position_dodge(width = 1),
                size = 2.8) +
      scale_fill_manual(values= c("lavender","purple")) +
      labs(title = "Weekly External Total Outstanding",
           subtitle = "in billion Rupiah", 
           x = "Island",
           y = "Total Outstanding",
           fill = "Date") +
      theme_minimal() +
      theme(legend.position = "bottom") 
    
    ggplotly(plot1.3.a, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "Weekly External Total Outstanding",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",  
                      y = -0.3)
      )
  })
  
  output$plot3b_bar <- renderPlotly({
    # Data Wrangling
    # Total amount DPD 90+
    external_DPD_90 <- sqllab_external_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    external_total_outstanding2 <- sqllab_external_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    external_NPL_Current <- external_DPD_90 %>% 
      inner_join(external_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    # tooltip
    external_NPL_Current <- external_NPL_Current %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 3.b: NPL Comparison by Island
    # PLOT 1.3.b: NPL Comparison by Island
    plot1.3.b <- ggplot(external_NPL_Current, aes(x = island, 
                                                  y = NPL,
                                                  text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      geom_text(aes(y = NPL + 0.12,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL),
                    group = Date),
                position = position_dodge(width = 1),
                size = 3.0) +
      scale_fill_manual(values= c("lavender", "purple")) +
      labs(title = "Weekly NPL External Comparison by Island",
           x = "Island",
           y = "NPL (%)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.3.b, tooltip = "text") %>% 
      layout(legend = list(orientation = "h",
                           y = -0.3)
      )
  })
  
  output$plot4_bar <- renderPlotly({
    # Gross Data Wrangling
    # Data Wrangling 1 
    # Total amount DPD 90+
    gross_DPD_90 <- sqllab_gross_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    gross_total_outstanding2 <- sqllab_gross_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    gross_NPL_Current <- gross_DPD_90 %>% 
      inner_join(gross_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    
    # Net Data Wrangling
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    net_NPL_Current <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    
    # External Data Wrangling
    # Data Wrangling
    # Total amount DPD 90+
    external_DPD_90 <- sqllab_external_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    external_total_outstanding2 <- sqllab_external_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    external_NPL_Current <- external_DPD_90 %>% 
      inner_join(external_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    # Data Wrangling ~> Nationally
    GNE_NPL_current <- gross_NPL_Current %>% 
      
      # Merging all current NPL datasets based on cut off date and island
      inner_join(net_NPL_Current, by= c("cut_off_date", "island")) %>% 
      inner_join(external_NPL_Current, by = c("cut_off_date", "island")) %>% 
      
      group_by(cut_off_date) %>% 
      summarise(
        # Aggregating/ summarizing data to national level
        DPD_90_gross = sum(DPD_90.x),
        total_outstanding_amount_gross = sum(total_outstanding_amount.x),
        DPD_90_net = sum(DPD_90.y),
        total_outstanding_amount_net = sum(total_outstanding_amount.y),
        DPD_90_external = sum(DPD_90),
        total_outstanding_amount_external = sum(total_outstanding_amount)
      ) %>% 
      ungroup() %>% 
      mutate(
        # NPL Gross, Net, and External
        Gross = round((DPD_90_gross / total_outstanding_amount_gross * 100),2),
        Net = round((DPD_90_net / total_outstanding_amount_net * 100),2),
        External = round((DPD_90_external / total_outstanding_amount_external * 100),2),
        
        # Date ~> for legend
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>% 
      
      select(
        -c(DPD_90_gross,
           total_outstanding_amount_gross,
           DPD_90_net,
           total_outstanding_amount_net,
           DPD_90_external,
           total_outstanding_amount_external)
      )
    
    # pivot longer
    GNE_NPL_current <- GNE_NPL_current %>% 
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "NPL_type",
        values_to = "NPL"
      ) %>% 
      mutate(NPL_type = factor(NPL_type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    GNE_NPL_current <- GNE_NPL_current %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.4: Weekly National NPL Comparison by Type
    plot1.4 <- ggplot(GNE_NPL_current, aes(x = NPL_type, 
                                           y = NPL,
                                           text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Date)) +
      geom_text(aes(y = NPL + 1.4,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL),
                    group = Date),
                position = position_dodge(width = 1),
                size = 3.0) +
      scale_fill_manual(values= c("lavender", "purple")) +
      labs(title = "Weekly National NPL Comparison by Type",
           x = "NPL Types",
           y = "NPL (%)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.4, tooltip = "text") %>% 
      layout(legend = list(orientation = "h",
                           y = -0.3))
  })
  
  output$plot6_bar <- renderPlotly({
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    net_NPL_Current <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    net_NPL_weekly_comparison <- net_NPL_Current %>% 
      group_by(cut_off_date) %>% 
      summarise(NPL_weekly = (sum(DPD_90)/sum(total_outstanding_amount))*100) %>%
      ungroup() %>% 
      mutate(
        NPL_weekly = round(NPL_weekly,2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)), "%d %b %Y"))
      ) %>% 
      arrange(desc(cut_off_date))
    
    # tooltip
    net_NPL_weekly_comparison <- net_NPL_weekly_comparison %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL_weekly,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.5: NPL Net Weekly Comparison
    plot1.5 <- ggplot(net_NPL_weekly_comparison, aes(x = Date, 
                                                     y = NPL_weekly,
                                                     text = label)) +
      
      geom_col(position = "dodge", aes(fill = Date)) +
      scale_fill_manual(values= c("lavender", "purple")) +
      geom_text(aes(y = NPL_weekly + 0.3,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL_weekly),
                    group = Date),
                size = 3.5)  +
      labs(title = "NPL Net Weekly Comparison",
           x = "Cut Off Date",
           y = "NPL (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot1.5, tooltip = "text")
  })
  
  output$npl_weekly_diff <- renderValueBox({
    
    # Net Data Wrangling
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    net_NPL_Current <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    net_NPL_weekly_comparison <- net_NPL_Current %>% 
      group_by(cut_off_date) %>% 
      summarise(NPL_weekly = (sum(DPD_90)/sum(total_outstanding_amount))*100) %>%
      ungroup() %>% 
      mutate(
        NPL_weekly = round(NPL_weekly,2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)), "%d %b %Y"))
      ) %>% 
      arrange(desc(cut_off_date))
    
    # Difference NPL in %
    # Difference NPL in %
    net_Diff_NPL_Weekly <- net_NPL_weekly_comparison %>%
      summarise(NPL_diff = NPL_weekly[1] - NPL_weekly[2],
                NPL_diff = if_else(
                  NPL_diff >0,
                  paste0("+", format(round(NPL_diff, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), "%"), # if condition
                  paste0(format(round(NPL_diff, 2), big.mark = ".", decimal.mark = ",", nsmall = 2), "%") # else condition
                )) %>% 
      pull(NPL_diff)
    
    valueBox(
      subtitle = "NPL Weekly Difference",
      value = net_Diff_NPL_Weekly,
      icon = icon("line-chart"),
      color = "purple"
    )
  })
  
  output$DPD90_amount_weekly_diff <- renderValueBox({
    
    # Net Data Wrangling
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Merge DPD 90+ outstanding amount with total outstanding amount to calculate current NPL
    net_NPL_Current <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        NPL = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) %>% 
      head(14) # this and last week
    
    # Difference DPD 90+ in Rp
    net_Diff_DPD90_weekly <- net_NPL_Current %>% 
      group_by(cut_off_date) %>% 
      summarise(Total_DPD_90_weekly = sum(DPD_90)) %>% 
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    # Diff DPD 90+ in RP
    # Difference Net DPD 90+ in Rp
    net_Diff_DPD90_weekly <- net_Diff_DPD90_weekly %>% 
      summarise(Diff_DPD90_weekly= Total_DPD_90_weekly[1]-Total_DPD_90_weekly[2],
                Diff_DPD90_weekly = if_else(
                  Diff_DPD90_weekly >0,
                  paste0(" + IDR ", comma(Diff_DPD90_weekly)), # ini if-nya
                  paste0(" - IDR ", comma(Diff_DPD90_weekly * (-1))) # ini else-nya
                ),
                
                Diff_DPD90_weekly = gsub(",", ".", Diff_DPD90_weekly),  # change thousand sep to "."
                Diff_DPD90_weekly = sub("\\.(\\d{2})$", ",\\1", Diff_DPD90_weekly) # change decimal sep to ","
      ) %>% 
      pull(Diff_DPD90_weekly)
    
    valueBox(
      subtitle = "DPD 90+ Outstanding Weekly Change",
      value = net_Diff_DPD90_weekly,
      icon = icon("money-bill-trend-up"),
      color = "purple"
    )
    
  })
  
  output$plot7_line <- renderPlotly({
    
    # Gross Data Wrangling
    # Data Wrangling 1 
    # Total amount DPD 90+
    gross_DPD_90 <- sqllab_gross_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    gross_total_outstanding2 <- sqllab_gross_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: Gross NPL for each island & cut off date
    gross_NPL_last_8_weeks <- gross_DPD_90 %>% 
      inner_join(gross_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        Gross = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) 
    
    # National Gross NPL value for each cut off date
    gross_NPL_last_8_weeks <- gross_NPL_last_8_weeks %>%
      group_by(cut_off_date) %>% 
      summarise(Gross= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    
    # Net Data Wrangling
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: Net NPL for each island & cut off date
    net_NPL_last_8_weeks <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        Net = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)
    
    # National Net NPL value for each cut off date 
    net_NPL_last_8_weeks <- net_NPL_last_8_weeks %>%
      group_by(cut_off_date) %>% 
      summarise(Net= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    
    # External Data Wrangling
    # Data Wrangling
    # Total amount DPD 90+
    external_DPD_90 <- sqllab_external_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    external_total_outstanding2 <- sqllab_external_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: External NPL for each island & cut off date
    external_NPL_last_8_weeks <- external_DPD_90 %>% 
      inner_join(external_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        External = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)
    
    # National External NPL value for each cut off date
    external_NPL_last_8_weeks <- external_NPL_last_8_weeks %>%
      group_by(cut_off_date) %>% 
      summarise(External= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    # Merging Gross, Net, and External national NPL values into one dataset
    GNE_NPL_last_8_weeks <- gross_NPL_last_8_weeks %>% 
      inner_join(net_NPL_last_8_weeks, by = c("cut_off_date")) %>%
      inner_join(external_NPL_last_8_weeks, by = c("cut_off_date")) %>% 
      mutate(
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      arrange(desc(cut_off_date)) 
    
    # Pivot and taking the last 8 weeks national NPL values
    GNE_NPL_last_8_weeks <- GNE_NPL_last_8_weeks %>% 
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "NPL_type",
        values_to = "NPL"
      ) %>% 
      mutate(NPL_type = factor(NPL_type, levels = c("Gross", "Net", "External"))) %>% 
      head(24)
    
    # tooltip
    GNE_NPL_last_8_weeks <- GNE_NPL_last_8_weeks %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.6: Last 8 Weeks NPL Position by Type
    plot1.6 <- ggplot(GNE_NPL_last_8_weeks, aes(x = cut_off_date, 
                                                y = NPL,
                                                color = NPL_type,
                                                group = NPL_type,
                                                text = label)) +
      
      geom_point() +
      geom_line() +
      geom_text(aes(y = NPL + 1.5,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL)),
                size = 3) +
      scale_x_date(
        breaks = GNE_NPL_last_8_weeks$cut_off_date, 
        date_labels = "%b %d"
      ) +
      scale_y_continuous(
        limits = c(0, max(GNE_NPL_last_8_weeks$NPL) + 10),
        breaks = seq(0,100,5)
      ) +
      scale_color_manual(values = c(
        "Gross" = "darkgreen",
        "Net" = "red",
        "External" = "blue"),
        name = NULL) +
      labs(title = "Last 8 Weeks NPL Position by Type",
           x = "Cut Off Date",
           y = "NPL (%)",
           color = "NPL Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.6, tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        y = -0.3
      ))
  })
  
  output$plot8_line <- renderPlotly({
    
    # Gross Data Wrangling
    # Data Wrangling 1 
    # Total amount DPD 90+
    gross_DPD_90 <- sqllab_gross_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    gross_total_outstanding2 <- sqllab_gross_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: Gross NPL for each island & cut off date
    gross_NPL_last_4_months <- gross_DPD_90 %>% 
      inner_join(gross_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        Gross = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)  
    
    # National Gross NPL value for each cut off date
    gross_NPL_last_4_months <- gross_NPL_last_4_months %>%
      group_by(cut_off_date) %>% 
      summarise(Gross= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    
    # Net Data Wrangling
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: Net NPL for each island & cut off date
    net_NPL_last_4_months <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        Net = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)
    
    # National Net NPL value for each cut off date
    net_NPL_last_4_months <- net_NPL_last_4_months %>%
      group_by(cut_off_date) %>% 
      summarise(Net= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    
    # External Data Wrangling
    # Data Wrangling
    # Total amount DPD 90+
    external_DPD_90 <- sqllab_external_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    external_total_outstanding2 <- sqllab_external_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: External NPL for each island & cut off date
    external_NPL_last_4_months <- external_DPD_90 %>% 
      inner_join(external_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        External = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)
    
    # National External NPL value for each cut off date
    external_NPL_last_4_months <- external_NPL_last_4_months %>%
      group_by(cut_off_date) %>% 
      summarise(External= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    # Merging Gross, Net, and External national NPL values into one dataset
    GNE_NPL_last_4_months <- gross_NPL_last_4_months %>%
      inner_join(net_NPL_last_4_months, by = c("cut_off_date")) %>%
      inner_join(external_NPL_last_4_months, by = c("cut_off_date")) %>% 
      mutate(
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      arrange(desc(cut_off_date)) 
    
    # Adding date, month, and year column
    GNE_NPL_last_4_months <- GNE_NPL_last_4_months %>% 
      mutate(
        date = day(cut_off_date),
        month = floor_date(cut_off_date, "month"),
        year = year(cut_off_date)
      ) %>% 
      arrange(desc(cut_off_date))
    
    # Retrieve the maximum date from each month and filter only for the last 4 months
    GNE_NPL_last_4_months <- GNE_NPL_last_4_months %>%
      group_by(month) %>% 
      filter(cut_off_date <= "2025-11-01") %>% 
      filter(date == max(date)) %>% 
      ungroup() %>% 
      head(4)
    
    # Pivot and adding NPL type column 
    GNE_NPL_last_4_months <- GNE_NPL_last_4_months %>% 
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "NPL_type",
        values_to = "NPL"
      ) %>% 
      mutate(NPL_type = factor(NPL_type, levels = c("Gross", "Net", "External"))) 
    
    # tooltip
    GNE_NPL_last_4_months <- GNE_NPL_last_4_months %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}"
        )
      )
    
    # PLOT 1.7: Last 4 Months NPL Position by Type
    plot1.7 <- ggplot(GNE_NPL_last_4_months, aes(x = cut_off_date, 
                                                 y = NPL,
                                                 color = NPL_type,
                                                 group = NPL_type,
                                                 text = label)) +
      
      geom_point() +
      geom_line() +
      geom_text(aes(y = NPL + 1.5,
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL)),
                size = 3) +
      scale_x_date(
        breaks = GNE_NPL_last_4_months$cut_off_date, 
        date_labels = "%b %d"
      ) +
      scale_y_continuous(
        limits = c(0, max(GNE_NPL_last_4_months$NPL) + 10),
        breaks = seq(0,100,5)
      ) +
      scale_color_manual(values = c(
        "Gross" = "darkgreen",
        "Net" = "red",
        "External" = "blue"),
        name = NULL) +
      labs(title = "Last 4 Months NPL Position by Type",
           x = "Cut Off Date",
           y = "NPL (%)",
           color = "NPL Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.7, tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        y = -0.3
      ))
  })
  
  output$plot9_line <- renderPlotly({
    
    # Gross Data Wrangling
    # Data Wrangling 1 
    # Total amount DPD 90+
    gross_DPD_90 <- sqllab_gross_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    gross_total_outstanding2 <- sqllab_gross_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: Gross NPL for each island & cut off date
    gross_NPL_last_4_quartals <- gross_DPD_90 %>% 
      inner_join(gross_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        Gross = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate) 
    
    # National Gross NPL value for each cut off date
    gross_NPL_last_4_quartals <- gross_NPL_last_4_quartals %>%
      group_by(cut_off_date) %>% 
      summarise(Gross= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    # Net Data Wrangling
    # Data Wrangling 1
    # Total amount DPD 90+
    net_DPD_90 <- sqllab_net_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    net_total_outstanding2 <- sqllab_net_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: Net NPL for each island & cut off date
    net_NPL_last_4_quartals <- net_DPD_90 %>% 
      inner_join(net_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        Net = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)
    
    # National Net NPL value for each cut off date
    net_NPL_last_4_quartals <- net_NPL_last_4_quartals %>%
      group_by(cut_off_date) %>% 
      summarise(Net= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    
    # External Data Wrangling
    # Data Wrangling
    # Total amount DPD 90+
    external_DPD_90 <- sqllab_external_clean %>%
      filter(bucket == "05. 90+ DPD") %>% 
      group_by(cut_off_date, island) %>% 
      summarise(DPD_90 = sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling 2
    # Total Outstanding 2
    external_total_outstanding2 <- sqllab_external_clean %>%
      group_by(cut_off_date, island) %>% 
      summarise(total_outstanding_amount= sum(outstanding_amount)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date)) %>% 
      mutate(
        date = day(cut_off_date)
      )
    
    # Data Wrangling: External NPL for each island & cut off date
    external_NPL_last_4_quartals <- external_DPD_90 %>% 
      inner_join(external_total_outstanding2, by = c("cut_off_date", "island")) %>% 
      mutate(
        External = round((DPD_90 / total_outstanding_amount * 100),2),
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      select (-c(date.x, date.y)) %>% 
      arrange(desc(cut_off_date)) %>%
      filter(cut_off_date <= input$input_cutoffdate)
    
    # National External NPL value for each cut off date
    external_NPL_last_4_quartals <- external_NPL_last_4_quartals %>%
      group_by(cut_off_date) %>% 
      summarise(External= round((sum(DPD_90)/sum(total_outstanding_amount)*100),2)) %>%
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    # Merging Gross, Net, and External national NPL values into one dataset
    GNE_NPL_last_4_quartals <- gross_NPL_last_4_quartals %>%
      inner_join(net_NPL_last_4_quartals, by = c("cut_off_date")) %>%
      inner_join(external_NPL_last_4_quartals, by = c("cut_off_date")) %>% 
      mutate(
        Date = factor(
          format(cut_off_date, "%d %b %Y"),
          levels = format(sort(unique(cut_off_date)),"%d %b %Y"))
      ) %>%
      arrange(desc(cut_off_date))  
    
    # Adding date, month, and year column
    GNE_NPL_last_4_quartals <- GNE_NPL_last_4_quartals %>% 
      mutate(
        date = day(cut_off_date),
        month = floor_date(cut_off_date, "month"),
        year = year(cut_off_date)
      ) %>% 
      arrange(desc(cut_off_date))
    
    # Function to determine Q1–Q4: Quarter
    quartal <- function(x)
    {
      if(x == 3)
      {
        z <- "Q1" # returns "Q1" if the value of x is 3 (March)
      }
      else if(x == 6)
      {
        z <- "Q2" # returns "Q2" if the value of x is 6 (June)
      }
      else if(x == 9)
      {
        z <- "Q3" # returns "Q3" if the value of x is 9 (September)
      }
      else
      {
        z <- "Q4" # returns "Q4" if the value of x is 12 (December)
      }
    }
    
    # Applying the Quartal function to extract only the most recent date from each quarter
    GNE_NPL_last_4_quartals <- GNE_NPL_last_4_quartals %>% 
      mutate(
        month_num = month(cut_off_date),
        quartal = sapply(month_num, quartal)
      ) %>% 
      filter(cut_off_date <= "2025-11-01") %>% 
      filter(
        month_num %in% c("3", "6", "9", "12"),
      ) %>% 
      group_by(quartal) %>% 
      filter(year == max(year)) %>% 
      group_by(month_num) %>% 
      filter(date == max(date)) %>% 
      ungroup() %>% 
      arrange(desc(cut_off_date))
    
    # Pivot and adding NPL type column 
    GNE_NPL_last_4_quartals <- GNE_NPL_last_4_quartals %>% 
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "NPL_type",
        values_to = "NPL"
      ) %>% 
      mutate(NPL_type = factor(NPL_type, levels = c("Gross", "Net", "External"))) 
    
    # tooltip
    GNE_NPL_last_4_quartals <- GNE_NPL_last_4_quartals %>% 
      mutate(
        label = glue(
          "NPL: {number(NPL,  big.mark = '.', decimal.mark = ',', accuracy = 0.01)}%
      Cut Off Date: {format(as.Date(cut_off_date), '%d %b %Y')}
      Quarter: {quartal}"
        )
      )
    
    # PLOT 1.8: Last 4 Quartals NPL Position by Type
    plot1.8 <- ggplot(GNE_NPL_last_4_quartals, aes(x = reorder(quartal, month), 
                                                   y = NPL,
                                                   color = NPL_type,
                                                   group = NPL_type,
                                                   text = label)) +
      
      geom_point() +
      geom_line() +
      geom_text(aes(y = ifelse(NPL_type == "External", NPL - 1.5, NPL + 1.5),
                    label = label_number(big.mark = ".",
                                         decimal.mark = ",", 
                                         accuracy = 0.01)
                    (NPL)),
                size = 3) +
      scale_y_continuous(
        limits = c(0, max(GNE_NPL_last_4_quartals$NPL) + 10),
        breaks = seq(0,100,5)
      ) +
      scale_color_manual(values = c(
        "Gross" = "darkgreen",
        "Net" = "red",
        "External" = "blue"),
        name = NULL) +
      labs(title = "Last 4 Quartals NPL Position by Type",
           x = "Cut Off Date",
           y = "NPL (%)",
           color = "NPL Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot1.8, tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        y = -0.3
      ))
  })
  
  # Tab 2: DPD Bucket
  output$plot7_bar <- renderPlotly({
    # Gross: Outstanding amount for "02. DPD 1-30" bucket
    DPD_bucket_by_amount02_gross <- sqllab_gross_clean %>% 
      filter(bucket == "02. 1-30 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Net: Outstanding amount for "02. DPD 1-30" bucket
    DPD_bucket_by_amount02_net <- sqllab_net_clean %>% 
      filter(bucket == "02. 1-30 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # External: Outstanding amount for "02. DPD 1-30" bucket
    DPD_bucket_by_amount02_external <- sqllab_external_clean %>% 
      filter(bucket == "02. 1-30 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "02. DPD 1-30" bucket outstanding amounts into one dataset
    DPD_bucket_by_amount02 <- DPD_bucket_by_amount02_gross %>% 
      inner_join(DPD_bucket_by_amount02_net, by = "island") %>% 
      inner_join(DPD_bucket_by_amount02_external, by = "island") %>% 
      rename(
        Gross = outstanding_amount.x,
        Net = outstanding_amount.y,
        External = outstanding_amount
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_amount02 <- DPD_bucket_by_amount02 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_OS"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_amount02 <- DPD_bucket_by_amount02 %>%
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(Total_OS, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.1: DPD Bucket by Amount (02. 1-30 DPD)
    plot2.1 <- ggplot(DPD_bucket_by_amount02, aes(x = island, 
                                                  y = Total_OS,
                                                  text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_OS + 2.5e9,
                    label = format(floor(Total_OS/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B", 
                                               accuracy = 1)) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 1-30",
           x = "Island",
           y = "Total Outstanding") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.1, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 1-30",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",
                      y = -0.3)
        )
  })
  
  output$plot8_bar <- renderPlotly({
    # Gross: Outstanding amount for "03. DPD 31-60" bucket
    DPD_bucket_by_amount03_gross <- sqllab_gross_clean %>% 
      filter(bucket == "03. 31-60 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Net: Outstanding amount for "03. DPD 31-60" bucket
    DPD_bucket_by_amount03_net <- sqllab_net_clean %>% 
      filter(bucket == "03. 31-60 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # External: Outstanding amount for "03. DPD 31-60" bucket
    DPD_bucket_by_amount03_external <- sqllab_external_clean %>% 
      filter(bucket == "03. 31-60 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "03. DPD 31-60" bucket outstanding amounts into one dataset
    DPD_bucket_by_amount03 <- DPD_bucket_by_amount03_gross %>% 
      inner_join(DPD_bucket_by_amount03_net, by = "island") %>% 
      inner_join(DPD_bucket_by_amount03_external, by = "island") %>% 
      rename(
        Gross = outstanding_amount.x,
        Net = outstanding_amount.y,
        External = outstanding_amount
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_amount03 <- DPD_bucket_by_amount03 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_OS"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_amount03 <- DPD_bucket_by_amount03 %>%
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(Total_OS, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.2: DPD Bucket by Amount (03. 31-60 DPD)
    plot2.2 <- ggplot(DPD_bucket_by_amount03, aes(x = island, 
                                                  y = Total_OS,
                                                  text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_OS + 2.5e9,
                    label = format(floor(Total_OS/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B", 
                                               accuracy = 1)) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 31-60",
           x = "Island",
           y = "Total Outstanding") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.2, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 31-60",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",
                      y = -0.3))
  })
  
  output$plot9_bar <- renderPlotly({
    # Gross: Outstanding amount for "04. DPD 61-90" bucket
    DPD_bucket_by_amount04_gross <- sqllab_gross_clean %>% 
      filter(bucket == "04. 61-90 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Net: Outstanding amount for "04. DPD 61-90" bucket
    DPD_bucket_by_amount04_net <- sqllab_net_clean %>% 
      filter(bucket == "04. 61-90 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # External: Outstanding amount for "04. DPD 61-90" bucket
    DPD_bucket_by_amount04_external <- sqllab_external_clean %>% 
      filter(bucket == "04. 61-90 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "04. DPD 61-90" bucket outstanding amounts into one dataset
    DPD_bucket_by_amount04 <- DPD_bucket_by_amount04_gross %>% 
      inner_join(DPD_bucket_by_amount04_net, by = "island") %>% 
      inner_join(DPD_bucket_by_amount04_external, by = "island") %>% 
      rename(
        Gross = outstanding_amount.x,
        Net = outstanding_amount.y,
        External = outstanding_amount
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_amount04 <- DPD_bucket_by_amount04 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_OS"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_amount04 <- DPD_bucket_by_amount04 %>%
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(Total_OS, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.3: DPD Bucket by Amount (04. 61-90 DPD)
    plot2.3 <- ggplot(DPD_bucket_by_amount04, aes(x = island, 
                                                  y = Total_OS,
                                                  text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_OS + 2.5e9,
                    label = format(floor(Total_OS/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B", 
                                               accuracy = 1)) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 61-90",
           x = "Island",
           y = "Total Outstanding") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.3, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 61-90",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",
                      y = -0.3))
  })
  
  output$plot10_bar <- renderPlotly({
    # Gross: Outstanding amount for "05. 90+ DPD" bucket
    DPD_bucket_by_amount05_gross <- sqllab_gross_clean %>% 
      filter(bucket == "05. 90+ DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Net: Outstanding amount for "05. 90+ DPD" bucket
    DPD_bucket_by_amount05_net <- sqllab_net_clean %>% 
      filter(bucket == "05. 90+ DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # External: Outstanding amount for "05. 90+ DPD" bucket
    DPD_bucket_by_amount05_external <- sqllab_external_clean %>% 
      filter(bucket == "05. 90+ DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(outstanding_amount = sum(outstanding_amount)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "05. 90+ DPD" bucket outstanding amounts into one dataset
    DPD_bucket_by_amount05 <- DPD_bucket_by_amount05_gross %>% 
      inner_join(DPD_bucket_by_amount05_net, by = "island") %>% 
      inner_join(DPD_bucket_by_amount05_external, by = "island") %>% 
      rename(
        Gross = outstanding_amount.x,
        Net = outstanding_amount.y,
        External = outstanding_amount
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_amount05 <- DPD_bucket_by_amount05 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_OS"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_amount05 <- DPD_bucket_by_amount05 %>%
      mutate(
        label = glue(
          "Total Oustanding:IDR {number(Total_OS, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.4: DPD Bucket by Amount (05. 90+ DPD)
    plot2.4 <- ggplot(DPD_bucket_by_amount05, aes(x = island, 
                                                  y = Total_OS,
                                                  text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_OS + 2.5e9,
                    label = format(floor(Total_OS/1e8)/10,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 0.9),
                size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-9, 
                                               suffix = "B", 
                                               accuracy = 1)) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 90+",
           x = "Island",
           y = "Total Outstanding") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.4, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 90+",
        "<br><sup>in billion Rupiah</sup>")),
        legend = list(orientation = "h",
                      y = -0.3))
  })
  
  output$plot11_bar <- renderPlotly({
    # Gross: NOA for "02. DPD 1-30" bucket
    DPD_bucket_by_accounts02_gross <- sqllab_gross_clean %>% 
      filter(bucket == "02. 1-30 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Net: NOA for "02. DPD 1-30" bucket
    DPD_bucket_by_accounts02_net <- sqllab_net_clean %>% 
      filter(bucket == "02. 1-30 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa))%>% 
      ungroup()
    
    # External: NOA for "02. DPD 1-30" bucket
    DPD_bucket_by_accounts02_external <- sqllab_external_clean %>% 
      filter(bucket == "02. 1-30 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "02. DPD 1-30" bucket NOA into one dataset
    DPD_bucket_by_accounts02 <- DPD_bucket_by_accounts02_gross %>% 
      inner_join(DPD_bucket_by_accounts02_net, by = "island") %>% 
      inner_join(DPD_bucket_by_accounts02_external, by = "island") %>% 
      rename(
        Gross = total_noa.x,
        Net = total_noa.y,
        External = total_noa
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_accounts02 <- DPD_bucket_by_accounts02 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_accounts"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_accounts02 <- DPD_bucket_by_accounts02 %>%
      mutate(
        label = glue(
          "Total accounts: {number(Total_accounts, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.5: DPD Bucket by accounts (02. 1-30 DPD)
    plot2.5 <- ggplot(DPD_bucket_by_accounts02, aes(x = island, 
                                                    y = Total_accounts,
                                                    text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_accounts + 500,
                    label = format(Total_accounts,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 2.5) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 1-30",
           x = "Island",
           y = "Total NOA") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.5, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 1-30")),
        legend = list(orientation = "h",
                      y = -0.3))
  })
  
  output$plot12_bar <- renderPlotly({
    # Gross: NOA for "03. 31-60 DPD" bucket
    DPD_bucket_by_accounts03_gross <- sqllab_gross_clean %>% 
      filter(bucket == "03. 31-60 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Net: NOA for "03. 31-60 DPD" bucket
    DPD_bucket_by_accounts03_net <- sqllab_net_clean %>% 
      filter(bucket == "03. 31-60 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa))%>% 
      ungroup()
    
    # External: NOA for "03. 31-60 DPD" bucket
    DPD_bucket_by_accounts03_external <- sqllab_external_clean %>% 
      filter(bucket == "03. 31-60 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "03. 31-60 DPD" bucket NOA into one dataset
    DPD_bucket_by_accounts03 <- DPD_bucket_by_accounts03_gross %>% 
      inner_join(DPD_bucket_by_accounts03_net, by = "island") %>% 
      inner_join(DPD_bucket_by_accounts03_external, by = "island") %>% 
      rename(
        Gross = total_noa.x,
        Net = total_noa.y,
        External = total_noa
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_accounts03 <- DPD_bucket_by_accounts03 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_accounts"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_accounts03 <- DPD_bucket_by_accounts03 %>%
      mutate(
        label = glue(
          "Total accounts: {number(Total_accounts, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.6: DPD Bucket by accounts (03. 31-60 DPD)
    plot2.6 <- ggplot(DPD_bucket_by_accounts03, aes(x = island, 
                                                    y = Total_accounts,
                                                    text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_accounts + 500,
                    label = format(Total_accounts,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 2.5) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 31-60",
           x = "Island",
           y = "Total NOA") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.6, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 31-60")),
        legend = list(orientation = "h",
                      y = -0.3))
  })
  
  output$plot13_bar <- renderPlotly({
    # Gross: NOA for "04. 61-90 DPD" bucket
    DPD_bucket_by_accounts04_gross <- sqllab_gross_clean %>% 
      filter(bucket == "04. 61-90 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Net: NOA for "04. 61-90 DPD" bucket
    DPD_bucket_by_accounts04_net <- sqllab_net_clean %>% 
      filter(bucket == "04. 61-90 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa))%>% 
      ungroup()
    
    # External: NOA for "04. 61-90 DPD" bucket
    DPD_bucket_by_accounts04_external <- sqllab_external_clean %>% 
      filter(bucket == "04. 61-90 DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "04. 61-90 DPD" bucket NOA into one dataset
    DPD_bucket_by_accounts04 <- DPD_bucket_by_accounts04_gross %>% 
      inner_join(DPD_bucket_by_accounts04_net, by = "island") %>% 
      inner_join(DPD_bucket_by_accounts04_external, by = "island") %>% 
      rename(
        Gross = total_noa.x,
        Net = total_noa.y,
        External = total_noa
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_accounts04 <- DPD_bucket_by_accounts04 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_accounts"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_accounts04 <- DPD_bucket_by_accounts04 %>%
      mutate(
        label = glue(
          "Total accounts: {number(Total_accounts, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.7: DPD Bucket by accounts (04. 61-90 DPD)
    plot2.7<- ggplot(DPD_bucket_by_accounts04, aes(x = island, 
                                                   y = Total_accounts,
                                                   text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_accounts + 500,
                    label = format(Total_accounts,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 2.5) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 61-90",
           x = "Island",
           y = "Total NOA") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.7, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 61-90")),
        legend = list(orientation = "h",
                      y = -0.3))
  })
  
  output$plot14_bar <- renderPlotly({
    # Gross: NOA for "05. 90+ DPD" bucket
    DPD_bucket_by_accounts05_gross <- sqllab_gross_clean %>% 
      filter(bucket == "05. 90+ DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Net: NOA for "05. 90+ DPD" bucket
    DPD_bucket_by_accounts05_net <- sqllab_net_clean %>% 
      filter(bucket == "05. 90+ DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa))%>% 
      ungroup()
    
    # External: NOA for "05. 90+ DPD" bucket
    DPD_bucket_by_accounts05_external <- sqllab_external_clean %>% 
      filter(bucket == "05. 90+ DPD",
             cut_off_date == input$input_cutoffdate2) %>% 
      group_by(island) %>% 
      summarise(total_noa = sum(noa)) %>% 
      ungroup()
    
    # Merging Gross, Net, and External "05. 90+ DPD" bucket NOA into one dataset
    DPD_bucket_by_accounts05 <- DPD_bucket_by_accounts05_gross %>% 
      inner_join(DPD_bucket_by_accounts05_net, by = "island") %>% 
      inner_join(DPD_bucket_by_accounts05_external, by = "island") %>% 
      rename(
        Gross = total_noa.x,
        Net = total_noa.y,
        External = total_noa
      )
    
    # Pivot and adding NPL type column 
    DPD_bucket_by_accounts05 <- DPD_bucket_by_accounts05 %>%
      pivot_longer(
        cols = c(Gross, Net, External),
        names_to = "Type",
        values_to = "Total_accounts"
      ) %>% 
      mutate(Type = factor(Type, levels = c("Gross", "Net", "External")))
    
    # tooltip
    DPD_bucket_by_accounts05 <- DPD_bucket_by_accounts05 %>%
      mutate(
        label = glue(
          "Total accounts: {number(Total_accounts, big.mark = '.', decimal.mark = ',', accuracy = 1)}"
        )
      )
    
    # PLOT 2.8: DPD Bucket by accounts (05. 90+ DPD)
    plot2.8 <- ggplot(DPD_bucket_by_accounts05, aes(x = island, 
                                                    y = Total_accounts,
                                                    text = label)) +
      
      geom_col(position = position_dodge(width = 0.9),
               aes(fill = Type)) +
      geom_text(aes(y = Total_accounts + 750,
                    label = format(Total_accounts,
                                   big.mark = ".",
                                   decimal.mark = ","),
                    group = Type),
                position = position_dodge(width = 1),
                size = 2.5) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      scale_fill_manual(values = c("darkgreen", "red","blue"),
                        name = NULL) +
      labs(title = "DPD 90+",
           x = "Island",
           y = "Total NOA") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(plot2.8, tooltip = "text") %>% 
      layout(title = list(text = paste0(
        "DPD 90+")),
        legend = list(orientation = "h",
                      y = -0.3)
        )
  })
  
  # Tab 3: NPL Detail
  output$table1 <- DT::renderDT({
    # ISLAND: SPECIFIC ~> 2 inputs: Cut Off Date & Island
    island_table <- sqllab_net_clean %>% 
      filter(cut_off_date == input$input_cutoffdate3) 
    
    if(input$input_island!= "All")
    {
      island_table <- island_table %>% 
        filter(island == input$input_island)
    }
    
    island_table <- island_table %>% 
      group_by(island) %>% 
      summarise(
        OS_Current = sum(ifelse(bucket == "01. Current", outstanding_amount, 0)),
        NOA_Current = sum(ifelse(bucket == "01. Current", noa, 0)),
        OS_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", outstanding_amount, 0)),
        NOA_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", noa, 0)),
        OS_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", outstanding_amount, 0)),
        NOA_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", noa, 0)),
        OS_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", outstanding_amount, 0)),
        NOA_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", noa, 0)),
        OS_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", outstanding_amount, 0)),
        NOA_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", noa, 0)),
        OS_Total = sum(outstanding_amount),
        NOA_Total = sum(noa),
        NPL = round(OS_DPD_90_plus / OS_Total, 4)) %>% 
      rename(
        Island = island
      )
    
    DT::datatable(island_table,
                  options = list(
                    scrollX = TRUE
                  )) %>% 
      DT::formatCurrency(c("OS_Current", "NOA_Current", "OS_DPD_1_30", "NOA_DPD_1_30", "OS_DPD_31_60", "NOA_DPD_31_60", "OS_DPD_61_90", "NOA_DPD_61_90", "OS_DPD_90_plus", "NOA_DPD_90_plus", "OS_Total", "NOA_Total"), # column 
                         currency = "", # currency symbol
                         digits = 0, # digits behind comma
                         mark = ".", # thousand separator
                         dec.mark = ",") %>% # decimal separator
      DT::formatPercentage("NPL", digits = 2, dec.mark = ",")
  })
  
  output$table2 <- DT:: renderDT({
    # REGION: SPECIFIC ~> 2 inputs: Cut Off Date & Region
    region_table <- sqllab_net_clean %>%
      filter(cut_off_date == input$input_cutoffdate3) 
    
    if(input$input_island != "All")
    {
      region_table <- region_table %>% 
        filter(island == input$input_island)
      
      if(input$input_region != "All")
      {
        region_table <- region_table %>% 
          filter(region == input$input_region) 
      }
    }
    
    region_table <- region_table %>% 
      group_by(region) %>% 
      summarise(
        OS_Current = sum(ifelse(bucket == "01. Current", outstanding_amount, 0)),
        NOA_Current = sum(ifelse(bucket == "01. Current", noa, 0)),
        OS_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", outstanding_amount, 0)),
        NOA_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", noa, 0)),
        OS_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", outstanding_amount, 0)),
        NOA_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", noa, 0)),
        OS_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", outstanding_amount, 0)),
        NOA_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", noa, 0)),
        OS_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", outstanding_amount, 0)),
        NOA_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", noa, 0)),
        OS_Total = sum(outstanding_amount),
        NOA_Total = sum(noa),
        NPL = round(OS_DPD_90_plus / OS_Total, 4)) %>% 
      rename(
        Region = region
      )  
    
    DT::datatable(region_table,
                  options = list(
                    scrollX = TRUE
                  )) %>% 
      DT::formatCurrency(c("OS_Current", "NOA_Current", "OS_DPD_1_30", "NOA_DPD_1_30", "OS_DPD_31_60", "NOA_DPD_31_60", "OS_DPD_61_90", "NOA_DPD_61_90", "OS_DPD_90_plus", "NOA_DPD_90_plus", "OS_Total", "NOA_Total"), # column 
                         currency = "", # currency symbol
                         digits = 0, # digits behind comma
                         mark = ".", # thousand separator
                         dec.mark = ",") %>%  # decimal separator
      DT::formatPercentage("NPL", digits = 2, dec.mark = ",")
    
  })
  
  output$table3 <- DT::renderDT({
    # Area: SPECIFIC ~> 2 inputs: Cut Off Date & Region
    area_table <- sqllab_net_clean %>% 
      filter(cut_off_date == input$input_cutoffdate3)
    
    if(input$input_island != "All")
    {
      area_table <- area_table %>% 
        filter(island == input$input_island)
      
      if(input$input_region != "All")
      {
        area_table <- area_table %>% 
          filter(region == input$input_region) 
      }
    }
    
    area_table <- area_table %>%
      group_by(area) %>% 
      summarise(
        OS_Current = sum(ifelse(bucket == "01. Current", outstanding_amount, 0)),
        NOA_Current = sum(ifelse(bucket == "01. Current", noa, 0)),
        OS_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", outstanding_amount, 0)),
        NOA_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", noa, 0)),
        OS_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", outstanding_amount, 0)),
        NOA_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", noa, 0)),
        OS_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", outstanding_amount, 0)),
        NOA_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", noa, 0)),
        OS_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", outstanding_amount, 0)),
        NOA_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", noa, 0)),
        OS_Total = sum(outstanding_amount),
        NOA_Total = sum(noa),
        NPL = round(OS_DPD_90_plus / OS_Total, 4)) %>% 
      rename(
        Area = area
      )  
    
    DT::datatable(area_table,
                  options = list(
                    scrollX = TRUE
                  )) %>% 
      DT::formatCurrency(c("OS_Current", "NOA_Current", "OS_DPD_1_30", "NOA_DPD_1_30", "OS_DPD_31_60", "NOA_DPD_31_60", "OS_DPD_61_90", "NOA_DPD_61_90", "OS_DPD_90_plus", "NOA_DPD_90_plus", "OS_Total", "NOA_Total"), # column 
                         currency = "", # currency symbol
                         digits = 0, # digits behind comma
                         mark = ".", # thousand separator
                         dec.mark = ",") %>%  # decimal separator
      DT::formatPercentage("NPL", digits = 2, dec.mark = ",")
    
  })
  
  output$table4 <- DT::renderDT({
    # BRANCH: SPECIFIC ~> 3 inputs: Cut Off Date & Region, and Area
    branch_table <- sqllab_net_clean %>%
      filter(cut_off_date == input$input_cutoffdate3)
    
    if(input$input_island != "All")
    {
      branch_table <- branch_table %>% 
        filter(island == input$input_island)
      
      if(input$input_region != "All")
      {
        branch_table <- branch_table %>% 
          filter(region == input$input_region)
        
        if(input$input_area != "All")
        {
          branch_table <- branch_table %>% 
            filter(region == input$input_region,
                   area == input$input_area)
        }
      }
    }
    
    
    branch_table <- branch_table %>% 
      group_by(area, branch) %>% 
      summarise(
        OS_Current = sum(ifelse(bucket == "01. Current", outstanding_amount, 0)),
        NOA_Current = sum(ifelse(bucket == "01. Current", noa, 0)),
        OS_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", outstanding_amount, 0)),
        NOA_DPD_1_30 = sum(ifelse(bucket == "02. 1-30 DPD", noa, 0)),
        OS_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", outstanding_amount, 0)),
        NOA_DPD_31_60 = sum(ifelse(bucket == "03. 31-60 DPD", noa, 0)),
        OS_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", outstanding_amount, 0)),
        NOA_DPD_61_90 = sum(ifelse(bucket == "04. 61-90 DPD", noa, 0)),
        OS_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", outstanding_amount, 0)),
        NOA_DPD_90_plus = sum(ifelse(bucket == "05. 90+ DPD", noa, 0)),
        OS_Total = sum(outstanding_amount),
        NOA_Total = sum(noa),
        NPL = round(OS_DPD_90_plus / OS_Total, 4)) %>% 
      rename(
        Area = area,
        Branch = branch
      )  
    
    DT::datatable(branch_table,
                  options = list(
                    scrollX = TRUE
                  )) %>% 
      DT::formatCurrency(c("OS_Current", "NOA_Current", "OS_DPD_1_30", "NOA_DPD_1_30", "OS_DPD_31_60", "NOA_DPD_31_60", "OS_DPD_61_90", "NOA_DPD_61_90", "OS_DPD_90_plus", "NOA_DPD_90_plus", "OS_Total", "NOA_Total"), # column 
                         currency = "", # currency symbol
                         digits = 0, # digits behind comma
                         mark = ".", # thousand separator
                         dec.mark = ",") %>%  # decimal separator
      DT::formatPercentage("NPL", digits = 2, dec.mark = ",")
    
  })
  
  
  # Tab 4: Dataset
  output$dataset_net <- DT::renderDT(
    sqllab_net_clean %>% 
      select(-c(date, month, year)),
    options = list(scrollX = TRUE,  # Horizontal scroll
                   scrollY = TRUE)  # Vertical scroll
  )
  
  output$dataset_gross <- DT::renderDT(
    sqllab_gross_clean %>% 
      select(-c(date, month, year)),
    options = list(scrollX = TRUE,  # Horizontal scroll
                   scrollY = TRUE)  # Vertical scroll
  )
  
  output$dataset_external <- DT::renderDT(
    sqllab_external_clean %>% 
      select(-c(date, month, year)),
    options = list(scrollX = TRUE,  # Horizontal scroll
                   scrollY = TRUE)  # Vertical scroll
  )
  
  
}
