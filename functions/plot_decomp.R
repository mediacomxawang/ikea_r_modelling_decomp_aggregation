plot_decomp <- function(decomp_output, tbl_cico, roll_by = "original", mapping_tbl, 
                        grouping_column, FY_start = 4, pct = FALSE, gridlines = FALSE, yaxis_title = "Quotes") {
  
  decomp_df <- decomp_group_by_custom_dates(decomp_output, tbl_cico, roll_by, FY_start) %>% 
    decomp_group_by_grouping_column(mapping_tbl, grouping_column) %>% 
    rename(group = 2) %>% 
    mutate(group = toupper(group)) %>% 
    group_by(date, group) %>% 
    summarise(contributions = sum(contributions)) %>% 
    ungroup() %>% 
    group_by(date) %>% 
    mutate(total = sum(contributions)) %>% 
    ungroup() %>% 
    mutate(contributions_pct = round((contributions/total)*100, 2)) 
    
  # if (roll_by == "original") {
  #   
  #   kpi_var <- decomp_output@KPIData %>% 
  #     mutate(y = exp(y))
  #   
  #   decomp_df <- decomp_df %>% 
  #     left_join(kpi_var, by = "date") %>% 
  #     mutate(date = as.Date(date))
  # }
  
  # convert to 100% stack bars if pct == TRUE
  if (pct == TRUE) {
    decomp_df <- decomp_df %>% 
      group_by(date) %>% 
      mutate(total = sum(contributions)) %>% 
      ungroup() %>% 
      mutate(contributions = round((contributions/total)*100, 2))
  }
  
  halo_vars <- unique(decomp_df$group)[grepl("HALO", unique(decomp_df$group))]
  
  if (length(halo_vars) == 0) {
    halo_vars <- "halo vars"
  }
  
  if (length(halo_vars) > 0) {
    halo_var_colours <- c("#3bb392", "#622569", "#96ceb4", "#ffcc5c")[1:length(halo_vars)]
  }
  
  media_vars <- unique(decomp_df$group)[grepl("MEDIA$", unique(decomp_df$group))]
  
  if (length(media_vars) == 0) {
    media_vars <- "media vars"
  }
  
  media_var_colours <- c("#a2c617", "#5d39e8")[1:length(media_vars)]
  
  if (grouping_column == "Group_StackedBar") {
    
    order_of_colours <- c("#363061", #base 
                          "#97305f", #brand
                          "#fff381", #covid
                          "#e1005d", #market/shopping 
                          "#3487bb", #sq2q
                          "#4a4a49", #abr
                          "#7d510d", #pcw market dynamic
                          #"#a2c617", #home/range media
                          media_var_colours,
                          "#ee7707", #masterbrand
                          #"#3bb392", #halo vars
                          halo_var_colours,
                          "#870038", #competitors
                          "#D9D9D9", #premium
                          "#D9D9D9", #price
                          "#D9D9D9", #sales
                          "#3487bb", #start quotes
                          "#D9D9D9", #visits
                          "#3487bb", #vsq
                          "#D9D9D9" # quotes
    )
    
    order_of_vars <- c("BASE", "BRAND", "COVID", "MARKET/SHOPPING", "SQ2Q", 
                       "ABR", "PCW MARKET DYNAMIC", media_vars, "MASTERBRAND", halo_vars, "COMPETITORS",
                       "PREMIUM", "PRICE", "SALES", "START QUOTES", "VISITS", "VSQ", "QUOTES")
    
    col_table <- tibble(order_of_colours, order_of_vars)
    
    decomp_df <- decomp_df %>% 
      left_join(col_table, by = c("group" = "order_of_vars")) 
    
    unmatched_groups <- decomp_df %>% 
      filter(is.na(order_of_colours)) %>% distinct(group) %>% pull()
    
    if (length(unmatched_groups) > 0) {
      stop(glue::glue("Unmapped groups found: {unmatched_groups}"))
    }
    
    decomp_df$group <- factor(decomp_df$group, levels = rev(order_of_vars))
    
  } else if (grouping_column == "Group_BaseBreakDown_MediaBreakDown_Price") {
    
    order_of_vars <- c("BASE", "SEASONALITY", "PCW RANKINGS", "PCW REJECTION RATE", 
                       "BRAND", "COVID", "MARKET", "SHOPPING", "SQ2Q", "ABR", "PCW MARKET DYNAMIC", "AFFILIATES",
                       "BRTV", "BVOD", "CINEMA", "DIGITAL AUDIO", "DM", "DRTV", 
                       "DIGITAL DISPLAY", "EMAIL", "OOH", "PPC GENERIC", "PPC BRAND", "PAID SOCIAL", 
                       "PHONE", "RADIO", "RADIO SPONSORSHIP", "TV SPONSORSHIP", "VOD", 
                       "RANGE BRTV", "RANGE BVOD", "RANGE CINEMA", "RANGE DIGITAL AUDIO",
                       "RANGE PPC BRAND", "RANGE PAID SOCIAL", "RANGE RADIO", "RANGE RADIO SPONSORSHIP", "RANGE TV SPONSORSHIP",
                       "MASTERBRAND", halo_vars, "COMPETITORS", "PCW COMPETITORS")
    
    order_of_colours <- c("#363061", #base
                          "#A9B2B9", #seasonality
                          "#262F37", #pcw rankings
                          "#FFB6C1", #pcw rejection rate
                          "#97305f", #brand
                          "#fff381", #covid
                          "#e1005d", #market
                          "#E6E6FA", #shopping
                          "#3487bb", #sq2q
                          "#4a4a49", #abr
                          "#7d510d", #PCW MARKET DYNAMIC
                          #"#a2c617", #home/range media
                          "#50394c", #affiliates
                          "#a2c617", #brtv
                          "#FA8072", #bvod
                          "#17a2c6", #cinema
                          "#c1502e", #digital audio
                          "#FF8DBC", #dm
                          "#766DB7", #drtv
                          "#8B0000", #digital display
                          "#227E70", #email
                          "#FFFF00", #ooh
                          "#66CDAA", #ppc generic
                          "#2ec175", #ppc brand
                          "#00FFFF", #paid social
                          "#e866e8", #phone
                          "#DDA0DD", #radio
                          "#708090", #radio sponsorship
                          "#4682B4", #tv sponsorship
                          "#FFC0CB", #vod
                          "#c6be17", #range brtv
                          "#fa7286", #range bvod
                          "#3b17c6", #range cinema
                          "#c1752e", #range digital audio
                          "#c12e30", #range ppc brand
                          "#00ffbf", #range paid social
                          "#a0dda0", #range radio
                          "#709090", #range radio sponsorship
                          "#907090", #range tv sponsorship
                          "#ee7707", #masterbrand
                          halo_var_colours,
                          #"#3bb392", #halo vars
                          "#870038", #competitors
                          "#D9D9D9" #pcw competitors
                          #"#D9D9D9", #premium
                          #"#D9D9D9", #price
                          #"#D9D9D9", #sales
                          #"#3487bb", #start quotes
                          #"#D9D9D9", #visits
                          #"#3487bb", #vsq
                          #"#D9D9D9" # quotes
    )
    
    col_table <- tibble(order_of_colours, order_of_vars)
    
    decomp_df <- decomp_df %>% 
      left_join(col_table, by = c("group" = "order_of_vars")) 
    
    unmatched_groups <- decomp_df %>% 
      filter(is.na(order_of_colours)) %>% distinct(group) %>% pull()
    
    if (length(unmatched_groups) > 0) {
      stop(glue::glue("Unmapped groups found: {unmatched_groups}"))
    }
    
    decomp_df$group <- factor(decomp_df$group, levels = rev(order_of_vars))
    
  }
  
  
  if (roll_by == "rolling52wks" | roll_by == "year" | roll_by == "FY"| roll_by == "rolling26wks" | roll_by == "HY") {
    
    if (pct == FALSE) {
      
      col <- as.character(decomp_df$order_of_colours)
      names(col) <- as.character(decomp_df$group)
      
      bar_totals <- decomp_df %>%
        group_by(date) %>% 
        mutate(sum_pos = ifelse(sign(contributions) == 1, contributions, 0),
               sum_neg = ifelse(sign(contributions) == -1, abs(contributions), 0),
               sum_pos = sum(sum_pos),
               sum_neg = sum(sum_neg)) %>% 
        ungroup() %>% 
        mutate(total_y = sum_pos #+ sum_neg
               ) %>% 
        distinct(date, total_y, total)
      
      decomp_df$date <- factor(decomp_df$date, levels = rev(unique(decomp_df$date)))
        
      d <- ggplot(decomp_df, aes(fill=group, y=contributions, x=date, 
                                 #label = scales::percent(contributions_pct, scale = 1, accuracy = 1))) + 
                                 label = scales::comma(round(contributions)))) + 
        geom_bar(position="stack", stat="identity")+
        geom_text(size = 3.5, position = position_stack(vjust = 0.5), color = "white")+
        geom_text(aes(date, total_y + (total_y * 0.05), label = scales::comma(round(total)), fill = NULL), 
                  data = bar_totals, vjust = -1, colour = "#595757", size = 3.5)+
        # geom_text(
        #   aes(label = after_stat(y), group = date), 
        #   stat = 'summary', fun = sum, vjust = -1
        # )+
        scale_fill_manual(values = col)+
        scale_y_continuous(labels = scales::comma, guide = guide_axis(n.dodge = 2))+
        ylab(yaxis_title)+
        theme(legend.title = element_blank(), legend.position = "bottom", 
              axis.title.y = element_text(color = "#595757"),
              axis.text.y=element_blank(),  #remove y axis labels
              axis.ticks.y=element_blank(),
              axis.ticks.x=element_blank(), #remove x axis ticks
              panel.background = element_blank(),
              legend.text=element_text(size=8, colour = "#595757")) 
      
      d <- ggplotly(d) %>% 
        layout(xaxis = list(title = ""), 
               #yaxis = list(title = "Quotes", color = "red", size = 28),
               #yaxis = list(title ='Quotes', font = list(size = 0.1, color = "red")),
               legend = list(title = "", orientation = "h"))
      
    } else {
      # output 100% stacked bar chart
      
      d <- 
      plot_ly(decomp_df, x = ~date, y = ~contributions, type = 'bar', textposition = 'inside', 
              name = ~group, color = ~group, colors = mediacom_palette,
              text = ~paste(format(contributions, digits = 2), "%", sep = ""), 
              textfont = list(color = 'black', size = 12)) %>% 
        layout(yaxis = list(title = 'Contribution', ticksuffix="%", showgrid = FALSE), barmode = 'relative')
    }
    
    
  } else {
    
    col <- as.character(decomp_df$order_of_colours)
    names(col) <- as.character(decomp_df$group)
    
    d <- ggplot(decomp_df, aes(fill=group, y=contributions, x=date)) + 
      geom_bar(position="stack", stat="identity") +
      #geom_line(aes(x = date, y = y), colour = "#4a4a49") +
      scale_fill_manual(values = col)+
      scale_x_date(date_labels = "%b-%y", date_breaks = "3 months")+
      scale_y_continuous(labels = scales::comma, guide = guide_axis(n.dodge = 2))+
      ylab(yaxis_title)+
      theme(legend.title = element_blank(), legend.position = "bottom", 
            axis.title.y = element_text(color = "#595757"),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.ticks.x=element_blank(),
            legend.text=element_text(size=8, colour = "#595757")) 
    
    d <- ggplotly(d) %>% 
      layout(xaxis = list(title = ""), 
             legend = list(title = "", orientation = "h"))
  
    
  }
  
  return(d)
  
}