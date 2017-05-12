library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(RColorBrewer)

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}
human_usd   <- function(x){human_numbers(x, smbl = "$")}

census <- read.csv('business_census_2014.txt')

census_vet <- census %>%
  filter(grepl("^54194$",NAICS)) %>%
  filter(ENTRSIZE == 1)

census_med <- census %>%
  filter(grepl("^62..$",NAICS)) %>%
  filter(ENTRSIZE == 1) %>%
  filter(!(grepl("^624",NAICS)))

census_scrub <- rbind(census_vet,census_med)

keep_naic <- unique(census_scrub$NAICS)

new_entr <- c("<20","20-99","20-99","20-99","20-99","20-99","20-99","20-99",
              "100-499","100-499","100-499","100-499","100-499","500-999","500-999",
              "1000-4,999","1000-4,999","1000-4,999","1000-4,999","5,000+")
ENTRSIZE <- c(6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26)

new_ent_df <- data.frame(new_entr, ENTRSIZE)
new_ent_df$new_entr <- factor(new_ent_df$new_entr, 
                              levels = c("<20","20-99","100-499","500-999","1000-4,999","5,000+",
                                         ordered = T))

census_scrub_all <- census %>%
  filter(NAICS %in% keep_naic) %>%
  left_join(new_ent_df, by = "ENTRSIZE") %>%
  filter(!(is.na(new_entr)))

naics_descrip <- census_scrub_all %>%
  group_by(NAICS,NAICSDSCR) %>%
  summarise()

naics_descrip$NAICS <- as.character(naics_descrip$NAICS)
naics_descrip$NAICSDSCR <- as.character(naics_descrip$NAICSDSCR)

census_scrub_mod <- census_scrub %>%
  mutate(NAICS = ifelse(NAICS %in% c("6222","6223"),"6221",
                        ifelse(NAICS == "6219", "6214",
                               ifelse(NAICS == "6239", "6232",as.character(NAICS))))) %>%
  group_by(NAICS) %>%
  summarise(FIRM = sum(FIRM),
            EMPL_N = sum(EMPL_N),
            ESTB = sum(ESTB),
            PAYR_N = sum(PAYR_N)) %>%
  left_join(naics_descrip, by ="NAICS") %>%
  mutate(NAICSDSCR = ifelse(NAICS == "6233", "Assisted Living Facilities",
                            ifelse(NAICS == "6232", "Mental Health Facilities", 
                                   ifelse(NAICS == "6231", "Skilled Nursing Facilities", NAICSDSCR)))) %>%
  mutate(new_entr = factor("Total")) %>%
  select(NAICS,new_entr,FIRM,EMPL_N,ESTB,PAYR_N,NAICSDSCR)

census_scrub_all_mod <- census_scrub_all %>%
  mutate(NAICS = ifelse(NAICS %in% c("6222","6223"),"6221",
                        ifelse(NAICS == "6219", "6214",
                               ifelse(NAICS == "6239", "6232",as.character(NAICS))))) %>%
  group_by(NAICS, new_entr) %>%
  summarise(FIRM = sum(FIRM),
            EMPL_N = sum(EMPL_N),
            ESTB = sum(ESTB),
            PAYR_N = sum(PAYR_N)) %>%
  left_join(naics_descrip, by ="NAICS")%>%
  mutate(NAICSDSCR = ifelse(NAICS == "6233", "Assisted Living Facilities",
                            ifelse(NAICS == "6232", "Mental Health Facilities", 
                                   ifelse(NAICS == "6231", "Skilled Nursing Facilities", NAICSDSCR))))


csm_final <- bind_rows(census_scrub_mod,census_scrub_all_mod)

csm_final$new_entr <- factor(csm_final$new_entr, 
                             levels = c("<20","20-99","100-499","500-999","1000-4,999","5,000+","Total",
                                        ordered = T))
csm_gt <- census_scrub_mod %>%
  summarise(Firms = human_numbers(sum(FIRM)),
            Establishments = human_numbers(sum(ESTB)),
            Employees = human_numbers(sum(EMPL_N)),
            Payroll = human_usd(sum(PAYR_N)),
            NAICS_Category = "Total")

pal <- RColorBrewer::brewer.pal(11,'Paired')

shinyServer(function(input, output) {
  
  output$bubPlot <- renderPlot({
    
    naics_filter <- input$naics_filter
    
    if(naics_filter == "All"){
      
      ggplot(census_scrub_mod, aes(x=ESTB, y=PAYR_N, fill=NAICSDSCR))+
        geom_point(aes(size=EMPL_N), alpha = .7, shape=21, color = 'gray40') +
        scale_size_continuous(range = c(8, 28), labels= human_numbers)+
        scale_fill_brewer(type = 'qual', palette=3) +
        theme_bw() +
        theme(panel.border = element_blank(),
              plot.title = element_text(size=18),
              text=element_text(family="sans", size=16),
              legend.position = "left") +
        scale_x_continuous(labels=human_numbers,limits = c(-2000,250000))+
        scale_y_continuous(labels=human_usd, limits = c(0,400000000),breaks=c(seq(0,400000000,50000000)))+
        guides(fill = guide_legend(order = 1, override.aes = list(size=7)),
               size = guide_legend(override.aes = list(shape=21,color = 'gray40', fill='gray90')))+
        labs(title='', y="Total Payroll", x="Number of Establishments", fill = "NAICS Category", size="Number of Employees")
      
    }
    else{
      
      csm_filtered <- census_scrub_mod %>%
        mutate(alph = ifelse(NAICSDSCR == naics_filter, 1, .2))
      
      naics_xy <- csm_filtered %>%
        filter(NAICSDSCR == naics_filter) %>%
        mutate(vjus = ifelse(NAICS %in% c('6211','6221'), -3, -2),
               hjus = ifelse(NAICS == '6211', 1.2, -.1))
      
      ggplot(csm_filtered, aes(x=ESTB, y=PAYR_N, fill=NAICSDSCR))+
        geom_point(aes(size=EMPL_N, alpha = alph), color = 'gray40', shape=21) +
        geom_text(data=naics_xy, aes(x=ESTB, y=PAYR_N, label = NAICSDSCR, vjust=vjus, hjust=hjus)) +
        scale_size_continuous(range = c(8, 28), labels= human_numbers)+
        scale_fill_brewer(type = 'qual', palette=3) +
        scale_alpha_continuous(range=(c(.2,1))) +
        theme_bw() +
        theme(panel.border = element_blank(),
              plot.title = element_text(size=18),
              text=element_text(family="sans", size=16),
              legend.position = "left") +
        scale_x_continuous(labels=human_numbers,limits = c(-2000,250000))+
        scale_y_continuous(labels=human_usd, limits = c(0,400000000),breaks=c(seq(0,400000000,50000000)))+
        guides(fill = guide_legend(order = 1, override.aes = list(size=7, alpha=.7)),
               size = guide_legend(override.aes = list(shape=21,color = 'gray40', fill='gray90')),
               alpha = F)+
        labs(title='', y="Total Payroll", x="Number of Establishments", fill = "NAICS Category", size="Number of Employees")
      
    }
    
  })
  
  output$dtlTable <- renderTable({
    
    naics_filter <- input$naics_filter
    
    if(naics_filter == "All"){
      
      csm1 <- census_scrub_mod %>%
        mutate(Firms = human_numbers(FIRM),
               Establishments = human_numbers(ESTB),
               Employees = human_numbers(EMPL_N),
               Payroll = human_usd(PAYR_N),
               NAICS_Category = NAICSDSCR) %>%
        arrange(desc(PAYR_N)) %>%
        select(NAICS_Category, Payroll, Employees, Firms, Establishments)
      
      rbind(csm1,csm_gt)
      
    }
    
    else{
      
      csm_final %>%
        filter(NAICSDSCR == naics_filter) %>%
        arrange(new_entr) %>%
        mutate(Firms = human_numbers(FIRM),
               Establishments = human_numbers(ESTB),
               Employees = ifelse(EMPL_N == 0, NA, human_numbers(EMPL_N)),
               Payroll = ifelse(PAYR_N == 0, NA, human_usd(PAYR_N)),
               Enterprise_Size = new_entr) %>%
        select(Enterprise_Size, Payroll, Employees, Firms, Establishments)
    }
    
  }, spacing='xs', width='530px', striped=T)
  
  output$expln <- renderText('*NA - Data withheld to avoid disclosure')
  
  output$estbPie <- renderPlotly({
    
    census_scrub_mod$color <- factor(census_scrub_mod$NAICSDSCR, labels = pal)
    
    census_scrub_mod <- census_scrub_mod %>%
      mutate(NAICSDSCR = ifelse(NAICS == '6221', 'Hospitals',
                                ifelse(NAICS == '6215', 'Diagnostic Labs', NAICSDSCR)))
    
    plot_ly(census_scrub_mod, labels = ~NAICSDSCR, values = ~ESTB, type = 'pie',
                 textposition = 'outside',
                 textinfo = 'label+value',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'percent',
                 text = ~ESTB,
                 marker = list(colors = ~color,
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(title = 'Establishment Distribution',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
    
    })
  
  output$emplPie <- renderPlotly({
    
    census_scrub_mod$color <- factor(census_scrub_mod$NAICSDSCR, labels = pal)
    
    census_scrub_mod <- census_scrub_mod %>%
      mutate(NAICSDSCR = ifelse(NAICS == '6221', 'Hospitals',
                                ifelse(NAICS == '6215', 'Diagnostic Labs', NAICSDSCR)))
    
    plot_ly(census_scrub_mod, labels = ~NAICSDSCR, values = ~EMPL_N, type = 'pie',
                  textposition = 'outside',
                  textinfo = 'label+value',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'percent',
                  text = ~ESTB,
                  marker = list(colors = ~color,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>%
      layout(title = 'Employee Distribution',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
})
