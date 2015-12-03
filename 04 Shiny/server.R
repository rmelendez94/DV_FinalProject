# server.R
require(jsonlite)
require(RCurl)
require(ggplot2)
require(dplyr)
require(tidyr)
require(shiny)
require(leaflet)
require(shinydashboard)
require(DT)

shinyServer(function(input, output) {
  
  df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BNKMKTG"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_rm46926', PASS='orcl_rm46926', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  output$scatterPlot <- renderPlot({
    # Start your code here.
    # Here is the scatter plot
    if (input$OutcomeSelectionFilter == 1)
      ofilter = 'no'
    else if (input$OutcomeSelectionFilter == 2)
      ofilter = 'yes'
    else
      ofilter = 'all'
    
    dfs <- df %>% select(DURATION, Y, CONS_PRICE_IDX) %>% filter(Y != ofilter)
    
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Portuguese Bank Marketing Campaign Effectiveness\nScatter Plot') +
      labs(x="Duration", y=paste("Consumer Price Index")) +
      layer(data=dfs, 
            mapping=aes(x=as.numeric(as.character(DURATION)), y=as.numeric(as.character(CONS_PRICE_IDX)), color=Y),
            stat="identity",
            stat_params=list(),
            geom="point",
            geom_params=list(alpha=.8), 
            position=position_jitter(width=0, height=0)
      ) +
      layer(data=dfs, 
            mapping=aes(x=as.numeric(as.character(DURATION)), y=as.numeric(as.character(CONS_PRICE_IDX)), color=Y),
            stat="smooth",
            stat_params=list(method= lm, se= FALSE),
            geom="smooth",
            geom_params=list(alpha= .8), 
            position=position_jitter(width=0, height=0)
        )
    
    # End your code here.
    return(plot1)
  })
  
  output$barPlot <- renderPlot({
    # Start your code here.
    # Here is the bar chart
    
    plottitle = "Portuguese Bank Marketing Campaign Effectiveness\nBar Chart:"
    dfb <- df %>% group_by(POUTCOME, Y) %>% summarise(AVG_CAMPAIGN = mean(CAMPAIGN))
    if (input$ReferenceLine == 1) {
      # Window Avg
      subtitle = "AVG_CAMPAIGN, WINDOW_AVG_CAMPAIGN"
      dfb1 <- dfb %>% ungroup %>% group_by(POUTCOME) %>% summarise(measure_value = mean(AVG_CAMPAIGN))}
    else if (input$ReferenceLine == 2) {
      # Window Min
      subtitle = "AVG_CAMPAIGN, WINDOW_MIN_CAMPAIGN"
      dfb1 <- dfb %>% ungroup %>% group_by(POUTCOME) %>% summarise(measure_value = min(AVG_CAMPAIGN))}
    else if (input$ReferenceLine == 3) {
      # Window Max
      subtitle = "AVG_CAMPAIGN, WINDOW_MAX_CAMPAIGN"
      dfb1 <- dfb %>% ungroup %>% group_by(POUTCOME) %>% summarise(measure_value = max(AVG_CAMPAIGN))}
    else {
      # Window Sum
      subtitle = "AVG_CAMPAIGN, WINDOW_SUM_CAMPAIGN"
      dfb1 <- dfb %>% ungroup %>% group_by(POUTCOME) %>% summarise(measure_value = sum(AVG_CAMPAIGN))}
    dfb <- inner_join(dfb, dfb1, by="POUTCOME")
    
    #spread(dfb, Y, AVG_CAMPAIGN) %>% View
    
    plot2 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~POUTCOME, ncol=1) +
      labs(title=paste(plottitle,subtitle,sep = " ")) +
      labs(x=paste("Y (OUTCOME)"), y=paste("AVG_CAMPAIGN")) +
      layer(data=dfb, 
            mapping=aes(x=Y, y=AVG_CAMPAIGN, color=Y, fill=Y), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(width=.25), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfb, 
            mapping=aes(x=Y, y=measure_value, label=round(measure_value, 4)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=1.5, size=3.5), 
            position=position_identity()
      ) +
      layer(data=dfb, 
            mapping=aes(yintercept = measure_value), 
            geom="hline",
            geom_params=list(colour="red")
      ) 
    
    
    # End your code here.
    return(plot2)
  })

  output$crosstabPlot <- renderPlot({
# Start your code here.

# Here is the Crosstab and KPI
    
KPI_Low_Max_value = input$KPI1     
KPI_Medium_Max_value = input$KPI2
    
#df %>% group_by(JOB) %>% summarize() %>% View()

dfc <- df %>% mutate(Yyes = ifelse(Y == 'yes', 1, 0), Yno = ifelse(Y == 'no', 1, 0)) %>% group_by(EDUCATION) %>% mutate(Ratio = sum(Yyes)/sum(Yno)) %>% ungroup() %>% group_by(EDUCATION, Y, HOUSING) %>% summarize(AVG_DURATION = round(mean(DURATION),1), Ratio = mean(Ratio)) %>% mutate(KPI = ifelse(Ratio <= KPI_Low_Max_value, '03 Low', ifelse(Ratio <= KPI_Medium_Max_value, '02 Medium', '01 High')))

#spread(dfc, Y, AVG_DURATION) %>% View

dfc$EDUCATION <- factor(dfc$EDUCATION, levels = c("illiterate", "basic4y", "basic6y", "basic9y", "highschool", "universitydegree", "professionalcourse", "unknown"))
    
plot3 <- ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  scale_fill_manual(values = c("green","yellow","red")) + 
  facet_grid(.~EDUCATION) + 
  labs(title='Portuguese Bank Marketing Campaign Effectiveness\nCrosstab\nAVG_DURATION') +
  labs(x=paste("EDUCATION/Y"), y=paste("HOUSING")) +
  layer(data=dfc, 
        mapping=aes(x=Y, y=HOUSING, label=AVG_DURATION), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", size=2.8), 
        position=position_identity()
  ) +
  layer(data=dfc, 
        mapping=aes(x=Y, y=HOUSING, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  )

# End your code here.
      return(plot3)
  })
  
  output$blendedPlot <- renderPlot({
    # Start your code here.
    # Here is the blended bar chart
    
    plottitle = "Portuguese Bank Marketing Campaign Effectiveness\nBlending\nAVG_SALARY:"
    dfbl <-
      data.frame(fromJSON(getURL(
        URLencode(
          gsub(
            "\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            """select JOB_TYPE as job_name, \\\'AVERAGE_SALARY\\\' as measure_names, 
            sum(AVERAGE_SALARY) as measure_values 
            from JOBTYPE
            group by JOB_TYPE
            union all
            select JOB as job_name, \\\'CAMPAIGN\\\' as measure_names, sum(CAMPAIGN) as measure_values from BNKMKTG
            group by JOB;"""'
          )
          ), httpheader = c(
            DB = 'jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER =
              'C##cs329e_rm46926', PASS = 'orcl_rm46926', MODE = 'native_mode', MODEL = 'model', returnDimensions = 'False', returnFor = 'JSON'
        ), verbose = TRUE
      ))); #View(dfbl)
    
    # Rearranges measure_names into usable columns
    ndfbl <- spread(dfbl, MEASURE_NAMES, MEASURE_VALUES) %>% arrange(desc(AVERAGE_SALARY))
    
    # Creates an ordered column of job type to be used for ordering in ggplot
    ndfbl$ORDERED_JOBS <- reorder(ndfbl$JOB_NAME, ndfbl$AVERAGE_SALARY)
    
    plot4 <- ggplot() +
      coord_cartesian() +
      scale_x_discrete() +
      scale_y_continuous(limits = c(0,100000)) +
      scale_fill_gradient(low = "grey90", high = "darkgreen", na.value = "grey90", guide = "colourbar") +
      labs(title = 'Portuguese Bank Marketing Campaign Effectiveness\nBlending\nAVG_SALARY') +
      labs(x = paste("JOB TYPE"), y = paste("AVERAGE SALARY")) +
      theme(panel.background=element_rect(fill='grey100')) +
      layer(
        data = ndfbl,
        mapping = aes(x = ORDERED_JOBS, y = AVERAGE_SALARY, fill = CAMPAIGN),
        stat = "identity",
        stat_params = list(),
        geom = "bar",
        geom_params = list(width=.5),
        position = position_identity()
      ) +
      layer(
        data = ndfbl,
        mapping = aes(
          x = ORDERED_JOBS, y = AVERAGE_SALARY, label = round(CAMPAIGN)
        ),
        stat = "identity",
        stat_params = list(),
        geom = "text",
        geom_params = list(colour = "black", hjust = -0.1),
        position = position_identity()
      ) + coord_flip() 
    
    # End your code here.
    return(plot4)
  })
})
