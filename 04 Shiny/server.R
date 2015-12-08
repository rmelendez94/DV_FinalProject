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
    
    Lower_Tail_Percent = input$LowerTail     
    Upper_Tail_Percent = input$UpperTail
    
    if (input$OutcomeSelectionFilter == 1)
      ofilter = 'Lower Tail'
    else if (input$OutcomeSelectionFilter == 2)
      ofilter = 'Upper Tail'
    else
      ofilter = 'All'
  
    if(ofilter == 'All')
      sdf <- df %>% mutate(DURATIONPERCENT = ntile(DURATION, 100)) %>% arrange(DURATIONPERCENT) %>% select(POUTCOME, DURATION, Y, DURATIONPERCENT) 
    #View(pd) # Uncomment to view the results
    if(ofilter == 'Lower Tail')
      #This shows below the 20th percentile for call duration during the Marketing campaign 
      sdf <- df %>% mutate(DURATIONPERCENT = ntile(DURATION, 100)) %>% arrange(DURATIONPERCENT) %>% select(POUTCOME, DURATION, Y, DURATIONPERCENT) %>% filter(DURATIONPERCENT < Lower_Tail_Percent) 
    #View(npd) # Uncomment to view the results
    if(ofilter == 'Upper Tail')
      #This shows above the 80th percentile for call duration during the Marketing campaign 
      sdf <- df %>% mutate(DURATIONPERCENT = ntile(DURATION, 100)) %>% arrange(DURATIONPERCENT) %>% select(POUTCOME, DURATION, Y, DURATIONPERCENT) %>% filter(DURATIONPERCENT > Upper_Tail_Percent) 
    
    
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_discrete() +
      facet_grid(Y~.) +
      labs(title='Portuguese Bank Marketing Campaign Effectiveness') +
      labs(x="Duration", y=paste("Past Marketing Effectiveness")) +
      layer(data=sdf, 
            mapping=aes(x=as.numeric(as.character(DURATION)), y=as.character(POUTCOME), color=Y), 
            stat="identity",
            stat_params=list(),
            geom="point",
            geom_params=list(alpha=.8), 
            position=position_jitter(width=0, height=0.3)
      )
    # End your code here.
    return(plot1)
  })
  
  output$barPlot <- renderPlot({
    # Start your code here.
    # Here is the bar chart
    
    plottitle = "Portuguese Bank Marketing Campaign Effectiveness\nBar Chart Blended:"
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
   
    
    
    if (input$BarchartSorting == 1) {
      ndfbl <- spread(dfbl, MEASURE_NAMES, MEASURE_VALUES) %>% arrange(desc(AVERAGE_SALARY))
      # Creates an ordered column of job type to be used for ordering in ggplot
      ndfbl$ORDERED_JOBS <- reorder(ndfbl$JOB_NAME, ndfbl$AVERAGE_SALARY)}
    else if (input$BarchartSorting == 2) {
      ndfbl <- spread(dfbl, MEASURE_NAMES, MEASURE_VALUES) %>% arrange(desc(CAMPAIGN))
      # Creates an ordered column of job type to be used for ordering in ggplot
      ndfbl$ORDERED_JOBS <- reorder(ndfbl$JOB_NAME, ndfbl$CAMPAIGN)}

    #dfb <- inner_join(dfb, dfb1, by="POUTCOME")
    
    #spread(dfb, Y, AVG_CAMPAIGN) %>% View
    
    plot2 <- ggplot() +
      coord_cartesian() +
      scale_x_discrete() +
      scale_y_continuous(limits = c(0,100000)) +
      scale_fill_gradient(low = "grey90", high = "darkgreen", na.value = "grey90", guide = "colourbar") +
      labs(title = 'Portuguese Bank Marketing Campaign Effectiveness\nBlending') +
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
