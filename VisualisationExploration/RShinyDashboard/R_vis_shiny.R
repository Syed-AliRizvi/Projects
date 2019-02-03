# importing required libraries
library(shiny)
library(ggplot2)
library(leaflet)
library(geosphere)
library(mapproj)
library(ggmap)
library(dplyr)
library(tidyr)
library(stringr)
library(mgcv)
library(DT)
library(tidyverse)
library(shinythemes)

##########################################################################################
# DATA READING AND CHANGING IMPORTED TYPES AND ORDER
#-----------------------------------------------------------------------------------------

#data import
em_oc <- read.csv('em_oc.csv') 
em_co <- read.csv('em_co.csv')
em_pr <- read.csv('em_pr.csv')
em_ca <- read.csv('em_ca.csv')
em_co_lat_long <- read.csv('em_co_lat_long.csv')
df <- read.csv('r_project_cleaned.csv', stringsAsFactors=FALSE)

#removing unwanted columns
em_pr <- em_pr[-1]
em_co <- em_co[-1]
em_ca <- em_ca[-1]
em_oc <- em_oc[-1]
em_co_lat_long <- em_co_lat_long[-1]

#fixing column types
df$sa_isl_month <- factor(df$sa_isl_month, levels=c('muharram', 'safar', 'rabi al-awwal', 'rabi ath-thani', 'jumada al-ula', 'jumada al-akhira', 'rajab', 'shaban', 'ramadan', 'shawal', 'dhu al-qadah', 'dhu al-hijjah'))
df$sa_Time <- factor(df$sa_Time, levels=c('Morning', 'Afternoon', 'Evening', 'Night'))
df$gr_week_day <- factor(df$gr_week_day, levels=c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
df <- df %>% select(-X)

#creating dataframes incase of download
sa <- df %>% filter(sa==1) %>%
  select(starts_with("sa_"), Date, gr_week_day, gr_day, gr_month, gr_year)
  names(sa) <- str_replace_all(names(sa), '(sa_)','')
da <- df %>% filter(da==1) %>%
  select(starts_with("da_"), Date, gr_week_day, gr_day, gr_month, gr_year)
  names(da) <- str_replace_all(names(da), '(da_)','')


#fix the lat and longs of drone data
col <- c('Longitude','Latitude')
da[which(da$Latitude > 36 & da$Longitude < 60), col] <- da %>% filter(Latitude > 36 & Longitude < 60) %>% select(Latitude, Longitude)

  
#update data to plot map data
s <- sa %>% mutate(dead=Killed.Max)  %>% select(Latitude, Longitude, dead) %>% mutate(type='Suicide')
d <- da %>% mutate(dead=Total.Died.Max)  %>% select(Latitude, Longitude, dead) %>% mutate(type='Drone')
ds <- rbind(s,d) %>% na.omit()

#plot map target type
t_df <- sa %>% select(Latitude, Longitude, Target.Type, Killed.Max)
t_df <- t_df %>% na.omit()
  
##########################################################################################
#SHINY 
#-----------------------------------------------------------------------------------------


# UI.


# Define UI for application that plots features of movies
ui <- navbarPage("Pakistan's Story",
    theme = shinytheme('slate'),
    
              
    #first tab - introduction ------------------------------------------------------------------------------
    
    tabPanel('Introduction',
             br(),
             br(),
             absolutePanel(
               id = "x", 
               fixed = FALSE,
               draggable = TRUE, 
               top = 85, 
               left = 95, 
               right = 'auto', 
               bottom = "auto",
               width = 720, 
               height = "auto",
               style = "opacity: 1; z-index: 1000;",
               HTML("<br><br><h1> September 11, 2001 </h1> <p> The September 11 attacks in New York City and Washington DC gave rise to the United States <b>War on Terror </b>. <br>
                      Due to the high impact of this mission in Afghanistan, many Taliban and Al Qaeda members alike started crossing
                    into Pakistan's northern regions through the border. <br> This caused high pressure by the US on Pakistan's government to
                    intervene and retaliate. The United States from the year 2004 started its target on militant
                    groups in the two northern regions of Pakistan, Khyber Pakhtunkhwa (KPK) and the Federally Administered Tribal Areas (FATA). <br>
                    <br><b>Although, the aim of the mission was to erradicate terrorism, the spill over affects of this war has <br>impacted the people of Pakistan more than it should've ever done; The evidence of which is 
                    <br>provided by this application in the following tabs.</b></p>")
             ),
             plotOutput(outputId = "intro_plot", hover = "plot_hover"),
             br(),
             br(),
             wellPanel(
             fluidRow(
                 column(3,offset = 3,
                        checkboxGroupInput(inputId = 'intro_filter_type',
                                           label = 'Toggle Graphs',
                                           choices = c('Suicide Attack'='Suicide', 'Drone Attack'='Drone'),
                                           selected = c('Suicide Attack'='Suicide', 'Drone Attack'='Drone'),
                                           inline=TRUE),
                        br()
                        ),
                 column(6,
                        tableOutput(outputId = "intro_table")
                        )
               )
    )
  ),
  
  #second tab - chapter 1 ------------------------------------------------------------------------------
  
  tabPanel('Chapter1: War on Terror',
           sidebarLayout(
             sidebarPanel(width = 4,
               sliderInput(inputId = "attack_lim", 
                           label = "Casualty Count", 
                           min = 0, max = 148, 
                           value = c(0,148)),
               HTML("<h6>
                    Tips for interaction:
                      <br><br>
                      <ul>
                          <li>Use the slider above to toggle between attacks of specific casualties</li>
                          <li>Use the filter on the top right of map to filter markers by label</li>
                      </ul>
                    </h6>"),
               hr(),
               HTML(
                 "<p>
                  The Taliban and Al Qaeda's main source of entry into Pakistan was through the northern regions; KPK and FATA. Due to this the US led drone strikes are focused only in those regions. The suicide bombing attacks, although appearing to be more spread out, are also focused mainly in the northern regions of Pakistan. 
                 <br> <br>
                 The suicide attacks are mainly focused on Civilian, Government Officials, Military and Police as a form of retaliation against the drone strikes being conducted and can not be considered as random targetting. This can easily be seen by viewing the attacks by target type in the next sub-tab labelled Suicide by Target. 
                 <br> <br>
                 This is a way to add pressure to the government with the objective of ending the drone strikes being conducted. 
                 <br> <br>
                 The next two tabs on Casualties and Affect provide evidence of the impact of the increase in drone strikes and suicide attacks on the people of Pakistan.
                 <br>
                 <br>
                 <b>
                 <h6><i>End of Chapter.</i></h6>
                 </b>
                 </p>"
               )
             ),
             mainPanel(width = 8,
                       tabsetPanel(
                         tabPanel('Suicide & Drone',
                                  leafletOutput('leaf_map', height = '650px', width = '100%')
                         ),
                         tabPanel('Suicide by Target',
                                  leafletOutput('leaf_map_target', height = '650px', width = '100%')
                         )
                       )
             )
           )
  ),
  
  #third tab - chapter 2 ------------------------------------------------------------------------------
  
  tabPanel('Chapter2: Casualties',
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 HTML(
                   "<h6>
                    Tips for interaction:
                    <br><br>
                   <ul>
                   <li>Brush over points to see the summary statistics of the casualties by year and type.</li>
                   <li>Click the show events buttom to view the US linked political events along the timeline.</li>
                   </ul>
                   </h6>"
                 ),
                 hr(),
                 HTML(
                   "<p>
                   The number of suicide bombings increased after the 9/11 attack but rocketted in numbers after the start of the drone strikes, which was then followed by a further increase in the count of drone strikes conducted. 
                   <br><br>
                   During his presidency in the US, drastically increased the number of drone strikes being conducted. This caused the count of suicide attacks to lower in numbers, however this tactic was unsuccesful interms of completely finishing the suicide bombing attacks. 
                   <br><br>
                   As briefly mentioned, the suicide attacks were mainly targetted towards Civilians, Military, Police, and Government Officials, in order to add pressure on Pakistan's goverment to put a stop to the drone strikes. 
                   <br><br>
                   Although, the objective of the drone strikes was to eradicate terrorism and suicide bombings, the main casualties of the attacks were civilians. In the app this can be seen by brushing over mutiple points to view the casualties by type.  
                   <br>
                   In a nutshell, the main impact of both the suicide attacks and the drone strikes is on the civilians, as they have the highest count of casualties in both. 
                   <br>
                   <br>
                   <h6><i>End of Chapter</h6></i>
                   <p>"
                 )
               ),
                mainPanel(
                  tabsetPanel(type = 'tabs',
                              tabPanel('Overview',
                                       br(),
                                       br(),
                                       plotOutput(outputId = "casualty_plot", brush = "plot_brush"),
                                       br(),
                                       br(),
                                       tableOutput(outputId = "casualty_dt_table"),
                                       absolutePanel(
                                         id = "x", 
                                         fixed = FALSE,
                                         draggable = TRUE, 
                                         top = 200, 
                                         left = 80, 
                                         right = 'auto', 
                                         bottom = "auto",
                                         width = 'auto', 
                                         height = "auto",
                                         style = "opacity: 1; z-index: 1000;",
                                         checkboxInput("show_event", "Show Events", FALSE),
                                         br(),
                                         div(tableOutput(outputId = "casualty_table"), style = "font-size: 75%; width: 75%")
                                       )
                              ),
                              tabPanel('Suicide Bombings',
                                       br(),
                                       br(),
                                       plotOutput(outputId = "casualty_sa_plot", brush = "plot_sa_brush"),
                                       br(),
                                       fluidRow(
                                       column(5, offset = 4,
                                              h5('Brush Over Points to view Summary of Casualties')
                                              ),
                                       br()
                                       ),
                                       fluidRow(
                                         column(6,
                                                plotOutput(outputId = "casualty_sa_brush_bar_year")
                                         ),
                                         column(6,
                                                plotOutput(outputId = "casualty_sa_brush_bar_type")      
                                         )
                                       ),
                                       fluidRow(
                                         br(),
                                         br(),
                                         tableOutput(outputId = "casualty_sa_dt_table")
                                       )
                              ),
                              tabPanel('Drone Strikes',
                                       br(),
                                       br(),
                                       plotOutput(outputId = "casualty_da_plot", brush = "plot_da_brush"),
                                       br(),
                                       fluidRow(
                                         column(5, offset = 4,
                                                h5('Brush Over Points to view Summary of Casualties')
                                         ),
                                         br()
                                       ),
                                       fluidRow(
                                         column(6,
                                                plotOutput(outputId = "casualty_da_brush_bar_year")
                                         ),
                                         column(6,
                                                plotOutput(outputId = "casualty_da_brush_bar_type")      
                                         )
                                       ),
                                       fluidRow(
                                         br(),
                                         br(),
                                         #dataTableOutput(outputId = "casualty_da_dt_table")
                                         tableOutput(outputId = "casualty_da_dt_table")
                                       )
                               )
                  )
                )
             )
           )
           ),
  
  #fourth tab - chapter 3 ------------------------------------------------------------------------------
  
  tabPanel('Chapter3: Affect',
           tabsetPanel(type = 'tabs',
                       tabPanel('Emigration by Type',
                                sidebarLayout(
                                  sidebarPanel(
                                    checkboxInput("drill_switch_to_skill", "Switch to display Emigration by Skill", FALSE),
                                    conditionalPanel('input.drill_switch_to_skill == false',
                                                     checkboxGroupInput(inputId = 'selected_province_list',
                                                                        label = 'Provinces',
                                                                        choices = c('Azad Kashir' ='azad.kashmir',
                                                                                    'Federal' = 'federal',
                                                                                    'Northern Area' = 'northern.area',
                                                                                    'Sindh' = 'sindh',
                                                                                    'Balochistan' = 'balochistan',
                                                                                    'KPK' = 'kpk' ,
                                                                                    'Punjab' = 'punjab',
                                                                                    'Tribal Area' = 'tribal.area'),
                                                                        selected = c('Azad Kashir' ='azad.kashmir',
                                                                                     'Federal' = 'federal',
                                                                                     'Northern Area' = 'northern.area',
                                                                                     'Sindh' = 'sindh',
                                                                                     'Balochistan' = 'balochistan',
                                                                                     'KPK' = 'kpk' ,
                                                                                     'Punjab' = 'punjab',
                                                                                     'Tribal Area' = 'tribal.area')
                                                     )
                                    ),
                                    conditionalPanel('input.drill_switch_to_skill == true',
                                                     checkboxGroupInput(inputId = 'selected_skill_list',
                                                                        label = 'Skill Level',
                                                                        choices = c('Highly Qualified' = 'Highly..Qualified',
                                                                                    'Highly Skilled' = 'Highly.Skilled',
                                                                                    'Semi Skilled' = 'Semi.SKilled',
                                                                                    'Skilled' = 'Skilled',
                                                                                    'Un-Skilled' = 'Un.Skilled'
                                                                        ),
                                                                        selected = c('Highly Qualified' = 'Highly..Qualified',
                                                                                     'Highly Skilled' = 'Highly.Skilled',
                                                                                     'Semi Skilled' = 'Semi.SKilled',
                                                                                     'Skilled' = 'Skilled',
                                                                                     'Un-Skilled' = 'Un.Skilled'
                                                                        )
                                                     )
                                    ),
                                    hr(),
                                    HTML(
                                      "<h6>
                                      Tips for interaction:
                                      <br><br>
                                      <ul>
                                      <li>Toggle between viewing the emigration counts by skill and provinces.</li>
                                      <li>Select fewer provinces/skill types to view the points in isolation</li>
                                      <li>View emigration by location in the next tab.</i>
                                      </ul>
                                      </h6>"
                                    ),
                                    hr(),
                                    HTML(
                                      "<p>
                                      Due to this war on terror and high casualty rates of civilians, there was a sudden increase in the number of people emigrating out of Pakistan. 
                                      <br><br>
                                      This increase was mainly feuled by the increase of emigratino from KPK and Punjab, i.e. the regions closest to the the core area of attacks, FATA. This ultimately resulted in a loss of both skilled and unskilled labour from the Pakistan economy. The unskilled labour mainly arising from KPK, while the skilled mainly were from Punjab. 
                                      <br><br>
                                      This high supply of labour emigrating out of Pakistan was mainly met by three major countries, Saudi Arabia, United Arab Emirate and Oman. The reason behind this was that these countries were both close to Pakistan aswell as had huge demand for both skilled and unskilled workers. 
                                      <br>
                                      <br>
                                      <h6><i>End of Chapter</h6></i>
                                      <p>"
                                    ),
                                    width = 5
                                  ),
                                  mainPanel(
                                    fluidRow(
                                      column(10, 
                                             offset=1,
                                             plotOutput(outputId = "skill_emig_plot")
                                      )
                                    ),
                                    br(),
                                    fluidRow(
                                      column(10,
                                             offset=1,
                                             plotOutput(outputId = "skill_emig_bar")
                                        
                                      )
                                    ),
                                    width = 7
                                  )
                                )
                                ),
                       tabPanel('Emigration by Location',
                                sidebarLayout(
                                  sidebarPanel(
                                    checkboxInput("select_top_emig", "Display only highest emigrations", FALSE),
                                    conditionalPanel('input.select_top_emig == true',
                                                     numericInput(inputId = "top_n_emig", 
                                                                  label = "Top Limit:", 
                                                                  min = 1, max = nrow(em_co_lat_long), 
                                                                  value = 3)
                                                     ),
                                    hr(),
                                    conditionalPanel('input.select_top_emig == false',
                                        checkboxInput("show_500_more_emig", "Display countries with > 500 emigrations", TRUE),
                                          conditionalPanel('input.show_500_more_emig == true', 
                                                           sliderInput(inputId = "emig_count_slider_more", 
                                                                       label = ">500 Emigration Count:", 
                                                                       min = 500, max = 3000000, 
                                                                       value = c(500,3000000))
                                          ),
                                        checkboxInput("show_500_less_emig", "Display countries with < 500 emigrations", FALSE),
                                          conditionalPanel('input.show_500_less_emig == true', 
                                                           sliderInput(inputId = "emig_count_slider_less", 
                                                                       label = "<500 Emigration Count:", 
                                                                       min = 0, max = 500, 
                                                                       value = c(0,500))
                                                           )
                                    ),
                                    br(),
                                    br(),
                                    HTML(
                                      "<h6>
                                      Tips for interaction:
                                      <br><br>
                                      <ul>
                                      <li>View the emigration map by location and filter between emigration counts to fovus on specific countries.</i>
                                      <li>use the top limit feature to view only the top 'n' countries with the highest emigration</li>
                                      </ul>
                                      </h6>"
                                    )
                                  ),
                                  mainPanel(
                                    plotOutput(outputId = "emig_flow_plot", brush = "plot_brush_emig"),
                                    br(),
                                    tableOutput(outputId = "emig_dt_table")
                                  )
                                )
                       )    
           )
  ),
  
  #fifth tab - Data ------------------------------------------------------------------------------
  
  tabPanel('Data',
           sidebarLayout(
             
             # Input(s)
             sidebarPanel(
               h5("Select filetype and variables, then hit 'Download data'."),
               hr(),
               #select file
               selectInput(inputId='file_chosen', 
                           label='Select File to Download', 
                           choices = c('Emigration by Province' = 'em_pr',
                                       'Emmigration by Country' = 'em_co',
                                       'Emigration by Skill' = 'em_oc',
                                       'Emmigration with Lat & Long' = 'em_co_lat_long',
                                       'Suicide Bombing' = 'sa',
                                       'Drone Strike' = 'da'
                                       ),
                           selected = c('Suicide Bombing' = 'sa')
                           ),
               # Select filetype
               radioButtons(inputId = "filetype",
                            label = "Select filetype:",
                            choices = c("csv", "tsv"),
                            selected = "csv"),
               textInput(inputId='file_download_name', label='Name to download file with (without Extension)', 'data'),
               hr(),
               downloadButton("download_data", "Download data")
             ),
             # Output(s)
             mainPanel(
               br(),
               #dataTableOutput(outputId = "download_dt_table")
               tableOutput(outputId = "download_dt_table")
             )
           )
  ),
  
  #sixth tab - References ------------------------------------------------------------------------------
  
  tabPanel('References',
           br(),
           h4('Data sets'),
           p('Drone Attacks', br(), tags$a('https://www.kaggle.com/zusmani/pakistandroneattacks')),
           br(),
           p('Suicide Bombings', br(), tags$a('https://www.kaggle.com/zusmani/pakistansuicideattacks')),
           br(),
           p('Immigration', br(), tags$a('http://beoe.gov.pk/reports-and-statistics')),
           br(),
           p('Latitudes and Longitudes', br(), tags$a('https://developers.google.com/public-data/docs/canonical/countries_csv')),
           br(),
           br(),
           h4('Research Paper'),
           p('Zeeshan-ul-hassan Usmani and Hira Bashir, "The Impact of Drone Strikes in Pakistan", Cost of War Project, Brown University, December 16, 2014',br(),
             tags$a('http://watson.brown.edu/costsofwar/files/cow/imce/papers/2015/The%20Impact%20of%20Drone%20Strikes%20in%20Pakistan.pdf'))
           )
)


# SERVER. 


# Define server function required to create the scatterplot
server <- function(input, output) {
  
  casualty_df <- reactive({
    tt <- df %>%
      select(gr_year, gr_month, sa_Killed.Max, da_Total.Died.Max) %>%
      gather('type', 'val', -gr_year, -gr_month) %>%
      group_by(gr_year,gr_month, type) %>%
      summarise(cnt=sum(!is.na(val)), dead=sum(val, na.rm=TRUE)) %>%
      filter(cnt!=0) %>%
      ungroup() 
    tt[which(tt$type=='sa_Killed.Max'),'type'] <- 'Suicide'
    tt[which(tt$type=='da_Total.Died.Max'),'type'] <- 'Drone'
    tt
  })
  
  #tab 1: introduction #########################################################
  
  intro_df <- reactive(
              casualty_df() %>%
              group_by(type) %>%
              mutate(cnt = cumsum(cnt)) %>%
              mutate(dead = cumsum(dead)) %>%
              ungroup())
  
  # create initial introduction plot (cumulative deaths)
  output$intro_plot <- renderPlot({
    intro_df() %>% filter(type %in% input$intro_filter_type) %>%
      ggplot() +
      geom_point(aes(x=gr_year, y=cnt, color=type), position='jitter', alpha=0.5, size=5) +
      geom_smooth(aes(x=gr_year, y=cnt, color=type),method='gam', formula=y~s(x), se=FALSE, size=1) +
      scale_color_manual(name='CUMULATIVE CASUALTIES', values=c('steelblue', 'red')) +
      scale_x_continuous(limits = c(1995, 2017.5)) +
      theme(legend.position=c(0.825,0.3),
            axis.ticks = element_blank(), 
            rect = element_rect(fill = "transparent"),
            panel.grid.minor = element_blank(), #c(0.825,0.3)
            panel.grid.major = element_blank(),
            axis.title.x=element_blank(),
            legend.text=element_text(size=14, color='white'),
            legend.title=element_text(size=16, color='white'),
            axis.title.y=element_blank(),
            axis.text.x=element_text(size=12, color='white'),
            axis.text.y=element_text(size=12, color='white'),
            plot.background = element_rect(fill = "transparent", color=NA),
            panel.background = element_rect(fill = "transparent", color=NA),
            legend.key = element_blank()
            ) #axis.text.x=element_blank(),axis.text.y=element_blank()
  }, bg="transparent")
  
  output$intro_table <- renderTable({
      hover_point <- nearPoints(intro_df(), input$plot_hover, threshold = 30, maxpoints = 1)
      names(hover_point) <- c('Year', 'Month', 'Attack Type', 'Cumulative Count', 'Cumulative Deaths')
      hover_point
   })
  
  
  #tab 2: impact #########################################################
  
  #dataframe used to sa tab
  casualty_sa_df <- reactive({df %>% 
                              filter(sa==1) %>% 
                              group_by(gr_year, sa_Target.Type) %>% 
                              summarise(cnt=sum(sa_No..of.Suicide.Blasts, na.rm=TRUE), dead=sum(sa_Killed.Max, na.rm=TRUE)) %>%
                              ungroup()
  })
  
  #dataframe used for da tab
  casualty_da_df <- reactive({
    b<- df %>% 
      filter(da==1) %>% 
      select(gr_year, da_Al.Qaeda ,da_Taliban ,da_Civilians.Max ,da_Foreigners.Max, da_No.of.Strike) %>% 
      group_by(gr_year) %>% 
      summarise_all(sum, na.rm=TRUE) %>%
      gather('type', 'dead', -gr_year, -da_No.of.Strike) %>%
      arrange(gr_year) %>% 
      ungroup()
    b$type <- str_replace_all(b$type, '(da_)|(.Max)','')
    b
   
  })
  
  #subtab sa -----------------------------------------
  
  #casualties Suicide
  output$casualty_sa_plot <- renderPlot({
        casualty_sa_df() %>% #group_by(gr_year) %>% summarise(cnt=sum(cnt), dead=sum(dead)) %>%
          ggplot() + geom_point(aes(x=gr_year, y=cnt, color='Suicide Attack Count', size=dead), position='jitter') + 
          geom_smooth(aes(x=gr_year, y=cnt, color='Smoother'), method='gam', formula=y~s(x), se=FALSE) + 
          labs(x='Years', y='Count of Suicide Attacks', size='Total Casualties') +
          scale_color_manual(name='Legend', values=c('steelblue', 'deeppink')) + 
          theme(
            legend.position='top',
            axis.ticks.x = element_line(color='white'),
            axis.ticks.y = element_line(color='white'),
            axis.text.x=element_text(angle=90,size=12, color='white'),
            axis.text.y=element_text(size=12, color='white'),
            rect = element_rect(fill = "transparent"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.title.x=element_text(color='white'),
            axis.title.y=element_text(color='white'),
            legend.text=element_text(size=14, color='white'),
            legend.title=element_text(size=16, color='white'),
            plot.background = element_rect(fill = "transparent", color=NA),
            panel.background = element_rect(fill = "transparent", color=NA),
            legend.key = element_blank()
            ) +
            scale_x_continuous(breaks=seq(1995,2017,1)) 
  }, bg="transparent")
  
  #normal brushed points table
  output$casualty_sa_dt_table <- renderTable({
    tt <- brushedPoints(casualty_sa_df(), brush=input$plot_sa_brush, xvar='gr_year', yvar='cnt') 
    tt
  }, width = 860)
  
  #summary of brushed points by type
  output$casualty_sa_brush_bar_type <- renderPlot({
    brushedPoints(casualty_sa_df(), brush=input$plot_sa_brush, xvar='gr_year', yvar='cnt') %>%
    ggplot() + 
      geom_bar(aes(x=reorder(sa_Target.Type,-dead), y=dead, fill=sa_Target.Type), stat='identity', position='dodge') + 
      labs(x='Year', y='Number of Deaths') +
      theme( 
        legend.position='none',
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_line(color='white'),
        axis.ticks.y = element_line(color='white'),
        axis.text.x=element_text(angle=90,size=12, color='white'),
        axis.text.y=element_text(size=12, color='white'),
        rect = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=14, color='white'),
        legend.title=element_text(size=16, color='white'),
        plot.background = element_rect(fill = "transparent", color=NA),
        panel.background = element_rect(fill = "transparent", color=NA)
        ) +
      coord_flip()
  }, bg="transparent")
  
  #summary of brushed points by year
  output$casualty_sa_brush_bar_year <- renderPlot({
    brushedPoints(casualty_sa_df(), brush=input$plot_sa_brush, xvar='gr_year', yvar='cnt') %>%
    ggplot() + 
      geom_bar(aes(x=gr_year, y=dead, fill=gr_year), stat='identity', position='dodge') + 
      labs(x='Year', y='Number of Deaths') +
      theme(
        legend.position='none',
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_line(color='white'),
        axis.ticks.y = element_line(color='white'),
        axis.text.x=element_text(angle=90,size=12, color='white'),
        axis.text.y=element_text(size=12, color='white'),
        rect = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=14, color='white'),
        legend.title=element_text(size=16, color='white'),
        plot.background = element_rect(fill = "transparent", color=NA),
        panel.background = element_rect(fill = "transparent", color=NA)
      )
  }, bg="transparent")
  
  #subtab drone -------------------------------------------
  
  # plot of da casualties
  output$casualty_da_plot <- renderPlot({
    casualty_da_df() %>% 
    ggplot() + geom_point(aes(x=gr_year, y=da_No.of.Strike, color='Suicide Attack Count', size=dead)) + 
      geom_smooth(aes(x=gr_year, y=da_No.of.Strike, color='Smoother'), method='gam', formula=y~s(x), se=FALSE) + 
      labs(x='Years', y='Count of Suicide Attacks', size='Total Casualties') +
      scale_color_manual(name='Legend', values=c('steelblue', 'deeppink')) + 
      theme(
        legend.position='top',
        axis.ticks.x = element_line(color='white'),
        axis.ticks.y = element_line(color='white'),
        axis.text.x=element_text(angle=90,size=12, color='white'),
        axis.text.y=element_text(size=12, color='white'),
        rect = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x=element_text(color='white'),
        axis.title.y=element_text(color='white'),
        legend.text=element_text(size=14, color='white'),
        legend.title=element_text(size=16, color='white'),
        plot.background = element_rect(fill = "transparent", color=NA),
        panel.background = element_rect(fill = "transparent", color=NA),
        legend.key = element_blank()
      ) +
      scale_x_continuous(breaks=seq(1995,2017,1))
  }, bg="transparent")
  
  #noraml brushed points table
  output$casualty_da_dt_table <- renderTable({
    tt <- brushedPoints(casualty_da_df(), brush=input$plot_da_brush, xvar='gr_year', yvar='da_No.of.Strike') 
    tt
  }, width = 860)
  
  #summary of brushed points by type
  output$casualty_da_brush_bar_type <- renderPlot({
    brushedPoints(casualty_da_df(), brush=input$plot_da_brush, xvar='gr_year', yvar='da_No.of.Strike') %>%
      ggplot() + 
      geom_bar(aes(x=type, y=dead, fill=type), stat='identity', position='dodge') + 
      labs(x='Year', y='Number of Deaths') +
      theme(
        legend.position='none',
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_line(color='white'),
        axis.ticks.y = element_line(color='white'),
        axis.text.x=element_text(size=12, color='white'),
        axis.text.y=element_text(size=12, color='white'),
        rect = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=14, color='white'),
        legend.title=element_text(size=16, color='white'),
        plot.background = element_rect(fill = "transparent", color=NA),
        panel.background = element_rect(fill = "transparent", color=NA)
      )
  }, bg="transparent")
  
  #summary of brushed points by year
  output$casualty_da_brush_bar_year <- renderPlot({
    brushedPoints(casualty_da_df(), brush=input$plot_da_brush, xvar='gr_year', yvar='da_No.of.Strike') %>%
      ggplot() + 
      geom_bar(aes(x=gr_year, y=dead, fill=gr_year), stat='identity', position='dodge') + 
      labs(x='Year', y='Number of Deaths') +
      theme(
        legend.position='none',
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_line(color='white'),
        axis.ticks.y = element_line(color='white'),
        axis.text.x=element_text(size=12, color='white'),
        axis.text.y=element_text(size=12, color='white'),
        rect = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=14, color='white'),
        legend.title=element_text(size=16, color='white'),
        plot.background = element_rect(fill = "transparent", color=NA),
        panel.background = element_rect(fill = "transparent", color=NA)
      )
  }, bg="transparent")
  
  
  #subtab summary -----------------------------------------
  
  #main plot
  output$casualty_plot <- renderPlot({
    plot <- casualty_df() %>%
            ggplot() + 
              labs(x='Year', y='Count of Suicide Attacks/Drone Strikes') + 
              scale_color_manual(name='Legend', values=c('steelblue', 'deeppink')) + 
              theme(
                legend.position='top',
                axis.ticks.x = element_line(color='white'),
                axis.ticks.y = element_line(color='white'),
                axis.text.x=element_text(angle=90,size=12, color='white'),
                axis.text.y=element_text(size=12, color='white'),
                rect = element_rect(fill = "transparent"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.title.x=element_text(color='white'),
                axis.title.y=element_text(color='white'),
                legend.text=element_text(size=14, color='white'),
                legend.title=element_text(size=16, color='white'),
                plot.background = element_rect(fill = "transparent", color=NA),
                panel.background = element_rect(fill = "transparent", color=NA),
                legend.key = element_blank()
              ) #,legend.title=element_text(size=16)
    if (input$show_event) {
      plot = plot + 
              geom_rect(fill='gray20', xmin=2001, xmax=2009, ymin=-Inf, ymax=Inf, alpha=0.5) +
              geom_rect(fill='gray20', xmin=2017, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5) +
              geom_vline(xintercept=2001.75, color='red', size=0.5) + 
              annotate(geom = "text", x = 1998, y = 25, label = "Bill Clinton", color='white', size=8) +
              annotate(geom = "text", x = 2005, y = 25, label = "Bush", size=8, color='white') +
              annotate(geom = "text", x = 2013, y = 25, label = "Obama", color='white', size=8) +
              annotate(geom = 'text', x = 2002.1, y=20, label= '9/11', angle=90, size=6, color='white') 
    }
    plot +
      geom_point(aes(x=gr_year, y=cnt, color=type), position='jitter', alpha=0.5, size=3) + 
      geom_smooth(aes(x=gr_year, y=cnt, color=type),method='gam', formula=y~s(x), se=FALSE, size=1) 
    }, bg="transparent")
  
    #brush point little SUMMARY table
    output$casualty_table <- renderTable({
      tt <- brushedPoints(casualty_df(), brush=input$plot_brush, 'gr_year', 'cnt') %>% group_by(type) %>% summarise(cnt=sum(cnt), dead=sum(dead))
      names(tt) <- c('Attack', 'Count', 'Casualties')
      tt
  })
    
    output$casualty_dt_table <- renderTable({
      tt <- brushedPoints(casualty_df(), brush=input$plot_brush, 'gr_year', 'cnt')
      names(tt) <- c('Year','Month', 'Type', 'Count', 'Casualties')
      tt
    }, width = 860)
    
  #emigration ######################################################
    
    # subtab Type -----------------------------------------------------
    
    #for skill emigration
    em_oc_l <- reactive({
      tt <- em_oc %>% select(-Total) %>% gather('Type', 'Count', -Year)
      tt$Type <- factor(tt$Type, levels=c('Highly..Qualified', 'Highly.Skilled', 'Skilled', 'Semi.Skilled', 'Un.Skilled'))
      tt
      })
    
    em_pr_l <- reactive({
      tt <- em_pr %>% select(-total) %>% gather('Type', 'Count', -year)
      names(tt) <- c('Year', 'Type', 'Count')
      tt
    })
    
    plot_df <- reactive({
      if (input$drill_switch_to_skill) {
        plot_df <- em_oc_l() %>% filter(Type %in% input$selected_skill_list) 
      }
      else {
        plot_df <- em_pr_l() %>% filter(Type %in% input$selected_province_list) 
        
      }
      plot_df
    })
    
    output$skill_emig_plot <- renderPlot({
          
          plot_df() %>% 
            ggplot() + 
            labs(x='Year', y='Count of Emigration') +
            geom_point(aes(x=Year, y=Count, color=Type), size=3) + 
            geom_vline(xintercept=2001.75, color='red') + 
            annotate(geom = 'text', x = 2002.6, y=350000, label= '9/11', angle=90, color='white') + 
            theme(
              legend.position='top',
              axis.ticks.x = element_line(color='white'),
              axis.ticks.y = element_line(color='white'),
              axis.text.x=element_text(angle=90,size=12, color='white'),
              axis.text.y=element_text(size=12, color='white'),
              rect = element_rect(fill = "transparent"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.title.x=element_text(color='white'),
              axis.title.y=element_text(color='white'),
              legend.text=element_text(size=14, color='white'),
              legend.title=element_text(size=16, color='white'),
              plot.background = element_rect(fill = "transparent", color=NA),
              panel.background = element_rect(fill = "transparent", color=NA),
              legend.key = element_blank()
            ) 
            #scale_color_brewer(palette="Set1")
    }, bg="transparent")
    
    
    output$skill_emig_bar <- renderPlot({
      plot_df() %>%
        ggplot() + 
        geom_bar(aes(x=Type, y=Count, fill=Type), stat='identity', position='stack') + 
        labs(x='Year', y='Number of Deaths', fill='Type of Casualty') +
        theme(
          legend.position='none',
          axis.ticks.x = element_line(color='white'),
          axis.ticks.y = element_line(color='white'),
          axis.text.x=element_text(angle=90,size=12, color='white'),
          axis.text.y=element_text(size=12, color='white'),
          rect = element_rect(fill = "transparent"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x=element_text(color='white'),
          axis.title.y=element_text(color='white'),
          legend.text=element_text(size=14, color='white'),
          legend.title=element_text(size=16, color='white'),
          plot.background = element_rect(fill = "transparent", color=NA),
          panel.background = element_rect(fill = "transparent", color=NA),
          legend.key = element_blank()
        )
    }, bg="transparent")
    
    
    # subtab Locations -------------------------------------------------------
    
    #filtering datafraem
    flow_df_full <- reactive({em_co_lat_long %>% filter(!is.na(em_co_lat_long$latitude))})
    
    flow_df <- reactive({
                flow_df <- flow_df_full()
    
                if (input$show_500_less_emig) {
                  flow_df <- flow_df_full() %>% filter((cnt>input$emig_count_slider_less[1] & cnt<input$emig_count_slider_less[2]) | cnt>500)
                }
                else {
                  flow_df <- flow_df_full() %>% filter(cnt>500)
                }
                
                if (input$show_500_more_emig == FALSE) {
                  flow_df <- flow_df %>% filter(cnt<500)
                }
                else {
                  flow_df <- flow_df %>% filter((cnt>input$emig_count_slider_more[1] & cnt<input$emig_count_slider_more[2]) | cnt<500)
                }
                
                if (input$select_top_emig) {
                  flow_df <- flow_df_full() %>% arrange(desc(cnt)) %>% head(req(input$top_n_emig))
                }
                
                flow_df
    })
    
    output$emig_flow_plot <- renderPlot({
          
          
          #creating map
          par(mar=c(0,0,0,0))
          maps::map('world',col='gray35', fill=TRUE, bg=NULL, lwd=0.1,mar=rep(0,4),border=1, ylim=c(-80,80) )#"#f2f2f2"
          
          #add points only if the dataframe has any points
          if (nrow(flow_df()) != 0) {
            #adding flow lines
            for (i in 1: nrow(flow_df())) {
              inter <- gcIntermediate(
                #pakistan
                c(69.34511599999996,30.375321),
                c(flow_df()[i,]$longitude, flow_df()[i,]$latitude),
                n = 100,
                addStartEnd=TRUE
              )
              points(x=flow_df()[i,]$longitude, y=flow_df()[i,]$latitude, col="brown", cex=1.5, pch=20)
              lines(inter, col='gray80', lwd=2, alpha=0.05)
            }
            
            #adding pakistan
            points(x=69.34511599999996, y=30.375321, col="blue", cex=2, pch=20)
            
          }
          
    },bg='transparent')

    
    output$emig_dt_table <- renderTable({
      tt <- brushedPoints(flow_df(), brush=input$plot_brush_emig, 'longitude', 'latitude') %>% select(-country)
      names(tt) <- c('Country', 'Count of Emigrations', 'Latitude', 'Longitude')
      tt
    }, width = 860)
    
    #Location ##################################################################
    
    ds_filt <- reactive({
      ds %>% filter(dead >= input$attack_lim[1] & dead <= input$attack_lim[2])
    })
    
    #color panel for target leaflet map
    colorp = topo.colors(10, alpha = NULL)
    colorp
    
    t <- reactive({
      t_df %>% filter(Killed.Max >= input$attack_lim[1] & Killed.Max <= input$attack_lim[2])
    })
    
    output$leaf_map_target <- renderLeaflet({
      leaflet(t()) %>% 
        # addTiles() %>%
        addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%  #OpenMapSurfer.Grayscale
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Civilian'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[1],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Civilian')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Media'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[2],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Media')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Religious'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[3],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Religious')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Military'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[4],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Military')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Government Official'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[5],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Government Official')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Foreigner'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[6],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Foreigner')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Police'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[7],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Police')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Anit-Militans'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[8],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Anit-Militans')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Unknown'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[9],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Unknown')
        ) %>% 
        addCircleMarkers(
          data = t() %>% filter(Target.Type=='Judges & Lawyers'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = colorp[10],
          popup = paste('ln: ',t()$Longitude,'<br>','lt: ', t()$Latitude, '<br>', 'deaths:', t()$Killed.Max), #popup
          label = paste(t()$Target.Type,'Attack','Casualties: ',as.character(t()$Killed.Max)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (t()$Killed.Max/max(t()$Killed.Max))*10,
          group=c('Judges & Lawyers')
        ) %>% 
        addLayersControl(overlayGroups = c("Foreigner","Media","Religious","Military","Government Official","Civilian",
                                           "Police","Anti-Militants","Unknown","Judges & Lawyers"),
                         options = layersControlOptions(collapsed = T)) %>%
        addLegend('bottomright',colors = colorp, labels = c("Civilian","Media","Religious","Military","Government Official","Foreigner",
                                                            "Police","Anti-Militants","Unknown","Judges & Lawyers"))
    })
    
    output$leaf_map <- renderLeaflet({
      
      leaflet(req(ds_filt())) %>% 
        addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%  #OpenMapSurfer.Grayscale
        addCircleMarkers(
          data = ds_filt() %>% filter(type=='Drone'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = 'steelblue',
          popup = paste('ln: ',ds_filt()$Longitude,'<br>','lt: ', ds_filt()$Latitude), #popup
          label = paste(ds_filt()$type,'Attack','Casualties: ',as.character(ds_filt()$dead)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (ds_filt()$dead/max(ds_filt()$dead))*10,
          group=c('Drone')
        ) %>%
        addCircleMarkers(
          data = ds_filt() %>% filter(type=='Suicide'),
          lng = ~Longitude, 
          lat = ~Latitude, #location of marker
          color = 'deeppink',
          popup = paste('ln: ',ds_filt()$Longitude,'<br>','lt: ', ds_filt()$Latitude), #popup
          label = paste(ds_filt()$type,'Attack','Casualties: ',as.character(ds_filt()$dead)), #what the label will be 
          labelOptions = labelOptions(noHide = F), #show labels throughout
          stroke = FALSE, 
          fillOpacity = 0.5,
          radius = (ds_filt()$dead/max(ds_filt()$dead))*10,
          group=c('Suicide')
        ) %>% 
        addLayersControl(overlayGroups = c('Suicide', 'Drone'),
                         options = layersControlOptions(collapsed = T)) %>%
        addLegend('bottomright',colors = c('navy', 'red'), labels = c('Drone', 'Suicide'))
      
    })
    
    # Downloading File ##################################################################################
    
    file_to_download <- reactive({
      if (input$file_chosen=='sa') {
        tt <- sa
      }
      else if (input$file_chosen=='da') {
        tt <- da
      }
      else if (input$file_chosen=='em_pr') {
        tt <- em_pr
      }
      else if (input$file_chosen=='em_oc') {
        tt <- em_oc
      }
      else if (input$file_chosen=='em_co') {
        tt <- em_co
      }
      else if (input$file_chosen=='em_co_lat_long') {
        tt <- em_co_lat_long
      }
      tt
    })
    
    
    # Download file
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(req(input$file_download_name),'.' , input$filetype)
      },
      content = function(file) { 
        if(input$filetype == "csv"){ 
          write_csv(file_to_download(), file) 
        }
        if(input$filetype == "tsv"){ 
          write_tsv(file_to_download(), file) 
        }
      }
    )

    output$download_dt_table <- renderTable({
      file_to_download() %>% head(10)
    }, width = 860)
    
}


# APP CREATION FUNCTION

# Create a Shiny app object
shinyApp(ui = ui, server = server)















