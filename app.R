library(shiny)
library(shinydashboard)
library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(sf)
library(rcartocolor)

ui <- dashboardPage(
  dashboardHeader(title = "O-NET"),
  
  dashboardSidebar(disable = TRUE,
                   sidebarMenu()
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css?family=Sarabun:wght@200&display=swap",
        type = "text/css"
      ),
      tags$style(HTML(".skin-blue .main-header .logo, .skin-blue .main-header .logo:hover { font-family: 'Sarabun', sans-serif; }")),
      tags$style(HTML(".my-dashboard-header .logo-text { white-space: normal; }")),
      tags$style(HTML(".my-dashboard-header h1, .my-dashboard-header h3, .my-dashboard-header h4 { color: #FF0000; }"))  # เปลี่ยนสีตัวอักษรเป็นสีแดง
    
      ),
    
    
    fluidRow(
      h1("รายงานตัวชี้วัดตามเป้าหมายของแผนการศึกษาชาติ"),
      column(6,
             h3(style = "text-decoration: underline;","คุณภาพการศึกษาข้อที่ 2"),
             h4("ร้อยละของนักเรียนที่มีคะแนนผลการทดสอบทางการศึกษาระดับชาติขั้นพื้นฐาน (O-NET) แต่ละวิชาผ่านเกณฑ์คะแนนร้อยละ 50 ขึ้นไปเพิ่มขึ้น"),
             tabsetPanel(id = "tab_kpi1", type = "tabs",
                         #hr(),
                         h3("แผนภาพแสดงแนวโน้มของจำนวนนักเรียนที่สอบได้คะแนนเกินครึ่ง (ร้อยละ)"),
                         tabPanel(tabName = "P6",("ประถมศึกษาปีที่ 6"),
                                  value = "P6",
                                  plotlyOutput("plot_kpi1_P6")),
                         tabPanel(tabName = "M3",value = "M3",
                                  ("มัธยมศึกษาปีที่ 3"),
                                  plotlyOutput("plot_kpi1_M3")
                         ),
                         tabPanel(tabName = "M3",("มัธยมศึกษาปีที่ 6"),
                                  value = "M6",
                                  plotlyOutput("plot_kpi1_M6")
                         )
             )
      ),#end of left column
      
      column(6,h3(style = "text-decoration: underline;","คุณภาพการศึกษาข้อที่ 3"),
             h4("ความแตกต่างระหว่างคะแนนเฉลี่ยผลการทดสอบทางการศึกษาระดับชาติขั้นพื้นฐาน (O-NET) ของนักเรียนระหว่างภาคการศึกษาในวิชาคณิตศาสตร์และภาษาอังกฤษ"),
             tabsetPanel(id = "tab_sub", type = "tabs",
                         
                         selectInput("select1", label = ("เลือกระดับชั้น"), 
                                     choices = list("ชั้นประถมศึกษาปีที่ 6" = "ชั้นประถมศึกษาปีที่ 6",
                                                    "ชั้นมัธยมศึกษาปีที่ 3" = "ชั้นมัธยมศึกษาปีที่ 3",
                                                    "ชั้นมัธยมศึกษาปีที่ 6" = "ชั้นมัธยมศึกษาปีที่ 6"), 
                                     selected = 1), 
                         #    radioButtons(inputId = "class",
                         #                label = "เลือกชั้นปี:",
                         #               choices = c("ชั้นประถมศึกษาปีที่ 6",
                         #                          "ชั้นมัธยมศึกษาปีที่ 3",
                         #                         "ชั้นมัธยมศึกษาปีที่ 6"),
                         #            selected = "ชั้นประถมศึกษาปีที่ 6"),
                         sliderInput(inputId = "edYear", "ปีการศึกษา:",
                                     min = 2560, max = 2564,
                                     value = 5,
                                     sep = ""),
                         h3("แผนภาพแสดงความแตกต่างระหว่างคะแนนเฉลี่ยระหว่างภาคการศึกษา"),
                         tabPanel(tabName = "sub4",("วิชาคณิตศาสตร์"),
                                  value = "คณิตศาสตร์",
                                  plotOutput("plot_sub4")
                         ),
                         tabPanel(tabName = "sub3",value = "ภาษาอังกฤษ",
                                  ("วิชาภาษาอังกฤษ"),
                                  plotOutput("plot_sub3")
                         )
                         
                         
                         
             )
      )#end of right column
    )
  )
)

server <- function(input, output, session) { 
  observe({
    # เมื่อมีการเปลี่ยนแท็บ
    updateTabsetPanel(session, "tab_kpi1", selected = input$tab_kpi1)
    updateTabsetPanel(session, "tab_sub", selected = input$tab_sub)
  })
  
  #เตรียมข้อมูล
  plot_kpi1 <- reactive({
    dat = read_excel("onet_kpi1.xlsx")
    
    p = dat%>%
      filter(stuYearID == input$tab_kpi1)%>%
      ggplot()+
      geom_line(aes(x=edYear,y=percent_check,
                    color = subjectName),
                linetype = "dashed", linewidth = 0.3)+
      geom_point(aes(x=edYear,y=percent_check,
                     color = subjectName,
                     text = paste("รายวิชา: ",subjectName,
                                  "<br>ปีการศึกษา: ",edYear,
                                  "<br>คะแนนสูงสุด: ",max,
                                  "<br>คะแนนต่ำสุด: ",min,
                                  "<br>คะแนนเฉลี่ย: ",average_total,
                                  "<br>จำนวนนักเรียนที่ได้คะแนนเกินครึ่ง",percent_check,"%")))+
      theme_light()+
      theme(text = element_text(family = "Sarabun, sans-serif"),
            legend.position="none",
            axis.text = element_text(colour = "black", size = (8)),
            axis.title = element_text(size = (10), colour = "black"))+
      labs(x="ปีการศึกษา",
           y="จำนวนนักเรียนที่ได้คะแนนเกินครึ่ง (ร้อยละ)")+
      ylim(0,100)
    
    q = ggplotly(p,
                 tooltip = "text")
    return(q)
  }) #end of input data
  
  output$plot_kpi1_P6 <- renderPlotly({
    plot_kpi1()
  })
  output$plot_kpi1_M3 <- renderPlotly({
    plot_kpi1()
  })
  output$plot_kpi1_M6 <- renderPlotly({
    plot_kpi1()
  })
  
  #---------------------------------------------
  prepare_heat_map = reactive({
    dat2a=read_excel("dat_kpi2.2.xlsx")
    dat2b = dat2a %>% filter(subjectName == input$tab_sub)
    
    return(dat2b)
  }) #end of input data
  
  
  
  output$plot_sub3 <- renderPlot({
    dat3 = prepare_heat_map()
      dist =dat3 %>%filter(edYear==as.numeric(input$edYear),
                            stuYear == input$select1)%>%
      drop_na()%>%
      ungroup()%>%
      select(dist)%>%
      dist()
    
    dist_df= as.data.frame(as.matrix(dist))
    area = c("ศธภ.1",
             "ศธภ.2",
             "ศธภ.3",
             "ศธภ.4",
             "ศธภ.5",
             "ศธภ.6",
             "ศธภ.7",
             "ศธภ.8",
             "ศธภ.9",
             "ศธภ.10",
             "ศธภ.11",
             "ศธภ.12",
             "ศธภ.13",
             "ศธภ.14",
             "ศธภ.15",
             "ศธภ.16",
             "ศธภ.17",
             "ศธภ.18",
             "กทม.")
    
    colnames(dist_df)=area
    
    
    
    
    
    #names(area)[1]="area" 
    name_area = c("ศธภ.1",
                  "ศธภ.2",
                  "ศธภ.3",
                  "ศธภ.4",
                  "ศธภ.5",
                  "ศธภ.6",
                  "ศธภ.7",
                  "ศธภ.8",
                  "ศธภ.9",
                  "ศธภ.10",
                  "ศธภ.11",
                  "ศธภ.12",
                  "ศธภ.13",
                  "ศธภ.14",
                  "ศธภ.15",
                  "ศธภ.16",
                  "ศธภ.17",
                  "ศธภ.18",
                  "กทม.")%>%as.data.frame()
    area_dist = bind_cols(name_area,dist_df)
    names(area_dist)[1]="area"
    
    p = area_dist%>%
      pivot_longer(2:20,names_to = "area2",values_to = "dist")%>%
      ggplot(aes(x=area,y=area2))+
      geom_tile(aes(fill = dist))+
      theme_void()+
      scale_fill_carto_c(palette = 3)+
      theme(text = element_text(family = "Sarabun, sans-serif"),
            plot.title = element_text(size = 20,
                                      margin = margin(t = 20)),
            legend.position="bottom",
            axis.text = element_text(colour = "black", size = (10)),
            axis.title = element_text(size = (18), colour = "black"),
            legend.text = element_text(size = (8), colour = "black"),
            legend.title = element_text(size = (10), colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(x="",
           y="",
           fill="ความแตกต่างของคะแนนเฉลี่ย",
           title = "",
           subtitle = "")
   
     return(p)
    
  })
  
  output$plot_sub4 <- renderPlot({
    dat3 = prepare_heat_map()
    dist =dat3 %>%filter(edYear==as.numeric(input$edYear),
                         stuYear == input$select1)%>%
      drop_na()%>%
      ungroup()%>%
      select(dist)%>%
      dist()
    
    dist_df= as.data.frame(as.matrix(dist))
    area = c("ศธภ.1",
             "ศธภ.2",
             "ศธภ.3",
             "ศธภ.4",
             "ศธภ.5",
             "ศธภ.6",
             "ศธภ.7",
             "ศธภ.8",
             "ศธภ.9",
             "ศธภ.10",
             "ศธภ.11",
             "ศธภ.12",
             "ศธภ.13",
             "ศธภ.14",
             "ศธภ.15",
             "ศธภ.16",
             "ศธภ.17",
             "ศธภ.18",
             "กทม.")
    
    colnames(dist_df)=area
    
    
    
    
    
    #names(area)[1]="area" 
    name_area = c("ศธภ.1",
                  "ศธภ.2",
                  "ศธภ.3",
                  "ศธภ.4",
                  "ศธภ.5",
                  "ศธภ.6",
                  "ศธภ.7",
                  "ศธภ.8",
                  "ศธภ.9",
                  "ศธภ.10",
                  "ศธภ.11",
                  "ศธภ.12",
                  "ศธภ.13",
                  "ศธภ.14",
                  "ศธภ.15",
                  "ศธภ.16",
                  "ศธภ.17",
                  "ศธภ.18",
                  "กทม.")%>%as.data.frame()
    area_dist = bind_cols(name_area,dist_df)
    names(area_dist)[1]="area"
    
    p = area_dist%>%
      pivot_longer(2:20,names_to = "area2",values_to = "dist")%>%
      ggplot(aes(x=area,y=area2))+
      geom_tile(aes(fill = dist))+
      theme_void()+
      scale_fill_carto_c(palette = 3)+
      theme(text = element_text(family = "Sarabun, sans-serif"),
            plot.title = element_text(size = 20,
                                      margin = margin(t = 20)),
            legend.position="bottom",
            axis.text = element_text(colour = "black", size = (10)),
            axis.title = element_text(size = (18), colour = "black"),
            legend.text = element_text(size = (8), colour = "black"),
            legend.title = element_text(size = (10), colour = "black"),
            axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(x="",
           y="",
           fill="ความแตกต่างของคะแนนเฉลี่ย",
           title = "",
           subtitle = "")
    
    return(p)
    
  })
 
  
  
}

shinyApp(ui, server)
