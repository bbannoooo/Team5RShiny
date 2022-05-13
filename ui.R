library(shinycssloaders)
library(dplyr)
library(googleway)
library(shinyjs)
library(plotly)  #인터랙티브한 그래프
library(shinycustomloader) #로딩중

fluidPage(
  useShinyjs(),
  
  # fluid1
  fluidRow( id = "fluid1",

            div(style = "position: relative;",
                img(src = "https://drive.google.com/uc?id=1R7OcJI9ldJupsmuZDQxR4IK2YM5mg6Y3",
                    style = "height: 80vh; width: 100vh"),
               
                div(style = "position:absolute;
                            left: 42vh;
                            top: 61vh;
                            z-index: 9;
                            height: 16rem;
                            width: 16rem;",
                    # API key 입력받기
                    passwordInput("APIkey", "비밀 코드를 입력하세요"),
                    actionButton("go_Btn", " 가자!", icon("paper-plane")
                                 , style="float:right; color: #2D7262; background-color: #5DEDCB; border-color: #2D7262"
                                 )
                )
                )
            
          ),

  # fluid2
  hidden(fluidRow( id = "fluid2",
                   style = "border-style: groove; background-color:#D4E4DE; border-color:#2D7262",
    # fluid2/column1 / 주소 입력 받기       
                   column(style = "padding:10px",
                       width = 4,
                       textInput("userA",
      
                                 label = p("A 사용자의 주소를 입력하세요")
                       )

                    ),
                    column(style = "padding:10px",
                      width = 4,
                      textInput("userB",
                                label = p("B 사용자의 주소를 입력하세요")
                      ),
                      
                      #Find Place
                      actionButton("find_Btn", " Find Place", icon("search")
                                   , style="font-weight:bold; float:right; color: #2D7262; background-color: #5DEDCB; border-color: #2D7262")
                    ),
    
                   
    
                    # fluid2/column3/ 시각화 그래프
                    column(style = "padding:10px",
                      width = 4,
                      dateInput("date", label = p("약속일을 입력하세요"), value = Sys.Date())
                    )

    
                )

      ),
  
  hidden(fluidRow( id = "fluid3",
                   style = "border-style: groove; border-color:#2D7262",
      # tabsetPanel 생성            
      tabsetPanel(type = "tabs",
                  # tabPanel way1
                  tabPanel("추천1",
                           fluidRow(
                             # style = "border-style: groove; border-color:#2D7262",
                             column(
                               width = 8,
                               # style = "border-style: groove",
                               withLoader(google_mapOutput(outputId = "way1"), type="html", loader = "loader4")
                             ),
                             column(
                               width = 4,
                               # style = "border-style: groove",
                               style="height: 400px; overflow-y:scroll;",
                               htmlOutput("UI1")  
                             )
                           ),
                           br(),
                           fluidRow(
                             column(
                               width = 8,
                               # verbatimTextOutput("txt1")
                               htmlOutput("txt1")
                             ),
                             column(
                               width = 4,
                               actionButton("txt_Btn1",
                                            "자세히 알아보기")
                             )
                             ),
                           br(),
                           fluidRow(
                             style = "padding : 10px; margin:10px",
                             plotlyOutput("dong_plot1")
                           )
                          ),
                  # tabPanel way2
                  tabPanel("추천2",
                           fluidRow(
                             column(
                               width = 8,
                               # style = "border-style: groove",
                               withLoader(google_mapOutput(outputId = "way2"), type="html", loader = "loader4")
                             ),
                             column(
                               width = 4,
                               # style = "border-style: groove",
                               style="max-height: 400px; overflow-y:scroll;",
                               htmlOutput("UI2")
                             )
                           ),
                           br(),
                           fluidRow(
                             column(
                               width = 8,
                               htmlOutput("txt2")
                             ),
                             column(
                               width = 4,
                               actionButton("txt_Btn2",
                                            "자세히 알아보기"))
                             ),
                           br(),
                           fluidRow(
                             style = "padding : 10px; margin:10px",
                             plotlyOutput("dong_plot2")
                           )
                          ),
                  # tabPanel way3
                  tabPanel("추천3",
                           fluidRow(
                             column(
                               width = 8,
                               # style = "border-style: groove",
                               withLoader(google_mapOutput(outputId = "way3"), type="html", loader = "loader4")
                             ),
                             column(
                               width = 4,
                               # style = "border-style: groove",
                               style="max-height: 400px; overflow-y:scroll;",
                               htmlOutput("UI3")
                             )
                           ),
                           br(),
                           fluidRow(
                             column(
                               width = 8,
                               htmlOutput("txt3")
                             ),
                             column(
                               width = 4,
                               actionButton("txt_Btn3",
                                            "자세히 알아보기" )
                             )
                           ),
                           br(),
                           fluidRow(
                             style = "padding : 10px; margin:10px",
                             plotlyOutput("dong_plot3")
                           )
                          )
       )
  )
  ),
  br(),
  hidden(fluidRow( id = "fluid4",
                   # style = "border-style: groove;",
                   column(width = 2),
                   column(width = 8,
                          style = "border-style: groove; border-color:#2D7262",
                          tableOutput(
                            outputId = "table"
                            )
                          )
                   )
         ),
  
  hidden(fixedPanel(
    id = "fixedPanel",
    # style = "opacity: 0.65, right: 0; top: 0;",
    style = "position: absolute; width: 90vh; height: 100vh; left: 5vh; top: 5vh",
    # draggable = TRUE,
    withLoader(plotlyOutput("graph_plot"), type="html", loader = "loader10"),
    actionButton("plotlyBtn",
                 "뒤로")
    )
  )
)


# ) %>% withSpinner(type = 3, color="#0dc5c1", color.background = "#FFFFFF"), # 로딩 화면 출력
