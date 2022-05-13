library(ggmap)
library(jsonlite)
library(readxl)
library(shinyjs)
library(datasets)
library(prophet) #이번에 핵심이 되는 프로핏 라이브러리
library(dplyr)   #데이터프레임 다룰 때 필수인 라이브러리
library(ggplot2) #그래프 그릴때 필수인 라이브러리
library(scales)  #그래프 x축을 표현할 때 사용
library(plotly)  #인터랙티브한 그래프
library(stringr)



googleAPIkey <- NULL
res_data <-NULL
res_table <- NULL
sub_res <- NULL
userA_geo <- NULL
userB_geo <- NULL

map_api = "https://maps.googleapis.com/maps/api/directions/json?"

# 아이콘 이미지 
icon_a <- "https://drive.google.com/uc?id=1_brvvIwweOgvR6ks_NphOs0NozUPd7s8"
icon_b <- "https://drive.google.com/uc?id=10EYhH6wrvNowpsqWbfEpva_zMUeLq0AW"
# icon_c <- "https://drive.google.com/uc?id=1O6Hqrz4cfjQ3-6syDDwS5E5r5XsgD9VJ"
icon_shop <- "https://drive.google.com/uc?id=1RwPMC9lrceLGVvqixxp9sFEvM0Lj2rVi"
# icon_person1 <- "https://drive.google.com/uc?id=1p2FTb9dnLB760VHpa2wh0nR_-cflvr-Y"
# icon_person2 <-"https://drive.google.com/uc?id=1GGLyQx2p6vwoJVJk4HJ-v9YjxJ-D9Slh"
main_icon <- "https://drive.google.com/uc?id=1R7OcJI9ldJupsmuZDQxR4IK2YM5mg6Y3"


  
duration_time <- function(start, end) {
  #출발지, 도착지의 경도,위도 값 받기
  start_code <- start
  end_code <- end
  
  gg_map_url <- paste0(map_api,
                       "origin=", start_code$lat, ",",
                       start_code$lon, 
                       "&destination=",end_code$lat, ",",
                       end_code$lon, 
                       "&mode=transit&departure_time=now&key=", googleAPIkey
  )
  #JSON 받기
  tmp <- fromJSON(gg_map_url)
  tmp$routes$legs
  legs <- toJSON(tmp$routes$legs)
  legs <- fromJSON(legs)
  result_duration = legs[[1]]$duration
  result_duration <- unlist(strsplit(result_duration$text, " "))
  result_duration <- ifelse(length(result_duration) == 4, 
                            as.integer(result_duration[1])*60 + as.integer(result_duration[3]),
                            as.integer(result_duration[1]))
  
  #result_duration값 분단위 숫자로 반환하기!!
  return(result_duration)
}

# 상권 위치가 사각형 범위 내에 있는지
# is_possible <- function(A_geo, B_geo, shop_lon, shop_lat) {
#   return( (min(A_geo$lon, B_geo$lon) <= shop_lon) & (shop_lon <= max(A_geo$lon, B_geo$lon)) & 
#             (min(A_geo$lat, B_geo$lat) <= shop_lat) & (shop_lat <= max(A_geo$lat, B_geo$lat)) )
# }



# 상권 위치가 원 범위 내에 있는지
is_in_round <- function(A_geo, B_geo, shop_lon, shop_lat) {
  # 중심점 좌표 x, y 구하기
  x <- 0
  y <- 0
  x <- ifelse(A_geo$lon >= B_geo$lon, B_geo$lon + (A_geo$lon - B_geo$lon)/2, A_geo$lon + (B_geo$lon - A_geo$lon)/2)
  y <- ifelse(A_geo$lat >= B_geo$lat, B_geo$lat + (A_geo$lat - B_geo$lat)/2, A_geo$lat + (B_geo$lat - A_geo$lat)/2)
  
  ifelse(sqrt((shop_lon - x)^2 + (shop_lat - y)^2) <= 0.04, return(TRUE), return(FALSE))
}

# 거리의 차이가 가장 적은 상권 데이터 추출하기
is_in_shop <- function(data, userA_geo, userB_geo){
  # userA_geo <<- geocode(userA)
  # userB_geo <<- geocode(userB)
  
  in_shop <- NULL
  idx <- NULL
  i <- 1
  for (i in 1:nrow(data)) {
    if (is_in_round(userA_geo, userB_geo, data$lon[i], data$lat[i]))
      in_shop <- rbind(in_shop, data[i,])
  }
  
  # Sys.sleep(5)
  
  df <-as.data.frame(in_shop)
  df["timeA"] <- 0
  df["timeB"] <- 0
  i <- 1
  for (i in 1:nrow(df)) {
    df$timeA[i] <- duration_time(userA_geo, in_shop[i,c("lon","lat")])
    df$timeB[i] <- duration_time(userB_geo, in_shop[i,c("lon","lat")])
  }
  df$diff <- abs(df$timeA - df$timeB)
  df <- arrange(df,diff)
  ifelse(nrow(df) >= 5, return(df[c(1,3,5),]), return(head(df,3)))
  # return(head(df, 3))
}

# 맛집정보 처리
restaurant_info <- function(data, data2, street_idx) {
  cur_st <- data[street_idx, ]
  
  #string으로 "," 을 구분자로 넣은 res 값을 다시 split
  tmp_res_lst <- strsplit(cur_st$res, ",")[[1]] 
  
  #기본 초기화
  res_info <- data2[as.integer(tmp_res_lst[1]),]
  if (length(tmp_res_lst) > 1) {
    for(i in 2:length(tmp_res_lst)) {
      res_info <- rbind(res_info, data2[as.integer(tmp_res_lst[i]),])
    }
  }
  
  res <- NULL
  for(i in 1:length(res_info$name)) {
    tmp <- tags$div( style = "background-color:#D4E4DE;margin:10px; padding:10px",
      if(!is.na(res_info$site[i])) {
        tags$h4(tags$b(tags$a(res_info$name[i], href=res_info$site[i], style="color:#2D7262")))
      }
      else {
        tags$h4(tags$b(res_info$name[i], style="color:#2D7262"))
      },
      "주소 : ", res_info$new_address[i],
      if(!is.na(res_info$call[i])) {
        tags$h5("전화번호 : ",res_info$call[i])
      },
      if (!is.na(res_info$open_time[i])) {
        tags$h5("운영시간 : ",res_info$open_time[i])
      },
      if(!is.na(res_info$menu_info[i])) {
        tags$h5("메뉴정보 : ",res_info$menu_info[i])
      },
      tags$p(),
      tags$p()
    )
    res <- paste(res, tmp) 
  }
  return(res)
}

graph <- function(sub_res,excel_data, street_idx){
  #인터렉티브 그래프 만들기
  # p <- ggplot(data = mtcars$mpg, aes(x = displ, y = hwy, col = drv))+
  #   geom_point()
  station_name <- str_sub(excel_data$sub_name[street_idx], 1, nchar(excel_data$sub_name[street_idx])-1)
  
  new_sub_res <- sub_res[,c(2,4,5)]
  fns2 <- subset(new_sub_res, Group.1 == station_name)
  # head(fns2)
  new_fns2 <- fns2[,c(2,3)]
  
  names(new_fns2) = c("ds","y") #prophet형식에 맞게 바꿈
  new_fns2$ds <- as.character(new_fns2$ds)
  #yyyy-mm-dd형식으로 바꿔주기
  new_fns2$ds <- as.Date(new_fns2$ds,format="%Y%m%d")
  
  # Prophet
  #12월 31일까지 데이터를 기반으로 시계열 예측 #기한을 설정 100일 
  m <- prophet(new_fns2)
  future <- make_future_dataframe(m, periods = 100)
  
  forecast <- predict(m, future)
  # as$yhat
  quan <- quantile(fns2$x,prob=c(0.75,0.25))
  
  #ggplot 그래프
  p<- plot(m,forecast) + 
    geom_line(color = "red", size = 1) + geom_point(color = "red", size = 0.1)+
    ggtitle(paste0(station_name, "역 승차객 수 시계열 예측"))+
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
    labs(x="날짜", y="승차객 수")+
    theme(axis.title = element_text(size = 18, color = "darkblue"))+
    scale_y_continuous(breaks=seq(0,60000,5000))+ 
    scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%m월"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
    geom_hline(yintercept = quan[1]) +
    geom_hline(yintercept = quan[2])

  return(p)
}

confusion <- function(sub_res, excel_data, street_idx, date){
  
  new_sub_res <- sub_res[,c(2,4,5)]
  #해당 역만 빼내기
  street_idx <- as.integer(street_idx)
  station_name <- str_sub(excel_data$sub_name[street_idx], 1, nchar(excel_data$sub_name[street_idx])-1)
  fns2 <- subset(new_sub_res, Group.1 == station_name)
  new_fns2 <- fns2[,c(2,3)]
  #파일 읽어오기

  names(new_fns2) = c("ds","y") #prophet형식에 맞게 바꿈
  new_fns2$ds <- as.character(new_fns2$ds)

  #yyyy-mm-dd형식으로 바꿔주기
  new_fns2$ds <- as.Date(new_fns2$ds,format="%Y%m%d")

  #Facebook Prophet
  #12월 31일까지 데이터를 기반으로 시계열 예측 #기한을 설정 50일
  m <- prophet(new_fns2)
  future <- make_future_dataframe(m, periods = 100)
  forecast <- predict(m, future)
  as <- filter(forecast, ds == as.Date(date))
  # as$yhat
  quan <- quantile(fns2$x,prob=c(0.75,0.25))

  confusion_res <- ""
  if(as$yhat>quan[1]){
    confusion_res <- span(style="color:red", "혼잡")
  } else if (as$yhat<quan[2]){
    confusion_res <- span(style="color:blue", "원활")
  } else{
    confusion_res <- span(style="color:orange", "보통")
  }

  return(confusion_res)
}

draw_plot <- function(time_p, dong_num, dong_name) {
  tmp_graph <- cbind(time_p[, 1], time_p[as.character(dong_num)])
  colnames(tmp_graph) <- c("idx", "v1")
  time <- factor(tmp_graph$idx)
  q <- ggplot(tmp_graph, aes(x=idx, y=v1)) + 
    geom_point(aes(colour = time), size = 4) +
    geom_line() + 
    stat_smooth(color='black',fill='grey'
                , formula = y ~ x, method = loess
    )+
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)) +
    labs(x="time", y="population", title= paste(dong_name, "시간대별 인구수"))
  
  return(q)
  
  # tmp_graph <- cbind(time_p[, 1], time_p[as.character(dong_num)])
  # colnames(tmp_graph) <- c("idx", "v1")
  # q <- ggplot(tmp_graph, aes(x=idx, y=v1)) + 
  #   geom_point(aes(colour = factor(idx)), size = 4) +
  #   geom_line() + 
  #   stat_smooth(color='black',fill='grey')+ 
  #   labs(x="time", y="population", title= "행정동 시간대별 인구수")
  # 
  # return(q)
} 


function(input, output, session) {
  
  restaurants <-
    reactive({
      restaurants <- read_excel("./www/restaurant_geocode.xlsx")
    })
  
  sub_res <-
    reactive({
      sub_res <- read.csv("./www/sub_res.csv")
    })
  
  excel_data <-
    reactive({
      restaurants <- read_excel("./www/excel_data.xlsx")
    })
  
  time_p <-
    reactive({
      restaurants <- read_excel("./www/time_p.xlsx")
    })
  
  # resister API_KEY
  observeEvent(input$go_Btn,{
      googleAPIkey <<- input$APIkey
      register_google(googleAPIkey)
   
      shinyjs::hide(id = "fluid1")
      shinyjs::show(id = "fluid2")
  })
  
  # 버튼 이벤트 / 지도 그리기 
  observeEvent(input$find_Btn,{
    shinyjs::show(id = "fluid3")
    shinyjs::show(id = "fluid4")
    
    userA_geo <<- geocode(input$userA)
    userB_geo <<- geocode(input$userB)
    
    
    res_data <<- is_in_shop(excel_data(), userA_geo, userB_geo)
    idx_table <- data.frame("추천" = c(1,2,3))
    res_table <<- res_data[,c("sub_name","cityname", "dong", "timeA", "timeB", "diff")]
    res_table <<- rename(res_table,
                         c("역명" = "sub_name",
                           "지역명" = "cityname",
                           "행정동" = "dong",
                           "시간A" = "timeA",
                           "시간B" = "timeB",
                           "시간차" = "diff"))
    res_table <<- cbind(idx_table, res_table)
    res_table$추천 <- as.integer(res_table$추천)
    
  ########################탭1번#############################  
    output$way1 <- renderGoogle_map({
      map <- google_map(
        key = googleAPIkey,
        location = c(res_data$lat[1], res_data$lon[1]),
        zoom = 15
      )
      map %>% googleway::add_markers(lat = c(userA_geo$lat, res_data$lat[1], userB_geo$lat),
                          lon = c(userA_geo$lon, res_data$lon[1], userB_geo$lon),
                          marker_icon = c(icon_a, icon_shop, icon_b),
                          mouse_over = c(input$userA, res_data$area_name[1], input$userB)) %>%
        add_polylines(lat = c(userA_geo$lat, res_data$lat[1], userB_geo$lat),
                      lon = c(userA_geo$lon, res_data$lon[1], userB_geo$lon),
                      stroke_weight = 7, stroke_colour = "#FF0000"
        )
    })
    output$UI1 <- renderUI({
      HTML(restaurant_info(excel_data(), restaurants(), as.numeric(res_data$idx[1])))
    })
    
    output$dong_plot1 <- renderPlotly({
      ggplotly(draw_plot(time_p(), as.numeric(res_data$dong_code[1]), res_data$dong[1]))
    })
    
    output$txt1 <- renderUI({
      tags$div(
        style = "display:inline; padding: 10px; font-weight:bold; font-size:15pt",
        tags$span(as.character(input$date)), 
        tags$span(paste0("일자의 ", res_data$sub_name[1], " 혼잡도는 '")),
        confusion(sub_res(), excel_data(), res_data$idx[1], input$date),
        tags$span("'입니다." )
        # confusion(sub_res(), excel_data(), res_data$idx[1], input$date)
        # "test"
      )
      # paste(input$date, " ",res_data$sub_name[1],"역, 혼잡도는 ", confusion(sub_res(), excel_data(), res_data$idx[1], input$date)," 예상됩니다.")
      # "test"
    })
    

    ########################탭2번#############################
    output$way2 <- renderGoogle_map({
      map <- google_map(
        key = googleAPIkey,
        location = c(res_data$lat[2], res_data$lon[2]),
        zoom = 15
      )
      map %>% googleway::add_markers(lat = c(userA_geo$lat, res_data$lat[2], userB_geo$lat),
                          lon = c(userA_geo$lon, res_data$lon[2], userB_geo$lon),
                          marker_icon = c(icon_a, icon_shop, icon_b),
                          mouse_over = c(input$userA, res_data$area_name[2], input$userB)
                          ) %>% 
        add_polylines(lat = c(userA_geo$lat, res_data$lat[2], userB_geo$lat),
                      lon = c(userA_geo$lon, res_data$lon[2], userB_geo$lon),
                      stroke_weight = 7, stroke_colour = "#FF0000"
        )
    })
    
    output$UI2 <- renderUI({
      HTML(restaurant_info(excel_data(), restaurants(), as.numeric(res_data$idx[2])))
    })
    
    output$dong_plot2 <- renderPlotly({
      ggplotly(draw_plot(time_p(),as.numeric(res_data$dong_code[2]), res_data$dong[2]))
    })
    
    output$txt2 <- renderUI({
      tags$div(
        style = "display:inline; padding: 10px; font-weight:bold; font-size:15pt",
        tags$span(as.character(input$date)), 
        tags$span(paste0("일자의 ", res_data$sub_name[2], " 혼잡도는 '")),
        confusion(sub_res(), excel_data(), res_data$idx[2], input$date),
        tags$span("'입니다." )
        # confusion(sub_res(), excel_data(), res_data$idx[1], input$date)
        # "test"
      )
      # paste(input$date, " ",res_data$sub_name[2],"역, 혼잡도는 ", confusion(sub_res(), excel_data(), res_data$idx[2], input$date)," 예상됩니다.")
    })
    
    ########################탭3번#############################
    output$way3 <- renderGoogle_map({
      map <- google_map(
        key = googleAPIkey,
        location = c(res_data$lat[3], res_data$lon[3]),
        zoom = 15
      )
      map %>% googleway::add_markers(lat = c(userA_geo$lat, res_data$lat[3], userB_geo$lat),
                          lon = c(userA_geo$lon, res_data$lon[3], userB_geo$lon),
                          marker_icon = c(icon_a, icon_shop, icon_b),
                          mouse_over = c(input$userA, res_data$area_name[3], input$userB)) %>% 
        add_polylines(lat = c(userA_geo$lat, res_data$lat[3], userB_geo$lat),
                      lon = c(userA_geo$lon, res_data$lon[3], userB_geo$lon),
                      stroke_weight = 7, stroke_colour = "#FF0000"
        )
    })
    
    output$UI3 <- renderUI({
      HTML(restaurant_info(excel_data(), restaurants(), as.numeric(res_data$idx[3])))
    })
    
    output$dong_plot3 <- renderPlotly({
      ggplotly(draw_plot(time_p(),as.numeric(res_data$dong_code[3]), res_data$dong[3]))
    })
    
    output$txt3 <- renderUI({
      tags$div(
        style = "display:inline; padding: 10px; font-weight:bold; font-size:15pt",
        tags$span(as.character(input$date)), 
        tags$span(paste0("일자의 ", res_data$sub_name[3], " 혼잡도는 '")),
        confusion(sub_res(), excel_data(), res_data$idx[3], input$date),
        tags$span("'입니다." )
        # confusion(sub_res(), excel_data(), res_data$idx[1], input$date)
        # "test"
      )
      # tag$h3(
      # paste(input$date, " ",res_data$sub_name[3],"역, 혼잡도는 ", confusion(sub_res(), excel_data(), res_data$idx[3], input$date)," 예상됩니다.")
      # )
      })
    
    ##################res_table###################################################################
    output$table <- renderTable({
      # res_data
      align = "c"
      res_table
    })
    
  })
  
  ################################자세히 알아보기####################################
  observeEvent(input$txt_Btn1,{
    
    shinyjs::hide(id = "fluid2")
    shinyjs::hide(id = "fluid3")
    shinyjs::hide(id = "fluid4")
    shinyjs::show(id = "fixedPanel")
    
    output$graph_plot <- renderPlotly({
      ggplotly(graph(sub_res(), excel_data(), res_data$idx[1]))
    })
  })
  
  observeEvent(input$txt_Btn2,{
    
    shinyjs::hide(id = "fluid2")
    shinyjs::hide(id = "fluid3")
    shinyjs::hide(id = "fluid4")
    shinyjs::show(id = "fixedPanel")
    
    output$graph_plot <- renderPlotly({
      ggplotly(graph(sub_res(), excel_data(), res_data$idx[2]))
    })
  })
  
  observeEvent(input$txt_Btn3,{
    
    shinyjs::hide(id = "fluid2")
    shinyjs::hide(id = "fluid3")
    shinyjs::hide(id = "fluid4")
    shinyjs::show(id = "fixedPanel")
    
    output$graph_plot <- renderPlotly({
      ggplotly(graph(sub_res(), excel_data(), res_data$idx[3]))
    })
  })
  
  observeEvent(input$plotlyBtn,{
    shinyjs::hide(id = "fixedPanel")
    shinyjs::show(id = "fluid2")
    shinyjs::show(id = "fluid3")
    shinyjs::show(id = "fluid4")
  })
}
