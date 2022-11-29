
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cyborg"),
                tags$head(tags$style(
                    HTML('
         #sidebar {
            background-color: #000000;
         }
        
        #navbar{background-color: #222222;}

        body, label, input, button, select { 
          font-family: "Arial";
        }')
                )),
    navbarPage(id = "navbar",
    "Online Chess Game Analyzer",
    tabPanel(
        "Home",icon=icon("bars"),
        div(style="width: 30%;",sidebarPanel(id = "sidebar", width = 5)),
        mainPanel(align="center",
                  h2("Online Chess Game Analysis"),
                  h4("Evaluation and Comparison vs Engine"),br(),
                  tags$image(src="Home_chess_pic.jpg",height="500px",width="800px",
                  style="display: block; margin-left: auto; margin-right: auto;"),br(),br() 
                  )
    ),
    tabPanel("About",icon = icon("table"),
             sidebarLayout(sidebarPanel(h4("Openings"),
                                        tags$image(src="openings.png",height="300px",width="300px"),
                                        br(),
                                        HTML("<p>If you want to check various openings , please consider <a href='https://support.chess.com/article/285-what-are-chess-openings'> going here</a>!</p>"),
                                        h4("Rating"),
                                        HTML("<p>To know everything about elo rating and rating list, please consider <a href='https://ratings.fide.com/calc.phtml?page=change'> going here </a>!</p>"),
                                        h4("Centipawn-loss"),
                                        HTML("<p>To know more about centipawn-loss and how it is calculated, please consider <a href='https://web.chessdigits.com/articles/predicting-rating-from-centipawn-loss'> going here </a>!</p>"),
                                        ),

             mainPanel(
                         h4("Abstract"),
                       p("
                         In this project, we have taken a data set containing online chess games
                         played on “Lichess”. We use visualization techniques in R to evaluate
                         the level of play vs that of the “strongest” engine available. Following this, we try to compare the “centipawn loss” for “rating”, “blunders”, “mistakes”, and “inaccuracies”.This is done for both “black” and
                         “white”.After the entire process of visualization is done we make a dashboard to explore the above-posed questions.
                         Keywords: Rating,Centipawn Loss,Opening Ply,R, ggplot, visualisation"),
                       hr(),
                        h4("Introduction"),
                       p("Chess has been a symbol of intelligence, impeccable strategy, and superior
understanding for the last 4 thousand years. Its roots have been linked to India
and sure enough, our country has produced some of the greatest players in the
history of the game. But like most things, chess also been immense if not the most
affected by the advent of computers and AI. Till now the game of chess has not
been solved and therein lies the beauty of the game.",
br(),br(),                        
"Things to remember:",
br(),
"• Chess is not solved",
br(),
"• Objective evaluation refers to the best move played by the “strongest”
engine",
br(),
"• Ratings given to various players are relative to the play of the
general community"),hr(),
h4("Definition of Key variables"),
p("• Rating-A chess rating system is a system used in chess to estimate the
   strength of a player, based on their performance versus other players.",
   br(),br(),
   "• Opening Ply-In two-player sequential games, a ply is one turn taken by one
   of the players. In standard chess terminology, one move consists of a turn by
   each player, therefore a ply in chess is a half-move.",
  br(),br(),
   "• Centipawm Loss-Centipawn loss is how many hundredths of a pawn your
   move differs from the engine’s best move. The closer to zero your score the
   better you are. Average Centipawn Loss is simply the average of all the
   centipawn losses per move over a whole game, or even many games if you
   care to calculate.")
                       ))
                  
    ),
tabPanel("Data Analysis",icon = icon("bar-chart-o"),
         sidebarMenu(
             menuItem("About the dataset",tabName = "menu3"),
             menuItem("Univariate Plots",tabName = "menu1"),
             menuItem("Multivariate Plots",tabName = "menu2"),
             menuItem("Opening Studies",tabName="menu4"),hr()
            # menuItem("Conclusion",tabName = "menu5")
             
         ),
         tabItems(
             tabItem("menu1", h3(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp"),"Univariate Plots"),fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("coloumn","Choose the variable",choices = c("Main line theory","Accuracy of White Games","Accuracy of Black Games","Accuracy of White Opening Ply","Accuracy of Black Opening Ply")),
                         actionButton("go","Apply")
                     ),
                     mainPanel(plotOutput("chart"),textOutput("write"))
                 )
             )),
    
             tabItem("menu2",h3("Effect on centipawn loss against type of Bad Moves"),fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectInput("tp_mv","Type of bad moves:",
                                            c("Inaccuracies","Mistakes","Blunders","Inaccuracies and Mistakes","Inaccuracies and Blunders","Mistakes and Blunders","Inaccuracies, Mistakes and Blunders")),
                         radioButtons("player","Choose side",choices=c("White","Black")),
                         actionButton("go1","Apply")
                     ),
                     mainPanel(plotOutput("chart1"),textOutput("text1"))
                 )
             ),
             h3("Proportion of Games by Number Of Bad Moves"),fluidPage(sidebarLayout(
                 sidebarPanel(
                    
                     radioButtons("player1","Choose side",choices=c("White","Black")),
                     actionButton("go2","Apply")
                 ),
                 mainPanel(plotOutput("chart2"),textOutput("text2"))
             )
             )
             ),
             tabItem("menu4",h3("Comparison of Opening Plies"),fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("player2","Choose side",choices=c("White","Black")),
                         actionButton("go3","Apply")
                     ),
                     mainPanel(plotOutput("chart3"),textOutput("text3"))
                 )
             )),
             tabItem("menu3",h3("Summary Statistics of DataSet"),fluidPage(
                  sidebarLayout(
                      sidebarPanel(
                          radioButtons("stat","Choose table",choices=c("Head","Tail","Summary")),
                          actionButton("go4","Apply")
                      ),
                      mainPanel(dataTableOutput("table1"))
                  ) 
             ))
             
             
             
             )),

tabPanel("Conclusion",icon = icon("list-alt"),
         
    mainPanel(h3("Conclusion :"),br(),
        p("• Effect of Rating-General play from white and black leads to lower centipawn loss
with increasing rating.",
          br(),br(),
          "• Effect of Theory-General play from white and black leads to lower centipawn loss
when following more “Opening Plies”",
          br(),br(),
          "• Effect of “bad moves”-Blunders lead to greater centipawn loss affecting
accuracy levels for the same number of Mistakes and Inaccuracies. The least
is due to inaccuracies.",
          br(),br(),
          "• General Trend of “Bad Moves”-In general more games have more minor
inaccuracies in comparison to blunders or big mistakes.
",
          br(),br(),
          "• Effect of Opening Choice-Certain opening choices, in the long run, favor
white like that of the “French Defence”, some pretty solid positions result
from “English”, “Spanish”, “Queen Pawn”.The “Italian” leads to a
dual-edged position, whereas “King’s Gambit” is dubious for white with
correct play.",style = "font-size:15px"),
        hr(),
        h3("Shortcomings :"),br(),
        p("• Engines are ever evolving.There is no absolute best",
          br(),br(),
          "• Ratings are relative.",
          br(),br(),
          "• There is bias of theory", style = "font-size:15px"),hr()
    )
),
tabPanel("References & Contact Info.",icon = icon("circle-info"),
         mainPanel(br(),br(),hr(),h4("References :"),
                   HTML("<p> • If you want to check the data set in kaggle , please consider  going to <a href='https://www.kaggle.com/datasets/ahmedalghafri/lichess-chess-games-statistics?resource=download'>  Kaggle-Lichess:Python Chess Games Statistics</a>!</p>"),
                   hr(),
                   br(),br(),
                   h4("Contact Info :"),
                   p("My email id is listed as below:"),
                   uiOutput("email"),
                   hr()
                   ))
    
    

        ) )



# Define server logic required to draw a histogram
server <- function(input, output) {
    url <- a("shreyanc@cmi.ac.in", href = "mailto: shreyanc@cmi.ac.in")
    output$email <- renderUI({
        tagList("", url)
    })
    chess_dataset<-read.csv('./data/Chess games stats.csv')
    attach(chess_dataset)
    Op=array(chess_dataset["Opening.Ply"])
    counter<-c(rep(0,28))
    for(i in 1:18637){for(j in 1:28){
        if(Opening.Ply[i]==j){counter[j]<-counter[j]+1}    
    }}
    Mop<-max(Opening.Ply)
    CO<-as.data.frame(counter)
    N_PLY<-c(seq(1,28,1))
    CO["OPENING_PLY"]<-N_PLY
    
    df1 <- chess_dataset %>% 
        select(White.s.Number.of.Inaccuracies, White.Centi.pawn.Loss)   %>%
        group_by(White.s.Number.of.Inaccuracies) %>% 
        summarise(mean_wcpl1 = mean(White.Centi.pawn.Loss)) 
    
    df2 <- chess_dataset %>% 
        select(White.s.Number.of.Mistakes, White.Centi.pawn.Loss) %>%
        group_by(White.s.Number.of.Mistakes) %>% 
        summarise(mean_wcpl2 = mean(White.Centi.pawn.Loss)) 
    df3 <- chess_dataset %>% 
        select(White.s.Number.of.Blunders, White.Centi.pawn.Loss) %>%
        group_by(White.s.Number.of.Blunders) %>% 
        summarise(mean_wcpl3 = mean(White.Centi.pawn.Loss)) 
    
    
    bf1 <- chess_dataset %>% 
        select(Black.s.Number.of.Inaccuracies, Black.Centi.pawn.Loss)   %>%
        group_by(Black.s.Number.of.Inaccuracies) %>% 
        summarise(mean_bcpl1 = mean(Black.Centi.pawn.Loss)) 
    
    bf2 <- chess_dataset %>% 
        select(Black.s.Number.of.Mistakes, Black.Centi.pawn.Loss) %>%
        group_by(Black.s.Number.of.Mistakes) %>% 
        summarise(mean_bcpl2 = mean(Black.Centi.pawn.Loss)) 
    bf3 <- chess_dataset %>% 
        select(Black.s.Number.of.Blunders, Black.Centi.pawn.Loss) %>%
        group_by(Black.s.Number.of.Blunders) %>% 
        summarise(mean_bcpl3 = mean(Black.Centi.pawn.Loss)) 
    
    x1<-chess_dataset %>%
        select(White.s.Number.of.Blunders)
    x1["Counter"]<-(c(rep(1,18637)))
    x3<-chess_dataset %>%
        select(White.s.Number.of.Mistakes)
    x3["Counter"]<-(c(rep(2,18637)))
    x5<-chess_dataset %>%
        select(White.s.Number.of.Inaccuracies)
    x5["Counter"]<-(c(rep(3,18637)))
    colnames(x1)<-c("Value","Counter")
    colnames(x3)<-c("Value","Counter")
    colnames(x5)<-c("Value","Counter")
    x2<-rbind(x1,x3)  
    x4<-rbind(x2,x5)
    
    z1<-chess_dataset %>%
        select(Black.s.Number.of.Blunders)
    z1["Counter"]<-(c(rep(1,18637)))
    z3<-chess_dataset %>%
        select(Black.s.Number.of.Mistakes)
    z3["Counter"]<-(c(rep(2,18637)))
    z5<-chess_dataset %>%
        select(Black.s.Number.of.Inaccuracies)
    z5["Counter"]<-(c(rep(3,18637)))
    colnames(z1)<-c("Value","Counter")
    colnames(z3)<-c("Value","Counter")
    colnames(z5)<-c("Value","Counter")
    z2<-rbind(z1,z3)  
    z4<-rbind(z2,z5)
    
    
    y1<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C0") | str_detect(Opening.ECO, "C1")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_wcpl = mean(White.Centi.pawn.Loss))
    ctm<-array(0,6)
    ctm[1]<-mean(y1$avg_wcpl)    
    y2<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C3")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_wcpl1 = mean(White.Centi.pawn.Loss))
    ctm[2]<-mean(y2$avg_wcpl1)
    
    y3<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C5")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_wcpl2 = mean(White.Centi.pawn.Loss))
    ctm[3]<-mean(y3$avg_wcpl2)
    
    y4<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C6")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_wcpl3 = mean(White.Centi.pawn.Loss))
    ctm[4]<-mean(y4$avg_wcpl3)
    
    y5<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "A1") | str_detect(Opening.ECO, "A2")|str_detect(Opening.ECO, "A3")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_wcpl4 = mean(White.Centi.pawn.Loss))
    ctm[5]<-mean(y5$avg_wcpl4)
    
    y6<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "A4") | str_detect(Opening.ECO, "A5")|str_detect(Opening.ECO, "A6")|str_detect(Opening.ECO, "A7")|str_detect(Opening.ECO, "A8")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_wcpl5 = mean(White.Centi.pawn.Loss))
    ctm[6]<-mean(y6$avg_wcpl5)
    BOX<-data.frame(ctm)
    BOX["Opening"]<-c("French Defence","King's Gambit","Italian","Spanish","English","Queen Pawn")
    colnames(BOX)<-c("White's average Centipawn Loss","Opening")
    
    BOX1<-BOX %>%
          arrange(`White's average Centipawn Loss`) %>% 
          mutate(Opening = fct_reorder(Opening, `White's average Centipawn Loss`))
    
    b1<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C0") | str_detect(Opening.ECO, "C1")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_bcpl = mean(Black.Centi.pawn.Loss))
    btm<-array(0,6)
    btm[1]<-mean(b1$avg_bcpl)    
    b2<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C3")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_bcpl1 = mean(Black.Centi.pawn.Loss))
    btm[2]<-mean(b2$avg_bcpl1)
    
    b3<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C5")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_bcpl2 = mean(Black.Centi.pawn.Loss))
    btm[3]<-mean(b3$avg_bcpl2)
    
    b4<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "C6")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_bcpl3 = mean(Black.Centi.pawn.Loss))
    btm[4]<-mean(b4$avg_bcpl3)
    
    b5<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "A1") | str_detect(Opening.ECO, "A2")|str_detect(Opening.ECO, "A3")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_bcpl4 = mean(Black.Centi.pawn.Loss))
    btm[5]<-mean(b5$avg_bcpl4)
    
    b6<-chess_dataset %>% 
        filter(str_detect(Opening.ECO, "A4") | str_detect(Opening.ECO, "A5")|str_detect(Opening.ECO, "A6")|str_detect(Opening.ECO, "A7")|str_detect(Opening.ECO, "A8")) %>% 
        group_by(Opening.ECO) %>% 
        summarise(avg_bcpl5 = mean(Black.Centi.pawn.Loss))
    btm[6]<-mean(b6$avg_bcpl5)
    BOX2<-data.frame(btm)
    BOX2["Opening"]<-c("French Defence","King's Gambit","Italian","Spanish","English","Queen Pawn")
    colnames(BOX2)<-c("Black's average Centipawn Loss","Opening")
    
    
    BOX3<-BOX2 %>%
        arrange(`Black's average Centipawn Loss`) %>% 
        mutate(Opening = fct_reorder(Opening, `Black's average Centipawn Loss`))
    
    
      observeEvent(input$go,
                 if(input$coloumn=="Main line theory"){
                     output$chart<-renderPlot({
                         ggplot(chess_dataset,aes(x=Opening.Ply)) +
                             geom_histogram(bins=28,binwidth=0.5,color="darkblue", fill="lightblue")+ 
                             labs(title="MAIN LINE THEORY")+
                             xlab("Number of moves played following theory")+
                             ylab("Number of games played")
                     })
                     output$write<-renderPrint({
                         print("As seen from the above graph,with increase in number of opening plies or theoretical moves ,the frequency of such games decreases")
                     })}
                 else if(input$coloumn=="Accuracy of White Games"){
                         output$chart<-renderPlot({
                             ggplot(chess_dataset, aes(x=(White.Rating), y=(White.Centi.pawn.Loss))) +
                                 geom_point(col="blue",size=0.5)+
                                 labs(title="ACCURACY OF  WHITE GAMES")+
                                 xlab("White Rating")+
                                 ylab("Average Centipawn Loss")
                             
                     })
                         output$write<-renderPrint({
                             print("Scatter Plot representation:We see that with increasing rating the
centipawn loss per game seems to decrease to a lower cluster.")
                         })     
                 }
                 else if(input$coloumn=="Accuracy of Black Games"){
                     output$chart<-renderPlot({
                         ggplot(chess_dataset, aes(x=(Black.Rating), y=(Black.Centi.pawn.Loss))) +
                             geom_point(col="blue",size=0.5)+
                             labs(title="ACCURACY OF  BLACK GAMES")+
                             xlab("Black Rating")+
                             ylab("Average Centipawn Loss")
                         
                     })
                     output$write<-renderPrint({
                         print("Scatter Plot representation:We see that with increasing rating the
centipawn loss per game seems to decrease to a lower cluster.")
                     })     
                 }
                 
                 else if(input$coloumn=="Accuracy of White Opening Ply"){
                     output$chart<-renderPlot({
                         ggplot(chess_dataset, aes(x=Opening.Ply, y=White.Centi.pawn.Loss))+
                             geom_point()+
                             labs(title="ACCURACY OF WHITE OPENING PLY", x="Opening Ply")+
                             theme(plot.title = element_text(hjust = 0.5))
                         
                         
                     })
                     output$write<-renderPrint({
                         print("Scatter Plot representation:As intution would have suggested,centipawn loss decreases with more no of “opening plies”
i.e theory being followed.")
                     })     
                 }
                 
                 else if(input$coloumn=="Accuracy of Black Opening Ply"){
                     output$chart<-renderPlot({
                         ggplot(chess_dataset, aes(x=Opening.Ply, y=Black.Centi.pawn.Loss))+
                             geom_point()+
                             labs(title="ACCURACY OF BLACK OPENING PLY", x="Opening Ply")+
                             theme(plot.title = element_text(hjust = 0.5))
                         
                         
                     })
                     output$write<-renderPrint({
                         print("Scatter Plot representation:As intution would have suggested,centipawn loss decreases with more no of “opening plies”
i.e theory being followed.")
                     })     
                 } 
                 )
flag1=0
flag2=0
flag3=0
    observeEvent(input$go1,
                 if(input$player=="White"){
                 if(input$tp_mv==c("Inaccuracies")){
                     flag1=1
                     output$chart1<-renderPlot({
                         ggplot(data=df1,aes(White.s.Number.of.Inaccuracies,mean_wcpl1)) +
                             geom_line()+
                             geom_smooth(aes(color="Yellow")) +
                             scale_color_identity(labels=c("Inaccuracies"),guide = "legend")+
                             geom_point(size = 1)+
                             labs(title = "Centipawn loss for Inaccuracies",
                                  x = "Counter",
                                  y = "White's Mean  Centipawn Loss",
                                  color="Deviation From Best Moves")
                     })
                     output$text1<-renderPrint({print("Line Graph representation: “Inaccuracies” result in minimun centipawn  loss")})
                 }
                 else if(input$tp_mv==c("Mistakes")){
                     flag2=1
                     output$chart1<-renderPlot({
                         ggplot(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2)) +
                             geom_line(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),col="purple")+
                             geom_smooth(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2,color="Orange"))+
                             scale_color_identity(labels=c("Mistakes"),guide = "legend")+
                             geom_point(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),size=1)+
                             labs(title = "Centipawn loss for Blunders",
                                  x = "Counter",
                                  y = "White's Mean  Centipawn Loss",
                                  color="Deviation From Best Moves")
                     })
                     
                     output$text1<-renderPrint({print("Line Graph representation: “Mistakes” result in more centipawn  loss")})
                 }
                 else if(input$tp_mv==c("Blunders")){
                     flag3=1
                     output$chart1<-renderPlot({
                         ggplot(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3))+
                             geom_line(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),col="blue")+
                             geom_smooth(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3,color="Red"))+
                             scale_color_identity(labels=c("Blunders"),guide = "legend")+
                             geom_point(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),size=1)+
                             labs(title = "Centipawn loss for Blunders",
                                  x = "Counter",
                                  y = "White's Mean  Centipawn Loss",
                                  color="Deviation From Best Moves")
                 })
                     output$text1<-renderPrint({print("Line Graph representation: “Blunders” result in maximum centipawn  loss")})
                 }
                 else if(input$tp_mv=="Inaccuracies and Mistakes"){
                    output$chart1<-renderPlot({
                        ggplot(data=df1,aes(White.s.Number.of.Inaccuracies,mean_wcpl1)) +
                            geom_line()+
                            geom_smooth(aes(color="Yellow")) +
                            scale_color_identity(labels = c("Inaccuracies", "Mistakes"),guide = "legend")+
                            geom_line(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),col="purple")+
                            geom_smooth(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2,color="Orange"))+
                            scale_color_identity(labels = c("Inaccuracies", "Mistakes"),guide = "legend")+
                            geom_point(size = 1) +
                            geom_point(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),size=1)+
                            labs(title = "Centipawn loss for Inaccuracies and Mistakes",
                                 x = "Counter",
                                 y = "White's Mean  Centipawn Loss",
                                 color="Deviation From Best Moves")
                        
                    })
                     
                    output$text1<-renderPrint({print("Line Graph representation: 'Inaccuracies' result in less centipawn  loss than 'Mistakes'")})
                 }
                     else if(input$tp_mv=="Inaccuracies and Blunders"){
                         output$chart1<-renderPlot({
                             ggplot(data=df1,aes(White.s.Number.of.Inaccuracies,mean_wcpl1)) +
                                 geom_line()+
                                 geom_smooth(aes(color="Yellow")) +
                                 scale_color_identity(labels = c("Inaccuracies", "Blunders"),guide = "legend")+
                                 geom_line(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),col="blue")+
                                 geom_smooth(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3,color="Red"))+
                                 geom_point(size = 1) +
                                 geom_point(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),size=1)+
                                 labs(title = "Centipawn loss for Inaccuracies and Blunders",
                                      x = "Counter",
                                      y = "White's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                             
                         })
                         
                         output$text1<-renderPrint({print("Line Graph representation: 'Blunders' have a much greater effect on centipawn loss than 'Inaccuracies'")})
                     }
                     
                     else if(input$tp_mv=="Mistakes and Blunders"){
                         output$chart1<-renderPlot({
                             ggplot(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2))+
                                 geom_line(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),col="purple")+
                                 geom_smooth(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2,color="Orange"))+
                                 scale_color_identity(labels = c("Mistakes","Blunders"),guide = "legend")+
                                 geom_line(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),col="blue")+
                                 geom_smooth(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3,color="Red"))+
                                 geom_point(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),size=1)+
                                 geom_point(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),size=1)+
                                 labs(title = "Centipawn loss for Mistakes and Blunders",
                                      x = "Counter",
                                      y = "White's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                         })
                         
                         output$text1<-renderPrint({print("Line Graph representation: 'Blunders' results in maximum centipawn  loss,followed by 'Mistakes'")})
                     }
                     
                     else if(input$tp_mv=="Inaccuracies, Mistakes and Blunders"){
                         output$chart1<-renderPlot({
                             ggplot(data=df1,aes(White.s.Number.of.Inaccuracies,mean_wcpl1)) +
                                 geom_line()+
                                 geom_smooth(aes(color="Yellow")) +
                                 scale_color_identity(guide = "legend")+
                                 geom_line(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),col="purple")+
                                 geom_smooth(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2,color="Orange"))+
                                 scale_color_identity(guide = "legend")+
                                 geom_line(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),col="blue")+
                                 geom_smooth(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3,color="Red"))+
                                 scale_color_identity(breaks = c("Yellow", "Orange", "Red"),
                                                      labels = c("Inaccuracies", "Mistakes", "Blunders"),guide = "legend")+
                                 geom_point(size = 1) +
                                 geom_point(data=df2,aes(White.s.Number.of.Mistakes,mean_wcpl2),size=1)+
                                 geom_point(data=df3,aes(White.s.Number.of.Blunders,mean_wcpl3),size=1)+
                                 labs(title = "Centipawn loss for Inaccuracies,Mistakes and Blunders",
                                      x = "Counter",
                                      y = "White's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                         })
                         
                         output$text1<-renderPrint({print("Line Graph representation: “Blunders” results in maximum centipawn loss followed by “Mistakes” and least effect is due to “Inaccuracies.”")})
                     }
                 } 
                 
                 else if(input$player=="Black"){
                     if(input$tp_mv==c("Inaccuracies")){
                         flag1=1
                         output$chart1<-renderPlot({
                             ggplot(data=bf1,aes(Black.s.Number.of.Inaccuracies,mean_bcpl1)) +
                                 geom_line()+
                                 geom_smooth(aes(color="Yellow")) +
                                 scale_color_identity(labels=c("Inaccuracies"),guide = "legend")+
                                 geom_point(size = 1)+labs(title = "Centipawn loss for Inaccuracies",
                                                           x = "Counter",
                                                           y = "Black's Mean  Centipawn Loss",
                                                           color="Deviation From Best Moves")
                         })
                         
                         output$text1<-renderPrint({print("Line Graph representation: “Inaccuracies” result in minimun centipawn  loss")})
                     }
                     else if(input$tp_mv==c("Mistakes")){
                         flag2=1
                         output$chart1<-renderPlot({
                             ggplot(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2)) +
                                 geom_line(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),col="purple")+
                                 geom_smooth(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2,color="Orange"))+
                                 scale_color_identity(labels=c("Mistakes"),guide = "legend")+
                                 geom_point(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),size=1)+
                                 labs(title = "Centipawn loss for Mistakes",
                                      x = "Counter",
                                      y = "Black's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                         })
                         output$text1<-renderPrint({print("Line Graph representation: “Mistakes” result in more centipawn  loss")})
                         
                     }
                     else if(input$tp_mv==c("Blunders")){
                         flag3=1
                         output$chart1<-renderPlot({
                             ggplot(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3))+
                                 geom_line(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),col="blue")+
                                 geom_smooth(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3,color="Red"))+
                                 scale_color_identity(labels=c("Blunders"),guide = "legend")+
                                 geom_point(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),size=1)+
                                 labs(title = "Centipawn loss for Blunders",
                                      x = "Counter",
                                      y = "Black's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                         })
                         output$text1<-renderPrint({print("Line Graph representation: “Blunders ”result in maximum centipawn  loss")})
                     }
                     
                     else if(input$tp_mv=="Inaccuracies and Mistakes"){
                         output$chart1<-renderPlot({
                             ggplot(data=bf1,aes(Black.s.Number.of.Inaccuracies,mean_bcpl1)) +
                                 geom_line()+
                                 geom_smooth(aes(color="Yellow")) +
                                 scale_color_identity(labels = c("Inaccuracies", "Mistakes"),guide = "legend")+
                                 geom_line(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),col="purple")+
                                 geom_smooth(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2,color="Orange"))+
                                 scale_color_identity(breaks = c("Yellow", "Orange"),labels = c("Inaccuracies", "Mistakes"),guide = "legend")+
                                 geom_point(size = 1) +
                                 geom_point(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),size=1)+
                                 labs(title = "Centipawn loss for Inaccuracies and Mistakes",
                                      x = "Counter",
                                      y = "Black's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                             
                         })
                         output$text1<-renderPrint({print("Line Graph representation: 'Inaccuracies' result in less centipawn  loss than 'Mistakes'")})
                         
                     }
                     
                     else if(input$tp_mv=="Inaccuracies and Blunders"){
                         output$chart1<-renderPlot({
                             ggplot(data=bf1,aes(Black.s.Number.of.Inaccuracies,mean_bcpl1)) +
                                 geom_line(data=bf1,aes(Black.s.Number.of.Inaccuracies,mean_bcpl1))+
                                 geom_smooth(data=bf1,aes(Black.s.Number.of.Inaccuracies,mean_bcpl1,color="Yellow")) +
                                 scale_color_identity(labels = c("Inaccuracies","Blunders"),guide = "legend")+
                                 geom_line(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),col="blue")+
                                 geom_smooth(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3,color="Red"))+
                                 scale_color_identity(breaks = c("Yellow", "Red"),labels = c("Inaccuracies","Blunders"),guide = "legend")+
                                 geom_point(size = 1) +
                                 geom_point(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),size=1)+
                                 labs(title = "Centipawn loss for Inaccuracies and Blunders",
                                      x = "Counter",
                                      y = "Black's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                             
                         })
                         output$text1<-renderPrint({print("Line Graph representation: 'Blunders' have a much greater effect on centipawn loss than 'Inaccuracies'")})
                     }
                     else if(input$tp_mv=="Mistakes and Blunders"){
                         output$chart1<-renderPlot({
                             ggplot(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2))+
                                 geom_line(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),col="purple")+
                                 geom_smooth(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2,color="Orange"))+
                                 scale_color_identity(labels = c("Mistakes","Blunders"),guide = "legend")+
                                 geom_line(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),col="blue")+
                                 geom_smooth(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3,color="Red"))+
                                 geom_point(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),size=1)+
                                 geom_point(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),size=1)+
                                 labs(title = "Centipawn loss for Mistakes and Blunders",
                                      x = "Counter",
                                      y = "Black's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                         })
                         output$text1<-renderPrint({print("Line Graph representation: 'Blunders' result in maximum centipawn  loss,followed by 'Mistakes'")})
                     } 
                     else if(input$tp_mv=="Inaccuracies, Mistakes and Blunders"){
                         output$chart1<-renderPlot({
                             ggplot(data=bf1,aes(Black.s.Number.of.Inaccuracies,mean_bcpl1)) +
                                 geom_line()+
                                 geom_smooth(aes(color="Yellow")) +
                                 scale_color_identity(guide = "legend")+
                                 geom_line(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),col="purple")+
                                 geom_smooth(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2,color="Orange"))+
                                 scale_color_identity(guide = "legend")+
                                 geom_line(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),col="blue")+
                                 geom_smooth(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3,color="Red"))+
                                 scale_color_identity(breaks = c("Yellow", "Orange", "Red"),
                                                      labels = c("Inaccuracies", "Mistakes", "Blunders"),guide = "legend")+
                                 geom_point(size = 1) +
                                 geom_point(data=bf2,aes(Black.s.Number.of.Mistakes,mean_bcpl2),size=1)+
                                 geom_point(data=bf3,aes(Black.s.Number.of.Blunders,mean_bcpl3),size=1)+
                                 labs(title = "Centipawn loss for Inaccuracies,Mistakes and Blunders",
                                      x = "Counter",
                                      y = "Black's Mean  Centipawn Loss",
                                      color="Deviation From Best Moves")
                         })
                         
                         output$text1<-renderPrint({print("Line Graph representation: “Blunders” results in maximum centipawn loss followed by “Mistakes” and least effect is due to “Inaccuracies.”
")})
                     }
                 }
        )
    
    observeEvent(input$go2,
                 if(input$player1=="White"){
                     output$chart2<-renderPlot({
                         x4 %>%
                             ggplot(aes(Value,group=Counter,fill=as.factor(Counter)))+
                             geom_histogram(bins=28,binwidth=0.5,color="darkblue")+
                             guides(color = guide_legend(title = "Deviations from Best Move"))+
                             scale_fill_discrete(labels=c("Blunders", "Mistakes","Inaccuracies")) +
                             labs(y="No of Games",x="No of bad moves",fill="Type of\nBad Moves",title="Proportion of  Games by no of Bad Moves of White")
                     })
                     
                     output$text2<-renderPrint({print("Divided stacked bar plot:As no of bad moves increase, the no of
games for those moves decreases and the proportion of blunders decrease
significantly.Inaccuracies are more prone to occur.")})
                 }
                 
                 else if(input$player1=="Black"){
                     output$chart2<-renderPlot({
                         z4 %>%
                             ggplot(aes(Value,group=Counter,fill=as.factor(Counter)))+
                             geom_histogram(bins=28,binwidth=0.5,color="darkblue")+
                             guides(color = guide_legend(title = "Deviations from Best Move"))+
                             scale_fill_discrete(labels=c("Blunders", "Mistakes","Inaccuracies")) +
                             labs(y="No of Games",x="No of bad moves",fill="Type of\nBad Moves",title="Proportion of  Games by no of Bad Moves of Black")
                     })
                     
                     output$text2<-renderPrint({print("Divided stacked bar plot:As no of bad moves increase, the no of
games for those moves decreases and the proportion of blunders decrease
significantly.Inaccuracies are more prone to occur.")}) 
                 }
    ) 
      
    observeEvent(input$go3,
                 if(input$player2=="White"){
                     output$chart3<-renderPlot({
                         BOX1 %>%
                             ggplot(aes(Opening,`White's average Centipawn Loss`,color=Opening))+
                             geom_point(size=4)+
                             labs(x = "Opening Types",title="Comparison of White's Openings")
                     })
                     
                     output$text3<-renderPrint({print("Scatter Plot representation:“French Defence” result in maximum
advantage to white ,followed by “English” , “Spanish” , “Queen
Pawn”.Moreover “Italian” opening results in a dual edge play and
least advantage is due to “King’s Gambit.” ")})
                 }
                 
                 else if(input$player2=="Black"){
                     output$chart3<-renderPlot({
                         BOX3 %>%
                             ggplot(aes(Opening,`Black's average Centipawn Loss`,color=Opening))+
                             geom_point(size=4)+
                             labs(x = "Opening Types",title="Comparison of Black's Openings")
                     })
                     
                     output$text3<-renderPrint({print("Scatter Plot representation:“French Defence” result in maximum
advantage to white ,followed by “English” , “Spanish” , “Queen
Pawn”.Moreover “Italian” opening results in a dual edge play and
least advantage is due to “King’s Gambit.” ")})
                 }
    )
    observeEvent(input$go4,
                 if(input$stat=="Head"){
                       output$table1<-renderDataTable({head(chess_dataset)
                       })}
                else if(input$stat=="Tail"){
                    output$table1<-renderDataTable({tail(chess_dataset)
                    })
                }
                else if(input$stat=="Summary"){
                    output$table1<-renderDataTable({summary(chess_dataset)
                    })
                }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
