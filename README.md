<pre>
# RDashboad
#Download & Install R- {https://www.r-project.org/}
#Download & Install RStudio - {https://www.rstudio.com/products/rstudio/download/}




Code --

install.packages("shinydashboard")
install.packages("shiny")
install.packages("DT")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("shinydashboard")
install.packages("plotrix")
install.packages("tm")
install.packages("SnowballC")
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(plotrix)
library(tm)
library(SnowballC)
header <- dashboardHeader(title = "Wittyfeed Data Analysis", titleWidth = 290)

sidebar <- dashboardSidebar(
  width = 290,
  sidebarMenu(
    menuItem("Traffic_Analysis", tabName = "traffic_Analysis", icon = icon("hourglass-half")),
    menuItem("Category_Analysis", tabName = "category_Analysis", icon = icon("columns")),
    menuItem("OneFeed_KMeans_Recm_Sys", tabName = "oneFeed_KMeans_Recm_Sys", icon = icon("certificate")),
    menuItem("Publishers_Dashboard", tabName = "publishers_Dashboard", icon = icon("desktop"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "traffic_Analysis",
            fluidRow(
              box(
                title = "Time Series",background = "maroon", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot1", height = 400)),
              tabBox(
                title = tagList(shiny::icon("hourglass-half"), "Traffic Analysis"),
                id = "tabset1", height = "400",
                tabPanel("Monthly", plotOutput("plot4", height = 400)),
                tabPanel("Weekly", plotOutput("plot3", height = 400))
              )
            )
    ),
    tabItem(tabName = "category_Analysis",
            fluidRow( 
              box(
                title = "Category Analysis",background = "black", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot5", height = 400)),
              tabBox(
                title = tagList(shiny::icon("columns"), "Category Choice"),
                id = "tabset1", height = "400",
                tabPanel("wf.com", plotOutput("plot6", height = 400)),
                tabPanel("wf.me", plotOutput("plot7", height = 400))
              )),
            fluidRow(
              box(
                title = "Category_Hits",background = "olive", solidHeader = TRUE, width = 800,
                collapsible = TRUE,
                DT::dataTableOutput("mytable2", width = 1000),style = "overflow-x: scroll;")
            )
    ),
    tabItem(tabName = "oneFeed_KMeans_Recm_Sys",
            fluidRow( 
              box(
                title = "cluster_proportion",background = "green", solidHeader = TRUE,width = 800,
                collapsible = TRUE,
                plotOutput("plot2", height = 400, width = 1000))
              
            ),
            fluidRow(
              box(
                title = "Count_cluster",background = "green", solidHeader = TRUE, width = 800,
                collapsible = TRUE,
                DT::dataTableOutput("mytable", width = 1000))
            ),
            fluidRow( 
              box(
                title = "Normalise",background = "green", solidHeader = TRUE,width = 800,
                collapsible = TRUE,
                plotOutput("plot8", height = 400, width = 1000))
              
            )
    ),
    tabItem(tabName = "publishers_Dashboard",
            fluidRow(
              box(
                title = "articles_AKTU",background = "olive", solidHeader = TRUE, width = 800,
                collapsible = TRUE,
                DT::dataTableOutput("mytable1", width = 1000))
            ),
            fluidRow( 
              box(
                title = "cluster_proportion",background = "olive", solidHeader = TRUE,width = 800,
                collapsible = TRUE,
                plotOutput("plot9", height = 400, width = 1000))
              )
   )
  )   
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  traffic = read.csv(file = "wf_traffic_2016_2017.csv.csv")
  time3 = traffic[367:731,]
  time = ts(time3$Users, frequency = 30, start = c(0, 1))
  
  output$plot1 <- renderPlot({
    par(bg="white")
    plot(time, las = 1, ylab= ' ', main =  "Hits Vs time of year 2017", col.main = 4)
  })
  
  DF1 = read.csv(file = "DF1.csv")
  output$plot2 = renderPlot({
    ggplot(DF1, aes(x = cluster, y = value, fill = variable))+geom_bar(stat = "identity")+scale_fill_manual(values=mycols)+ scale_fill_manual(values = c("green", "coral","red","plum","black","lightgreen","blue","gray","purple","cyan","darkgreen","darkred","deeppink","gold","yellow","wheat3"))
  })
  kl = read.csv(file = "hits.csv", header = TRUE)
  kl[6,2] = 1400000
  kl = as.data.frame(kl)
  output$plot3 <- renderPlot({
    ggplot(kl, aes(y = Hits, x = Days))+geom_col(aes(fill = Days),fill = c("brown","#ddaa00","pink","#dd00dd", "lightblue","grey","lightgreen"))+ scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thrusday","Friday","Saturday"))
    
  })
  
  h11 = read.csv(file = "h11.csv")
  colnames(h11) = c("X", "Months", "Hits")
  output$plot4 <- renderPlot({
    ggplot(h11, aes(y = Hits, x = Months)) +geom_col(aes(fill = Months))+ scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))    
  })
  c2 = read.csv(file = "c2.csv")
  output$plot5 <- renderPlot({
    piepercent = round(100*c2$Freq/sum(c2$Freq),1)
    pie3D(c2$Freq,labels=piepercent,main="Chart",radius = 1,explode = 0, labelcex=1,col=c("brown","#ddaa00","pink","#dd00dd"))
    par(xpd=TRUE)
    legend(1,1,legend=c2$websites,cex=0.8,yjust=0.15, xjust = -0.01,fill = c("brown","#ddaa00","pink","#dd00dd")) 
  })
  m_pie8 = read.csv(file = "m_pie8.csv")
  percent = round(100*m_pie8$Freq/sum(m_pie8$Freq),0)
  output$plot6 <- renderPlot({
    ggplot(m_pie8, aes(y = percent, x = Category),axis = FALSE)+geom_col(aes(fill = Category))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+ ggtitle("CatWise_Hits_WF.com") 
  })
  wf = read.csv(file = "Witty.Me.csv")
  Percent = round(100*wf$Freq/sum(wf$Freq),0)
  output$plot7 <- renderPlot({
    ggplot(wf, aes(y = Percent, x = category),axis = FALSE)+geom_col(aes(fill = category))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+ ggtitle("CatWise_Hits_WF.me") 
  })
  count = read.csv(file = "count.csv")
  count = count[,-c(3,4,5)]
  output$mytable = DT::renderDataTable({
    my_vals  = unique(count$cluster)
    my_colors = ifelse(my_vals == 'cluster26','white','black')
    count%>%datatable%>%formatStyle('cluster', target = 'row', backgroundColor = styleEqual(my_vals,my_colors))
  })
  df3 = read.csv(file = "df3.csv")
  df3 = df3[,-1]
  melted = melt(df3, id.vars = "plot")
  output$plot8 <- renderPlot({
    ggplot(data= melted, aes(x = variable, y = value, group = plot, colour = plot)) + geom_line(size = 1)+theme(axis.text.x=element_text(size=8, angle=20))  
    })
  total = read.csv(file = "total.csv")
  total$X__2 = as.character(total$X__2)
  corpus = VCorpus(VectorSource(total$X__2))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("aman", stopwords("english")))
  corpus = tm_map(corpus, stemDocument)
  frequencies = DocumentTermMatrix(corpus)
  findFreqTerms(frequencies, lowfreq=20)
  
  tweetsSparse = as.data.frame(as.matrix(frequencies))
  colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
  distances = dist(tweetsSparse[1:877], method = "euclidean")
  clusterMovies = hclust(distances, method = "ward")
  output$plot9 = renderPlot({
    plot(clusterMovies,sub = "")
  })
  total__ = read.csv(file = "total__.csv")
  total__ = total__[,-c(3,4,5)]
  colnames(total__) = c("ID", "Articles")
  output$mytable1 = DT::renderDataTable({
    my_vals  = unique(total__$ID)
    my_colors = ifelse(my_vals == 'cluster26','white','black')
    total__%>%datatable%>%formatStyle('ID', target = 'row', backgroundColor = styleEqual(my_vals,my_colors))
  })
  p1 = read.csv(file = "P1.csv", header = TRUE)
  output$mytable2 = DT::renderDataTable({
    my_vals  = unique(p1$Animals)
    my_colors = ifelse(my_vals == 'cluster26','white','black')
    p1%>%datatable(list(paging = FALSE))%>%formatStyle('Animals', target = 'row', backgroundColor = styleEqual(my_vals,my_colors))
    })
  
  
}

shinyApp(ui, server)
</pre>
