#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(shiny)
library(rsconnect)
library(caret)
library(ranger)
library(caretEnsemble)
library(DT)
library(lattice)
library(ggplot2)
library(Boruta)
library(e1071)
heartdf<-read.csv("heart.csv")
intrain<-createDataPartition(heartdf$target,p=0.7,list=FALSE)
training<-heartdf[intrain,]
testing<-heartdf[-intrain,]
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions="final", classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
models <- caretList(make.names(target)~., data=training, trControl=control, methodList=algorithmList)
results <- resamples(models)
###########################
trctrl<-trainControl(method="repeatedcv",number=10,repeats=3)
svm<- train(target ~., data = training, method = "svmLinear",
            trControl=trctrl,
            preProcess = c("center", "scale"),
            tuneLength = 10)
svm
test_pred <- predict(svm, newdata = testing)
test_pred1<-as.data.frame(test_pred)
ximp<-varImp(svm,scale =FALSE)
plot(ximp)
#confusionMatrix(factor(test_pred1$test_pred),factor(testing$target))
plot(svmPerformance,xlim=c(0.50,1.00), ylim=c(0.50,1.00),col="blue",cex=1.5,type="p",pch=7)
##############################
Boruta12<-Boruta(target~.,data=na.omit(ftraining),doTrace=1)
fbsremoved<-read.csv("fbsremoved.csv")
fintrain<-createDataPartition(fbsremoved$target,p=0.7,list=FALSE)
ftraining<-fbsremoved[fintrain,]
ftesting<-fbsremoved[-fintrain,]
fcontrol <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions="final", classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
fmodels <- caretList(make.names(target)~., data=ftraining, trControl=fcontrol, methodList=algorithmList)
fresults <- resamples(fmodels)
################################
Borutaremoved<-read.csv("boruta.csv")
bointrain<-createDataPartition(Borutaremoved$target,p=0.7,list=FALSE)
botraining<-Borutaremoved[bointrain,]
botesting<-Borutaremoved[-bointrain,]
bocontrol <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions="final", classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
bomodels <- caretList(make.names(target)~., data=botraining, trControl=bocontrol, methodList=algorithmList)
boresults <- resamples(bomodels)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(
        h1("Efficient  approach to predict heart disease using feature selection and ensemble technique", align = "center")
    ),
    br(),
    h2("Cleveland dataset"),
    br(),
    DT::dataTableOutput("mytable"),
    br(),
    br(),
    br(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            br(),
            selectInput("v","Feature Selection technique description",choices=c("Based on importance attributes are deleted"=1,"Based on boruta attributes are deleted"=2)),
            br(),
            br(),
            selectInput("var","Select technique",choices=c("SVM"= 1,"Stacking before Feature Selection" = 2,"Stacking after removing attribute using varTmp" = 3,"Stacking after removing attributes using boruta"=4)),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            plotOutput("feature"),
            plotOutput("resultsheart"),
            
            #rpivotTableOutput("Heartdisease")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mytable = DT::renderDataTable({
        heartdf
    })
    
    output$feature<-renderPlot({
        if(input$v==1){
            plot(ximp,main="Importance of attributes")
            #plot(imp)
        }
        else if(input$v==2){
            plot(Boruta12,main="Boruta")
        }
    })
    output$resultsheart<-renderPlot({
        if(input$var==1){
            plot(svmPerformance,xlim=c(0.50,1.00), ylim=c(0.50,1.00),col="blue",cex=1.5,type="p",pch=7,main="SVM")
            
        }
        else if (input$var==2){
            dotplot(results,main="Stacking before Feature selection")
        }
        else if (input$var==3){
            dotplot(fresults,main="After applying varImp function")
        }
        else if (input$var==4){
            dotplot(boresults,main="After applying Boruta")
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

