library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)
library("caret")

shinyServer(function(input, output) {
  #reads the uploaded file
  
  data <- reactive({
    
    file1 <- input$file
    if(is.null(file1)){return()} 
      df<- read.csv(file1$datapath, header=TRUE, sep=',')
    
    })
  
  
   output$table <- renderDataTable({
    
    if(is.null(data())){return()}
    
    testing<-data()

    hr <- read.csv(file="./HR_comma_sep.csv", header=TRUE, sep=",")
    hr_leaving_people <- hr %>% filter(left==1)
    hr_good_leaving_people <- hr_leaving_people %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
    hr_good_leaving_people2 <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
    hr_good_people_select <- hr_good_leaving_people2 %>% select(satisfaction_level, number_project: promotion_last_5years)
    hr_model <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
    hr_model$left <- as.factor(hr_model$left)
    train_control<- trainControl(method="cv", number=5, repeats=3)
    
    # train the model 
    gmlmodel <- train(left~., data=hr_model, trControl=train_control, method="LogitBoost")
    # make predictions
    predictions<- predict(gmlmodel,hr_model)
    gmlmodelbinded <- cbind(hr_model,predictions)
    # summarize results
    confusionMatrix<- confusionMatrix(gmlmodelbinded$predictions,gmlmodelbinded$left)
    confusionMatrix
    
    set.seed(100)
    # Keep some data to test again the final model
    inTraining <- createDataPartition(hr_model$left, p = .75, list = FALSE)
    training <- hr_model[ inTraining,]
    # testing  <- hr_model[-inTraining,]
    # Estimate the drivers of attrition
    logreg = glm(left ~ ., family=binomial(logit), data=training)
    
    # Make predictions on the out-of-sample data
    probaToLeave=predict(logreg,newdata=testing,type="response")
    # Structure the prediction output in a table
    predattrition = data.frame(probaToLeave)
    # Add a column to the predattrition dataframe containing the performance
    predattrition$performance=testing$last_evaluation
    predattrition$ID=testing$employeeID
    predattrition$priority=predattrition$performance*predattrition$probaToLeave
    
    orderpredattrition=predattrition[order(predattrition$priority,decreasing = TRUE),]
    orderpredattrition <- head(orderpredattrition,n=300)
#this is to render the prediction table on the UI once the prediction algorithm has been completed.
    df2 <- orderpredattrition %>%
      dplyr::select(probaToLeave, performance, priority,ID) %>%
      dplyr::group_by(probaToLeave, performance,priority,ID) %>%
      dplyr::summarise_each(funs())

  })

  
    output$downloadtable <- downloadHandler(
    filename = function() {
      paste('stats', '.csv', sep='')
    },
    content = function(file) {
      df1 <- data()
      
      df2 <- df1 %>% 
        dplyr::select(satisfaction_level, salary, promotion_last_5years) %>% 
        dplyr::group_by(satisfaction_level, salary) %>%
        dplyr::summarise_each(funs(
          
        ))  
      
      write.csv(df2, file)
      }
    )
  
})