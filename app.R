#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#install.packages("caret")
#install.packages("fastDummies")
#install.packages("Metrics")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(randomForest)
library(caret)
library(fastDummies)
library(xgboost)
library(gbm)
library(rpart)
library(dplyr)
library(plotly)
library(tidyr)
library(Metrics)


#randomForestModel <- rf_model
ClickPrediction
ui <- dashboardPage(
  dashboardHeader(title = "Prediction Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "dataUpload", icon = icon("upload")),
      menuItem("Analysis", tabName = "analysis", icon = icon("line-chart")),
      menuItem("Model Evaluation", tabName = "modelEval", icon = icon("check-square"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                valueBoxOutput("conversionsBox", width = 3),
                valueBoxOutput("bestSocialMediaChannelBox", width = 3),
                valueBoxOutput("totalOrdersBox", width = 3),
                valueBoxOutput("ConversionRate", width = 3)
              ),
              fluidRow(
                box(title = "Restaurant Type Breakdown", plotOutput("plotRestaurantType"), width = 6),
                box(title = "Select Options", selectInput("dayOfWeek", "Select Day of the Week:", choices = c("All Week", levels(ClickPrediction$Weekday))),
                actionButton("submit", "Submit"))
              ),
              fluidRow(
                box(title = "Number of Previous Orders", plotOutput("plotPreviousOrders"), width = 6),
                box(title = "Social Media Channels", plotOutput("plotSocialMedia"), width = 6),
                box(title = "Conversion Rate Per Restaurant type", plotOutput("conversionRatePlot"), width = 6),
                box(title = "Conversion Rate Per Social Network", plotOutput("conversionRateSocialPlot"), width = 6)
              )
      ),
      tabItem(tabName = "dataUpload",
              fluidRow(
                valueBoxOutput("conversionsKPI"),
                valueBoxOutput("mostCommonSocialMediaKPI"),
                valueBoxOutput("mostPopularDayKPI")
              ),
              fluidRow(
                box(title = "Upload Data", 
                    fileInput("fileUpload", "Upload a .RData File with Dataframe ClickPredictions", accept = ".RData"),
                    radioButtons("modelSelection", "Select Model and click on the Submit Button:", 
                                       choices = c("Decision Tree" = "dt", "XGBoost" = "xgb", "KNN" = "knn", "Random Forest" = "rf", " Logistic Regression" = "logistic", "Ensemble" = "ensemble" )),
                    actionButton("preprocess", "Submit"), width = 5
                ),
                box(title = "Weekly Conversions", plotOutput("weeklyConversionsPlot"), width = 7)
              )
              ),
      tabItem(tabName = "modelEval",
              fluidRow(
                box(title = "Upload Data and Process", status = "primary", solidHeader = TRUE,
                    fileInput("rdataUpload", "Upload ClickPredictions (.RData)", accept = ".RData"),
                    fileInput("csvUpload", "Upload Click Conversions (CSV)", accept = ".csv"),
                    actionButton("processData", "Process Data", class = "btn-success"),
                    width = 12)
              ),
              fluidRow(
                box(title = "Model Accuracies", tableOutput("modelAccuracies"), width = 6),
                box(title = "Decision Tree Confusion Matrix", helpText("Predicted Values = Columns, Actual Values = Rows"), tableOutput("dtConfMatrix"), width = 6),
                box(title = "XGBoost Confusion Matrix", helpText("Predicted Values = Columns, Actual Values = Rows"),  tableOutput("xgbConfMatrix"), width = 6),
                box(title = "KNN Confusion Matrix", helpText("Predicted Values = Columns, Actual Values = Rows"), tableOutput("knnConfMatrix"), width = 6),
                box(title = "Logistic Regression Confusion Matrix", helpText("Predicted Values = Columns, Actual Values = Rows"),  tableOutput("logisticConfMatrix"), width = 6),
                box(title = "Random Forest Confusion Matrix", helpText("Predicted Values = Columns, Actual Values = Rows"), tableOutput("rfConfMatrix"), width = 6),
                box(title = "Ensemble Confusion Matrix", helpText("Predicted Values = Columns, Actual Values = Rows"),  tableOutput("ensembleConfMatrix"), width = 6)
              )
              # Add more UI elements as needed
      )
      )
    )
  )


# Server
server <- function(input, output, session) {
  
  selectedModel <- reactiveVal(NULL)
  
  observeEvent(input$modelSelection, {
    selectedModel(input$modelSelection)
  })
  
  # Render the selected model name in the "Analysis" tab
  output$selectedModelName <- renderText({
    req(selectedModel())
    paste("Selected Model:", selectedModel())
  })
  
  # Reactive expression to handle the input action
  filteredData <- reactive({
    req(input$submit)  # Require the button to be pressed
    
    if (input$dayOfWeek == "All Week") {
      data_filtered <- ClickPrediction %>%
        mutate(Time_On_Previous_Website_original = (Time_On_Previous_Website * time_on_previous_std) + time_on_previous_mean)
        #mutate(Number_of_Previous_Orders_original = (Number_of_Previous_Orders * Number_of_Previous_Orders_std) + Number_of_Previous_Orders_mean)
    } else {
      # Filter the data based on the selected day of the week
      data_filtered <- ClickPrediction %>%
        dplyr::filter(Weekday == input$dayOfWeek)%>%
        mutate(Time_On_Previous_Website_original = (Time_On_Previous_Website * time_on_previous_std) + time_on_previous_mean)
        #mutate(Number_of_Previous_Orders_original = (Number_of_Previous_Orders * Number_of_Previous_Orders_std) + Number_of_Previous_Orders_mean)
    }
    return(data_filtered)
  })
  
  # Plot for Restaurant Type Distribution
  output$plotRestaurantType <- renderPlot({
    data <- filteredData()
    
    # Calculate the percentage for each Restaurant_Type
    data_percentage <- data %>%
      group_by(Restaurant_Type) %>%
      summarise(percentage = n() / nrow(data) * 100)
    
    # Arrange the data in descending order by percentage
    data_percentage <- data_percentage %>%
      arrange(desc(Restaurant_Type))
    
    # Calculate the y position for text labels
    data_percentage <- data_percentage %>%
      mutate(lab.ypos = cumsum(percentage) - 0.5 * percentage)
    
    # Create the plot
    ggplot(data_percentage, aes(x = "", y = percentage, fill = Restaurant_Type)) + 
      geom_bar(width = 1, stat = "identity") + 
      coord_polar("y", start = 0) + 
      theme_void() +
      labs(fill = "Restaurant Type", title = "Restaurant Type Distribution") +
      theme(legend.position = "bottom") +
      geom_text(aes(y = lab.ypos, label = paste0(round(percentage, 1), "%")), color = "white", size = 4)
  })
  output$conversionRatePlot <- renderPlot({
    data <- req(filteredData())
    req(selectedModel())
    
    column_name <- paste0("binary_output_", selectedModel())
    
    conversion_restaurant <- data %>%
      group_by(Restaurant_Type) %>%
      summarise(conversion_rate = mean(.data[[column_name]], na.rm = TRUE)) %>%
      mutate(label = scales::percent(conversion_rate))  # Create a label column for the annotations
    
    # Create the plot
    ggplot(conversion_restaurant, aes(x = Restaurant_Type, y = conversion_rate, fill = Restaurant_Type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label), vjust = -0.3, size = 3.5) +  # Add text annotations on top of the bars
      labs(title = "Conversion Rate by Restaurant Type", y = "Conversion Rate") +
      theme_minimal() +
      theme(legend.position = "none")
  })
 

  
  # Plot for Social Media Channel Distribution
  output$plotSocialMedia <- renderPlot({
    data <- req(filteredData())
    
    # Calculate counts for each Social_Network
    counts <- data %>%
      group_by(Social_Network) %>%
      tally() %>%
      ungroup()
    
    # Create the plot using ggplot
    color_references <- c("Facebook" = "#3b5998", "Instagram" = "#8a3ab9", "Twitter" = "#1da1f2")
  
    ggplot(counts, aes(x = Social_Network, y = n, fill = Social_Network)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), vjust = -0.3, size = 3.5) +  # Add text annotations on top of the bars
      scale_fill_manual(values = color_references) +
      labs(title = "Social Media Channel Distribution") +
      theme_minimal()
  })
  
  # Plot for Social Media wise conversion rate
  output$conversionRateSocialPlot <- renderPlot({
    data <- req(filteredData())
    req(selectedModel())
    column_name <- paste0("binary_output_", selectedModel())
    
    conversion_social <- data %>%
      group_by(Social_Network) %>%
      summarise(conversion_rate = mean(.data[[column_name]], na.rm = TRUE)) %>%
      mutate(label = scales::percent(conversion_rate))  # Create a label column for the annotations
    
    # Create the plot
    ggplot(conversion_social, aes(x = Social_Network, y = conversion_rate, fill = Social_Network)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label), vjust = -0.3, size = 3.5) +  # Add text annotations on top of the bars
      labs(title = "Conversion Rate by Social Network", y = "Conversion Rate") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  # Plot for Number of Previous Orders
  output$plotPreviousOrders <- renderPlot({
    data <- req(filteredData())
    
    # Create a data frame with counts for each 'Number_of_Previous_Orders'
    count_data <- data %>%
      group_by(Number_of_Previous_Orders) %>%
      summarise(count = n())
    
    # Create the plot
    ggplot(count_data, aes(x = Number_of_Previous_Orders, y = count)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label = count), vjust = -0.3, size = 3) +  # Add text annotations
      labs(title = "Previous Orders Distribution") +
      theme_minimal()
  })
  
  
  # KPI for number of conversions
  output$conversionsBox <- renderValueBox({
    data <- filteredData()
    req(selectedModel())
    
    # Construct the dynamic column name
    column_name <- paste0("binary_output_", selectedModel())
    
    # Use the dynamic column name to calculate the sum
    # The [[ ]] notation is used to select columns with variable names
    total_conversions <- sum(data[[column_name]] == 1)
    
    valueBox(
      value = total_conversions,
      subtitle = "Number of Conversions",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  # KPI for the best social media channel
  output$bestSocialMediaChannelBox <- renderValueBox({
    data <- filteredData()
    best_channel <- names(which.max(table(data$Social_Network)))
    
    color <- if(best_channel == "Facebook") {
      "blue"
    } else if(best_channel == "Instagram") {
      "purple"
    } else {
      "light-blue"  # Default color or any other color
    }
    
    valueBox(
      value = best_channel,
      subtitle = "Best Social Media Channel",
      icon = icon("thumbs-up"),
      color = color
    )
  })
  
  # KPI for total number of orders
  output$totalOrdersBox <- renderValueBox({
    data <- filteredData()
    valueBox(
      value = nrow(data),
      subtitle = "Total Number of Profiles",
      icon = icon("person"),
      color = "yellow"
    )
  })
  
  # KPI for conversion rate
  output$ConversionRate <- renderValueBox({
    # Ensure the filtered data and selected model are available
    data <- req(filteredData())
    req(selectedModel())
    
    # Construct the dynamic column name based on the selected model
    column_name <- paste0("binary_output_", selectedModel())
    
    # Calculate the conversion rate using the dynamic column name
    conversion_rate <- 100 * sum(data[[column_name]] == 1, na.rm = TRUE) / nrow(data)
    
    valueBox(
      value = sprintf("%.2f%%", conversion_rate),  # Format the conversion rate to display as a percentage
      subtitle = "Conversion Rate",
      icon = icon("percentage"),  # Use a percentage symbol as the icon
      color = "red"  # The color for the valueBox
    )
  })
  
  observeEvent(input$preprocess, {
    req(input$fileUpload) # Ensure a file is uploaded
    load(input$fileUpload$datapath) # Load the .RData file
    
    # Replace missing values with mode for Restaurant_Type
    mode_restaurant_type <- names(sort(table(ClickTraining$Restaurant_Type), decreasing = TRUE))[1]
    mode_restaurant_type_pred <- names(sort(table(ClickPrediction$Restaurant_Type), decreasing = TRUE))[1]
    
    ClickTraining$Restaurant_Type[is.na(ClickTraining$Restaurant_Type)] <- mode_restaurant_type
    ClickPrediction$Restaurant_Type[is.na(ClickPrediction$Restaurant_Type)] <- mode_restaurant_type_pred
  
    # One-hot encode the categorical variables
    ClickTraining <- dummy_cols(ClickTraining, select_columns = c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))
    ClickPrediction <- dummy_cols(ClickPrediction, select_columns = c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))
    
    # Preprocess 'Time_On_Previous_Website' using centering and scaling
    preproc_time <- preProcess(ClickTraining[, "Time_On_Previous_Website", drop = FALSE], method = c("center", "scale"))
    ClickTraining[, "Time_On_Previous_Website"] <- predict(preproc_time, ClickTraining[, "Time_On_Previous_Website", drop = FALSE])
    ClickPrediction[, "Time_On_Previous_Website"] <- predict(preproc_time, ClickPrediction[, "Time_On_Previous_Website", drop = FALSE])
    
    # Preprocess 'Number_of_Previous_Orders' using median imputation and range normalization
    preproc_orders <- preProcess(ClickTraining[, "Number_of_Previous_Orders", drop = FALSE], method = c("medianImpute", "range"))
    ClickTraining[, "Number_of_Previous_Orders"] <- predict(preproc_orders, ClickTraining[, "Number_of_Previous_Orders", drop = FALSE])
    ClickPrediction[, "Number_of_Previous_Orders"] <- predict(preproc_orders, ClickPrediction[, "Number_of_Previous_Orders", drop = FALSE])
    
    set.seed(123)
    
    # Create indexes for training and testing
    indexes <- createDataPartition(ClickTraining$Clicks_Conversion, p = 0.8, list = FALSE)
    
    # Create training and testing datasets
    train_data <- ClickTraining[indexes, ]
    test_data <- ClickTraining[-indexes, ]
    
    # Now we can proceed with creating train_X, train_Y, test_X, test_Y
    train_X <- train_data[, -which(names(train_data) == "Clicks_Conversion")]
    train_dash_Y <- train_data$Clicks_Conversion
    test_X <- test_data[, -which(names(test_data) == "Clicks_Conversion")]
    test_Y <- test_data$Clicks_Conversion
    
    columns_to_keep <- c("Carrier_Free", "Daytime", "Time_On_Previous_Website", 
                         "Weekday_Tuesday", "Weekday_Monday", "Social_Network_Facebook", 
                         "Number_of_Previous_Orders", "Restaurant_Type_Groceries", 
                         "Restaurant_Type_Sushi", "Restaurant_Type_French", 
                         "Social_Network_Instagram", "Weekday_Saturday", 
                         "Social_Network_Twitter", "Weekday_Friday", "Carrier_Bouygues")
    
    train_X_dash_top15 <- train_X[, columns_to_keep]
    test_X_top15 <- test_X[, columns_to_keep]
    # Create the new dataframe with only the specified columns
    ClickPred_dash_top15 <- ClickPrediction[, columns_to_keep]
    
    
    
    # Depending on selected model, perform predictions
    if ("rf" %in% input$modelSelection) {
      ClickPred_rf <- ClickPrediction[, !(names(ClickPrediction) %in% c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))]
      output_rf <- predict(model_rf, newdata = data.frame(ClickPred_rf), type = "prob")[, 2]
      binary_output_rf <- ifelse(output_rf >= 0.5, 1, 0)
      ClickPrediction <- mutate(ClickPrediction, output_rf, binary_output_rf)
    }
    if ("dt" %in% input$modelSelection) {
      output_dt <- predict(model_dt, newdata = ClickPrediction, type = "prob")[, 2]
      binary_output_dt <- ifelse(output_dt >= 0.5, 1, 0)
      print("in Decision tree")
      ClickPrediction <- mutate(ClickPrediction, output_dt, binary_output_dt)
    }
    if ("xgb" %in% input$modelSelection) {
      # Assuming test_X is already defined
       output_xgb <- predict(model_xgb, newdata = as.matrix(ClickPred_top15), type = "response")
       binary_output_xgb <- ifelse(output_xgb >= 0.5, 1, 0)
       ClickPrediction <- mutate(ClickPrediction, output_xgb, binary_output_xgb)
    }
    if ("knn" %in% input$modelSelection) {
      
      output_knn_fact <- knn(train = as.matrix(train_X_dash_top15), test = as.matrix(ClickPred_top15), cl = train_dash_Y, k = 5)
      # Convert predictions to numeric if they're factors (depends on how your 'train_Y' is formatted)
      output_dash_knn <- as.numeric(as.character(output_knn_fact))
      binary_output_dash_knn <- ifelse(output_dash_knn >= 0.5, 1, 0)
      print("in KNN")
      ClickPrediction <- mutate(ClickPrediction, output_knn, binary_output_knn)
    }
    if ("logistic" %in% input$modelSelection) {
      output_logistic <- predict(model_logistic, newdata = data.frame(ClickPrediction), type = "response")
      binary_output_logistic <- ifelse(output_logistic >= 0.5, 1, 0)
      ClickPrediction <- mutate(ClickPrediction, output_logistic, binary_output_logistic)
    }
    if ("ensemble" %in% input$modelSelection) {
      #Add the RF and XGB code
      ClickPred_rf <- ClickPrediction[, !(names(ClickPrediction) %in% c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))]
      output_rf <- predict(model_rf, newdata = data.frame(ClickPred_rf), type = "prob")[, 2]
      binary_output_rf <- ifelse(output_rf >= 0.5, 1, 0)
      
      output_xgb <- predict(model_xgb, newdata = as.matrix(ClickPred_top15), type = "response")
      binary_output_xgb <- ifelse(output_xgb >= 0.5, 1, 0)
      ClickPrediction <- mutate(ClickPrediction, output_xgb, binary_output_xgb)
      
      #output_ensemble <- predict(model_ensemble, newdata = ClickPrediction, n.trees = 100, type = "response")
      output_ensemble <- (binary_output_rf + binary_output_xgb) / 2
      binary_output_ensemble <- ifelse(output_ensemble >= 0.5, 1, 0)
      print("in GBM")
      ClickPrediction <- mutate(ClickPrediction, output_ensemble, binary_output_ensemble)
    }
    
    # Now update the KPI outputs
    output$conversionsKPI <- renderValueBox({
      selected_binary_output <- paste0("binary_output_", input$modelSelection)
      valueBox(
        value = sum(ClickPrediction[[selected_binary_output]] == 1),
        subtitle = "Number of Conversions",
        icon = icon("newspaper"),
        color = "green"
      )
    })
    
    output$mostCommonSocialMediaKPI <- renderValueBox({
      selected_binary_output <- paste0("binary_output_", input$modelSelection)
      filtered_data <- ClickPrediction %>% 
        filter(.[[selected_binary_output]] == 1)
      most_common_channel <- names(which.max(table(filtered_data$Social_Network)))
      color <- if(most_common_channel == "Facebook") {
        "blue"
      } else if(most_common_channel == "Instagram") {
        "purple"
      } else {
        "light-blue"  # Default color or any other color
      }
      valueBox(
        value = most_common_channel,
        subtitle = "Most Common Social Media Channel",
        icon = icon("thumbs-up"),
        color = color
      )
    })
    
    output$mostPopularDayKPI <- renderValueBox({
      selected_binary_output <- paste0("binary_output_", input$modelSelection)
      filtered_data <- ClickPrediction %>% 
        filter(.[[selected_binary_output]] == 1)
      most_popular_day <- names(which.max(table(filtered_data$Weekday)))
      valueBox(
        value = most_popular_day,
        subtitle = "Most Popular Day",
        icon = icon("calendar"),
        color = "yellow"
      )
    })
    
    output$weeklyConversionsPlot <- renderPlot({
      req(input$modelSelection) # Ensure a model is selected
      selected_binary_output <- paste0("binary_output_", input$modelSelection)
      
      # Define the days of the week
      days_of_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      
      # Initialize an empty data frame for the results
      conversions_data <- data.frame(Day = character(), Type = character(), Count = numeric(), stringsAsFactors = FALSE)
      
      for (day in days_of_week) {
        # Create the column name for the weekday
        day_column <- paste0("Weekday_", day)
        
        # Check if the weekday column exists
        if (!day_column %in% names(ClickPrediction)) {
          next # Skip if the column doesn't exist
        }
        
        # Filter data for the specific day
        day_data <- ClickPrediction[ClickPrediction[[day_column]] == 1, ]
        
        # Calculate conversions and non-conversions
        conversions <- sum(day_data[[selected_binary_output]] == 1)
        non_conversions <- sum(day_data[[selected_binary_output]] == 0)
        
        # Append to the results data frame
        conversions_data <- rbind(conversions_data, data.frame(Day = day, Type = "Conversions", Count = conversions))
        conversions_data <- rbind(conversions_data, data.frame(Day = day, Type = "Non-Conversions", Count = non_conversions))
      }
      
      # Calculate cumulative sum for label positioning
      conversions_data <- conversions_data %>%
        arrange(Day, desc(Type)) %>%
        group_by(Day) %>%
        mutate(CumSum = cumsum(Count)) %>%
        ungroup()
      
      # Create the stacked bar chart
      ggplot(conversions_data, aes(x = Day, y = Count, fill = Type, label = Count)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(y = CumSum - (0.5 * Count)), position = position_stack(vjust = 0.5), size = 3, color = "white") +
        labs(title = "Weekly Conversions and Non-Conversions", x = "Day of the Week", y = "Count") +
        scale_fill_manual(values = c("Conversions" = "blue", "Non-Conversions" = "red")) +
        theme_minimal()
    })
    
    
  })
  observeEvent(input$processData, {
    req(input$rdataUpload)
    req(input$csvUpload)
    
    # Load the .RData file
    load(input$rdataUpload$datapath)
    
    # Assuming ClickPredictions is loaded from the .RData file
    # Load the CSV file and extract the Click_Conversion column
    csv_data <- read.csv(input$csvUpload$datapath)
    Prediction_results <- data.frame(Click_Conversions = csv_data$Click_Conversion)
    
    # Assuming ClickPrediction is loaded from the .RData file
    # Replace missing values with mode for Restaurant_Type
    mode_restaurant_type <- names(sort(table(ClickTraining$Restaurant_Type), decreasing = TRUE))[1]
    mode_restaurant_type_pred <- names(sort(table(ClickPrediction$Restaurant_Type), decreasing = TRUE))[1]
    ClickTraining$Restaurant_Type[is.na(ClickTraining$Restaurant_Type)] <- mode_restaurant_type
    ClickPrediction$Restaurant_Type[is.na(ClickPrediction$Restaurant_Type)] <- mode_restaurant_type_pred
    
    print("Completed restauranttype")
    # One-hot encode the categorical variables
    ClickPrediction <- dummy_cols(ClickPrediction, select_columns = c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))
    ClickTraining <- dummy_cols(ClickTraining, select_columns = c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))
    print("Completed OHE")
    # Preprocess 'Time_On_Previous_Website' using centering and scaling
    preproc_time <- preProcess(ClickTraining[, "Time_On_Previous_Website", drop = FALSE], method = c("center", "scale"))
    ClickTraining[, "Time_On_Previous_Website"] <- predict(preproc_time, ClickTraining[, "Time_On_Previous_Website", drop = FALSE])
    ClickPrediction[, "Time_On_Previous_Website"] <- predict(preproc_time, ClickPrediction[, "Time_On_Previous_Website", drop = FALSE])
    
    # Preprocess 'Number_of_Previous_Orders' using median imputation and range normalization
    preproc_orders <- preProcess(ClickTraining[, "Number_of_Previous_Orders", drop = FALSE], method = c("medianImpute", "range"))
    ClickTraining[, "Number_of_Previous_Orders"] <- predict(preproc_orders, ClickTraining[, "Number_of_Previous_Orders", drop = FALSE])
    ClickPrediction[, "Number_of_Previous_Orders"] <- predict(preproc_orders, ClickPrediction[, "Number_of_Previous_Orders", drop = FALSE])
    print("Completed Preprcessing")
    set.seed(123)
    
    # Create indexes for training and testing
    indexes <- createDataPartition(ClickTraining$Clicks_Conversion, p = 0.8, list = FALSE)
    
    # Create training and testing datasets
    train_data <- ClickTraining[indexes, ]
    test_data <- ClickTraining[-indexes, ]
    print("Completed Creating test train Datasets")
    
    # Now we can proceed with creating train_X, train_Y, test_X, test_Y
    train_X <- train_data[, -which(names(train_data) == "Clicks_Conversion")]
    train_dash_Y <- train_data$Clicks_Conversion
    test_X <- test_data[, -which(names(test_data) == "Clicks_Conversion")]
    test_Y <- test_data$Clicks_Conversion
    print("Completed Creating X Y Datasets")
    columns_to_keep <- c("Carrier_Free", "Daytime", "Time_On_Previous_Website", 
                         "Weekday_Tuesday", "Weekday_Monday", "Social_Network_Facebook", 
                         "Number_of_Previous_Orders", "Restaurant_Type_Groceries", 
                         "Restaurant_Type_Sushi", "Restaurant_Type_French", 
                         "Social_Network_Instagram", "Weekday_Saturday", 
                         "Social_Network_Twitter", "Weekday_Friday", "Carrier_Bouygues")
    print(columns_to_keep)
    train_X_dash_top15 <- train_X[, columns_to_keep]
    test_X_top15 <- test_X[, columns_to_keep]
    # Create the new dataframe with only the specified columns
    ClickPred_dash_top15 <- ClickPrediction[, columns_to_keep]
    
    print("Completed top 15")
    # Run the decision tree, XGB, KNN, Random Forest, Ensemble and Logistic models
    DT_Model_Eval <- predict(model_dt, newdata = ClickPrediction, type = "prob")[, 2]
    DT_Model_Eval_Bin <- ifelse(DT_Model_Eval >= 0.5, 1, 0)
    print("New DT Eval")
    XGB_Model_Eval <- predict(model_xgb, newdata = as.matrix(ClickPred_top15), type = "response")
    XGB_Model_Eval_Bin <- ifelse(XGB_Model_Eval >= 0.5, 1, 0)
    print("New XGB Eval")
    ClickPred_rf <- ClickPrediction[, !(names(ClickPrediction) %in% c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type"))]
    RF_Model_Eval <- predict(model_rf, newdata = data.frame(ClickPred_rf), type = "prob")[, 2]
    RF_Model_Eval_Bin <- ifelse(RF_Model_Eval >= 0.5, 1, 0)
    print("New RF Eval")
    Ensemble_Model_Eval <- (RF_Model_Eval_Bin + XGB_Model_Eval_Bin) / 2
    Ensemble_Model_Eval_Bin<- ifelse(Ensemble_Model_Eval >= 0.5, 1, 0)
    print("New Ensemble Eval")
    Logistic_Model_Eval <- predict(model_logistic, newdata = data.frame(ClickPrediction), type = "response")
    Logistic_Model_Eval_Bin <- ifelse(Logistic_Model_Eval >= 0.5, 1, 0)
    print("New Logistic Eval")
    output_knn_fact <- knn(train = as.matrix(train_X_dash_top15), test = as.matrix(ClickPred_top15), cl = train_dash_Y, k = 5)
    # Convert predictions to numeric if they're factors (depends on how your 'train_Y' is formatted)
    KNN_Model_Eval <- as.numeric(as.character(output_knn_fact))
    KNN_Model_Eval_Bin <- ifelse(KNN_Model_Eval >= 0.5, 1, 0)
    # Store the values
    
    ClickPrediction <- mutate(ClickPrediction, DT_Model_Eval, XGB_Model_Eval, RF_Model_Eval, Ensemble_Model_Eval, Logistic_Model_Eval, DT_Model_Eval_Bin, XGB_Model_Eval_Bin, RF_Model_Eval_Bin, Ensemble_Model_Eval_Bin, Logistic_Model_Eval_Bin)
    Prediction_results <- mutate(Prediction_results, DT_Model_Eval, XGB_Model_Eval, RF_Model_Eval, Ensemble_Model_Eval, Logistic_Model_Eval, DT_Model_Eval_Bin, XGB_Model_Eval_Bin, RF_Model_Eval_Bin, Ensemble_Model_Eval_Bin, Logistic_Model_Eval_Bin)
    
    accuracies <- reactive({
      data.frame(
        Model = c("Decision Tree", "XGBoost", "Random Forest","KNN", "Logistic Regression", "Ensemble" ),
        Accuracy = c(
          accuracy(Prediction_results$Click_Conversions, DT_Model_Eval_Bin),
          accuracy(Prediction_results$Click_Conversions, XGB_Model_Eval_Bin),
          accuracy(Prediction_results$Click_Conversions, RF_Model_Eval_Bin),
          accuracy(Prediction_results$Click_Conversions, KNN_Model_Eval_Bin),
          accuracy(Prediction_results$Click_Conversions, Logistic_Model_Eval_Bin),
          accuracy(Prediction_results$Click_Conversions, Ensemble_Model_Eval_Bin)
        )
      )
    })
    
    # Function to create confusion matrix 
    createConfMatrix <- function(predicted, actual) {
      # Ensure factors have the same levels
      factor_predicted <- factor(predicted, levels = c(0, 1))
      factor_actual <- factor(actual, levels = c(0, 1))
      
      # Create the confusion matrix
      confMatrix <- table(factor_predicted, factor_actual)
      
      # Convert to a matrix and then to a dataframe for display
      as.data.frame.matrix(confMatrix)
    }
    
    # Compute confusion matrices
    dtConfMatrix <- reactive({ createConfMatrix(DT_Model_Eval_Bin, Prediction_results$Click_Conversions) })
    xgbConfMatrix <- reactive({ createConfMatrix(XGB_Model_Eval_Bin, Prediction_results$Click_Conversions) })
    knnConfMatrix <- reactive({ createConfMatrix(KNN_Model_Eval_Bin, Prediction_results$Click_Conversions) })
    logisticConfMatrix <- reactive({ createConfMatrix(Logistic_Model_Eval_Bin, Prediction_results$Click_Conversions) })
    rfConfMatrix <- reactive({ createConfMatrix(RF_Model_Eval_Bin, Prediction_results$Click_Conversions) })
    ensembleConfMatrix <- reactive({ createConfMatrix(Ensemble_Model_Eval_Bin, Prediction_results$Click_Conversions) })
    
    
    
    # Render accuracies and confusion matrices
    output$modelAccuracies <- renderTable({ accuracies() })
    # Render confusion matrices
    output$dtConfMatrix <- renderTable({ dtConfMatrix() }, rownames = TRUE)
    output$xgbConfMatrix <- renderTable({ xgbConfMatrix() }, rownames = TRUE)
    output$knnConfMatrix <- renderTable({ knnConfMatrix() }, rownames = TRUE)
    output$logisticConfMatrix <- renderTable({ logisticConfMatrix() }, rownames = TRUE)
    output$rfConfMatrix <- renderTable({ rfConfMatrix() }, rownames = TRUE)
    output$ensembleConfMatrix <- renderTable({ ensembleConfMatrix() }, rownames = TRUE)

  })
}

# Run the application 
shinyApp(ui = ui, server = server)
