library(bestglm)
library(Boruta)
library(caret)
library(corrplot)
library(datasets)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(leaps)
library(naniar)
library(png)
library(polycor)
library(purrr)
library(RColorBrewer)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(treemapify)
library(viridis)

set.seed(004)

# Define server logic
shinyServer(function(input, output, session) {
  
  # Import dataframes from csvs
  all_data <- read.csv(file = "diabetic_data.csv")
  plot_age <- read.csv(file = "plot_age.csv")
  plot_gender <- read.csv(file = "plot_gender.csv")
  plot_med <- read.csv(file = "plot_med.csv")
  plot_race <- read.csv(file = "plot_race.csv")
  correlation <- read.csv(file = "da_data.csv")
  lo_predict <- read_csv("logit_predict.csv")
  nn_predict <- read_csv("nn_predict.csv", 
                         col_types = cols(choice = col_factor(
                           levels = c("0","1")), 
                           actual = col_factor(levels = c("0","1"))))
  rf_predict <- read_csv("rf_predict.csv", 
                         col_types = cols(choice = col_factor(
                           levels = c("0","1")), 
                           actual = col_factor(levels = c("0","1"))))
  
  #Use Reactive function for filtering on Explore Readmission Rates tab
  filtered_data <- reactive({
    req(input$Age, input$Gender, input$Age)
    filtered_data <- filter(all_data, 
                            age %in% input$Age, gender %in% input$Gender, 
                            race %in% input$Race)
    return(filtered_data)
    
  })
  
  #Calculate Correlation Matrix
  cordata <- subset(correlation, select = c(45:48,50:58,60))
  cordata2 <- sapply(cordata,as.factor)
  corr <- hetcor(cordata2)$cor
  M <-cor(corr)
  p.mat <- cor.mtest(corr)
  
  #Below Code Cleans all_data dataframe
  #Replace ? values with NA in exploratory analysis
  df <- all_data %>% dplyr::na_if("?")
  
  # /***************************************************************************
  # *    Reference
  # *    Title: Predicting Hospital Readmission of Diabetics
  # *    Author: CC
  # *    Date: May 2021
  # *    Code version: Version 9
  # *    Availability: 
  # *     https://www.kaggle.com/chongchong33/predicting-hospital-readmission-of
  # *     -diabetics
  # *
  # ****************************************************************************
  
  #categorize "readmitted"
  #1 --patient was readmitted within 30 days 
  #0-- readmission after 30 days and no readmission
  
  df$readmitted <- case_when(df$readmitted %in% c(">30","NO") ~ "0",
                             TRUE ~ "1")
  df$readmitted <- as.factor(df$readmitted)
  
    output$activeTab <- reactive({
      return(input$tab)
    })
    outputOptions(output, 'activeTab', suspendWhenHidden=FALSE)
  
    #FOR EXPLORE READMISSION RATES TAB
    output$explore <- renderPlot({
      ggplot(data = filtered_data(), aes(x = readmitted)) + 
        geom_bar(fill = "steelblue4") + 
      ggtitle("Readmission Count")
    })
  
    output$exploreTxt <- renderUI({
      HTML("Examine readmissions rates among different subsets of patients by 
           using the filters on the bottom of the side panel.<br><br>
           <b>Result Definitions:</b><br> 
           <ul>
            <li><30: Patient readmitted less than 30 days after discharge</li> 
            <li>>30: Patient readmitted over 30 days after discharge</li> 
            <li>NO: Patient not readmitted</li>
           </ul>")
    })
    
    #FOR EXPLORATORY ANALYSIS TAB
    output$edaTitle1 <- renderUI({
      HTML(
        "<b>Structure: </b>"
      )  
    })

    output$edaTitle2 <- renderUI({
      HTML(
        "<b>NA values: </b><br>"
      )  
    })
    
    output$edaTitle3 <- renderUI({
      HTML(
        "<b>Readmission counts: </b>"
      )  
    })
    
    output$edaTitle4 <- renderUI({
      HTML(
        "<b>Proportion of patients readmitted: </b>"
      )  
    })
    
    output$edaTxt1 <- renderUI({
      HTML(
        "In this analysis, we explore patient readmission rates using 
        data obtained from the UCI Machine Learning Repository. The dataset 
        spans 10 years (1999-2008) across 130 hospitals and includes 
        55 attributes containing information about the patient's hospital 
        experience (hospital location, diagnosis, test results, etc.), patient 
        demographics, and medications prescribed. We begin by taking a look at 
        the structure of our data.<br><br>"
      )
    })
    output$edaTxt2 <- renderUI({
      HTML("Upon our inital look at the data, we found NA values were coded as 
        ? and most of our attributes are character or integer types. We also 
        found two medications, Citoglipton and Examide, have only one distinct 
        value in their levels. No duplicate rows were found in the data frame.
        Missing values were changed from ? to NA, and then the amount of 
        missing values in each attribute was visualized. Weight was missing in 
        almost all records, while Payer code and Medical specialty had over 40% 
        of their values missing.
        Readmission rates were coded as <30, >30 or NO. To examine readmissions 
        that occur the soonest after discharge, >30 and NO were recoded to 0 
        and <30 as 1. After recoding the readmission rates, we find that 12.6% 
        of the records were readmitted within 30 days.")
    })
    
    output$edaTxt3 <- renderUI({
      HTML("Readmissions rates of patients were examined by if a patient was 
           or was not prescribed diabetes medications. While more 
           individauls on diabetes medications were readmiited, we saw no large 
           diffences between the proportions between the populations.")
    })
    
    output$edaTxt4 <- renderUI({
      HTML("We also examined readmission rates of patients based on admission 
           type. While admission type 1 had the most readmissions, we again saw 
           no large differences between the proportions of the populations 
           compared.")
    })
    
    output$edaTxt5 <- renderUI({
      HTML("We see the most readmissions occur with batients aged between 
           70-80, but proportionally, the 20-30 age group has the highest 
           readmission rates. The under 20 age group contained very low 
           proportions of patients readmitted.")
    })
    
    output$edaTxt6 <- renderUI({
      HTML("Finally, we explored readmisions by race. Most readmissions 
           occurred among Caucasians and the fewest readmissions occurred among 
           the Asian population. Proportionally however, there were few 
           differences in the readmission rates between races.")
    })
    
    output$edaOut1 <- renderPrint({
      str(all_data)
    })
    
    output$edaOut2 <- renderPrint({
      sum(df$readmitted == 1)/sum(df$readmitted == 0)
    })
    
    output$edaPlot1 <- renderPlot({
      #Replace ? values with NA
      #df <- all_data %>% dplyr::na_if("?")
  
      #Observe Missing Values among variables
      gg_miss_var(df)
    })
    
    output$edaPlot2 <- renderPlot({
      ggplot(all_data, aes(x = readmitted)) + geom_bar(fill = "darkorchid4") + 
      ggtitle("Readmission Count")
    })  
    
    output$edaPlot3 <- renderPlot({
      ggplot(all_data, aes(x=diabetesMed, fill = readmitted)) +
        geom_bar() +
        #scale_fill_viridis(discrete = TRUE) +
        ggtitle("Patients with Diabetes Medications") +
        scale_fill_brewer()
    })  
    
    output$edaPlot4 <- renderPlot({
      #Visualize readmission rates
      ggplot(df, aes(x = readmitted)) + 
        geom_bar() +
        ggtitle("Readmission Count")  
    })
    
    output$edaPlot5 <- renderPlot({
      #Visualizing readmissions based on if patient was 
      #taking diabetes medications
      ggplot(df, aes(x=diabetesMed, fill = readmitted)) +
        geom_bar() +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Patients with Diabetes Medications")
    })  
    
    output$edaPlot6 <- renderPlot({
      ggplot(df, aes(x=diabetesMed, fill = readmitted)) +
        geom_bar(position = "fill") +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Patients with Diabetes Medications")
    })  
    
    output$edaPlot7 <- renderPlot({
    #Visualizing readmissions based on admission type
    ggplot(df, aes(x=admission_type_id, fill = readmitted)) +
      geom_bar() +
        scale_x_discrete(limits=c("1" = "1", "2" = "2","3" = "3",
                                  "4" = "4","5" = "5",
                                  "6" = "6","7" = "7","8" = "8")) +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Readmission Rates Admission Type") 
    })
    
    output$edaPlot8 <- renderPlot({
      ggplot(df, aes(x=admission_type_id, fill = readmitted)) +
        geom_bar(position = "fill") +
          scale_x_discrete(limits=c("1" = "1", "2" = "2","3" = "3",
                                    "4" = "4","5" = "5",
                                    "6" = "6","7" = "7","8" = "8")) +
          scale_fill_viridis(discrete = TRUE) +
          ggtitle("Readmission Rates Admission Type") 
    })
    
    output$edaPlot9 <- renderPlot({
      #Visualizing readmissions based on age
      ggplot(df, aes(x=age, fill = readmitted)) +
        geom_bar() +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Readmission Rates by Age")
    })
    
    output$edaPlot10 <- renderPlot({
      ggplot(df, aes(x = age, fill = readmitted)) +
        geom_bar(position = "fill") +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Readmission Rates by Age")
    })
    
    output$edaPlot11 <- renderPlot({
      #Visualizing readmissions based on race
      ggplot(df, aes(x = race, fill = readmitted)) +
        geom_bar() +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Readmission Rates by Race")
    })

    output$edaPlot12 <- renderPlot({
      ggplot(df, aes(x = race, fill = readmitted)) +
        geom_bar(position = "fill") +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle("Readmission Rates by Race")
    })
    
    #FOR ABOUT TAB
    output$about <- renderUI({
      HTML("This Diabetes Readmission Rate applet explores patient data 
      obtained from the UCI Machine Learning Repository and explores methods 
      in which to predict the readmission rate of patients. Or goal is to 
      provide an insight on how we can use this type of data to prevent 
      hospital readmissions <br><br>
      Below you can finda quick summary of each of the sections in the applet. 
      See the Read Me for an in-depth description of each section and our 
      references.<br><br>
      
      <ul>
        <li>Explore Readmission Rates: Allows you to examine the readmission 
        rates of specific patients by filtering the data based on Age, Gender, 
        and Race</li>
        
        <li>EDA and Visualization: A detailed view of the Diabetecs Readmission 
        Rate dataset</li>
        
        <li>Treemaps: A dimensional visualization of the dataset.</li>
        
        <li>Analysis: Further analysis of the dataset including Feature 
        Selection and a Correlation Matrix</li>
        
        <li>Models: Models used to predict the readmission rate of patients 
        including: Regression, Random Forest, and Neural Network</li>
        
        <li>Code: Code used to create this applet and analyze the Diabetics 
        Readmission Rate dataset</li>
        
        <li>Read Me: An in-depth description of the applet and each section</li>
      </ul>")
    })
    
    #FOR TREEMAPS TAB  
    output$treemapTxt <- renderUI({
      HTML("We utilized treemaps to visualize the structure of our data,
      examinining the impact age, gender, medication, and race effect the 
      readmission rates of diabetic patients. 
      <br><br>
      Use the dropdown box below to explore these visualizations yourself.
      ")
    })
    
    # Takes input from selection dropdown and determines which plot to display
    output$tree_plot <- renderPlot({
      if(input$tree_plot == "Age"){
        G = ggplot(plot_age, aes(area = n, fill = age,
                                 label = age, subgroup = readmitted)) +
          geom_treemap() +
          geom_treemap_subgroup_border(colour = "grey", size = 7) +
          geom_treemap_subgroup_text(place = "bottom", grow = FALSE,
                                     alpha = 0.25, colour = "black",
                                     fontface = "italic") +
          geom_treemap_text(colour = "black", place = "centre",
                            size = 15, grow = FALSE, alpha=0.5)
      }
      
      if(input$tree_plot == "Gender"){
        G = ggplot(plot_gender, aes(area = n, fill = gender,
                                    label = gender, subgroup = readmitted)) +
          geom_treemap() +
          geom_treemap_subgroup_border(colour = "grey", size = 7) +
          geom_treemap_subgroup_text(place = "bottom", grow = FALSE,
                                     alpha = 0.25, colour = "black",
                                     fontface = "italic") +
          geom_treemap_text(colour = "black", place = "centre",
                            size = 15, grow = FALSE, alpha=0.5)
      }
      
      if(input$tree_plot == "Medicated"){
        G = ggplot(plot_med, aes(area = n, fill = diabetesMed,
                                 label = diabetesMed, subgroup = readmitted)) +
          geom_treemap() +
          geom_treemap_subgroup_border(colour = "grey", size = 7) +
          geom_treemap_subgroup_text(place = "bottom", grow = FALSE,
                                     alpha = 0.25, colour = "black",
                                     fontface = "italic") +
          geom_treemap_text(colour = "black", place = "centre",
                            size = 15, grow = FALSE, alpha=0.5)
      }
      
      if(input$tree_plot == "Race"){
        G = ggplot(plot_race, aes(area = n, fill = race,
                                  label = race, subgroup = readmitted)) +
          geom_treemap() +
          geom_treemap_subgroup_border(colour = "grey", size = 7) +
          geom_treemap_subgroup_text(place = "bottom", grow = FALSE,
                                     alpha = 0.25, colour = "black",
                                     fontface = "italic") +
          geom_treemap_text(colour = "black", place = "centre",
                            size = 15, grow = FALSE, alpha=0.5)
      }
      G
    })

    #FOR FEATURE SELECTION TAB
    output$fsImg1 <- renderImage({
      # expression will re-run whenever they change.
      width  <- session$clientData$output_fsImg1_width
      height <- session$clientData$output_fsImg1_height
      
      # For high-res displays, this will be greater than 1
      pixelratio <- session$clientData$pixelratio
      
      # A temp file to save the output.
      outfile <- normalizePath(file.path('./www',
                                          paste('Assignment2_Img2', input$n, '.png', sep='')))
      
      # Generate the image file
      png(outfile, width = width*pixelratio, height = height*pixelratio,
          res = 72*pixelratio)
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           width = width,
           height = height,
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    output$fsImg2 <- renderImage({
      # expression will re-run whenever they change.
      width  <- session$clientData$output_fsImg2_width
      height <- session$clientData$output_fsImg2_height
      
      # For high-res displays, this will be greater than 1
      pixelratio <- session$clientData$pixelratio
      
      # A temp file to save the output.
      outfile <- normalizePath(file.path('./www',
                                         paste('Assignment2_Img3', input$n, '.png', sep='')))
      
      # Generate the image file
      png(outfile, width = width*pixelratio, height = height*pixelratio,
          res = 72*pixelratio)
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           width = width,
           height = height,
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    output$fsTxt1 <- renderUI({
      HTML("
        <b>Summary: </b><br><br>
        Using the Boruta library, we identified the important attributes in our 
        data set. These attributes, listed on the right, were used to 
        create a predictive logistic model. The visualizations identifying the 
        important attributes are shown below.
        <br><br><br>
      ")
    })
    
    output$fsTxt2 <- renderUI({
      HTML("
        <b>Important Attributes</b>
        <br>
          race<br>
          age<br>
          admission_type_id</br>
          discharge_disposition_id<br>
          admission_source_id<br>
          time_in_hospital<br>
          num_lab_procedures<br>
          num_procedures<br>
          num_medications<br>
          number_outpatient<br>
          <br><br>
      ")
    })
    
    output$fsTxt3 <- renderUI({
      HTML("
        <b><i>Important Attributes Continued</i></b>
        <br>
          number_emergency<br>
          number_inpatient<br>
          number_diagnoses<br>
          max_glu_serum<br>
          A1Cresult<br>
          metformin<br>
          insulin<br>
          change<br>
          diabetesMed<br>
        <br><br>
      ")
    })
    
    output$fsTxt4 <- renderUI({
      HTML("
        <b>Plot 1</b>
      ")
    })
    
    output$fsTxt5 <- renderUI({
      HTML("
        <br>
        <b>Plot 2</b>
      ")
    })
    
    #FOR CORRELATION MATRIX ANALYSIS TAB
    output$corrTxt <- renderUI({
      HTML("
        Using selected features, a correlation matrix was created and 
        visualized using the ‘corrplot’ and ‘hector’ packages. From this 
        analysis, we observed that no significant 
        correlations exit between any of our variables and noted that we should 
        not consider variable interactions in our future models.
        <br><br>
        <b>Correlation Plot:</b>
      ")
    })
    
    output$corr_plot <- renderPlot({
      corrplot(M, method = "color",
               outline = T, addgrid.col = "darkgray", order="hclust",
               addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b",
               tl.col = "indianred4", tl.cex = 1, cl.cex = 1, 
               addCoef.col = "white", number.digits = 2, number.cex = 0.5, 
               col = colorRampPalette(c("darkred","white","midnightblue"))(100))
    })
    
    #FOR LOGISTIC REGRESSION MODEL TAB  
    # Calculate prediction accuracy and create value box object
    lo_predict_vbox <-  valueBox(
      value = percent(mean(lo_predict$predicted == lo_predict$readmitted), 
                      accuracy = .01),
      subtitle = "Percentage Correctly Predicted",
      icon = icon("percentage"),
      width = 4,
      color = "light-blue",
      href = NULL)
    
    output$lo_vbox <- renderValueBox(lo_predict_vbox)
    
    output$lo_plot <- renderPlot({fourfoldplot(table(lo_predict$predicted, 
                                                     lo_predict$readmitted), 
                                               color = c("lightseagreen", 
                                                         "lightskyblue"),
                                               conf.level = 0, margin = 1, 
                                               main = "Confusion Matrix")}) 
    
    output$logitTxt <- renderUI({
      HTML(
        "After performing feature selection to isolate the important 
      attributes of our dataset, a logistic model was utilized to predict the 
      readmission rates of diabetic patients. Splitting our data into 80% train 
      and 20% test sets, we performed a 10-fold cross validation attaining a 
      prediction accuracy of 88.79%. 
      ")
    })
    
    #FOR RANDOM FOREST MODEL TAB
    rf_predict_vbox <-  valueBox(
      value = percent(mean(rf_predict$choice == rf_predict$actual), 
                      accuracy = .01),
      subtitle = "Percentage Correctly Predicted",
      icon = icon("percentage"),
      width = 4,
      color = "light-blue",
      href = NULL)
    
    output$rf_vbox <- renderValueBox(rf_predict_vbox)
    
    output$rf_plot <- renderPlot({fourfoldplot(table(rf_predict$choice, 
                                                     rf_predict$actual), 
                                               color = c("lightseagreen", 
                                                         "lightskyblue"),
                                               conf.level = 0, margin = 1, 
                                               main = "Confusion Matrix")})
    output$rfTxt <- renderUI({
      HTML("
        Initially, we created a Random Forest model with all possible variables
        and used native libraries in R to predict readmission rates. This
        process took a large amout of computation time. Due to this 
        constraint, we elected to seek alternative approaches. 
        <br><br>
        Through feature selection and utilizing the h2o library for machine 
        learning, we were able to create a more efficient model, decreasing 
        computation time from hours to minutes and achieving an accuracy rate 
        of 88.66%.
      ")
    })
    
    #FOR NEURAL NETWORK MODEL TAB
    nn_predict_vbox <-  valueBox(
      value = percent(mean(nn_predict$choice == nn_predict$actual), 
                      accuracy = .01),
      subtitle = "Percentage Correctly Predicted",
      icon = icon("percentage"),
      width = 4,
      color = "light-blue",
      href = NULL)
    
    output$nn_vbox <- renderValueBox(nn_predict_vbox)
    
    output$nn_plot <- renderPlot({fourfoldplot(table(nn_predict$choice, 
                                                     nn_predict$actual), 
                                               color = c("lightseagreen", 
                                                         "lightskyblue"),
                                               conf.level = 0, margin = 1, 
                                               main = "Confusion Matrix")})
    
    output$nnTxt <- renderUI({
      HTML("
        Through feature selection and the utilization of the deeplearning 
        function from the h2o library, an artificial neural network model was 
        created. We used 10-fold cross validation for our model and then 
        predicted the readmission rates of patients in our test set. 
        <br><br>
        Creating a neural network was the most computationally expensive model, 
        however we attained an accuracy of 88.75%. 
      ")
    })
    
})
  
