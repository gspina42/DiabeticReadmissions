### Important Links

**Github:**https://github.com/gspina42/DiabeticReadmissions

**Webapp:** https://nwise.shinyapps.io/ADSFinalProject/

### Research Question

The objective of our research was to analyze readmission rates based on patient 
data and provide an insight on how we can use this type of data to prevent 
hospital readmissions by predicting patients who are most likely to be 
readmitted. 

To perform this task, we set three goals: 

1. Explore the structure of our data
2. Highlight important features of the data 
3. Create a reliable prediction model. 

Additionally, we aimed to display this information in a user-friendly applet 
(RShiny) to assist others in understanding this data. 


### Data

Diabetic patient readmission rates were obtained through the UCI Machine 
Learning Repository. The dataset contains approximately 100,000 rows and spans 
10 years (1999-2008) across 130 hospitals and includes 55 attributes containing 
information about the patient’s hospital experience (hospital location, 
diagnosis, test results, etc.), patient demographics, and medications 
prescribed.

The data was collected by the Center for Clinical and Translational Research at 
Virginia Commonwealth University in cooperation with the Cerner Corporation. 
As the data was collected by a university and studies using this data have 
been published in a peer reviewed journal (BioMed Research International), we 
believe the data source to be reliable, but possibly limited in scope, as the 
dataset only contains information about diabetic patients.   


### Design

RShiny was utilized to build an applet for our project. 

To promote a consistent and user-friendly design aesthetic, we maintained a 
default background color throughout the entire applet, increased the font size 
from the default size to 15px and added padded the margins of the body for a 
cleaner look. 

We also wanted to ensure that our applet was easily navigable, to do this we 
elected to use a sidebar design, assigning a tab or sub-tab to each task 
performed. 

Conditional panels and dropdown menus were used to add filtering and user 
selection capabilities. To avoid issues with loading times in the tabs 
‘Treemaps’ and ‘Models’ we uploaded csv files of data summaries and the results 
of our previously constructed prediction models, allowing us to avoid a long 
processing time when these tabs were selected. 


### Tasks

Our project consisted of eight tasks broken down across five tabs within the 
applet. A discussion of each tab and task is below. 

**Exploring Readmission Rates:**
Using readmission data, a visualization was constructed using ggplot that 
displayed the count of readmitted patients. Reactive input was added to allow a 
user to filter on age, gender, or race and observe how these variables impact 
the readmission rate of a diabetic patient.

**Exploratory Data Analysis – Data Structure and Visualization:**
Native R functions and ggplot visualizations were used to observe the structure 
and contents of each variable in our dataset.

We found NA values were coded as ? and most of our attributes are character or 
integer types. Data was cleaned by changing missing values from ? to NA, and 
then the amount of missing values in each attribute was calculated. Weight was 
missing in almost all records, while Payer code and Medical specialty had over 
40% of their values missing.

We continued our exploratory data analysis through visualization first 
examining the number of readmissions. We found that 12.6% of the records were 
readmitted within 30 days. Patient attributes and demographics were then 
explored.

The first visualizations compared readmission rates to DiabetesMed, a variable 
indicating if a patient was taking a diabetes medication and Admission type. A 
large portion of our data frame were taking Diabetes medications as admitted as 
with Admission type 1. Exploring the proportions of those readmitted among 
DiabetesMed and Admission type, we saw no large differences between the populations.

Readmissions were explored by race and age. Most readmissions occurred among 
the Caucasian population, but proportionally, there did not appear to be many 
differences between races. The age range with the most readmissions were 
patients between 70-80, but proportionally the 20-30 age range had the highest 
readmissions while those under 20 had very low proportions of patients 
readmitted.

**Tree Map Visualization**
This visualization allowed us to provide a user with capabilities to examine 
the impact age, gender, medication, and race have on the readmission rates of 
diabetic patients.

To create the interactive treeplots we first created a dropdown input using 
selectInput in the appropriate tabItem in the ui.R file. To simplify 
implementation, we imported csv files containing the summarized data to 
construct our tree maps. A series of if statements in the server.R file that 
checked what the selectInput was returning that in turn rendered the 
appropriate plot. 


**(In-Depth Analysis - Feature Selection and Correlation Matrix)**
Feature selection is an important part of any ML pipeline. If too many features 
are kept, then there is the possibility of introducing irrelevant noise in your 
data, but if you remove too many features then there is the possibility useful 
information not being captured in the analysis. The ‘boruta’ package, which is 
a wrapper algorithm built around Random Forest, was used to determine important 
features, 19 were identified in our data. 

From these features that were selected, a correlation matrix was created and 
visualized using the ‘corrplot’ and ‘hector’ packages. From this analysis, we 
can see that there are no significant positive correlations between any of the 
variables.

**Modeling – Logistic Model, Random Forest Model, Neural Network**
To predict readmission rates of a patient, we explored three possible models: 
Logistic Regression, Random Forest and a Neural Network.  For each model we 
imported a csv file containing predicted and actual values from each 
constructed model. Value boxes which calculate and display the percentage of 
correct predictions for each model were added. In addition to the value boxes, 
a confusion matrix was added for each model to visually show the predictive 
performance.

We found similar performance among all models but noted that the computation 
time to create the Random Forest model and neural network was much larger than 
our logistic model.


### Challenges

This project was the first time any of our team encountered building an applet. 
As such, there were some growing pangs in discovering how to perform certain 
tasks, finding the best practices to use when creating an applet, and 
understanding the connections between UI and server code. We relied on 
training from DataCamp, our course modules, as well as online forums to resolve 
coding issues we encountered.


### References

Alice, M. (2018, June). How to Perform a Logistic Regression in R. 
Data Science Plus. datascienceplus.com/performlogistic-regression-in-r/

Anand, Raj, et al. (2013, January). K-Fold Cross Validation and Classiﬁcation 
Accuracy of PIMA Indian Diabetes Data Set Using Higher Order Neural Network and 
PCA. International Journal of Soft Computing and Engineering (IJSCE), vol. 2, 
no. 6,  pp. 436-438.

Andrie (2018, April). Showing only the first few lines of the results of a code 
chunk. Rstudio Community. 
https://community.rstudio.com/t/showing-only-the-first-few-lines-of-the-results-of-a-code-chunk/6963

C.C. (2021, May). Predicting Hospital Readmission of Diabetics. Kaggle. https://www.kaggle.com/chongch ong33/predicting-hospital-readmission-of-diabetics

ggnot.(2021, March). Neural Networks in R Tutorial (H2O tutorial in R: how to 
create an ANN for binary classiﬁcation). Youtube. https://www.youtube.com/watch?v=Ck10_VtN_88.

H2O.ai. “H2O Documentation.” H2O.ai, https://docs.h2o.ai/h2o/latest-stable/h2o-docs/faq/r.html.

Kabacoﬀ, Rob. (2020). “Data Visualization with R.” https://rkabacoff.github.io/datavis/.

Kaplan, Jacob. (2020). Package ‘fastDummies’. CRAN, https://github.com/jacobkap/fastDummies.

Strack, B., DeShazo, J.P., Gennings, C., Olmo, J.L., Ventura, S., Cios, K.J. 
and Clore, J.N. (2014). Impact of HbA1c Measurement on Hospital Readmission 
Rates: Analysis of 70,000 Clinical Database Patient Records, vol(2014), 
11 pages. https://doi.org/10.1155/2014/781670
