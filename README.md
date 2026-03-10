# 0701_Political Affiliation: Understanding Independent Voters #  
This project explores political party affiliation based on various demographic and socioeconomic factors.  The pipeline includes data ingestion, cleaning, and multiple predictive modeling approaches in R, SQL, EXCEL and WEKA.  As per my perpetual problem, I am redoing the original analysis a good bit and entering it as I go.. i.e. the documentation is not finished yet! This README includes the following information to recreate each part of the analysis.  Any suggestions, comments, discussions, or critiques are welcome!  

## OVERVIEW ##
This study stems from the desire to better understand how Democrat, Independent, and Republican voters differ in both their socioeconomic circumstances and their views/beliefs.  
- In STAGE 1, I use logistic regression to look determine the utility of using sociodemographic variables for predicting political party affiliation.
- In STAGE 2, k-NN and Random Forest analyses are used to examine if independent voters are closer to democrats or republicans on several beliefs.
- In STAGE 3, cluster analysis was used to see how clustering groups voters and if there are insights that can be gained. 


## DATA DESCRIPTION  ##
The Pulse of the Nation dataset from September, October, and November 2017 captures monthly public opinion on politics and social issues among nationally sampled U.S. adults, collected as part of a year‑long polling project funded by Cards Against Humanity. Across these three months, respondents report demographic information such as age, income, education, and political party affiliation, along with their views on Donald Trump’s job performance and broader scientific, social, and cultural topics. The data are individual‑level, with each row representing a survey respondent and each column representing a specific question or characteristic, allowing for rich multivariate analysis of political attitudes over time.

Beyond core political questions, the surveys in these months mix serious and lighthearted items, including beliefs about climate change, perceptions of scientists’ honesty, attitudes toward vaccines, expectations about job automation, pop‑culture preferences (such as number of Transformers movies watched), and beliefs in ghosts. This blend of variables makes the dataset especially useful for exploring how demographic traits and unconventional attitudes relate to political affiliation and presidential approval across the early Trump administration, while also enabling comparison of response patterns from September through November 2017 to study short‑term shifts in public sentiment.  

### The RAW datafiles for September, October & November 2017 are provided below. ###
- [September 2017 data](data/201709-CAH_PulseOfTheNation.csv)
- [October 2017 data](data/201710-CAH_PulseOfTheNation.csv)
- [November 2017 data](data/201711-CAH_PulseOfTheNation.csv)

Demographic questionaire items are similar for each month of the study, however social questionaire items differ from month to month.  A full list of the three questionaire items is provided here: -[Pulse of the Nation Questionaire Items](/Questionaire_PulseOfNation_all.docx).  For Further Information regarding the Pulse Of The Nation study at [PulseOfTheNation.com](https://thepulseofthenation.com/) 

## STAGE 1: DATA CLEANING / PREPROCESSING ##
The first step to this analysis was create a dataframe containing only variables common to all three datasets: AGE, INCOME, EDUCATIONAL LEVEL, RACE, SEX, and POLITICAL PARTY AFFILIATION.  I used EXCEL to first omit the variables which were not required for the analysis, and then join all of the observations into one large dataframe totaling n = 1615 instances.  Unfortunately the INCOME attribute contained 344 missing cases.  Using WEKA, I imputed missing values using the k-NN Imputation Filte - [INFO: k-NN Imputation Filter description](/INFO_WEKA_Imputation_Filter.docx)  

The second step to this analysis was to identify outliers.  WEKA was used to identify and remove outliers using the Interquartile Range filter, and RemoveValues filter respectively. Eighty-two instances were removed, leaving n = 1517 instances for analysis. The .csv for this dataframe is provided here: - [Stage 1: Logistic Regression Data](/logistic_5_nooutliers.csv).  At this point that dataframe was imported into MySQL Workbench and the variables were recoded into more general categories. The results are shown in the DESCRIPTIVE UNIVARIATE ANALYSIS below. 


## STAGE 1a: DESCRIPTIVE UNIVARIATE ANALYSIS ##
Logistic regression makes no assumptions regarding normality for its independent variables.  Nontheless, the distributions are provided here to help better understand the data.  In the event that error estimate appear inflated, we can come back and transform them and compare the AIC measures of the raw & tranformed variables after conducting the logistic regression. To create the following charts, utilize the attached file - [Descriptive Univariate Analysis R-script](/descriptives_allVariables_logisticRegression.R)

<table align="center">
<tr>
<td align="center">
  <b>Income: Raw</b><br>
  <img src="https://github.com/user-attachments/assets/80f04b29-afe4-4d51-894b-8ce96ad1a48d" 
       alt="Income Raw"
       style="border: 3px solid #333; border-radius: 8px; padding: 4px;" 
       width="350" height="438" />
</td>
<td align="center">
  <b>Age: Raw</b><br>
  <img src="https://github.com/user-attachments/assets/a929d327-ef0e-46d7-b6c5-672048261f3e"
       alt="Age Raw" 
       style="border: 3px solid #333; border-radius: 8px; padding: 4px;" 
       width="350" height="438" />
</td>
</tr>
</table>


The categorical variables below were each collapsed, and the tables below show the frequencies PRIOR & AFTER Ccollapse.  The coding to collapse categories was completed in SQLWorkbench using data in the file, - [Election_Logistic_recode](/Election_Logistic_recode.sql), and saved as the file, -[Election_Research_2](/Election_Research_2.csv) ZZ.  The visualization was conducted in RStudio using the file -   
 [Descriptive Univariate Analysis R-script](/descriptives_allVariables_logisticRegression.R) _NOTE:_ to use this file to create the visualizations of PRE and AFTER frequecies you must alter the 'df <-data.file() in LINE 3 by replacing the dataset _Logistic_5_nooutliers_ with the dataset _Election_Research_2_. 
## _PRIOR TO COLLAPSING CATEGORIES_
### RAW FREQUENCIES of Gender, Educational Level, Race and Political Affiliation
<table align="center" style="width:100%; border-spacing: 20px;"> <!-- creates & center table -->
<tr> <!-- defines 1 table row -->
<td align="center" valign="top" style="padding: 10px;"> <!-- table data cell -->
<img width="281" height="241" alt="Gender" src="https://github.com/user-attachments/assets/e682a6ee-33dd-4844-bf5b-43bd5fde486d" />
</td>
<td align="center" valign="top" style="padding: 10px;">
<img width="282" height="503" alt="Age" src="https://github.com/user-attachments/assets/719dcef8-f46d-4881-ad76-28d534e9f5a6" />
</td>
<td align="center" valign="top" style="padding: 10px;">
<img width="292" height="310" alt="Education" src="https://github.com/user-attachments/assets/4e0f6f27-49c2-4a09-93b9-bbcfbdc35bb0" />
</td>
<td align="center" valign="top" style="padding: 10px;">
<img width="298" height="483" alt="Political Affiliation" src="https://github.com/user-attachments/assets/5c0d1ced-764a-4427-8446-d69305bac19b" />
</td>
</tr>
</table>
</div>

## _AFTER COLLAPSING CATEGORIES_
### RECODED VARIABLES: Gender, Educational Level, Race and Political Affiliation
NOTES - {Gender: Male = 0, Female = 1}, {Education Level: Other type of Training = 1, < High School Degree = 2, High School Degree = 3, Some College = 4, College Degree = 5, Graduate Degree = 6}
<table align="center" style="width:100%; border-spacing: 20px;"> <!-- creates & center table -->
<tr> <!-- defines 1 table row -->
<td align="center" valign="top" style="padding: 10px;"> <!-- table data cell -->
<img width="281" height="241" alt="Gender" src="https://github.com/user-attachments/assets/877b69e8-99b8-406d-9083-bf96394b3a7c" />
</td>
<td align="center" valign="top" style="padding: 10px;">
<img width="282" height="503" alt="Age" src="https://github.com/user-attachments/assets/1b669f25-b688-476c-bd21-ab5a00ecaf7d" />
</td>
<td align="center" valign="top" style="padding: 10px;">
<img width="292" height="310" alt="Education" src="https://github.com/user-attachments/assets/63e666e4-db99-4c4f-85c8-51adfbca5dbd" />
</td>
<td align="center" valign="top" style="padding: 10px;">
<img width="298" height="483" alt="Political Affiliation" src="https://github.com/user-attachments/assets/2967f49c-692e-4d6e-9281-56ed1456ec61" />
</td>
</tr>
</table>
</div>
  ___
<p align="center">
THE FOLLOWING CHARTS show the bar charts for the final collapsed data (Gender, Age, Education, & Political Party)
</p>
  ___ 
<div style="display: flex; justify-content: space-around; align-items: center; flex-wrap: nowrap; width: 100%;">
  <img src="https://github.com/user-attachments/assets/07d05357-ff05-4479-9c1d-6ffc12a96dee" alt="image 1" style="width: 22%; height: auto;" />
  <img src="https://github.com/user-attachments/assets/1a28e10d-5034-4519-bb53-98041c82e2cb" alt="image 2" style="width: 22%; height: auto;" />
  <img src="https://github.com/user-attachments/assets/68104276-f200-4585-a4d3-5b6f9fe8db52" alt="image 3" style="width: 22%; height: auto;" />
  <img src="https://github.com/user-attachments/assets/f8730847-c6bc-4e53-9a9f-50251b2211f0" alt="image 4" style="width: 22%; height: auto;" />
</div>

<table align="center" style="width:100%; border-spacing: 2px;">
<tr>
<td align="center"><img src="https://github.com/user-attachments/assets/07d05357-ff05-4479-9c1d-6ffc12a96dee" width="22%" alt="image 1"/></td>
<td align="center"><img src="https://github.com/user-attachments/assets/1a28e10d-5034-4519-bb53-98041c82e2cb" width="22%" alt="image 2"/></td>
<td align="center"><img src="https://github.com/user-attachments/assets/68104276-f200-4585-a4d3-5b6f9fe8db52" width="22%" alt="image 3"/></td>
<td align="center"><img src="https://github.com/user-attachments/assets/f8730847-c6bc-4e53-9a9f-50251b2211f0" width="22%" alt="image 4"/></td>
</tr>
</table>
