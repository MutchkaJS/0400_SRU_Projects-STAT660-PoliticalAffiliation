# 0400_SRU_Projects-STAT660-Political Affiliation #  
This project explores political party affiliation based on various demographic and socioeconomic factors.  The pipeline includes data ingestion, cleaning, and multiple predictive modeling approaches in R, SQL, EXCEL and WEKA.  This README includes the following information to recreate each part of the analysis.  Any suggestions, comments, discussions, or critiques are welcome!  

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
The first step to this analysis was create a dataframe containing only variables common to all three datasets: AGE, INCOME, EDUCATIONAL LEVEL, RACE, SEX, and POLITICAL PARTY AFFILIATION.  I used EXCEL to first omit the variables which were not required for the analysis, and then join all of the observations into one large dataframe totaling n = 1615 instances.  Unfortunately the INCOME attribute contained 344 missing cases.  Using WEKA, I imputed missing values using the k-NN Imputation Filte - [INFO: k-NN Imputation Filter description](https://github.com/MutchkaJS/0400_SRU_Projects-STAT660-PoliticalAffiliation/blob/main/INFO_WEKA_Imputation_Filter.docx)  
