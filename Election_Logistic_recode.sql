/*This code recode the variables income, gender, age, party, edlevel, and race.
The script uses the file `logistic_5_nooutlier.csv`*/

/*Generating the freq from the original likert values*/
/*Party*/
SELECT Party, COUNT(*) AS frequency
FROM election_research.logistic_5_nooutliers
GROUP BY Party
ORDER BY frequency DESC;
select count(*) from election_research.logistic_5_nooutliers;	/* */

/*Gender*/
SELECT gender, COUNT(*) AS frequency
FROM election_research.logistic_5_nooutliers
GROUP BY gender
ORDER BY frequency DESC;
select count(*) from election_research.logistic_5_nooutliers;	/* 1270*/

SELECT EdLevel, COUNT(*) AS frequency
FROM election_research.logistic_5_nooutliers
GROUP BY EdLevel
ORDER BY frequency desc; 

SELECT Race, COUNT(*) AS frequency
FROM election_research.logistic_5_nooutliers
GROUP BY Race
ORDER BY frequency desc; 

SELECT 
  COUNT(income) AS count_income,
  AVG(income) AS avg_income,
  SUM(income) AS total_income,
  MIN(income) AS min_income,
  MAX(income) AS max_income,
  VARIANCE(income) AS variance_income,
  STDDEV(income) AS stddev_income
FROM election_research.logistic_5_nooutliers;

SELECT 
  COUNT(age) AS count_age,
  AVG(age) AS avg_age,
  SUM(age) AS total_age,
  MIN(age) AS min_age,
  MAX(age) AS max_age,
  VARIANCE(age) AS variance_age,
  STDDEV(age) AS stddev_age
FROM election_research.logistic_5_nooutliers;

/*consolidate the data*/
/*CHANGING INCOME, GENDER, AGE, PARTY, EDLEVEL, AND RACE*/
SELECT
  CASE
    WHEN party = 'Independent' then 'Independent'
    WHEN party = 'Strong Democrat' THEN 'Democrat'
    WHEN party = 'Not Strong Democrat' THEN 'Democrat'
    WHEN party = 'Strong Republican' THEN 'Republican'
    WHEN party = 'Not Strong Republican' THEN 'Republican'
    WHEN party = 'Democrat' then 'Democrat'
    WHEN party = 'Republican' then 'Republican'
    WHEN party = 'DK/REF' then 'NA'
    ELSE party = '.'
  END AS party_short,
  COUNT(*) AS frequency
  FROM election_research.logistic_5_nooutliers
		GROUP BY party_short
        ORDER BY frequency desc;
  
  SELECT
    CASE
		WHEN gender = 'female' THEN 1
        WHEN gender = 'male' THEN 0
        WHEN gender = 'Other' THEN floor(rand()*1) /*assigns 0 or 1 randomly*/
        WHEN gender = 'DK/REF' THEN 'NA'
        ELSE gender = '.'
	END AS gender_short,
	COUNT(*) AS frequency
	FROM election_research.logistic_5_nooutliers
		GROUP BY gender_short
        ORDER BY frequency desc;
        
SELECT
	CASE
		WHEN Edlevel = 'Graduate Degree' THEN 6
		WHEN Edlevel = 'College Degree' THEN 5
		WHEN Edlevel = 'Some College' THEN 4
		WHEN Edlevel = 'High school' THEN 3
		WHEN Edlevel = 'High school or less' THEN 2
		WHEN Edlevel = 'Other' THEN 1
        WHEN Edlevel = 'DK/REF' THEN 'NA'
	END AS Edlevel_num,
	COUNT(*) AS frequency
    FROM election_research.logistic_5_nooutliers
		group by EdLevel_num
        order by frequency desc;

/*CREATE A NEW DUPLICATE TABLE WITH A NEW NAME*/
CREATE TABLE election_research.election_research_2 AS SELECT * FROM election_research.logistic_5_nooutliers;

/*Use this new table to UPDATE the variables for export*/

UPDATE election_research.election_research_2
SET party = CASE
    WHEN party = 'Independent' then 'Independent'
    WHEN party = 'Strong Democrat' THEN 'Democrat'
    WHEN party = 'Not Strong Democrat' THEN 'Democrat'
    WHEN party = 'Strong Republican' THEN 'Republican'
    WHEN party = 'Not Strong Republican' THEN 'Republican'
    WHEN party = 'Democrat' then 'Democrat'
    WHEN party = 'Republican' then 'Republican'
    WHEN party = 'DK/REF' then 'NA'
    ELSE party = '.'
  END;

UPDATE election_research.election_research_2
SET gender = CASE
		WHEN gender = 'female' THEN 1
        WHEN gender = 'male' THEN 0
        WHEN gender = 'Other' THEN floor(rand()*1) /*assigns 0 or 1 randomly*/
        WHEN gender = 'DK/REF' THEN 'NA'
        ELSE gender = '.'
	END;
	
UPDATE election_research.election_research_2
	SET Edlevel = CASE
		WHEN Edlevel = 'Graduate Degree' THEN 6
		WHEN Edlevel = 'College Degree' THEN 5
		WHEN Edlevel = 'Some College' THEN 4
		WHEN Edlevel = 'High school' THEN 3
		WHEN Edlevel = 'High school or less' THEN 2
		WHEN Edlevel = 'Other' THEN 1
        WHEN Edlevel = 'DK/REF' THEN 'NA'
	END;
        
UPDATE election_research.election_research_2
	SET Race = CASE
		WHEN Race = 'White' THEN 'White'
        WHEN Race = 'Black' THEN 'Black'
        WHEN Race = 'Latino' THEN 'Latino'
        WHEN Race = 'Asian' THEN 'Asian'
        WHEN Race = 'Other' THEN 'Other'
        WHEN Race = 'DK/REF' THEN 'NA'
        ELSE Race = '.'
	END;
    
/*MAKE THE DATA TABLE TO BE USED FOR THE R ANALYSIS*/
SELECT * FROM election_research.election_research_2;
