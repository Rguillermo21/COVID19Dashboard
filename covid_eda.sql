-- Creating our table so we can import our data into pgAdmin4
-- Make sure NA/NULL are treated in the CSV to 0, otherwise they will be read in as a text and will fail to be imported
-- Make sure to check CSV for scientific notation instead of numbers as well or import will fail
CREATE TABLE public.covid_data
(
    ID BIGSERIAL PRIMARY KEY NOT NULL,
    name text NOT NULL,
    date date NOT NULL,
    total_cases bigint,
    new_cases integer,
    total_deaths bigint,
    new_deaths integer,
    total_tests bigint,
    new_tests integer,
    total_vaccinations bigint,
    new_vaccinations integer,
    median_age real,
    population_density double precision,
    extreme_poverty real,
    gdp_per_capita double precision,
    handwashing_facilities double precision,
    life_expectancy real,
    hospital_beds_per_thousand double precision,
    population bigint,
    human_development_index double precision,
)

-- Familiarizing myself at the data and columns
SELECT *
FROM public.covid
LIMIT 10;

-- Common Table Expression
-- Glancing at the most recent data records for the main vars of interest using a temporary table
WITH cte_recent AS (
	SELECT 
		name,
		date,
		total_cases,
		new_cases,
		total_deaths,
		new_deaths,
		(total_deaths/total_cases)*100 AS death_pct,
		total_tests,
		new_tests
	FROM public.covid
	WHERE date = '2023-02-26')
	
SELECT (total_cases/total_deaths) AS death_pct
FROM cte_recent
WHERE date = '2023-02-26';

-- North Korea seems to have more deaths than reported cases according to the OWID data source
SELECT 
	name,
	date,
	total_cases,
	total_deaths,
	total_deaths/total_cases AS death_pct
FROM public.covid
WHERE total_cases != 0 AND total_deaths < total_cases
ORDER BY death_pct DESC;


-- WE CAN SEPARATE OUR DATA INTO DIFFERENT TABLES
-- We create a reference table to hold meta data and good for normalizing our database 
-- This way we can reduce storage space for redundant information

CREATE TABLE IF NOT EXISTS reference_table

(
	ID BIGSERIAL PRIMARY KEY,
	name TEXT,
	date DATE
);

INSERT INTO reference_table
(
	SELECT
		"ID" AS ID,
		name,
		date
	FROM public.covid
	
);

CREATE TABLE IF NOT EXISTS covid_deaths 
(
	ID BIGSERIAL PRIMARY KEY,
	total_deaths BIGINT,
	new_deaths INTEGER,
	population BIGINT
);

INSERT INTO covid_deaths (
		SELECT 
		"ID",
		total_deaths,
		new_deaths,
		population
	FROM public.covid
	);

CREATE TABLE IF NOT EXISTS covid_cases
(
	ID BIGSERIAL PRIMARY KEY,
	total_cases BIGINT,
	new_cases INTEGER,
	population BIGINT
);

INSERT INTO covid_cases 
(
	SELECT
		"ID",
		total_cases,
		new_cases,
		population
	FROM public.covid
);


CREATE TABLE IF NOT EXISTS covid_vaccinations
(
	ID BIGSERIAL PRIMARY KEY,
	total_vaccinations BIGINT,
	new_vaccinations INTEGER,
	population BIGINT
);

INSERT INTO covid_vaccinations
(
	SELECT
		"ID",
		total_vaccinations,
		new_vaccinations,
		population
	FROM public.covid
);

-- Subquery
-- How many cases result in deaths? Global + country perspective
-- From a global perspective, it seems that a little over 1% of the covid cases result in fatality
SELECT
	total_cases,	
	total_deaths,
	CAST(total_deaths AS NUMERIC)/ CAST(total_cases AS NUMERIC) * 100.000 AS death_pct
FROM 
	(
		SELECT 
			sum(new_cases) AS total_cases,
			sum(new_deaths) AS total_deaths
		FROM public.covid
	) AS t; -- Subquery in FROM is cleaner than create a temp table

-- Nested correlated subquery
-- Peru, Bulgaria, Bosnia and Herzegovina, Hungary and North Macedonia has high deaths per population
-- Peru is #1 deaths but also has very high vaccination rate per population. 
-- If we add the population and vacc_per_pop column for the next 5 countries, it would be similar to Peru's numbers

SELECT 
	name AS country,
	(SELECT total_cases 
	 FROM (SELECT 
		    name,
		    MAX(total_cases) AS total_cases 	
	 	FROM covid_cases AS cc
	 	LEFT JOIN reference_table AS ref
	        ON cc.id = ref.id
		GROUP BY NAME) AS cc
	 WHERE e.name = cc.name) AS total_cases,
	MAX(total_deaths) AS total_death,
	MAX(total_vaccinations) AS total_vacc,
	MAX(population) AS population,
	ROUND(SUM(CAST(new_deaths AS NUMERIC))/MAX(population)*100, 2) AS deaths_per_pop,
	ROUND(SUM(CAST(new_vaccinations AS NUMERIC))/MAX(population)*100, 2) AS vacc_per_pop
FROM (SELECT 
	    d.id,
	    ref.name,
	    ref.date,
	    total_deaths,
	    new_deaths,
	    total_vaccinations,
	    new_vaccinations,
	    d.population	  	
	  FROM covid_deaths AS d
	  LEFT JOIN reference_table AS ref
	  ON d.id = ref.id
	  LEFT JOIN covid_vaccinations AS v
	  ON d.id = v.id) AS e
WHERE name NOT LIKE 'International' 
GROUP BY name
ORDER BY deaths_per_pop DESC;


-- Which countries have higher than average cases/deaths/vaccinations
-- 		Look at raw numbers + pct to factor for differences in population magnitude
-- We look at the most recent date in our data for no duplicate country entries and max totals
SELECT 
	*, 
	ROUND((SELECT
		   	AVG(total_cases)
		   FROM covid
		   WHERE date = '2023-02-26'), 2) AS global_avg
FROM covid_cases
WHERE date = '2023-02-26' AND 
	  total_cases > (SELECT 
				AVG(total_cases)
			 FROM covid
			 WHERE date = '2023-02-26')
ORDER BY total_cases;

-- Correlated subquery
-- Which countries have, on average higher new cases per population compared to global average of new cases per day
SELECT 
	name,
	AVG(new_cases) AS average_cases_per_day,
	ROUND((SELECT
			AVG(new_cases)
		   FROM covid_cases), 2) AS average_global_cases,
	(SELECT 
	 	MAX(handwashing_facilities)
	 FROM covid AS c
	 GROUP BY name
	 HAVING cc.name = c.name) AS handwashing_facilities,
	 (SELECT MAX(median_age)
	  FROM covid AS t
	  GROUP BY name
	  HAVING cc.name = t.name) AS median_age
FROM (SELECT 
	    d.id,
	    ref.name,
	    ref.date,
	    total_cases, 
	    new_cases,
	    d.population	  	
	  FROM covid_cases AS d
	  LEFT JOIN reference_table AS ref
	  ON d.id = ref.id) AS cc
GROUP BY name
HAVING AVG(new_cases) > (SELECT 
				AVG(new_cases)
			 FROM covid_cases)
ORDER BY average_cases_per_day DESC;

-- Although we see that US, India, France, Germany, Brazil are in top 5 of cases per day. Maybe this is just due to large 
-- population and not policy mismanagement
-- We control for the differences in magnitude by finding the cases per day as a proportion to the population
-- Now we find that Austria, South Korea, France, Denmark and Portugal are top 5 for highest cases per population
SELECT 
	name,
	AVG(new_cases) AS average_cases_per_day,
	ROUND((SELECT
			AVG(new_cases)
	       FROM covid_cases), 2) AS average_global_cases,
	ROUND(MAX(CAST(total_cases AS NUMERIC))/MAX(CAST(population AS NUMERIC))*100, 2) AS case_per_pop,
	MAX(total_cases) AS total_cases,
	MAX(population) AS Population
FROM (SELECT 
	    d.id,
	    ref.name,
	    ref.date,
	    total_cases, 
	    new_cases,
	    d.population	  	
	  FROM covid_cases AS d
	  LEFT JOIN reference_table AS ref
	  ON d.id = ref.id) AS cc
GROUP BY name
HAVING AVG(new_cases) > (SELECT 
				 AVG(new_cases)
			 FROM covid_cases)
ORDER BY case_per_pop DESC;

-- Explore United States
-- On average, what is the probability that someone from the United States would contract COVID-19 on a given day?
-- Of course there are factors to consider that would increase/decrease this but all else equal, 0.027% would be the probability
-- that someone random in the United States would catch a COVID case.
-- In Canada, the probability is lower (0.011%)
-- Although the US has a population that is nearly 9x larger than Canada, the probability of infection is only 2x
SELECT
	name,
	MAX(population) AS population,
	sum(new_cases)/count(new_cases) AS average_new_cases,
	MAX(new_cases) AS max_new_cases,
	MAX(total_cases) AS total_cases,
	AVG(new_cases)/MAX(population)*100 AS new_cases_prop_of_pop
FROM covid_cases
GROUP BY name
HAVING name IN ('United States', 'Canada')

-- Which countries have higher than average vaccination and death per population
WITH vc AS 
(
	SELECT
		total_vaccinations,
		total_deaths,
		new_vaccinations,
		new_deaths,
		ref.name
	FROM covid_vaccinations AS v
	LEFT JOIN reference_table AS ref
	ON v.id = ref.id
	LEFT JOIN covid_deaths AS d
	ON d.id = v.id
)
			   
SELECT 
	name,
	ROUND(MAX(total_vaccinations), 2) AS total_vaccinations,
	ROUND(MAX(total_deaths), 2) AS total_deaths,
	ROUND(AVG(new_vaccinations), 2) AS avg_daily_vacc,
	ROUND(AVG(new_deaths), 2) AS avg_daily_deaths,
	ROUND((SELECT AVG(new_vaccinations) FROM vc), 2 ) AS global_avg_daily_vaccinations,
	ROUND((SELECT AVG(new_deaths) FROM vc), 2) AS global_avg_daily_deaths
FROM vc
GROUP BY name
HAVING AVG(new_vaccinations) > (SELECT AVG(new_vaccinations) FROM vc) AND
		AVG(new_deaths) > (SELECT AVG(new_deaths) FROM vc) 
ORDER BY avg_daily_deaths DESC

-- 25 countries receive more daily vaccinations on average than other countries, 
-- yet have more deaths per day than the global avg

