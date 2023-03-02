# COVID-19 Dashboard
Mapping global COVID-19 timeseries data through an online interactive dashboard.


VIEW HERE: 
https://rguillermo.shinyapps.io/COVID19_Dashboard/

The visualization chosen is a chloropeth map that reflects new cases globally. The chloropeth map was chosen as it helps to more easily visualize the distribution of global cases with respect to each country. Alternatively, another map that could be chosen (the option to change maps will be coming later...) that shows the proportion of cases per population (Either total cases or new cases). 

The data for this project was primarily sourced from https://ourworldindata.org/coronavirus 

The data for Western Sahara was sourced from Reuters.com but the data only reflects cases in the territories controlled by the Moroccan government and not the entire area because of their on-going border dispute.

COVID-19 case reports for Svalbard are unreliable and government reports are irregular.
https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Svalbard
https://www.sysselmesteren.no/en/news/2022/01/status-covid-19-on-svalbard-week-1/

The mapping package used for the spatial analytics was provided by leaflet using CARTO's Dark Matter theme. 

The spatial polygons that defined the country borders were provided by Bjorn Sandvik, themapticmapping.com.
Some of the defined spatial boreders are highly contested and some of the borders have even changed since the spatial polygons were initially created. Additionally, because some border disputes are still on-going and are unstable, the borders provided in this map may not accurately reflect the evolving geo-political landscape.


Various graphs are provided in the bottom-right of the dashbaord and are collapsible. The graphs, tables, and indicators are continuously updated to reflect the user selection for a specific time-period by the date range slider on the bottom-left. The map can be animated via the play button beside the slider and will update the chloropeth map and corresponding indicators and graphs to show how COVID cases have globally evolved through time.
