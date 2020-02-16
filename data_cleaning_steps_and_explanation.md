# Data Cleaning Steps
## The Conundrum
While the Motor Vehicle Collisions – Crashes" dataset on NYC Open Data contains every motor vehicle collision within the City of New York, there is a singular problem with this dataset that prevents us from using it for the purpose of creating a supervised, binary classification model: it only contains positive observations (i.e. motor vehicle crashes). Therefore, to make this dataset usable for the intended supervision problem as stated, negative observations must be created to complement the dataset. In addition, a binary response variable would be created afterwards to identify the positive and negative observations.

However, the creation of negative observations itself is not a simple problem to solve. for our motor vehicle collision dataset. Each row of the dataset represents a specific motor vehicle collilsion at a particular point in time, as represented by the `CRASH DATE` and `CRASH TIME` columns, and a particular point in space, as represented by the `LATITUDE` and `LONGITUDE` columns. For every motor vehicle collision that has been captured by the dataset, there are potentially thousands, or even hundreds of thousands of collisions that did not happen at any given point in time or space. To put into perspective, let us look at the statistics to show a rough estimate that the likelihood of an average driver being a participant of a car crash. For example, of the 3.8 million commuters in New York City on a regular workday, approximately 27% of the commuters do so by car, truck, or van (https://edc.nyc/article/new-yorkers-and-their-cars). Assuming that half the commuters carpool (https://www.citylab.com/transportation/2019/01/commuting-to-work-data-car-public-transit-bike/580507/), and the rest drive solo, this would mean, at the very least, there are a bit more than half a million vehicles on the road on any given workday.  Out of the half a million or so vehicles on New York City roads, there are only about 678 car crashes in New York each day (https://www.dandalaw.com/are-car-accidents-common-in-new-york-city/, but ideally we should calculate this from our dataset!!!). All these numbers point out that getting into a motor vehicle collision, even in a city with an unsafe road reputation like New York, is an unlikely event. 

Consequently, to generate negative observations would yield a hugely imblanaced dataset, consisting mostly of negative observations, and very few positive observations. In addition, these negative observations would also have to have generated features, such as `CRASH DATE`, `CRASH TIME`, `LATITUDE`, `LOGITUDE`, etc. This itself is also another issue, as we cannot randomly generate those features without understanding the distribution of New York City traffic across different areas and times, since some boroughs of New York, such as Staten Island, will have less traffic, and therefore less motor vehicle accidents than other boroughs (we should show graphs of the number of accidents per borough).

##The proposed solution
The easiest solution to the problem of generating negative observations is to: (a) group the positive observations by pre-determined datetime ranges, and pre-determined geolocation areas, (b) aggregate the number motor vehicle collisions into a column containing the counts of motor vehicle crashes given a pre-determined datetime range, and pre-determined geolocation area, and (c) via inference, generate negative observations from the grouped, and aggregated positive observations. By generalizing both the time and space of motor vehicle collisions, the generation of negative observations will not likely create a highly imbalnaced dataset that heavily skewers towards the negative class, but also there is now no need to generate those features whilst having to research the New York City traffic flow across different space-time groups. Nevertheless, it is still important to determine an appropriate datetime range, and geo-location areas to bin the observations; it must be taken into account these pre-determined datetime ranges, and geo-location areas should not only prevent extreme class imbalance towards either positive or negative class, but also be relevant to our business problem. 

For example, to look at the 

## Creation of the Precinct column
To map the NYPD precinct, we

## Creation of Negative Observations


## 

# Models
## Stochastic Gradient Boosting

## Random Forest

## KNN 
