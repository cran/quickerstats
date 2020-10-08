# quickerstats
An R client for the NASS Quick Stats API

Aaron Anderson, 2019-10-08

## Introduction
The USDA's National Agricultural Statistics Service collects a wide variety of
data on US agriculture. The agency maintains a web tool (Quick Stats) and an API for 
accessing the data. While powerful and flexible, both of the existing tools for 
pulling data can be cumbersome for many users. The goal of quickerstats is to
provide functionality for pulling data that is easier to use. The package does 
this in two ways:

* Provide a search tool. An important parameter of Quick Stats is 
'short_desc' (called Data Item in the web tool), which is a concatenation of six 
other parameters (commodity_desc, class_desc, prodn_practice_desc, 
util_practice_desc, statisticcat_desc, and unit_desc). A typical workflow would 
be to drill down into each of these categories to build the desired short_desc 
string. The problem is that the available options for each successive parameter 
depend on the values that have already been chosen. Therefore, it is easiest to 
simply search the available short_desc strings for what you want. Because there 
are so many short_desc options, quickerstats provides a search function so you 
can find what you want.

* Provide easy to use functions for pulling state and county data. Most users 
will be pulling state or county data. We streamline the workflow by providing 
functionality for only these two options (using FIPS codes). Users can access  
a particular data item for a single state or all states. Additionally, users 
can access a data item for all counties in the country, all counties in a 
state, or a single county.

See: <br/>
https://quickstats.nass.usda.gov/ <br/>
https://quickstats.nass.usda.gov/api

## Installation and setup

```
devtools::install_github('anderaa/quickerstats', force=TRUE, build_vignettes=TRUE, ref="master")
library('quickerstats')
```

You will need an API key from NASS. Go to https://quickstats.nass.usda.gov/api.
I recommend storing your key as an environmental variable. To do so, in your
console type:
```
file.edit("~/.Renviron")
```
And then add a line to this file:
```
NASS_KEY='your_nass_key'
```
Save the file, and restart your R session.
You can now access the variable with:
```
key <- Sys.getenv('NASS_KEY')
```

Full documentation is available with:
```
vignette('quickerstats')
```

## Search for a data item

Search for a data item like this:
```
search_data_items(key=key, search_terms=c('corn', 'harvested'), exclude=c('sweet'))
```
This will give you a list of data items. Each will contain all of the 
search terms and none of the exclude terms. The exclude argument is optional. A
typical workflow would be to start broad, view results, then incrementally
narrow your search using both arguments. Note that the search terms are not case
sensitive - the function handles this automatically.

You can also retrieve options associated with a data item:
```
get_options(key=key, data_item='CORN, GRAIN - ACRES HARVESTED')
```

## Retrieve data
Once you have found your desired data item, you can pull data in one of five 
ways:

#### 1. Pull state-level data for all states
To pull data for all states, the fips argument must be set to 'all':
```
get_state_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
```

#### 2. Pull state-level data for a single state
To pull data for a single county, the fips argument must be passed a 
2-character string that is the state FIPS code:
```
get_state_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
```

#### 3. Pull county-level data for all counties in the US
As in the state example, set fips to 'all':
```
get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
```

#### 4. Pull county-level data for all counties in a single state
To get data for counties in a state, fips must be the 2-character fips code of 
the desired state:
```
get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
```

#### 5. Pull county-level data for a single county
Simple pass fips a 5-character FIPS code for the county:
```
get_county_data(key=key, year=2017, data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069')
```

## Function documentation
```
?search_data_items
?get_options
?get_county_item_count
?get_county_data
?get_state_item_count
?get_state_data
```
