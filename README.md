# swotR

<img src="man/figures/swotR_logo.png"
     align="right"
     height="180"
     alt="swotR logo" />

<!-- badges: start -->

<!-- badges: end -->

### R package for downloading and processing SWOT river data

This package allows to download and spatially filter reaches and nodes from the SWORD river netword data ([Altenau et al. 2021](https://doi.org/10.1029/2021WR030054)). Then, it allows to use the reach or node ids from the network to download and filter [SWOT riverSP data](https://podaac.jpl.nasa.gov/dataset/SWOT_L2_HR_RiverSP_2.0) on water surface elevation and other information provided by the SWOT riverSP product. The access is done via the NASA's [Hydrocron API](https://podaac.github.io/hydrocron/).


### Installation
Currently, you can only install the swotR package from github.
```{r setup}
library(remotes)
remotes::install_github("river-sensing-lab/swotR")

```

### Overview of the package functionality
The package supports three major steps for working with SWOT riverSP data. Vignettes explaining the individual steps in full detail will be available soon.

1) Downloading, filtering and preprocessing SWORD river network data
2) Downloading. filtering and aggregating SWOT riverSP data
3) Analyzing SWOT data availability on the reach or node level and making visualization of river profiles and spatial-temporal development of variables

### Quick use guide
This example shows how to download the SWORD river network for the Rhone river basin in France, filtering it to a specific river and then download and process SWOT water surface elevation data for the reaches of this river (The Drome).

First, we will install and load the swotR package.

```{r setup}
library(remotes)
remotes::install_github("river-sensing-lab/swotR")

library(swotR)

```

Next, we will download the SWORD river network data and spatially filter it to the Rhone basin. The basin polygon used for spatial filtering is coming from the [HydroBasins database](https://www.hydrosheds.org/products/hydrobasins) and we are using level 4 for filtering. The polygon is also part of the sample data of the package. Note, that the data of the SWORD database are organized in individual files per continent, so we need to specify. The aoi parameter will clip the full network of the continent to the aoi only, in this case to the Rhone basin. 


```{r setup}
#Load the packages needed to work with the data
library(sf)
library(dplyr)

#Load the Rhone basin polygon
data(aoi_rhone)

#Download the SWORD reaches
rhone<-sword_download(continent="EU", category="reaches", aoi=rhone_basin)

#Visualize the network to show what we have downloaded
rhone |> select(reach_id) |> plot()

```
 After having downloaded the river network, we can create some summary statistics of the networks. In addition, we can get check whether river names are present in the network we could use for filtering the network for further analysis. 

```{r setup}
#Compute the summary of the Rhone network
sword_summary(rhone)

#Summarize by river name
sword_summary(rhone, by="river_name")

#Get information of river names stored in the data
#Default checking column "river_name"
sword_names(rhone)

#Using the local river names
sword_names(rhone, name_col="river_name_local")


```
Knowing the names, we can filter the network for individual rivers, let's say the Drome river. In case, the package has also the function sword_topology() to compute hack stream order and ids for each individual branch of the network to filter also without having names supplied in the dataset.
```{r setup}
drome<-filter(rhone, river_name_local=="La Drôme")

#Plot the result
drome |> select(reach_id) |> plot()

```
Now we have the reaches for an individual river and can start downloading the actual SWOT data. Download is supported for reaches and nodes, but the input network needs to match the type. If you supply reaches, you also need to specify "Reaches as" as type. The quality argument allows you to directly apply a quality filter for the SWOT data, but we can do this also in a next step. 
```{r setup}
#Downloading SWOT data for 2025 for the Drome with no quality filter applied. 
swot_drome<-swot_download(network=drome, type="Reach", start_time="2025-01-01",end_time="2025-12-31", quality="all")

```
After downloading the data, we can use the swot_availability function to check what is in the dataset. This will return a tibble with reach or node level information on frequency of observations, fraction of usuable and unusable data and first as well as last information. 
```{r setup}
#Run the availability function 
swot_availability(swot_drome)

```
Now, we can filter and aggregate the data for further analysis using swot_ts. This function allows to select individual parameters from the riverSP dataset, apply quality filtering using the provided quality flags and finally aggregate to monthly or yearly time series
```{r setup}
#Filter to usable SWOT observations only and return water surface elevation and river width
drome_ts<-swot_ts(swot_drome,variables=c("wse","width"), quality="usable")

#Returning the same dataset in long format, e.g. for plotting purpose
drome_ts<-swot_ts(swot_drome,variables=c("wse","width"), quality="usable",format="long")

#And now aggregate the time series to monthly median values
drome_ts_agg<-swot_ts(swot_drome,variables=c("wse","width"), quality="usable",aggregate="monthly",agg_fun="median",format="wide")

```
Finally, we can use the SWOT data to visualize the data as longitudinal plots or as spacetime plots. In the example, we make an examplary plot of the width profile along the Drome around the 20th of May as a randomly selected date. and look in the variation of water surface elevation trough time for 2025. For the reach scale data, as used here, this gives the reach centroides for now, no true reach length plot. This is something to be updated in future releases. 
```{r setup}
#Make longitudinal plot for river width around May 20
swot_profile(swot_drome,sword=drome,time="2025-05-20",quality="usable",plot_variable = "width",plot=TRUE)

#Make spacetime plot of the river width for 2025 and for (approximately) monthly temporal bins
swot_spacetime(swot_drome,drome,variable = "width",bin_days = 30,quality="usable",tile_width = 4,plot=TRUE)

```
