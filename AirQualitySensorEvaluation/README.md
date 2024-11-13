# How to use the AirQualitySensorEvaluation Toolkit

1. This toolkit is based on multiple R-Scripts. Before working with the code, please set the working directory to "AirQualitySensorEvaluation."
2. The "main.R" runs all necessary scripts in the subfolders to access all needed functions.
   2.1 match_parameters() is a function to get a list of the intersection of column names of two data sets.
   2.2 lookup_table() is a function that provides the labeling for the analysis plots depending on whether the gas data is in ppb ("ppb") or µg m-3 ("microgramm"). By default, the *main.R* script selects the "ppb" case.
4. To evaluate a sensor kit (device), one can utilize "template.R" in "test\2 Indoor Air Pollution Observation\AirQualitySensorEvaluation\src\Eval_functions\Device_Evaluation\template"

The evaluation is following Zimmerman et al. (2022; https://doi.org/10.1016/j.jaerosci.2021.105872.)

For external data of the German Weather Service, one can use the rdwd package (https://bookdown.org/brry/rdwd/).

**I'd like to let you know that this repository only works with the given data structure, and it is recommended that you use the utilized column names of the data frames produced by the getting-data functions of this repository. Otherwise, the labeling of the created plots might not work.**  

# Folder structure
## src
The Folder "AirQualitySensorEvalaution" contains the subfolder "src" with all the evaluation, data-getting, and utility functions needed to estimate the performance of an integrated sensor kit.

### Eval_functions
The subfolder "Eval_functions" of "src" contains the scripts to determine the accuracy ("accuracy.R"), conduct correlations ("correlation.R"), estimate the single device performance ("performance.R"), and determine the precision ("precision.R") of a device type.

#### accuracy.R
This script describes the accuracy of a device type measuring a specific air quality parameter against a chosen reference. The reference and device data must have the same column names for the air quality parameters. In "main.R" the function "lookup_table()" has to be adjusted accordingly. The data-getting functions load data sampled within the EDIAQI project with all the needed column names. The function evaluates the data within the period starting with **start** and **end**, for a given averaging period (**avg.time**, default="5 min") for a specific **parameter**. Plotting can be en-/disabled (plot=TRUE/FALSE), and names for the reference (**reference_device**) and test device (**devicetype**) can be given. Important: the accuracy is estimated for data sets containing at least **three** devices (data must contain a column named **device_id**).

Accuracy is defined as: 
```math
nRMSE=\frac{\sqrt{\frac{1}{N\cdot M}\sum_{i=1}^M{\left[\sum_{j=1}^N{({x_{i,j}-R_j)}^2}\right]}}}{\overline{R}}\cdot 100\%   
```
Where $x_{i,j}$ is the measurement $j$ (out of $N$ valid parallel measurements of at least three test devices) of test device $i$ (out of $M$ devices) and $R$ the respective measured value of the reference and $\overline{R}$ is mean of the reference over the whole period.

#### performance.R
The script contains functions to estimate the linear behavior of devices, summarise the linear behavior for each parameter, and estimate breakpoints in the linear models. The parameters are $R^2$, the coefficient of determination of the linear model, $r$, the Pearson-correlation coefficient, $a$ the slope of the linear fit, $b$ the intercept of the linear model with the y-axis, the $RMSE$ (root mean square error), and it's normalization with mean of the data set (nRMSE).

Details: Comments within the script.

#### correlation.R
The script contains functions to plot linear behavior and time series of devices against a reference, plot correlations coloring the data points concerning temperature, RH, and absolute humidity, and correlate the data along a value from the reference device. The linear fit in the correlation plots is estimated with the performance() function. 

Details: Comments within the script.

#### precision.R
The script contains the function precision() to calculate the precision of a device type from a given data set (**data**) for at least three different units within a given period (**start**, **end**) for a specified average period (**avg.time**) and an air-quality parameter (**parameter**) of interest.
The $CV$ is calculated with the following:
```math
CV = \frac{\sqrt{\frac{1 }{N \cdot M - 1} \cdot \sum_{i=1}^M\sum_{j=1}^N{({x_{i,j}-\overline{x_j})}^2}}}{\overline{x}} \cdot 100\%
```
Where $x_{i,j}$ is the measurement $j$ (out of $N$ valid parallel measurements of at least three test devices) of test device $i$ (out of $M$ devices) and $\overline{x_j}$ is the respective mean value of the three devices at measurement point $j$ measured, and $\overline{x}$ is mean of the measured parameter over the whole period of all units.

The precision is also displayed visually with a time series of the period of interest in plt\\*devicetype*\\Precision\\*parameter*_*start*_*end*_*avg.time*.png. Colors represent the different device units; the shaded area marks the range $\pm$, the precision around the mean of the devices displayed in black.

#### RH_test.R
The script contains a function to plot the behavior of the *RH* measured by the device and the bias (test device - reference) of a parameter of interest.

#### Device_Evaluation
The devices' evaluation for different experiments is given per sensor partner. The Palas AQ-Guard is checked for different periods against a reference.

**Important:* To use sink() in the scripts, the folder has to be created beforehand. Otherwise, functions throw an error. Also, if an error occurs before sink(file=NULL) (closes the file) is run, the code will write everything in the sink file. sink(file=NULL) is important.

Details: Comments within the script.

### getDataFunctions
getData.R is a script containing all functions needed to get the data records of the sensors used within this project, starting with the sensors partners or devices utilized by TROPOS.

*getLSData()* is a function to get (and return) the data of the LS-PID device from LabService Analytica with two possible data sources (a; the archive format **datasource**=*archive*, and b) the download format **datasource**=*device*). If **plot**=*TRUE*, the function provides a quick-look-plot for the data for the selected period (**start**, **end**). The path must be given to the main folder (here "\\data\\LS_PID\\" see repo). The device_id column in the data frame indicates the different units of the device. The function averages the data to a desired averaging period (**avg.time**).

Details: Comments within the script.

*getWingsData()* is a function to get (and return) the data within a specific period (between **start** and **end**; note: **end** must be one day ahead of the last day of the period of interest) of the airWings devices from the cloud. Available to get indoor **type**=*i* and outdoor **type**=*o* data. The API key **api_key** must be provided. If **plt**=*TRUE*, the function quickly looks for the data for the selected period.

Details: Comments within the script.

*getUBAdata()* is a function that provides air-quality parameter data from the German Environment Agency (UBA) via API access for a selected period (**start**, **end**) for a selected **station** (encoded in the station code), a scope, component  ("1" ~ "PM10", "2" ~ "CO", "3" ~ "O3", "4" ~ "SO2", "5" ~ "NO2"). Data is provided in CET. 

*all_data_uba_station()* is a function that gets the data of components 1 to 5 of a specific station (see above). It requires the getUBAdata() function. Adds a column to identify the UBA station.

Further information is given at: https://www.umweltbundesamt.de/sites/default/files/medien/358/dokumente/schnittstellenbeschreibung_luftdaten_api_v3.pdf.

Station codes on https://www.umweltbundesamt.de/daten/luft/luftdaten/stationen/eJzrXpScv9BwUXEykEhJXGVkYGSia2Cka2C-qCRzkaHxorzUBYuKSxYsSUl0K4LKGuoaGwL5IfnIqpMTJyzKrWJblJvctDgnseS0g-eqea8a5Y4vzslLP-2gcs7F4ZPFbAALoSvB

**Important: If data is used, please refer to it via: „Umweltbundesamt mit Daten der Messnetze der Länder und des Bundes“**

Details: Comments within the script.

*getChamberData()* is an analog function to the others but returns data on meteorological conditions within a given period (**start**, **end**) within the climate chamber. Quicklook is provided. Data is generated by listing all files in the folder of the given path. Here **.csv files.

Details: Comments within the script.

*getPalasData()* and *getPalasData_VOCmg()* are functions to get (and return) data from ASCII files of the Palas AQ-Guard device (assumed reference). Both functions are the same except **_VOCmg()** provides data of VOC in mg m $^{-3}$.

Details: Comments within the script.

*getRHTeraTermData()* is a function that converts RH and temperature data stored in ASCII files recorded by an HYT939 and an M5Stack into a data frame. Only works for a given data set data is stored in *data\\Vilnius\\Indoor\\RH_T_Indoor_Vilnius\\* with ending **.log

Header: date  seconds of day  number of satellites (99 is dummy)  time [GPS UTC]  seconds runtime RH_mean T_mean  RH_median   T_median    number of observations

Example: [Fri Mar 15 15:45:18.944 2024] 53101.312	99	2024-03-15 14:45:00	53101.3	24.79	28.19	24.79	28.19	10.000000

Details: Comments within the script.

*getThinnectData()* converts ASCII data of the Thinnect sensors recordings (stored in Excel spreadsheets) to a data frame averaged to a desired average time (*avg.time*) for a certain period (**start**, **end**). It does not apply to the CSV data files. Please look at eval_Thinnect.R for details. 
Plot of period (**plot**=TRUE/FALSE) can be provided.

Details: Comments within the script.

## **Data**
Contains data collected from the devices of interest.

### AQ_Guard 
This folder contains all data collected during the measurements at TROPOS recorded by the assumed reference for indoor air-quality measurements, PALAS AQ-Guard.

### LS_PID
This folder contains a subfolder, "Download," which includes data directly downloaded from the device via a browser-based web interface. "Archive" contains data downloaded using the web-based application provided by LS-Analytica (https://my.odorsens.cloud/).

### ChemGases
This folder contains data from TROPOS chemical sensors (**Caution: Only ozone data is calibrated**). The data recorded is stored in "\\data\\ChemGases" within this repository.

### climateChamber
This folder contains CSV files with information on the meteorological conditions within the climate chamber during the experiments, which can be used to infer the devices' RH and temperature dependency.

### Vilnius/Indoor/RH_T_Indoor_Vilnius
RH and temperature data were logged with TeraTerm (timestamp of record added). The data was recorded with an HYT-939 sensor and an M5-Stack and transmitted via serial port to the TeraTerm logger on the PC.

### Thinnect
There are different data available. The 1st and 2nd office periods in May 2023 and June 2023 (1st_office_period, 2nd_office_period) are ASCII (CSV) files. Data from laboratory tests are located in the Excel spreadsheets. 

### result.csv and result_zagreb.csv
Files summarize the statistical parameters (linear model parameters of the experiments conducted).

## References 
Naomi Zimmerman,
Tutorial: Guidelines for implementing low-cost sensor networks for aerosol monitoring, Journal of Aerosol Science, Volume 159, 2022, 105872, ISSN 0021-8502, https://doi.org/10.1016/j.jaerosci.2021.105872.




