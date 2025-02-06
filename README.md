# Learning goals

-   Use the `merge()` function to join two datasets.
-   Deal with missings and impute data.
-   Identify relevant observations using `quantile()`.
-   Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2025/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages).

``` r
library(data.table)
library(R.utils)  
```

    ## Loading required package: R.oo

    ## Loading required package: R.methodsS3

    ## R.methodsS3 v1.8.2 (2022-06-13 22:00:14 UTC) successfully loaded. See ?R.methodsS3 for help.

    ## R.oo v1.27.0 (2024-11-01 18:00:02 UTC) successfully loaded. See ?R.oo for help.

    ## 
    ## Attaching package: 'R.oo'

    ## The following object is masked from 'package:R.methodsS3':
    ## 
    ##     throw

    ## The following objects are masked from 'package:methods':
    ## 
    ##     getClasses, getMethods

    ## The following objects are masked from 'package:base':
    ## 
    ##     attach, detach, load, save

    ## R.utils v2.12.3 (2023-11-18 01:00:02 UTC) successfully loaded. See ?R.utils for help.

    ## 
    ## Attaching package: 'R.utils'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

    ## The following objects are masked from 'package:base':
    ## 
    ##     cat, commandArgs, getOption, isOpen, nullfile, parse, use, warnings

``` r
library(dtplyr)
library(tidyr)
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:R.utils':
    ## 
    ##     extract

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

1.  Load the met data from
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>,
    and also the station data. For the latter, you can use the code we
    used during lecture to pre-process the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data
```

``` r
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/met/met_all.gz",
  destfile = "met_all.gz",
  method = "curl",
  timeout = 60
)

met <- data.table::fread("met_all.gz")
```

1.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y = "USAF"
)
```

## Question 1: Representative station for the US

Across all weather stations, what stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
medians <- met[, .(
  temp_50 = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs=.5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs=.5, na.rm = TRUE)
)]
medians
```

    ##    temp_50 wind.sp_50 atm.press_50
    ##      <num>      <num>        <num>
    ## 1:    23.5        2.1       1014.1

``` r
station_med <- met[, .(
  temp = quantile(temp, probs= .5, na.rm = TRUE),
  wind.sp = quantile(wind.sp, probs= .5, na.rm = TRUE),
  atm.press = quantile(atm.press, probs= .5, na.rm = TRUE),
  lon = mean(lon, na.rm = TRUE),
  lat = mean(lat, na.rm = TRUE)
), by = .(USAFID, STATE)]
```

``` r
station_med[ , temp_dist := abs(temp-medians$temp_50)] 
meadian_temp_station <- station_med[temp_dist == 0]
meadian_temp_station
```

    ##    USAFID  STATE  temp wind.sp atm.press       lon      lat temp_dist
    ##     <int> <char> <num>   <num>     <num>     <num>    <num>     <num>
    ## 1: 720501     VA  23.5     1.5        NA -76.88300 37.85000         0
    ## 2: 722031     AL  23.5     0.0        NA -86.85800 34.26900         0
    ## 3: 722148     NC  23.5     0.0        NA -80.15100 35.41700         0
    ## 4: 723055     NC  23.5     0.0        NA -80.95687 35.76500         0
    ## 5: 723067     NC  23.5     1.5        NA -77.63127 35.31801         0
    ## 6: 723177     NC  23.5     0.0        NA -80.55300 36.46000         0
    ## 7: 725564     NE  23.5     2.6        NA -96.52000 41.44900         0

``` r
station_med[ , wind.sp_dist := abs(wind.sp-medians$wind.sp_50)] 
meadian_wind.sp_station <- station_med[wind.sp_dist == 0]
meadian_wind.sp_station
```

    ##      USAFID  STATE  temp wind.sp atm.press       lon      lat temp_dist
    ##       <int> <char> <num>   <num>     <num>     <num>    <num>     <num>
    ##   1: 720110     TX  31.0     2.1        NA  -98.6620 30.78400       7.5
    ##   2: 720258     MN  17.0     2.1        NA  -93.3100 46.61900       6.5
    ##   3: 720266     IN  21.0     2.1        NA  -85.8400 41.27500       2.5
    ##   4: 720272     WA  18.0     2.1        NA -122.4162 48.46717       5.5
    ##   5: 720273     TX  28.6     2.1        NA  -95.8630 28.97300       5.1
    ##  ---                                                                   
    ## 339: 726583     MN  21.0     2.1        NA  -94.5070 45.14115       2.5
    ## 340: 726589     MN  20.0     2.1        NA  -93.3670 43.68286       3.5
    ## 341: 726603     MN  20.7     2.1        NA  -93.0330 44.85700       2.8
    ## 342: 726626     WI  16.6     2.1        NA  -89.1110 45.15400       6.9
    ## 343: 726813     ID  22.8     2.1   1011.75 -116.6331 43.64963       0.7
    ##      wind.sp_dist
    ##             <num>
    ##   1:            0
    ##   2:            0
    ##   3:            0
    ##   4:            0
    ##   5:            0
    ##  ---             
    ## 339:            0
    ## 340:            0
    ## 341:            0
    ## 342:            0
    ## 343:            0

``` r
station_med[ , atm.press_dist := abs(atm.press-medians$atm.press)]
meadian_atm.press_station <- station_med[atm.press_dist == 0]
meadian_atm.press_station
```

    ##     USAFID  STATE  temp wind.sp atm.press        lon      lat temp_dist
    ##      <int> <char> <num>   <num>     <num>      <num>    <num>     <num>
    ##  1: 722420     TX  30.0     4.6    1014.1  -94.85900 29.27298       6.5
    ##  2: 723830     CA  23.3     5.1    1014.1 -118.72504 34.74468       0.2
    ##  3: 724885     NV  24.7     2.6    1014.1 -118.71577 39.41700       1.2
    ##  4: 724940     CA  18.9     5.1    1014.1 -122.36669 37.61960       4.6
    ##  5: 725376     MI  22.8     3.1    1014.1  -83.53291 42.23315       0.7
    ##  6: 725975     OR  16.1     2.1    1014.1 -123.36400 42.60000       7.4
    ##  7: 726183     ME  18.9     0.0    1014.1  -70.94800 43.99100       4.6
    ##  8: 726375     MI  21.1     3.1    1014.1  -83.41801 42.66500       2.4
    ##  9: 726579     MN  20.0     3.1    1014.1  -93.47097 44.83199       3.5
    ## 10: 726584     MN  20.0     3.1    1014.1  -93.05600 44.93200       3.5
    ## 11: 726590     SD  20.0     3.1    1014.1  -98.41344 45.44377       3.5
    ##     wind.sp_dist atm.press_dist
    ##            <num>          <num>
    ##  1:          2.5              0
    ##  2:          3.0              0
    ##  3:          0.5              0
    ##  4:          3.0              0
    ##  5:          1.0              0
    ##  6:          0.0              0
    ##  7:          2.1              0
    ##  8:          1.0              0
    ##  9:          1.0              0
    ## 10:          1.0              0
    ## 11:          1.0              0

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
station_med[, temp_50 := quantile(temp, probs= .5, na.rm=TRUE), by = STATE]
station_med[, wind.sp_50 := quantile(wind.sp, probs= .5, na.rm=TRUE), by = STATE]

station_med[, eudist := sqrt(
  (temp-temp_50)^2 + (wind.sp-wind.sp_50)^2
)]


id_station <- station_med[, .SD[which.min(eudist)], by = STATE]

id_station <- merge(
  x= id_station, y = stations,
  by.x = "USAFID", by.y = "USAF",
  all.x = TRUE, all.y = FALSE
)
```

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all ~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
state_mid <- met[, .(
  lon_50 = quantile(lon, probs = 0.5, na.rm = TRUE),
  lat_50 = quantile(lat, probs = 0.5, na.rm = TRUE)
), by = STATE]

mid <- merge(met, state_mid, by = "STATE", all.x = TRUE)
```

``` r
mid[, mid_eudist := sqrt(
  (lon - lon_50)^2 + (lat - lat_50)^2
)]

mid_station <- mid[, .SD[which.min(mid_eudist)], by = STATE]


leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(
    data = mid_station,
    lat = ~lat, lng = ~lon, popup = "geographic mid station",
    opacity = 1, fillOpacity = 1, radius = 400, color = "blue"
  ) %>%
  addCircles(
    data = id_station,
    lat = ~lat, lng = ~lon, popup = "eudist mid station",
    opacity = 1, fillOpacity = 1, radius = 400, color = "magenta"
  )
```

<div class="leaflet html-widget html-fill-item" id="htmlwidget-59f334bd0505059fa11c" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-59f334bd0505059fa11c">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[33.177,35.259,34.257,37.285,39.467,41.384,39.133,28.228,32.564,41.691,43.567,40.477,40.711,38.068,37.578,30.558,41.876,39.173,44.316,43.322,45.097,38.704,33.761,45.699,35.584,48.39,40.893,43.205,40.033,33.463,39.6,42.209,40.28,35.417,42.6,40.217,41.597,34.283,43.767,35.38,31.15,38.427,37.4,44.535,47.104,44.778,39,43.064],[-86.783,-93.093,-111.339,-120.512,-106.15,-72.506,-75.467,-82.15600000000001,-82.985,-93.566,-116.24,-88.916,-86.375,-97.27500000000001,-84.77,-92.099,-71.021,-76.684,-69.797,-84.688,-94.50700000000001,-93.18300000000001,-90.758,-110.448,-79.101,-100.024,-97.997,-71.503,-74.35299999999999,-105.535,-116.01,-75.98,-83.11499999999999,-97.383,-123.364,-76.851,-71.41200000000001,-80.56699999999999,-99.318,-86.246,-97.717,-113.012,-77.517,-72.614,-122.287,-89.667,-80.274,-108.458],400,null,null,{"interactive":true,"className":"","stroke":true,"color":"blue","weight":5,"opacity":1,"fill":true,"fillColor":"blue","fillOpacity":1},"geographic mid station",null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]},{"method":"addCircles","args":[[45.417,45.117,47.10383407671721,38.06665587044535,36.9,37.85,41.384,34.3,47.451,35.178,26.6847728531856,40.19995403837573,41.584,44.523,30.783,31.34989776357828,32.33479327731092,32.51595995423341,34.43254128440367,32.16695044052864,34.201,36.047,35.83100096711799,35.8519908045977,31.17804016245487,38.341,40.033,39.643,39.67400479846449,40.82000304878049,38.249,38.06763106796117,39.78407015032212,39.417,41.90999725274725,41.53299912816042,43.53286585365854,40.63296037820802,42.26697244094488,41.43378082191781,41.11736560509554,42.06100881057269,43.20409010339734,44.04994038461538,43.98898168870804,44.86699595551062,45.44376532399299,47.13328091236495],[-123.817,-95.133,-122.2868340767172,-83.98288529014845,-94.017,-76.883,-72.506,-81.633,-99.151,-86.066,-80.09910987996307,-87.5998161535029,-95.33903983885408,-114.2150489335006,-83.277,-85.66666773162939,-88.74462352941177,-92.04097597254004,-103.0827706422018,-110.883,-118.357007290401,-79.477,-90.646,-97.4140091954023,-99.324,-75.51300000000001,-74.35015621301774,-79.916,-75.60600095969289,-82.51799695121952,-86.95399999999999,-97.27500000000001,-104.5374209019327,-118.7157727272727,-70.729,-71.28299912816043,-72.95,-79.10023773075191,-84.46696850393701,-97.34963561643835,-111.9663656050955,-104.1579889867841,-71.50245420974889,-70.28302980769232,-76.02597253306205,-91.48798382204247,-98.41344220665499,-104.8003277310924],400,null,null,{"interactive":true,"className":"","stroke":true,"color":"magenta","weight":5,"opacity":1,"fill":true,"fillColor":"magenta","fillOpacity":1},"eudist mid station",null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[26.6847728531856,48.39],"lng":[-123.817,-69.797]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

-   low: temp \< 20
-   Mid: temp \>= 20 and temp \< 25
-   High: temp \>= 25

``` r
met[, elev_cat := fifelse(
  elev < 90, "low-elev", "high-elev"
)]
```

Once you are done with that, you can compute the following:

-   Number of entries (records),
-   Number of NA entries,
-   Number of stations,
-   Number of states included, and
-   Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

``` r
summary_table <- met |>
  group_by(STATE, elev_cat) |>
  summarize(temp_mean = mean(temp, na.rm = TRUE)) |>
  mutate(avg_temp_level = case_when(
    temp_mean < 20 ~ "low",
    temp_mean >= 20 & temp_mean < 25 ~ "mid", 
    temp_mean >= 25 ~ "high"
  )) |> 
  pivot_wider(names_from = elev_cat, values_from = temp_mean)
```

    ## `summarise()` has grouped output by 'STATE'. You can override using the
    ## `.groups` argument.

``` r
kable(summary_table, booktabs = TRUE) %>%
  kable_styling(font_size = 10) %>%
  kable_paper("hover", full_width = FALSE)
```

<table class="table lightable-paper lightable-hover" style="font-size: 10px; margin-left: auto; margin-right: auto; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
STATE
</th>
<th style="text-align:left;">
avg_temp_level
</th>
<th style="text-align:right;">
high-elev
</th>
<th style="text-align:right;">
low-elev
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
AL
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
25.92562
</td>
<td style="text-align:right;">
26.90432
</td>
</tr>
<tr>
<td style="text-align:left;">
AR
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
25.71858
</td>
<td style="text-align:right;">
26.87350
</td>
</tr>
<tr>
<td style="text-align:left;">
AZ
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
28.80596
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
CA
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
23.72283
</td>
<td style="text-align:right;">
21.13167
</td>
</tr>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
19.54725
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.81456
</td>
<td style="text-align:right;">
22.50812
</td>
</tr>
<tr>
<td style="text-align:left;">
DE
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
24.58116
</td>
</tr>
<tr>
<td style="text-align:left;">
FL
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
NaN
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
FL
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
27.53747
</td>
</tr>
<tr>
<td style="text-align:left;">
GA
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
26.35009
</td>
<td style="text-align:right;">
26.81120
</td>
</tr>
<tr>
<td style="text-align:left;">
IA
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.27773
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
20.69554
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
IL
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
22.41005
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
IN
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.76562
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
KS
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
24.25538
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
KY
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
23.87157
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
LA
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
27.97857
</td>
<td style="text-align:right;">
27.97381
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
20.37799
</td>
<td style="text-align:right;">
21.74306
</td>
</tr>
<tr>
<td style="text-align:left;">
MD
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
23.47545
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MD
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.19608
</td>
</tr>
<tr>
<td style="text-align:left;">
ME
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
18.32004
</td>
<td style="text-align:right;">
19.26441
</td>
</tr>
<tr>
<td style="text-align:left;">
MI
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
20.19981
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MN
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
19.31893
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MN
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.91976
</td>
</tr>
<tr>
<td style="text-align:left;">
MO
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
23.87039
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MS
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
26.04596
</td>
<td style="text-align:right;">
26.83332
</td>
</tr>
<tr>
<td style="text-align:left;">
MT
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
18.16680
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NC
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
23.51121
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NC
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.41548
</td>
</tr>
<tr>
<td style="text-align:left;">
ND
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
18.37173
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
22.10408
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NH
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
17.98781
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NH
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.88998
</td>
</tr>
<tr>
<td style="text-align:left;">
NJ
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.59745
</td>
<td style="text-align:right;">
23.43003
</td>
</tr>
<tr>
<td style="text-align:left;">
NM
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
24.47771
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NV
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
26.04296
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NY
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
19.31104
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NY
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.98619
</td>
</tr>
<tr>
<td style="text-align:left;">
OH
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.83450
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
OK
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
27.40891
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
OR
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
19.10970
</td>
<td style="text-align:right;">
17.16329
</td>
</tr>
<tr>
<td style="text-align:left;">
PA
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.46292
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
PA
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.00705
</td>
</tr>
<tr>
<td style="text-align:left;">
RI
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.02958
</td>
<td style="text-align:right;">
22.70043
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
25.25343
</td>
<td style="text-align:right;">
26.32267
</td>
</tr>
<tr>
<td style="text-align:left;">
SD
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
20.03650
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
TN
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
24.74959
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
TN
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
27.53806
</td>
</tr>
<tr>
<td style="text-align:left;">
TX
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
29.52913
</td>
<td style="text-align:right;">
29.80697
</td>
</tr>
<tr>
<td style="text-align:left;">
UT
</td>
<td style="text-align:left;">
high
</td>
<td style="text-align:right;">
25.82056
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
VA
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
22.94130
</td>
<td style="text-align:right;">
24.96217
</td>
</tr>
<tr>
<td style="text-align:left;">
VT
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
18.34464
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
VT
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.10825
</td>
</tr>
<tr>
<td style="text-align:left;">
WA
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
19.35326
</td>
<td style="text-align:right;">
18.98941
</td>
</tr>
<tr>
<td style="text-align:left;">
WI
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
18.57907
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
WV
</td>
<td style="text-align:left;">
mid
</td>
<td style="text-align:right;">
21.74214
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
WY
</td>
<td style="text-align:left;">
low
</td>
<td style="text-align:right;">
18.60170
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

-   using your data with the median values per station, examine the
    association between median temperature (y) and median wind speed
    (x). Create a scatterplot of the two variables using ggplot2. Add
    both a linear regression line and a smooth line.

-   fit both a linear model and a spline model (use `gam()` with a cubic
    regression spline on wind speed). Summarize and plot the results
    from the models and interpret which model is the best fit and why.

``` r
station_med_lt <- lazy_dt(station_med)
station_med_lt <- station_med_lt |>
  filter(between(atm.press, 1000, 1020)) |>
  collect()

ggplot(station_med_lt, aes(x = atm.press, y = temp)) +
  geom_point() +
  geom_smooth(method = "lm", col = 'cyan') +
  geom_smooth(method = "gam", col = 'blue')
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
lm_mod <- lm(temp ~ atm.press, data = station_med_lt)
summary(lm_mod)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ atm.press, data = station_med_lt)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.928  -2.390   0.044   2.525   8.323 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1041.37307   65.49707   15.90   <2e-16 ***
    ## atm.press     -1.00374    0.06459  -15.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.33 on 898 degrees of freedom
    ## Multiple R-squared:  0.212,  Adjusted R-squared:  0.2111 
    ## F-statistic: 241.5 on 1 and 898 DF,  p-value: < 2.2e-16

``` r
gam_mod <- gam(temp ~ s(atm.press, bs = "cr", k = 20), data = station_med_lt)
summary(gam_mod)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(atm.press, bs = "cr", k = 20)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  23.4852     0.1051   223.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                edf Ref.df     F p-value    
    ## s(atm.press) 9.861  11.83 31.76  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.293   Deviance explained = 30.1%
    ## GCV = 10.064  Scale est. = 9.9425    n = 900

``` r
plot(gam_mod)
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)
