code
================

``` r
library(coronavirus)
# update_dataset()
```

Note: must restart the R session to have the updates available

``` r
webshot::install_phantomjs()
```

    ## It seems that the version of `phantomjs` installed is greater than or equal to the requested version.To install the requested version or downgrade to another version, use `force = TRUE`.

``` r
head(coronavirus::coronavirus)
```

    ##         date province country     lat      long      type cases   uid iso2 iso3
    ## 1 2020-01-22  Alberta  Canada 53.9333 -116.5765 confirmed     0 12401   CA  CAN
    ## 2 2020-01-23  Alberta  Canada 53.9333 -116.5765 confirmed     0 12401   CA  CAN
    ## 3 2020-01-24  Alberta  Canada 53.9333 -116.5765 confirmed     0 12401   CA  CAN
    ## 4 2020-01-25  Alberta  Canada 53.9333 -116.5765 confirmed     0 12401   CA  CAN
    ## 5 2020-01-26  Alberta  Canada 53.9333 -116.5765 confirmed     0 12401   CA  CAN
    ## 6 2020-01-27  Alberta  Canada 53.9333 -116.5765 confirmed     0 12401   CA  CAN
    ##   code3    combined_key population continent_name continent_code
    ## 1   124 Alberta, Canada    4413146  North America             NA
    ## 2   124 Alberta, Canada    4413146  North America             NA
    ## 3   124 Alberta, Canada    4413146  North America             NA
    ## 4   124 Alberta, Canada    4413146  North America             NA
    ## 5   124 Alberta, Canada    4413146  North America             NA
    ## 6   124 Alberta, Canada    4413146  North America             NA

``` r
names(coronavirus)
```

    ##  [1] "date"           "province"       "country"        "lat"           
    ##  [5] "long"           "type"           "cases"          "uid"           
    ##  [9] "iso2"           "iso3"           "code3"          "combined_key"  
    ## [13] "population"     "continent_name" "continent_code"

``` r
head(coronavirus::covid19_vaccine)
```

    ##   country_region       date doses_admin people_partially_vaccinated
    ## 1         Canada 2020-12-14           5                           0
    ## 2          World 2020-12-14           5                           0
    ## 3         Canada 2020-12-15         723                           0
    ## 4          China 2020-12-15     1500000                           0
    ## 5         Russia 2020-12-15       28500                       28500
    ## 6          World 2020-12-15     1529223                       28500
    ##   people_fully_vaccinated report_date_string uid province_state iso2 iso3 code3
    ## 1                       0         2020-12-14 124           <NA>   CA  CAN   124
    ## 2                       0         2020-12-14  NA           <NA> <NA> <NA>    NA
    ## 3                       0         2020-12-15 124           <NA>   CA  CAN   124
    ## 4                       0         2020-12-15 156           <NA>   CN  CHN   156
    ## 5                       0         2020-12-15 643           <NA>   RU  RUS   643
    ## 6                       0         2020-12-15  NA           <NA> <NA> <NA>    NA
    ##   fips      lat     long combined_key population continent_name continent_code
    ## 1 <NA> 60.00000 -95.0000       Canada   37855702  North America             NA
    ## 2 <NA>       NA       NA         <NA>         NA           <NA>           <NA>
    ## 3 <NA> 60.00000 -95.0000       Canada   37855702  North America             NA
    ## 4 <NA> 35.86170 104.1954        China 1404676330           Asia             AS
    ## 5 <NA> 61.52401 105.3188       Russia  145934460         Europe             EU
    ## 6 <NA>       NA       NA         <NA>         NA           <NA>           <NA>

``` r
names(covid19_vaccine)
```

    ##  [1] "country_region"              "date"                       
    ##  [3] "doses_admin"                 "people_partially_vaccinated"
    ##  [5] "people_fully_vaccinated"     "report_date_string"         
    ##  [7] "uid"                         "province_state"             
    ##  [9] "iso2"                        "iso3"                       
    ## [11] "code3"                       "fips"                       
    ## [13] "lat"                         "long"                       
    ## [15] "combined_key"                "population"                 
    ## [17] "continent_name"              "continent_code"

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
summary_coron <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))

summary_coron %>%
  head(20)
```

    ## # A tibble: 20 × 2
    ##    country        total_cases
    ##    <chr>                <int>
    ##  1 US                84748884
    ##  2 India             43176817
    ##  3 Brazil            31137479
    ##  4 France            29813848
    ##  5 Germany           26493235
    ##  6 United Kingdom    22493328
    ##  7 Korea, South      18163686
    ##  8 Russia            18080277
    ##  9 Italy             17490451
    ## 10 Turkey            15072747
    ## 11 Spain             12403245
    ## 12 Vietnam           10724554
    ## 13 Argentina          9230573
    ## 14 Japan              8918984
    ## 15 Netherlands        8276872
    ## 16 Australia          7425669
    ## 17 Iran               7232678
    ## 18 Colombia           6109105
    ## 19 Indonesia          6056017
    ## 20 Poland             6008550

``` r
library(plotly)
```

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(tidyr)

coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovery) %>%
  mutate(active_total = cumsum(active),
                recovered_total = cumsum(recovery),
                death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
                  y = ~ active_total,
                  name = 'Active', 
                  fillcolor = '#1f77b4',
                  type = 'scatter',
                  mode = 'none', 
                  stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
             name = "Death",
             fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recovered', 
            fillcolor = 'forestgreen') %>%
  layout(title = "Distribution of Covid19 Cases Worldwide",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))
```

    ## `summarise()` has grouped output by 'type'. You can override using the `.groups`
    ## argument.

![](code_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(ggplot2)

plot_1 <- coronavirus %>%
  filter(country == "Brazil", type == "confirmed") %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(color = "blue") +
  ggtitle("Confirmed Cases in Brazil")
plot_1
```

![](code_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot_2 <- coronavirus %>%
  filter(country == "Brazil", type == "death") %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(color = "red")+
  ggtitle("Death Cases in Brazil")
plot_2
```

![](code_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
gridExtra::grid.arrange(plot_1, plot_2, nrow = 2, ncol = 1)
```

![](code_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
conf_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup() 
  
  plot_ly(data = conf_df,
          type= "treemap",
          values = ~total_cases,
          labels= ~ country,
          parents=  ~parents,
          domain = list(column=0),
          name = "Confirmed",
          textinfo="label+value+percent parent")
```

![](code_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
covid19_vaccine %>% 
  filter(date == max(date),
         !is.na(population)) %>% 
  mutate(fully_vaccinated_ratio = people_fully_vaccinated / population) %>%
  arrange(- fully_vaccinated_ratio) %>%
  slice_head(n = 20) %>%
  arrange(fully_vaccinated_ratio) %>%
  mutate(country = factor(country_region, levels = country_region)) %>%
  plot_ly(y = ~ country,
          x = ~ round(100 * fully_vaccinated_ratio, 2),
          text = ~ paste(round(100 * fully_vaccinated_ratio, 1), "%"),
          textposition = 'auto',
          orientation = "h",
          type = "bar") %>%
  layout(title = "Percentage of Fully Vaccineted Population - Top 20 Countries",
         yaxis = list(title = ""),
         xaxis = list(title = "Source: Johns Hopkins Centers for Civic Impact",
                      ticksuffix = "%"))
```

![](code_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
