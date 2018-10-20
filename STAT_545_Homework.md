STAT 545 Homework 5
================
Anita
October 17, 2018

-   [Part 1: Factor management](#part-1-factor-management)
    -   [Let's explore the gapminder dataset, the continent variable](#lets-explore-the-gapminder-dataset-the-continent-variable)
    -   [Drop Oceania](#drop-oceania)

Part 1: Factor management
=========================

With the data set of your choice, after ensuring the variable(s) you’re exploring are indeed factors, you are expected to:

Drop factor / levels; Reorder levels based on knowledge from data. We’ve elaborated on these steps for the gapminder and singer data sets below.

Be sure to also characterize the (derived) data before and after your factor re-leveling:

Explore the effects of arrange(). Does merely arranging the data have any effect on, say, a figure? Explore the effects of reordering a factor and factor reordering coupled with arrange(). Especially, what effect does this have on a figure? These explorations should involve the data, the factor levels, and some figures.

Elaboration for the gapminder data set Drop Oceania. Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

``` r
library(gapminder)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(plotly)
```

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

Let's explore the gapminder dataset, the continent variable
-----------------------------------------------------------

``` r
is.factor(gapminder$continent)
```

    ## [1] TRUE

``` r
head(gapminder)
```

    ## # A tibble: 6 x 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

``` r
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
gapminder%>%
  group_by(continent)%>%
  summarize(num=n())
```

    ## # A tibble: 5 x 2
    ##   continent   num
    ##   <fct>     <int>
    ## 1 Africa      624
    ## 2 Americas    300
    ## 3 Asia        396
    ## 4 Europe      360
    ## 5 Oceania      24

Continent is a factor with five levels, and a total of

``` r
624+300+396+360+24
```

    ## [1] 1704

rows

Drop Oceania
------------

First, we will look at the data with Oceania

``` r
gapminder %>% 
  summarize(
    nrow = nrow(gapminder),
    nlevels_continent = nlevels(gapminder$continent),
    nlevels_country = nlevels(gapminder$country)) %>% 
  knitr::kable(col.names = c("Total rows in gapminder", "Levels of continent", "Levels of country"))
```

|  Total rows in gapminder|  Levels of continent|  Levels of country|
|------------------------:|--------------------:|------------------:|
|                     1704|                    5|                142|

Now, let's see how the rows cgange if Oceania gets dropped.

``` r
gapminder_without_oceania <- gapminder %>%
  filter(continent != "Oceania") 
gapminder_without_oceania %>% 
  summarize(
    nrow = nrow(gapminder_without_oceania),
    nlevels_continent = nlevels(gapminder_without_oceania$continent),
    nlevels_country = nlevels(gapminder_without_oceania$country)) %>% 
  knitr::kable(col.names = c("Total rows in gapminder without Oceania", "Levels of continent without Oceania", "Levels of country without Oceania"))
```

|  Total rows in gapminder without Oceania|  Levels of continent without Oceania|  Levels of country without Oceania|
|----------------------------------------:|------------------------------------:|----------------------------------:|
|                                     1680|                                    5|                                142|

Let's look how many rows each continent has

``` r
gapminder_without_oceania%>%
  group_by(continent)%>%
  summarize(num=n())
```

    ## # A tibble: 4 x 2
    ##   continent   num
    ##   <fct>     <int>
    ## 1 Africa      624
    ## 2 Americas    300
    ## 3 Asia        396
    ## 4 Europe      360

Continent is now a factor with four levels, and a total of

``` r
624+300+396+360
```

    ## [1] 1680

rows

Reorder the levels of country or continent. Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

First let's look at the standard deviation of countries

``` r
library(forcats)

gapminder_original_order <- gapminder %>%
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  mutate(sd_life = sd(lifeExp)) %>% 
  select(country, sd_life) %>% 
  unique() # have to delete rows that repeat or I get an errr message
knitr::kable(gapminder_original_order) 
```

| country                  |   sd\_life|
|:-------------------------|----------:|
| Algeria                  |  10.340069|
| Angola                   |   4.005276|
| Benin                    |   6.128681|
| Botswana                 |   5.929476|
| Burkina Faso             |   6.845792|
| Burundi                  |   3.174882|
| Cameroon                 |   5.467960|
| Central African Republic |   4.720690|
| Chad                     |   4.887978|
| Comoros                  |   8.132353|
| Congo, Dem. Rep.         |   2.869210|
| Congo, Rep.              |   4.878987|
| Cote d'Ivoire            |   4.421421|
| Djibouti                 |   6.710003|
| Egypt                    |  10.062500|
| Equatorial Guinea        |   5.600456|
| Eritrea                  |   6.903925|
| Ethiopia                 |   5.627192|
| Gabon                    |   8.933194|
| Gambia                   |  10.545929|
| Ghana                    |   5.846972|
| Guinea                   |   7.743160|
| Guinea-Bissau            |   4.937368|
| Kenya                    |   5.596199|
| Lesotho                  |   5.914277|
| Liberia                  |   2.419094|
| Libya                    |  11.372181|
| Madagascar               |   7.297844|
| Malawi                   |   4.607323|
| Mali                     |   6.808537|
| Mauritania               |   8.057280|
| Mauritius                |   6.497274|
| Morocco                  |   9.806162|
| Mozambique               |   4.599184|
| Namibia                  |   6.303906|
| Niger                    |   6.509444|
| Nigeria                  |   4.021207|
| Reunion                  |   8.434938|
| Rwanda                   |   6.307415|
| Sao Tome and Principe    |   6.283923|
| Senegal                  |   9.141934|
| Sierra Leone             |   3.937828|
| Somalia                  |   4.503828|
| South Africa             |   5.455502|
| Sudan                    |   6.927843|
| Swaziland                |   6.562668|
| Tanzania                 |   3.602435|
| Togo                     |   7.247043|
| Tunisia                  |  10.701244|
| Uganda                   |   3.747267|
| Zambia                   |   4.453246|
| Zimbabwe                 |   7.071816|

Now, let's rearrange from highest to lowest standard deviation

``` r
gapminder_new_order <- gapminder_original_order %>% 
  arrange(desc(sd_life))

knitr::kable(gapminder_new_order) 
```

| country                  |   sd\_life|
|:-------------------------|----------:|
| Libya                    |  11.372181|
| Tunisia                  |  10.701244|
| Gambia                   |  10.545929|
| Algeria                  |  10.340069|
| Egypt                    |  10.062500|
| Morocco                  |   9.806162|
| Senegal                  |   9.141934|
| Gabon                    |   8.933194|
| Reunion                  |   8.434938|
| Comoros                  |   8.132353|
| Mauritania               |   8.057280|
| Guinea                   |   7.743160|
| Madagascar               |   7.297844|
| Togo                     |   7.247043|
| Zimbabwe                 |   7.071816|
| Sudan                    |   6.927843|
| Eritrea                  |   6.903925|
| Burkina Faso             |   6.845792|
| Mali                     |   6.808537|
| Djibouti                 |   6.710003|
| Swaziland                |   6.562668|
| Niger                    |   6.509444|
| Mauritius                |   6.497274|
| Rwanda                   |   6.307415|
| Namibia                  |   6.303906|
| Sao Tome and Principe    |   6.283923|
| Benin                    |   6.128681|
| Botswana                 |   5.929476|
| Lesotho                  |   5.914277|
| Ghana                    |   5.846972|
| Ethiopia                 |   5.627192|
| Equatorial Guinea        |   5.600456|
| Kenya                    |   5.596199|
| Cameroon                 |   5.467960|
| South Africa             |   5.455502|
| Guinea-Bissau            |   4.937368|
| Chad                     |   4.887978|
| Congo, Rep.              |   4.878987|
| Central African Republic |   4.720690|
| Malawi                   |   4.607323|
| Mozambique               |   4.599184|
| Somalia                  |   4.503828|
| Zambia                   |   4.453246|
| Cote d'Ivoire            |   4.421421|
| Nigeria                  |   4.021207|
| Angola                   |   4.005276|
| Sierra Leone             |   3.937828|
| Uganda                   |   3.747267|
| Tanzania                 |   3.602435|
| Burundi                  |   3.174882|
| Congo, Dem. Rep.         |   2.869210|
| Liberia                  |   2.419094|

Now lets look at a figure. Here we look at GDP per capita per country in 2007.

``` r
gap_2007 <- gapminder %>% 
  filter(year == 2007)
ggplot(gap_2007, aes(gdpPercap, country)) + geom_point()+
  xlab( "GDP per capita") +
  ylab( "Country" ) +
  ggtitle( "GDP per capita by country in 2007 unsorted" ) +
  theme_light()  
```

![](STAT_545_Homework_files/figure-markdown_github/unnamed-chunk-10-1.png)

Unfortunately, the data is unsorted and so it's not so easy to look at it. Now let's arrange it by GDP.

``` r
#Let's use `fct_reorder()` to reorder the countries by gdp per capita, and produce the same plot:
gap_2007 %>%
  mutate(country = fct_reorder(country, gdpPercap)) %>%
ggplot(aes(gdpPercap, country)) + geom_point()+
  xlab( "GDP per capita") +
  ylab( "Country" ) +
  ggtitle( "GDP per capita by country in 2007" ) +
  theme_light()
```

![](STAT_545_Homework_files/figure-markdown_github/unnamed-chunk-11-1.png)

Part 2: File I/O Experiment with one or more of write\_csv()/read\_csv() (and/or TSV friends), saveRDS()/readRDS(), dput()/dget(). Create something new, probably by filtering or grouped-summarization of Singer or Gapminder. I highly recommend you fiddle with the factor levels, i.e. make them non-alphabetical (see previous section). Explore whether this survives the round trip of writing to file then reading back in.

First I export the dataset I created above to a csv file.

``` r
write_csv(gap_2007, "gap_2007.csv") 
```

Now, let's see if this new data file when we try to create the same plot as above is ordered by GDP per capita

``` r
read_csv("gap_2007.csv") %>%  #import .csv
  ggplot(aes(gdpPercap, country)) + geom_point()+
  xlab( "GDP per capita") +
  ylab( "Country" ) +
  ggtitle( "GDP per capita by country in 2007" ) +
  theme_light()
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

![](STAT_545_Homework_files/figure-markdown_github/unnamed-chunk-13-1.png)

As we can see, it isn't.

Part 3: Visualization design Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color. Maybe juxtapose your first attempt and what you obtained after some time spent working on it. Reflect on the differences. If using Gapminder, you can use the country or continent color scheme that ships with Gapminder. Consult the dimensions listed in All the Graph Things.

Then, make a new graph by converting this visual (or another, if you’d like) to a plotly graph. What are some things that plotly makes possible, that are not possible with a regular ggplot2 graph?

Spread of GDP per cap by year by continent

Now let's look at a graph that I made for a previous homework asignment and compare ggplot with plotly

``` r
gdp.2 <-  gapminder %>%
  group_by(continent, year) %>%
  summarize(Std.Deviation = sd(gdpPercap),
            Variance = var(gdpPercap))

ggplot(gdp.2, aes(year)) +
  geom_line(aes(y=Std.Deviation, color=continent)) + 
  scale_size_area()
```

![](STAT_545_Homework_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
gdp_spread <- ggplot(gdp.2, aes(year)) +
  geom_line(aes(y=Std.Deviation, color=continent)) + 
  scale_size_area() 

ggplotly(gdp_spread)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-73e38adba96d7e8d05e6">{"x":{"data":[{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[982.952115538417,1134.5089183774,1461.83918875565,2847.71760343509,3286.8538840091,4142.39870672529,3242.6327527397,2566.53194679582,2644.07560232197,2820.728117107,2972.65130767007,3618.16349144035],"text":["Std.Deviation:   982.95212<br />continent: Africa<br />year: 1952","Std.Deviation:  1134.50892<br />continent: Africa<br />year: 1957","Std.Deviation:  1461.83919<br />continent: Africa<br />year: 1962","Std.Deviation:  2847.71760<br />continent: Africa<br />year: 1967","Std.Deviation:  3286.85388<br />continent: Africa<br />year: 1972","Std.Deviation:  4142.39871<br />continent: Africa<br />year: 1977","Std.Deviation:  3242.63275<br />continent: Africa<br />year: 1982","Std.Deviation:  2566.53195<br />continent: Africa<br />year: 1987","Std.Deviation:  2644.07560<br />continent: Africa<br />year: 1992","Std.Deviation:  2820.72812<br />continent: Africa<br />year: 1997","Std.Deviation:  2972.65131<br />continent: Africa<br />year: 2002","Std.Deviation:  3618.16349<br />continent: Africa<br />year: 2007"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3001.72752163266,3312.38108292101,3421.74056877156,4160.8855595904,4754.40432881002,5355.60251825134,5530.49047115146,6665.03950872974,7047.08919137503,7874.22514465829,8895.81778533521,9713.20930239591],"text":["Std.Deviation:  3001.72752<br />continent: Americas<br />year: 1952","Std.Deviation:  3312.38108<br />continent: Americas<br />year: 1957","Std.Deviation:  3421.74057<br />continent: Americas<br />year: 1962","Std.Deviation:  4160.88556<br />continent: Americas<br />year: 1967","Std.Deviation:  4754.40433<br />continent: Americas<br />year: 1972","Std.Deviation:  5355.60252<br />continent: Americas<br />year: 1977","Std.Deviation:  5530.49047<br />continent: Americas<br />year: 1982","Std.Deviation:  6665.03951<br />continent: Americas<br />year: 1987","Std.Deviation:  7047.08919<br />continent: Americas<br />year: 1992","Std.Deviation:  7874.22514<br />continent: Americas<br />year: 1997","Std.Deviation:  8895.81779<br />continent: Americas<br />year: 2002","Std.Deviation:  9713.20930<br />continent: Americas<br />year: 2007"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(163,165,0,1)","dash":"solid"},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[18634.8908654504,19506.5159589662,16415.857196395,14062.591362492,19087.5029184416,11815.777923037,8701.17649917184,8090.26276537004,9727.43108799162,11094.180481033,11150.7192027005,14154.9373428805],"text":["Std.Deviation: 18634.89087<br />continent: Asia<br />year: 1952","Std.Deviation: 19506.51596<br />continent: Asia<br />year: 1957","Std.Deviation: 16415.85720<br />continent: Asia<br />year: 1962","Std.Deviation: 14062.59136<br />continent: Asia<br />year: 1967","Std.Deviation: 19087.50292<br />continent: Asia<br />year: 1972","Std.Deviation: 11815.77792<br />continent: Asia<br />year: 1977","Std.Deviation:  8701.17650<br />continent: Asia<br />year: 1982","Std.Deviation:  8090.26277<br />continent: Asia<br />year: 1987","Std.Deviation:  9727.43109<br />continent: Asia<br />year: 1992","Std.Deviation: 11094.18048<br />continent: Asia<br />year: 1997","Std.Deviation: 11150.71920<br />continent: Asia<br />year: 2002","Std.Deviation: 14154.93734<br />continent: Asia<br />year: 2007"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,191,125,1)","dash":"solid"},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3114.06049265532,3677.95014597648,4199.19390641838,4724.98388865093,5509.69141095736,5874.46489596902,6453.23482661465,7482.95796003972,9109.80436105703,10065.4577161932,11197.3555167025,11800.3398108881],"text":["Std.Deviation:  3114.06049<br />continent: Europe<br />year: 1952","Std.Deviation:  3677.95015<br />continent: Europe<br />year: 1957","Std.Deviation:  4199.19391<br />continent: Europe<br />year: 1962","Std.Deviation:  4724.98389<br />continent: Europe<br />year: 1967","Std.Deviation:  5509.69141<br />continent: Europe<br />year: 1972","Std.Deviation:  5874.46490<br />continent: Europe<br />year: 1977","Std.Deviation:  6453.23483<br />continent: Europe<br />year: 1982","Std.Deviation:  7482.95796<br />continent: Europe<br />year: 1987","Std.Deviation:  9109.80436<br />continent: Europe<br />year: 1992","Std.Deviation: 10065.45772<br />continent: Europe<br />year: 1997","Std.Deviation: 11197.35552<br />continent: Europe<br />year: 2002","Std.Deviation: 11800.33981<br />continent: Europe<br />year: 2007"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,176,246,1)","dash":"solid"},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[365.560077879958,917.644805938885,677.727300529976,43.9860864405916,525.091980276236,1485.26351739643,1304.32837661711,2037.66801328395,3578.97988300066,4205.5337031416,5301.85368001859,6540.99110354847],"text":["Std.Deviation:   365.56008<br />continent: Oceania<br />year: 1952","Std.Deviation:   917.64481<br />continent: Oceania<br />year: 1957","Std.Deviation:   677.72730<br />continent: Oceania<br />year: 1962","Std.Deviation:    43.98609<br />continent: Oceania<br />year: 1967","Std.Deviation:   525.09198<br />continent: Oceania<br />year: 1972","Std.Deviation:  1485.26352<br />continent: Oceania<br />year: 1977","Std.Deviation:  1304.32838<br />continent: Oceania<br />year: 1982","Std.Deviation:  2037.66801<br />continent: Oceania<br />year: 1987","Std.Deviation:  3578.97988<br />continent: Oceania<br />year: 1992","Std.Deviation:  4205.53370<br />continent: Oceania<br />year: 1997","Std.Deviation:  5301.85368<br />continent: Oceania<br />year: 2002","Std.Deviation:  6540.99110<br />continent: Oceania<br />year: 2007"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(231,107,243,1)","dash":"solid"},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":54.7945205479452},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1949.25,2009.75],"tickmode":"array","ticktext":["1950","1960","1970","1980","1990","2000"],"tickvals":[1950,1960,1970,1980,1990,2000],"categoryorder":"array","categoryarray":["1950","1960","1970","1980","1990","2000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"year","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-929.140407185686,20479.6424525924],"tickmode":"array","ticktext":["0","5000","10000","15000","20000"],"tickvals":[0,5000,10000,15000,20000],"categoryorder":"array","categoryarray":["0","5000","10000","15000","20000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"Std.Deviation","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"continent","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"aea4c8a7362":{"y":{},"colour":{},"x":{},"type":"scatter"}},"cur_data":"aea4c8a7362","visdat":{"aea4c8a7362":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>
<!--/html_preserve-->
We can alos look at other functions that can make use of ggplot (such as visreg) and see if they can likewise be converted into plotly.

In this example, I'm looking at an interaction between population and GDP per capita in predicting life expectancy for the year 2007. (Note: This makes little sense theoretically and as we can see the interaction term is not significant, but it serves to illustrate the possibilities of plotly.)

``` r
gap_2007 <- gapminder %>% 
  filter(year == 2007)

m1 <- lm(lifeExp ~ gdpPercap*pop, data=gap_2007)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = lifeExp ~ gdpPercap * pop, data = gap_2007)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -22.466  -5.910   1.877   6.942  13.393 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.910e+01  1.056e+00  55.947   <2e-16 ***
    ## gdpPercap      6.575e-04  6.393e-05  10.284   <2e-16 ***
    ## pop            9.386e-09  6.428e-09   1.460    0.146    
    ## gdpPercap:pop -4.595e-13  7.586e-13  -0.606    0.546    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.89 on 138 degrees of freedom
    ## Multiple R-squared:  0.4693, Adjusted R-squared:  0.4578 
    ## F-statistic: 40.68 on 3 and 138 DF,  p-value: < 2.2e-16

``` r
psych::describe(gap_2007$pop) #extract mean and sd to look at population as the moderator and define three levels (mean and +/- 1sd)
```

    ##    vars   n     mean        sd   median  trimmed      mad    min
    ## X1    1 142 44021220 147621398 10517531 18112754 12212490 199579
    ##           max      range skew kurtosis       se
    ## X1 1318683096 1318483517 7.25    55.42 12388113

``` r
library(visreg)
visreg(m1, "gdpPercap", by="pop", breaks=c(-103600178,44021220,191642618), overlay=TRUE, 
       band=FALSE, ylab="Life Expectancy", xlab="GDP per capita", 
       bty="n", partial=FALSE, rug=FALSE, gg=TRUE)
```

![](STAT_545_Homework_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
gap_interaction <-visreg(m1, "gdpPercap", by="pop", breaks=c(-103600178,44021220,191642618), overlay=TRUE, 
       band=FALSE, ylab="Life Expectancy", xlab="GDP per capita", 
       bty="n", partial=FALSE, rug=FALSE, gg=TRUE) 

ggplotly(gap_interaction)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-08bff18fc34983453110">{"x":{"data":[{"x":[277.5518587,768.348241813,1259.144624926,1749.941008039,2240.737391152,2731.533774265,3222.330157378,3713.126540491,4203.922923604,4694.719306717,5185.51568983,5676.312072943,6167.108456056,6657.904839169,7148.701222282,7639.497605395,8130.293988508,8621.090371621,9111.886754734,9602.683137847,10093.47952096,10584.275904073,11075.072287186,11565.868670299,12056.665053412,12547.461436525,13038.257819638,13529.054202751,14019.850585864,14510.646968977,15001.44335209,15492.239735203,15983.036118316,16473.832501429,16964.628884542,17455.425267655,17946.221650768,18437.018033881,18927.814416994,19418.610800107,19909.40718322,20400.203566333,20890.999949446,21381.796332559,21872.592715672,22363.389098785,22854.185481898,23344.981865011,23835.778248124,24326.574631237,24817.37101435,25308.167397463,25798.963780576,26289.760163689,26780.556546802,27271.352929915,27762.149313028,28252.945696141,28743.742079254,29234.538462367,29725.33484548,30216.131228593,30706.927611706,31197.723994819,31688.520377932,32179.316761045,32670.113144158,33160.909527271,33651.705910384,34142.502293497,34633.29867661,35124.095059723,35614.891442836,36105.687825949,36596.484209062,37087.280592175,37578.076975288,38068.873358401,38559.669741514,39050.466124627,39541.26250774,40032.058890853,40522.855273966,41013.651657079,41504.448040192,41995.244423305,42486.040806418,42976.837189531,43467.633572644,43958.429955757,44449.22633887,44940.022721983,45430.819105096,45921.615488209,46412.411871322,46903.208254435,47394.004637548,47884.801020661,48375.597403774,48866.393786887,49357.19017],"y":[58.3260286737566,58.6720825410488,59.0181364083409,59.3641902756331,59.7102441429253,60.0562980102174,60.4023518775096,60.7484057448017,61.0944596120939,61.4405134793861,61.7865673466782,62.1326212139704,62.4786750812626,62.8247289485547,63.1707828158469,63.516836683139,63.8628905504312,64.2089444177234,64.5549982850155,64.9010521523077,65.2471060195999,65.593159886892,65.9392137541842,66.2852676214764,66.6313214887685,66.9773753560607,67.3234292233529,67.669483090645,68.0155369579372,68.3615908252293,68.7076446925215,69.0536985598137,69.3997524271058,69.745806294398,70.0918601616902,70.4379140289823,70.7839678962745,71.1300217635667,71.4760756308588,71.822129498151,72.1681833654432,72.5142372327353,72.8602911000275,73.2063449673197,73.5523988346118,73.898452701904,74.2445065691961,74.5905604364883,74.9366143037805,75.2826681710726,75.6287220383648,75.9747759056569,76.3208297729491,76.6668836402413,77.0129375075334,77.3589913748256,77.7050452421178,78.0510991094099,78.3971529767021,78.7432068439943,79.0892607112864,79.4353145785786,79.7813684458708,80.1274223131629,80.4734761804551,80.8195300477473,81.1655839150394,81.5116377823316,81.8576916496237,82.2037455169159,82.5497993842081,82.8958532515002,83.2419071187924,83.5879609860846,83.9340148533767,84.2800687206689,84.626122587961,84.9721764552532,85.3182303225454,85.6642841898375,86.0103380571297,86.3563919244219,86.702445791714,87.0484996590062,87.3945535262984,87.7406073935905,88.0866612608827,88.4327151281748,88.778768995467,89.1248228627592,89.4708767300513,89.8169305973435,90.1629844646357,90.5090383319278,90.85509219922,91.2011460665122,91.5471999338043,91.8932538010965,92.2393076683886,92.5853615356808,92.931415402973],"text":["pop: -103600178<br />pop: -103600178<br />x:   277.5519<br />y: 58.32603","pop: -103600178<br />pop: -103600178<br />x:   768.3482<br />y: 58.67208","pop: -103600178<br />pop: -103600178<br />x:  1259.1446<br />y: 59.01814","pop: -103600178<br />pop: -103600178<br />x:  1749.9410<br />y: 59.36419","pop: -103600178<br />pop: -103600178<br />x:  2240.7374<br />y: 59.71024","pop: -103600178<br />pop: -103600178<br />x:  2731.5338<br />y: 60.05630","pop: -103600178<br />pop: -103600178<br />x:  3222.3302<br />y: 60.40235","pop: -103600178<br />pop: -103600178<br />x:  3713.1265<br />y: 60.74841","pop: -103600178<br />pop: -103600178<br />x:  4203.9229<br />y: 61.09446","pop: -103600178<br />pop: -103600178<br />x:  4694.7193<br />y: 61.44051","pop: -103600178<br />pop: -103600178<br />x:  5185.5157<br />y: 61.78657","pop: -103600178<br />pop: -103600178<br />x:  5676.3121<br />y: 62.13262","pop: -103600178<br />pop: -103600178<br />x:  6167.1085<br />y: 62.47868","pop: -103600178<br />pop: -103600178<br />x:  6657.9048<br />y: 62.82473","pop: -103600178<br />pop: -103600178<br />x:  7148.7012<br />y: 63.17078","pop: -103600178<br />pop: -103600178<br />x:  7639.4976<br />y: 63.51684","pop: -103600178<br />pop: -103600178<br />x:  8130.2940<br />y: 63.86289","pop: -103600178<br />pop: -103600178<br />x:  8621.0904<br />y: 64.20894","pop: -103600178<br />pop: -103600178<br />x:  9111.8868<br />y: 64.55500","pop: -103600178<br />pop: -103600178<br />x:  9602.6831<br />y: 64.90105","pop: -103600178<br />pop: -103600178<br />x: 10093.4795<br />y: 65.24711","pop: -103600178<br />pop: -103600178<br />x: 10584.2759<br />y: 65.59316","pop: -103600178<br />pop: -103600178<br />x: 11075.0723<br />y: 65.93921","pop: -103600178<br />pop: -103600178<br />x: 11565.8687<br />y: 66.28527","pop: -103600178<br />pop: -103600178<br />x: 12056.6651<br />y: 66.63132","pop: -103600178<br />pop: -103600178<br />x: 12547.4614<br />y: 66.97738","pop: -103600178<br />pop: -103600178<br />x: 13038.2578<br />y: 67.32343","pop: -103600178<br />pop: -103600178<br />x: 13529.0542<br />y: 67.66948","pop: -103600178<br />pop: -103600178<br />x: 14019.8506<br />y: 68.01554","pop: -103600178<br />pop: -103600178<br />x: 14510.6470<br />y: 68.36159","pop: -103600178<br />pop: -103600178<br />x: 15001.4434<br />y: 68.70764","pop: -103600178<br />pop: -103600178<br />x: 15492.2397<br />y: 69.05370","pop: -103600178<br />pop: -103600178<br />x: 15983.0361<br />y: 69.39975","pop: -103600178<br />pop: -103600178<br />x: 16473.8325<br />y: 69.74581","pop: -103600178<br />pop: -103600178<br />x: 16964.6289<br />y: 70.09186","pop: -103600178<br />pop: -103600178<br />x: 17455.4253<br />y: 70.43791","pop: -103600178<br />pop: -103600178<br />x: 17946.2217<br />y: 70.78397","pop: -103600178<br />pop: -103600178<br />x: 18437.0180<br />y: 71.13002","pop: -103600178<br />pop: -103600178<br />x: 18927.8144<br />y: 71.47608","pop: -103600178<br />pop: -103600178<br />x: 19418.6108<br />y: 71.82213","pop: -103600178<br />pop: -103600178<br />x: 19909.4072<br />y: 72.16818","pop: -103600178<br />pop: -103600178<br />x: 20400.2036<br />y: 72.51424","pop: -103600178<br />pop: -103600178<br />x: 20890.9999<br />y: 72.86029","pop: -103600178<br />pop: -103600178<br />x: 21381.7963<br />y: 73.20634","pop: -103600178<br />pop: -103600178<br />x: 21872.5927<br />y: 73.55240","pop: -103600178<br />pop: -103600178<br />x: 22363.3891<br />y: 73.89845","pop: -103600178<br />pop: -103600178<br />x: 22854.1855<br />y: 74.24451","pop: -103600178<br />pop: -103600178<br />x: 23344.9819<br />y: 74.59056","pop: -103600178<br />pop: -103600178<br />x: 23835.7782<br />y: 74.93661","pop: -103600178<br />pop: -103600178<br />x: 24326.5746<br />y: 75.28267","pop: -103600178<br />pop: -103600178<br />x: 24817.3710<br />y: 75.62872","pop: -103600178<br />pop: -103600178<br />x: 25308.1674<br />y: 75.97478","pop: -103600178<br />pop: -103600178<br />x: 25798.9638<br />y: 76.32083","pop: -103600178<br />pop: -103600178<br />x: 26289.7602<br />y: 76.66688","pop: -103600178<br />pop: -103600178<br />x: 26780.5565<br />y: 77.01294","pop: -103600178<br />pop: -103600178<br />x: 27271.3529<br />y: 77.35899","pop: -103600178<br />pop: -103600178<br />x: 27762.1493<br />y: 77.70505","pop: -103600178<br />pop: -103600178<br />x: 28252.9457<br />y: 78.05110","pop: -103600178<br />pop: -103600178<br />x: 28743.7421<br />y: 78.39715","pop: -103600178<br />pop: -103600178<br />x: 29234.5385<br />y: 78.74321","pop: -103600178<br />pop: -103600178<br />x: 29725.3348<br />y: 79.08926","pop: -103600178<br />pop: -103600178<br />x: 30216.1312<br />y: 79.43531","pop: -103600178<br />pop: -103600178<br />x: 30706.9276<br />y: 79.78137","pop: -103600178<br />pop: -103600178<br />x: 31197.7240<br />y: 80.12742","pop: -103600178<br />pop: -103600178<br />x: 31688.5204<br />y: 80.47348","pop: -103600178<br />pop: -103600178<br />x: 32179.3168<br />y: 80.81953","pop: -103600178<br />pop: -103600178<br />x: 32670.1131<br />y: 81.16558","pop: -103600178<br />pop: -103600178<br />x: 33160.9095<br />y: 81.51164","pop: -103600178<br />pop: -103600178<br />x: 33651.7059<br />y: 81.85769","pop: -103600178<br />pop: -103600178<br />x: 34142.5023<br />y: 82.20375","pop: -103600178<br />pop: -103600178<br />x: 34633.2987<br />y: 82.54980","pop: -103600178<br />pop: -103600178<br />x: 35124.0951<br />y: 82.89585","pop: -103600178<br />pop: -103600178<br />x: 35614.8914<br />y: 83.24191","pop: -103600178<br />pop: -103600178<br />x: 36105.6878<br />y: 83.58796","pop: -103600178<br />pop: -103600178<br />x: 36596.4842<br />y: 83.93401","pop: -103600178<br />pop: -103600178<br />x: 37087.2806<br />y: 84.28007","pop: -103600178<br />pop: -103600178<br />x: 37578.0770<br />y: 84.62612","pop: -103600178<br />pop: -103600178<br />x: 38068.8734<br />y: 84.97218","pop: -103600178<br />pop: -103600178<br />x: 38559.6697<br />y: 85.31823","pop: -103600178<br />pop: -103600178<br />x: 39050.4661<br />y: 85.66428","pop: -103600178<br />pop: -103600178<br />x: 39541.2625<br />y: 86.01034","pop: -103600178<br />pop: -103600178<br />x: 40032.0589<br />y: 86.35639","pop: -103600178<br />pop: -103600178<br />x: 40522.8553<br />y: 86.70245","pop: -103600178<br />pop: -103600178<br />x: 41013.6517<br />y: 87.04850","pop: -103600178<br />pop: -103600178<br />x: 41504.4480<br />y: 87.39455","pop: -103600178<br />pop: -103600178<br />x: 41995.2444<br />y: 87.74061","pop: -103600178<br />pop: -103600178<br />x: 42486.0408<br />y: 88.08666","pop: -103600178<br />pop: -103600178<br />x: 42976.8372<br />y: 88.43272","pop: -103600178<br />pop: -103600178<br />x: 43467.6336<br />y: 88.77877","pop: -103600178<br />pop: -103600178<br />x: 43958.4300<br />y: 89.12482","pop: -103600178<br />pop: -103600178<br />x: 44449.2263<br />y: 89.47088","pop: -103600178<br />pop: -103600178<br />x: 44940.0227<br />y: 89.81693","pop: -103600178<br />pop: -103600178<br />x: 45430.8191<br />y: 90.16298","pop: -103600178<br />pop: -103600178<br />x: 45921.6155<br />y: 90.50904","pop: -103600178<br />pop: -103600178<br />x: 46412.4119<br />y: 90.85509","pop: -103600178<br />pop: -103600178<br />x: 46903.2083<br />y: 91.20115","pop: -103600178<br />pop: -103600178<br />x: 47394.0046<br />y: 91.54720","pop: -103600178<br />pop: -103600178<br />x: 47884.8010<br />y: 91.89325","pop: -103600178<br />pop: -103600178<br />x: 48375.5974<br />y: 92.23931","pop: -103600178<br />pop: -103600178<br />x: 48866.3938<br />y: 92.58536","pop: -103600178<br />pop: -103600178<br />x: 49357.1902<br />y: 92.93142"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"rgba(255,78,55,1)","dash":"solid"},"hoveron":"points","name":"(-103600178,1)","legendgroup":"(-103600178,1)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[277.5518587,768.348241813,1259.144624926,1749.941008039,2240.737391152,2731.533774265,3222.330157378,3713.126540491,4203.922923604,4694.719306717,5185.51568983,5676.312072943,6167.108456056,6657.904839169,7148.701222282,7639.497605395,8130.293988508,8621.090371621,9111.886754734,9602.683137847,10093.47952096,10584.275904073,11075.072287186,11565.868670299,12056.665053412,12547.461436525,13038.257819638,13529.054202751,14019.850585864,14510.646968977,15001.44335209,15492.239735203,15983.036118316,16473.832501429,16964.628884542,17455.425267655,17946.221650768,18437.018033881,18927.814416994,19418.610800107,19909.40718322,20400.203566333,20890.999949446,21381.796332559,21872.592715672,22363.389098785,22854.185481898,23344.981865011,23835.778248124,24326.574631237,24817.37101435,25308.167397463,25798.963780576,26289.760163689,26780.556546802,27271.352929915,27762.149313028,28252.945696141,28743.742079254,29234.538462367,29725.33484548,30216.131228593,30706.927611706,31197.723994819,31688.520377932,32179.316761045,32670.113144158,33160.909527271,33651.705910384,34142.502293497,34633.29867661,35124.095059723,35614.891442836,36105.687825949,36596.484209062,37087.280592175,37578.076975288,38068.873358401,38559.669741514,39050.466124627,39541.26250774,40032.058890853,40522.855273966,41013.651657079,41504.448040192,41995.244423305,42486.040806418,42976.837189531,43467.633572644,43958.429955757,44449.22633887,44940.022721983,45430.819105096,45921.615488209,46412.411871322,46903.208254435,47394.004637548,47884.801020661,48375.597403774,48866.393786887,49357.19017],"y":[59.6928466106349,60.0056108417505,60.3183750728661,60.6311393039817,60.9439035350973,61.2566677662129,61.5694319973285,61.8821962284441,62.1949604595596,62.5077246906752,62.8204889217908,63.1332531529064,63.446017384022,63.7587816151376,64.0715458462532,64.3843100773687,64.6970743084843,65.0098385395999,65.3226027707155,65.6353670018311,65.9481312329467,66.2608954640623,66.5736596951779,66.8864239262935,67.199188157409,67.5119523885246,67.8247166196402,68.1374808507558,68.4502450818714,68.763009312987,69.0757735441026,69.3885377752182,69.7013020063337,70.0140662374493,70.3268304685649,70.6395946996805,70.9523589307961,71.2651231619117,71.5778873930273,71.8906516241429,72.2034158552585,72.516180086374,72.8289443174896,73.1417085486052,73.4544727797208,73.7672370108364,74.080001241952,74.3927654730676,74.7055297041832,75.0182939352987,75.3310581664144,75.6438223975299,75.9565866286455,76.2693508597611,76.5821150908767,76.8948793219923,77.2076435531079,77.5204077842235,77.8331720153391,78.1459362464546,78.4587004775702,78.7714647086858,79.0842289398014,79.396993170917,79.7097574020326,80.0225216331482,80.3352858642638,80.6480500953793,80.9608143264949,81.2735785576105,81.5863427887261,81.8991070198417,82.2118712509573,82.5246354820729,82.8373997131885,83.150163944304,83.4629281754196,83.7756924065352,84.0884566376508,84.4012208687664,84.713985099882,85.0267493309976,85.3395135621132,85.6522777932287,85.9650420243444,86.2778062554599,86.5905704865755,86.9033347176911,87.2160989488067,87.5288631799223,87.8416274110379,88.1543916421535,88.467155873269,88.7799201043846,89.0926843355002,89.4054485666158,89.7182127977314,90.030977028847,90.3437412599626,90.6565054910782,90.9692697221938],"text":["pop: 44021220<br />pop: 44021220<br />x:   277.5519<br />y: 59.69285","pop: 44021220<br />pop: 44021220<br />x:   768.3482<br />y: 60.00561","pop: 44021220<br />pop: 44021220<br />x:  1259.1446<br />y: 60.31838","pop: 44021220<br />pop: 44021220<br />x:  1749.9410<br />y: 60.63114","pop: 44021220<br />pop: 44021220<br />x:  2240.7374<br />y: 60.94390","pop: 44021220<br />pop: 44021220<br />x:  2731.5338<br />y: 61.25667","pop: 44021220<br />pop: 44021220<br />x:  3222.3302<br />y: 61.56943","pop: 44021220<br />pop: 44021220<br />x:  3713.1265<br />y: 61.88220","pop: 44021220<br />pop: 44021220<br />x:  4203.9229<br />y: 62.19496","pop: 44021220<br />pop: 44021220<br />x:  4694.7193<br />y: 62.50772","pop: 44021220<br />pop: 44021220<br />x:  5185.5157<br />y: 62.82049","pop: 44021220<br />pop: 44021220<br />x:  5676.3121<br />y: 63.13325","pop: 44021220<br />pop: 44021220<br />x:  6167.1085<br />y: 63.44602","pop: 44021220<br />pop: 44021220<br />x:  6657.9048<br />y: 63.75878","pop: 44021220<br />pop: 44021220<br />x:  7148.7012<br />y: 64.07155","pop: 44021220<br />pop: 44021220<br />x:  7639.4976<br />y: 64.38431","pop: 44021220<br />pop: 44021220<br />x:  8130.2940<br />y: 64.69707","pop: 44021220<br />pop: 44021220<br />x:  8621.0904<br />y: 65.00984","pop: 44021220<br />pop: 44021220<br />x:  9111.8868<br />y: 65.32260","pop: 44021220<br />pop: 44021220<br />x:  9602.6831<br />y: 65.63537","pop: 44021220<br />pop: 44021220<br />x: 10093.4795<br />y: 65.94813","pop: 44021220<br />pop: 44021220<br />x: 10584.2759<br />y: 66.26090","pop: 44021220<br />pop: 44021220<br />x: 11075.0723<br />y: 66.57366","pop: 44021220<br />pop: 44021220<br />x: 11565.8687<br />y: 66.88642","pop: 44021220<br />pop: 44021220<br />x: 12056.6651<br />y: 67.19919","pop: 44021220<br />pop: 44021220<br />x: 12547.4614<br />y: 67.51195","pop: 44021220<br />pop: 44021220<br />x: 13038.2578<br />y: 67.82472","pop: 44021220<br />pop: 44021220<br />x: 13529.0542<br />y: 68.13748","pop: 44021220<br />pop: 44021220<br />x: 14019.8506<br />y: 68.45025","pop: 44021220<br />pop: 44021220<br />x: 14510.6470<br />y: 68.76301","pop: 44021220<br />pop: 44021220<br />x: 15001.4434<br />y: 69.07577","pop: 44021220<br />pop: 44021220<br />x: 15492.2397<br />y: 69.38854","pop: 44021220<br />pop: 44021220<br />x: 15983.0361<br />y: 69.70130","pop: 44021220<br />pop: 44021220<br />x: 16473.8325<br />y: 70.01407","pop: 44021220<br />pop: 44021220<br />x: 16964.6289<br />y: 70.32683","pop: 44021220<br />pop: 44021220<br />x: 17455.4253<br />y: 70.63959","pop: 44021220<br />pop: 44021220<br />x: 17946.2217<br />y: 70.95236","pop: 44021220<br />pop: 44021220<br />x: 18437.0180<br />y: 71.26512","pop: 44021220<br />pop: 44021220<br />x: 18927.8144<br />y: 71.57789","pop: 44021220<br />pop: 44021220<br />x: 19418.6108<br />y: 71.89065","pop: 44021220<br />pop: 44021220<br />x: 19909.4072<br />y: 72.20342","pop: 44021220<br />pop: 44021220<br />x: 20400.2036<br />y: 72.51618","pop: 44021220<br />pop: 44021220<br />x: 20890.9999<br />y: 72.82894","pop: 44021220<br />pop: 44021220<br />x: 21381.7963<br />y: 73.14171","pop: 44021220<br />pop: 44021220<br />x: 21872.5927<br />y: 73.45447","pop: 44021220<br />pop: 44021220<br />x: 22363.3891<br />y: 73.76724","pop: 44021220<br />pop: 44021220<br />x: 22854.1855<br />y: 74.08000","pop: 44021220<br />pop: 44021220<br />x: 23344.9819<br />y: 74.39277","pop: 44021220<br />pop: 44021220<br />x: 23835.7782<br />y: 74.70553","pop: 44021220<br />pop: 44021220<br />x: 24326.5746<br />y: 75.01829","pop: 44021220<br />pop: 44021220<br />x: 24817.3710<br />y: 75.33106","pop: 44021220<br />pop: 44021220<br />x: 25308.1674<br />y: 75.64382","pop: 44021220<br />pop: 44021220<br />x: 25798.9638<br />y: 75.95659","pop: 44021220<br />pop: 44021220<br />x: 26289.7602<br />y: 76.26935","pop: 44021220<br />pop: 44021220<br />x: 26780.5565<br />y: 76.58212","pop: 44021220<br />pop: 44021220<br />x: 27271.3529<br />y: 76.89488","pop: 44021220<br />pop: 44021220<br />x: 27762.1493<br />y: 77.20764","pop: 44021220<br />pop: 44021220<br />x: 28252.9457<br />y: 77.52041","pop: 44021220<br />pop: 44021220<br />x: 28743.7421<br />y: 77.83317","pop: 44021220<br />pop: 44021220<br />x: 29234.5385<br />y: 78.14594","pop: 44021220<br />pop: 44021220<br />x: 29725.3348<br />y: 78.45870","pop: 44021220<br />pop: 44021220<br />x: 30216.1312<br />y: 78.77146","pop: 44021220<br />pop: 44021220<br />x: 30706.9276<br />y: 79.08423","pop: 44021220<br />pop: 44021220<br />x: 31197.7240<br />y: 79.39699","pop: 44021220<br />pop: 44021220<br />x: 31688.5204<br />y: 79.70976","pop: 44021220<br />pop: 44021220<br />x: 32179.3168<br />y: 80.02252","pop: 44021220<br />pop: 44021220<br />x: 32670.1131<br />y: 80.33529","pop: 44021220<br />pop: 44021220<br />x: 33160.9095<br />y: 80.64805","pop: 44021220<br />pop: 44021220<br />x: 33651.7059<br />y: 80.96081","pop: 44021220<br />pop: 44021220<br />x: 34142.5023<br />y: 81.27358","pop: 44021220<br />pop: 44021220<br />x: 34633.2987<br />y: 81.58634","pop: 44021220<br />pop: 44021220<br />x: 35124.0951<br />y: 81.89911","pop: 44021220<br />pop: 44021220<br />x: 35614.8914<br />y: 82.21187","pop: 44021220<br />pop: 44021220<br />x: 36105.6878<br />y: 82.52464","pop: 44021220<br />pop: 44021220<br />x: 36596.4842<br />y: 82.83740","pop: 44021220<br />pop: 44021220<br />x: 37087.2806<br />y: 83.15016","pop: 44021220<br />pop: 44021220<br />x: 37578.0770<br />y: 83.46293","pop: 44021220<br />pop: 44021220<br />x: 38068.8734<br />y: 83.77569","pop: 44021220<br />pop: 44021220<br />x: 38559.6697<br />y: 84.08846","pop: 44021220<br />pop: 44021220<br />x: 39050.4661<br />y: 84.40122","pop: 44021220<br />pop: 44021220<br />x: 39541.2625<br />y: 84.71399","pop: 44021220<br />pop: 44021220<br />x: 40032.0589<br />y: 85.02675","pop: 44021220<br />pop: 44021220<br />x: 40522.8553<br />y: 85.33951","pop: 44021220<br />pop: 44021220<br />x: 41013.6517<br />y: 85.65228","pop: 44021220<br />pop: 44021220<br />x: 41504.4480<br />y: 85.96504","pop: 44021220<br />pop: 44021220<br />x: 41995.2444<br />y: 86.27781","pop: 44021220<br />pop: 44021220<br />x: 42486.0408<br />y: 86.59057","pop: 44021220<br />pop: 44021220<br />x: 42976.8372<br />y: 86.90333","pop: 44021220<br />pop: 44021220<br />x: 43467.6336<br />y: 87.21610","pop: 44021220<br />pop: 44021220<br />x: 43958.4300<br />y: 87.52886","pop: 44021220<br />pop: 44021220<br />x: 44449.2263<br />y: 87.84163","pop: 44021220<br />pop: 44021220<br />x: 44940.0227<br />y: 88.15439","pop: 44021220<br />pop: 44021220<br />x: 45430.8191<br />y: 88.46716","pop: 44021220<br />pop: 44021220<br />x: 45921.6155<br />y: 88.77992","pop: 44021220<br />pop: 44021220<br />x: 46412.4119<br />y: 89.09268","pop: 44021220<br />pop: 44021220<br />x: 46903.2083<br />y: 89.40545","pop: 44021220<br />pop: 44021220<br />x: 47394.0046<br />y: 89.71821","pop: 44021220<br />pop: 44021220<br />x: 47884.8010<br />y: 90.03098","pop: 44021220<br />pop: 44021220<br />x: 48375.5974<br />y: 90.34374","pop: 44021220<br />pop: 44021220<br />x: 48866.3938<br />y: 90.65651","pop: 44021220<br />pop: 44021220<br />x: 49357.1902<br />y: 90.96927"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"rgba(0,181,0,1)","dash":"solid"},"hoveron":"points","name":"(44021220,1)","legendgroup":"(44021220,1)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[277.5518587,768.348241813,1259.144624926,1749.941008039,2240.737391152,2731.533774265,3222.330157378,3713.126540491,4203.922923604,4694.719306717,5185.51568983,5676.312072943,6167.108456056,6657.904839169,7148.701222282,7639.497605395,8130.293988508,8621.090371621,9111.886754734,9602.683137847,10093.47952096,10584.275904073,11075.072287186,11565.868670299,12056.665053412,12547.461436525,13038.257819638,13529.054202751,14019.850585864,14510.646968977,15001.44335209,15492.239735203,15983.036118316,16473.832501429,16964.628884542,17455.425267655,17946.221650768,18437.018033881,18927.814416994,19418.610800107,19909.40718322,20400.203566333,20890.999949446,21381.796332559,21872.592715672,22363.389098785,22854.185481898,23344.981865011,23835.778248124,24326.574631237,24817.37101435,25308.167397463,25798.963780576,26289.760163689,26780.556546802,27271.352929915,27762.149313028,28252.945696141,28743.742079254,29234.538462367,29725.33484548,30216.131228593,30706.927611706,31197.723994819,31688.520377932,32179.316761045,32670.113144158,33160.909527271,33651.705910384,34142.502293497,34633.29867661,35124.095059723,35614.891442836,36105.687825949,36596.484209062,37087.280592175,37578.076975288,38068.873358401,38559.669741514,39050.466124627,39541.26250774,40032.058890853,40522.855273966,41013.651657079,41504.448040192,41995.244423305,42486.040806418,42976.837189531,43467.633572644,43958.429955757,44449.22633887,44940.022721983,45430.819105096,45921.615488209,46412.411871322,46903.208254435,47394.004637548,47884.801020661,48375.597403774,48866.393786887,49357.19017],"y":[61.0596645475133,61.3391391424523,61.6186137373913,61.8980883323303,62.1775629272693,62.4570375222083,62.7365121171473,63.0159867120863,63.2954613070254,63.5749359019644,63.8544104969034,64.1338850918424,64.4133596867814,64.6928342817204,64.9723088766594,65.2517834715984,65.5312580665375,65.8107326614765,66.0902072564155,66.3696818513545,66.6491564462935,66.9286310412325,67.2081056361715,67.4875802311105,67.7670548260496,68.0465294209886,68.3260040159276,68.6054786108666,68.8849532058056,69.1644278007446,69.4439023956836,69.7233769906227,70.0028515855617,70.2823261805007,70.5618007754397,70.8412753703787,71.1207499653177,71.4002245602567,71.6796991551957,71.9591737501348,72.2386483450738,72.5181229400128,72.7975975349518,73.0770721298908,73.3565467248298,73.6360213197688,73.9154959147078,74.1949705096469,74.4744451045859,74.7539196995249,75.0333942944639,75.3128688894029,75.5923434843419,75.8718180792809,76.1512926742199,76.430767269159,76.710241864098,76.989716459037,77.269191053976,77.548665648915,77.828140243854,78.107614838793,78.3870894337321,78.6665640286711,78.9460386236101,79.2255132185491,79.5049878134881,79.7844624084271,80.0639370033661,80.3434115983052,80.6228861932442,80.9023607881832,81.1818353831222,81.4613099780612,81.7407845730002,82.0202591679392,82.2997337628782,82.5792083578173,82.8586829527563,83.1381575476953,83.4176321426343,83.6971067375733,83.9765813325123,84.2560559274513,84.5355305223903,84.8150051173293,85.0944797122684,85.3739543072074,85.6534289021464,85.9329034970854,86.2123780920244,86.4918526869634,86.7713272819024,87.0508018768415,87.3302764717805,87.6097510667195,87.8892256616585,88.1687002565975,88.4481748515365,88.7276494464755,89.0071240414146],"text":["pop: 191642618<br />pop: 191642618<br />x:   277.5519<br />y: 61.05966","pop: 191642618<br />pop: 191642618<br />x:   768.3482<br />y: 61.33914","pop: 191642618<br />pop: 191642618<br />x:  1259.1446<br />y: 61.61861","pop: 191642618<br />pop: 191642618<br />x:  1749.9410<br />y: 61.89809","pop: 191642618<br />pop: 191642618<br />x:  2240.7374<br />y: 62.17756","pop: 191642618<br />pop: 191642618<br />x:  2731.5338<br />y: 62.45704","pop: 191642618<br />pop: 191642618<br />x:  3222.3302<br />y: 62.73651","pop: 191642618<br />pop: 191642618<br />x:  3713.1265<br />y: 63.01599","pop: 191642618<br />pop: 191642618<br />x:  4203.9229<br />y: 63.29546","pop: 191642618<br />pop: 191642618<br />x:  4694.7193<br />y: 63.57494","pop: 191642618<br />pop: 191642618<br />x:  5185.5157<br />y: 63.85441","pop: 191642618<br />pop: 191642618<br />x:  5676.3121<br />y: 64.13389","pop: 191642618<br />pop: 191642618<br />x:  6167.1085<br />y: 64.41336","pop: 191642618<br />pop: 191642618<br />x:  6657.9048<br />y: 64.69283","pop: 191642618<br />pop: 191642618<br />x:  7148.7012<br />y: 64.97231","pop: 191642618<br />pop: 191642618<br />x:  7639.4976<br />y: 65.25178","pop: 191642618<br />pop: 191642618<br />x:  8130.2940<br />y: 65.53126","pop: 191642618<br />pop: 191642618<br />x:  8621.0904<br />y: 65.81073","pop: 191642618<br />pop: 191642618<br />x:  9111.8868<br />y: 66.09021","pop: 191642618<br />pop: 191642618<br />x:  9602.6831<br />y: 66.36968","pop: 191642618<br />pop: 191642618<br />x: 10093.4795<br />y: 66.64916","pop: 191642618<br />pop: 191642618<br />x: 10584.2759<br />y: 66.92863","pop: 191642618<br />pop: 191642618<br />x: 11075.0723<br />y: 67.20811","pop: 191642618<br />pop: 191642618<br />x: 11565.8687<br />y: 67.48758","pop: 191642618<br />pop: 191642618<br />x: 12056.6651<br />y: 67.76705","pop: 191642618<br />pop: 191642618<br />x: 12547.4614<br />y: 68.04653","pop: 191642618<br />pop: 191642618<br />x: 13038.2578<br />y: 68.32600","pop: 191642618<br />pop: 191642618<br />x: 13529.0542<br />y: 68.60548","pop: 191642618<br />pop: 191642618<br />x: 14019.8506<br />y: 68.88495","pop: 191642618<br />pop: 191642618<br />x: 14510.6470<br />y: 69.16443","pop: 191642618<br />pop: 191642618<br />x: 15001.4434<br />y: 69.44390","pop: 191642618<br />pop: 191642618<br />x: 15492.2397<br />y: 69.72338","pop: 191642618<br />pop: 191642618<br />x: 15983.0361<br />y: 70.00285","pop: 191642618<br />pop: 191642618<br />x: 16473.8325<br />y: 70.28233","pop: 191642618<br />pop: 191642618<br />x: 16964.6289<br />y: 70.56180","pop: 191642618<br />pop: 191642618<br />x: 17455.4253<br />y: 70.84128","pop: 191642618<br />pop: 191642618<br />x: 17946.2217<br />y: 71.12075","pop: 191642618<br />pop: 191642618<br />x: 18437.0180<br />y: 71.40022","pop: 191642618<br />pop: 191642618<br />x: 18927.8144<br />y: 71.67970","pop: 191642618<br />pop: 191642618<br />x: 19418.6108<br />y: 71.95917","pop: 191642618<br />pop: 191642618<br />x: 19909.4072<br />y: 72.23865","pop: 191642618<br />pop: 191642618<br />x: 20400.2036<br />y: 72.51812","pop: 191642618<br />pop: 191642618<br />x: 20890.9999<br />y: 72.79760","pop: 191642618<br />pop: 191642618<br />x: 21381.7963<br />y: 73.07707","pop: 191642618<br />pop: 191642618<br />x: 21872.5927<br />y: 73.35655","pop: 191642618<br />pop: 191642618<br />x: 22363.3891<br />y: 73.63602","pop: 191642618<br />pop: 191642618<br />x: 22854.1855<br />y: 73.91550","pop: 191642618<br />pop: 191642618<br />x: 23344.9819<br />y: 74.19497","pop: 191642618<br />pop: 191642618<br />x: 23835.7782<br />y: 74.47445","pop: 191642618<br />pop: 191642618<br />x: 24326.5746<br />y: 74.75392","pop: 191642618<br />pop: 191642618<br />x: 24817.3710<br />y: 75.03339","pop: 191642618<br />pop: 191642618<br />x: 25308.1674<br />y: 75.31287","pop: 191642618<br />pop: 191642618<br />x: 25798.9638<br />y: 75.59234","pop: 191642618<br />pop: 191642618<br />x: 26289.7602<br />y: 75.87182","pop: 191642618<br />pop: 191642618<br />x: 26780.5565<br />y: 76.15129","pop: 191642618<br />pop: 191642618<br />x: 27271.3529<br />y: 76.43077","pop: 191642618<br />pop: 191642618<br />x: 27762.1493<br />y: 76.71024","pop: 191642618<br />pop: 191642618<br />x: 28252.9457<br />y: 76.98972","pop: 191642618<br />pop: 191642618<br />x: 28743.7421<br />y: 77.26919","pop: 191642618<br />pop: 191642618<br />x: 29234.5385<br />y: 77.54867","pop: 191642618<br />pop: 191642618<br />x: 29725.3348<br />y: 77.82814","pop: 191642618<br />pop: 191642618<br />x: 30216.1312<br />y: 78.10761","pop: 191642618<br />pop: 191642618<br />x: 30706.9276<br />y: 78.38709","pop: 191642618<br />pop: 191642618<br />x: 31197.7240<br />y: 78.66656","pop: 191642618<br />pop: 191642618<br />x: 31688.5204<br />y: 78.94604","pop: 191642618<br />pop: 191642618<br />x: 32179.3168<br />y: 79.22551","pop: 191642618<br />pop: 191642618<br />x: 32670.1131<br />y: 79.50499","pop: 191642618<br />pop: 191642618<br />x: 33160.9095<br />y: 79.78446","pop: 191642618<br />pop: 191642618<br />x: 33651.7059<br />y: 80.06394","pop: 191642618<br />pop: 191642618<br />x: 34142.5023<br />y: 80.34341","pop: 191642618<br />pop: 191642618<br />x: 34633.2987<br />y: 80.62289","pop: 191642618<br />pop: 191642618<br />x: 35124.0951<br />y: 80.90236","pop: 191642618<br />pop: 191642618<br />x: 35614.8914<br />y: 81.18184","pop: 191642618<br />pop: 191642618<br />x: 36105.6878<br />y: 81.46131","pop: 191642618<br />pop: 191642618<br />x: 36596.4842<br />y: 81.74078","pop: 191642618<br />pop: 191642618<br />x: 37087.2806<br />y: 82.02026","pop: 191642618<br />pop: 191642618<br />x: 37578.0770<br />y: 82.29973","pop: 191642618<br />pop: 191642618<br />x: 38068.8734<br />y: 82.57921","pop: 191642618<br />pop: 191642618<br />x: 38559.6697<br />y: 82.85868","pop: 191642618<br />pop: 191642618<br />x: 39050.4661<br />y: 83.13816","pop: 191642618<br />pop: 191642618<br />x: 39541.2625<br />y: 83.41763","pop: 191642618<br />pop: 191642618<br />x: 40032.0589<br />y: 83.69711","pop: 191642618<br />pop: 191642618<br />x: 40522.8553<br />y: 83.97658","pop: 191642618<br />pop: 191642618<br />x: 41013.6517<br />y: 84.25606","pop: 191642618<br />pop: 191642618<br />x: 41504.4480<br />y: 84.53553","pop: 191642618<br />pop: 191642618<br />x: 41995.2444<br />y: 84.81501","pop: 191642618<br />pop: 191642618<br />x: 42486.0408<br />y: 85.09448","pop: 191642618<br />pop: 191642618<br />x: 42976.8372<br />y: 85.37395","pop: 191642618<br />pop: 191642618<br />x: 43467.6336<br />y: 85.65343","pop: 191642618<br />pop: 191642618<br />x: 43958.4300<br />y: 85.93290","pop: 191642618<br />pop: 191642618<br />x: 44449.2263<br />y: 86.21238","pop: 191642618<br />pop: 191642618<br />x: 44940.0227<br />y: 86.49185","pop: 191642618<br />pop: 191642618<br />x: 45430.8191<br />y: 86.77133","pop: 191642618<br />pop: 191642618<br />x: 45921.6155<br />y: 87.05080","pop: 191642618<br />pop: 191642618<br />x: 46412.4119<br />y: 87.33028","pop: 191642618<br />pop: 191642618<br />x: 46903.2083<br />y: 87.60975","pop: 191642618<br />pop: 191642618<br />x: 47394.0046<br />y: 87.88923","pop: 191642618<br />pop: 191642618<br />x: 47884.8010<br />y: 88.16870","pop: 191642618<br />pop: 191642618<br />x: 48375.5974<br />y: 88.44817","pop: 191642618<br />pop: 191642618<br />x: 48866.3938<br />y: 88.72765","pop: 191642618<br />pop: 191642618<br />x: 49357.1902<br />y: 89.00712"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"rgba(0,141,255,1)","dash":"solid"},"hoveron":"points","name":"(191642618,1)","legendgroup":"(191642618,1)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2176.430056865,51811.172085565],"tickmode":"array","ticktext":["0","10000","20000","30000","40000","50000"],"tickvals":[0,10000,20000,30000,40000,50000],"categoryorder":"array","categoryarray":["0","10000","20000","30000","40000","50000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"GDP per capita","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[56.5957593372958,94.6616847394338],"tickmode":"array","ticktext":["60","70","80","90"],"tickvals":[60,70,80,90],"categoryorder":"array","categoryarray":["60","70","80","90"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"Life Expectancy","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"pop","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"aea41833611b":{"colour":{},"x":{},"y":{},"type":"scatter"}},"cur_data":"aea41833611b","visdat":{"aea41833611b":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>
<!--/html_preserve-->
Note; the plotly graphs won't render in this file, so you have to check them out seprately. Ytr hovering over them, they have many useful functions, such as zooming in and out.

Part 4: Writing figures to file Use ggsave() to explicitly save a plot to file. Then use to load and embed it in your report. You can play around with various options, such as:

Arguments of ggsave(), such as width, height, resolution or text scaling. Various graphics devices, e.g. a vector vs. raster format. Explicit provision of the plot object p via ggsave(..., plot = p). Show a situation in which this actually matters.

``` r
ggsave("gap_interaction.png", gap_interaction, width=40, height=40, units = "cm", device = 'png')
```

This graph got automatically saved in my Homework 5 folder on my laptop.
