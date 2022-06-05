# coronavirusdataset

---
title: "COVID dataset"
output: html_document
---

```{r setup, include=FALSE}
# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")
```

```{r}
library(coronavirus)
# update_dataset()
```
Note: must restart the R session to have the updates available
```{r}
head(coronavirus::coronavirus)
names(coronavirus)
```
```{r}
head(coronavirus::covid19_vaccine)
names(covid19_vaccine)
```
```{r}
library(dplyr)
summary_coron <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))

summary_coron %>%
  head(20)
```
```{r}
library(plotly)
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
![newplot](https://user-images.githubusercontent.com/86179638/172038726-1a5cfe8b-bcf7-4758-bab1-5112e9a9f50f.png)

```{r}
library(ggplot2)

plot_1 <- coronavirus %>%
  filter(country == "Brazil", type == "confirmed") %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(color = "blue") +
  ggtitle("Confirmed Cases in Brazil")
plot_1

plot_2 <- coronavirus %>%
  filter(country == "Brazil", type == "death") %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(color = "red")+
  ggtitle("Death Cases in Brazil")
plot_2

gridExtra::grid.arrange(plot_1, plot_2, nrow = 2, ncol = 1)
```
![00000a](https://user-images.githubusercontent.com/86179638/172038760-ed440836-919a-4ade-8337-5612f986ddb6.png)

```{r}
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
![newplot (1)](https://user-images.githubusercontent.com/86179638/172038773-0fdc0001-dd5b-4131-9af0-b26b50f1425b.png)

```{r}
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
![newplot (2)](https://user-images.githubusercontent.com/86179638/172038784-e077b442-989e-4867-aee7-fd4afd4ab03b.png)



