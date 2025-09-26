setwd("/Users/takayukitamura/Documents/R_Computing/ppi")
# Install/load required packages
library(tidyverse)
library(lubridate)
library(quantmod) 
library(ggtext)
library(fredr) 
library(glue)

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

#---Download Data---
# CPI
cpi <- fredr(series_id = "CPIAUCSL") %>% 
  select(date, value) %>% 
  mutate(cpi_yoy = (value/lag(value, 12) -1) * 100) 

# PPI
ppi <- fredr(series_id = "PPIACO") %>% 
  select(date, value) %>% 
  mutate(ppi_yoy = (value/lag(value, 12) -1) * 100) %>% 
  filter(date >= "2000-01-01") 

core_ppi <- fredr(series_id = "WPSFD49116") %>% 
  select(date, value) %>% 
  mutate(core_ppi_yoy = (value/lag(value, 12)-1) * 100) %>% 
  filter(date >= "2000-01-01")

ppi %>% 
  left_join(core_ppi, by = "date") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = ppi_yoy, colour = "PPI %")) +
  # geom_line(aes(y = core_ppi_yoy, colour = "PPI less foods & energy")) +
  labs(title = glue("US "),
       x = NULL, y = "Inflation(%)",
       caption = "Labor Department, FRED") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )



# Fed Fund Rate
fedfunds <- fredr(series_id = "FEDFUNDS",
                  observation_start = as.Date("2015-01-01")) %>%
  select(date, value) %>%
  rename(fedfunds = value)

df <- cpi %>% 
  left_join(ppi, by = "date") %>% 
  left_join(fedfunds, by = "date")

# --- Identify Fed Rate Cuts ---
# (when Fed Funds declined vs prior month)

cuts <- df %>% 
  filter(fedfunds < lag(fedfunds)) %>% 
  pull(date)

ggplot(df, aes(x = date)) +
  geom_line(aes(y = cpi_yoy, color = "CPI (YoY%)"), size = 1) +
  geom_line(aes(y = ppi_yoy, color = "PPI (YoY%)"), size = 1, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(cuts), linetype = "dotted", color = "gray40") +
  labs(title = "PPI vs CPI – 12-Month % Change",
       subtitle = "Shaded lines indicate months when the Fed cut rates",
       y = "Year-over-Year % Change",
       x = NULL,
       color = "") +
  scale_color_manual(values = c("CPI (YoY%)" = "blue", "PPI (YoY%)" = "red")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 14)
