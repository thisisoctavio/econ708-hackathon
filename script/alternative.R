library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(colorspace)

data <- read.csv(file = "data/WS_CPMI_CT1_csv_col.csv")

# We are interested in cashless payments, specifically, in the share of each payment method.
# We filter by the column "Indicator" and the value "Cashless payments".

cashless_payments <- data %>% 
  filter(Indicator..comparative.table. == "Cashless payments") %>% 
  filter(Unit.of.measure == "in % of total cashless payments")

# We will focus on transactions volume instead of value.
# We assume that volume better reflects the adoption of a payment method, since large transactions are probably carried out by a small number of people or institutions.

cashless_payments_number <- cashless_payments %>% 
  filter(MEASURE == "N")

# The data from 2022 has many missing values. Since 2020 and 2021 were affected by the COVID-19 pandemic, we focus on the data from 2019 which is the latest normal year with more data. It could be interesting to compare how and if the pandemic affected payment methods' share, during the period 2020-2021 and afterwards, in 2022.

# We clean the data.
cashless_payments_number_cleaner <- cashless_payments_number %>% 
  select(
    REP_CTY, # Country initials
    Reporting.country, # Country name
    Instrument.type..comparative.table., # Payment method 
    TITLE_TS, # Time Series title
    X2019, # 2019 data
    X2022 # 2022 data
  ) %>%
  rename(
    country_code = REP_CTY,
    country = Reporting.country,
    method = Instrument.type..comparative.table.,
    title = TITLE_TS,
    share_2019 = X2019,
    share_2022 = X2022
  ) %>% 
  filter(method != "Card and e-money payments, all")

# First try.
ggplot(cashless_payments_number_cleaner, aes(fill=method, y=share_2019, x=country)) + 
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(name = "Participación", labels = label_percent())+
  labs(
    x = "País",
    title = "Mundo diverso",
    subtitle = "Participación de métodos de pago en países seleccionados",
  )

# Subset: removed China and South Africa because they don't discriminate between card and e-money payments and European countries outside the G20.
cashless_payments_subset <- cashless_payments_number_cleaner %>% 
  filter(! country_code %in% c("CN", "ZA", "BE", "NL", "SE", "CH"))

# Add regions: North America, South America, Asia, Eurozone, Others.
cashless_payments_final <- cashless_payments_subset %>% 
  mutate(
    region = case_when(
      country_code %in% c("US", "CA", "MX") ~ "Norteamérica",
      country_code %in% c("AR", "BR") ~ "Sudamérica",
      country_code %in% c("JP", "KR", "IN", "ID", "SG") ~ "Asia",
      country_code %in% c("DE", "FR", "IT", "ES") ~ "Eurozone",
      TRUE ~ "Otros"
    )
  )

# Final plot
ggplot(cashless_payments_final, aes(fill=method, y=share_2019, x=country_code)) +
facet_grid(cols = vars(region), scales = "free_x", space = "free_x") + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(name = "Participación (%)", labels = label_percent(suffix = "")) +
  labs(
    x = "País",
    fill = NULL,
    title = "Un mundo diverso",
    subtitle = "Importancia relativa de diferentes métodos de pago en las principales economías del mundo",
  ) +
  scale_fill_discrete(labels=c(
    'Tarjeta de crédito',
    'Tarjeta de débito',
    "Tarjeta de débito diferido",
    "E-money",
    "Cheque",
    "Transferencia de crédito",
    "Débito automático",
    "Remesas de dinero",
    "Otros métodos"
  )) +
  theme_minimal() 
