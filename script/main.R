library(tidyverse)

# This library, recommended in R4DS, is used to clean column names.
install.packages("janitor")
library(janitor)

library(ggplot2)
install.packages("treemapify")
library(treemapify)

install.packages("scales")
library(scales)

library(RColorBrewer)

payments <- read.csv(file = "data/cashless_payments.csv")

# We clean the column names.
payments_cleaner <- payments %>% 
  clean_names()

# We make convert strings to numbers
payments_cleaner[3:11] <- as.numeric(unlist(payments_cleaner[3:11]))

# We make values percentages.
payments_cleaner[2:11] <- unlist(payments_cleaner[2:11]) / 100

# We remove the 5th column which aggregates cards and e-money payments.
payments_final <- payments_cleaner %>% 
  select(!5)

# We use pivot_longer
payments_methods <- payments_final %>% 
  pivot_longer(
    !country,
    names_to = "method",
    values_to = "share"
  )

# Prepare dataset for plot
small_set <- payments_final %>% 
  filter(country %in% c("Argentina", "Brazil", "Mexico", "Spain", "United States", "Japan"))

small_pivot <- small_set %>% 
  pivot_longer(
    !country,
    names_to = "method",
    values_to = "share"
  )

ggplot(small_pivot, aes(fill=method, y=share, x=country)) + 
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(name = "Participación", labels = label_percent())+
  labs(
    x = "País",
    title = "Un mundo diverso",
    subtitle = "Participación de métodos de pago en países seleccionados",
  ) +
  scale_color_brewer(palette = "Set1")

