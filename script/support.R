# Check if the eurozone countries have a similar structure
cashless_payments_eurozone <- cashless_payments_number_cleaner %>% 
  filter(country_code %in% c("BE", "FR", "DE", "IT", "NL", "ES"))

ggplot(cashless_payments_eurozone, aes(fill=method, y=share_2019, x=country)) + 
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(name = "Participación", labels = label_percent())+
  labs(
    x = "País",
    title = "Eurozona",
    subtitle = "Participación de métodos de pago en países seleccionados de la eurozona",
  )
