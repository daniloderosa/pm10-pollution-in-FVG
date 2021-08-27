# if you don't have any of these packages, install them through "install.packages(name_package)"
library(tidyverse)
library(janitor)
library(lubridate)


# Dataset import ----------------------------------------------------------

pm10 <- read_csv2(file = "pm10.csv", col_types = 
                     cols(`DATA MISURA` = col_character(),
                     `DATI INSUFF.` = col_character(),
                     `PRES. SUPERAM.` = col_character(),
                     `ATTIVO DAL` = col_character(),
                     `ATTIVO FINO AL` = col_character(), #copy this, otherwise it will parse it as logical variable
                     `UNITA' MISURA` = col_character(),
                     UBICAZIONE = col_character(),
                     RETE = col_character(),
                     `MEDIA GIORNALIERA` = col_double()
))


# Cleaning and tidying -----------------------------------------------------

# cleaning column names with janitor package
pm10 <- clean_names(pm10)

# changing the name of some places (mostly I'm adding the city-detail)
pm10$ubicazione <- pm10$ubicazione %>% str_replace_all(pattern = "A2A ", replacement = "")

pm10 <- pm10 %>% 
  mutate(ubicazione = case_when(
    ubicazione == "Via S.Daniele" ~ "Via S.Daniele (Udine)",
    ubicazione == "via Pitacco-ARPA" ~ "Via Pitacco (Trieste)",
    ubicazione == "Via del Ponticello" ~ "Via del Ponticello (Trieste)",
    ubicazione == "Via Carpineto" ~ "Via Carpineto (Trieste)",
    ubicazione == "V.Cairoli" ~ "V.Cairoli (Udine)", 
    ubicazione == "Pordenone Centro 1" ~ "Pordenone Centro", 
    ubicazione == "piazza Volontari Giuliani" ~ "Piazza Volontari Giuliani (Trieste)",
    ubicazione == "piazza Carlo Alberto" ~ "Piazza Carlo Alberto (Trieste)", 
    ubicazione == "p.le Rosmini" ~ "P.le Rosmini (Trieste)", 
    ubicazione == "Vermegliano" ~ "Vermegliano (Ronchi)",
    ubicazione == "Area verde" ~ "Area verde (Monfalcone)", 
    ubicazione == "Via Natisone" ~ "Via Natisone (Monfalcone)",
    TRUE ~ ubicazione
  ))

# filtering out rows with insufficient data
pm10 <- pm10 %>% 
  filter(
    dati_insuff == "False" | dati_insuff == "FALSO"
  )

# filtering out rows that are no longer activated 
pm10 <- pm10 %>% 
  filter(
    is.na(attivo_fino_al)
  )

# transforming date columns in date format via lubridate
pm10$attivo_dal <- ymd(pm10$attivo_dal)
pm10$data_misura <- ymd(pm10$data_misura)

# replacement of incoherent and similar strings
pm10$dati_insuff <- str_replace_all(pm10$dati_insuff, pattern = "False", replacement = "FALSO")
pm10$pres_superam <- str_replace_all(pm10$pres_superam, pattern = "False", replacement = "FALSO")
pm10$pres_superam <- str_replace_all(pm10$pres_superam, pattern = "True", replacement = "VERO")
# pm10$pres_superam <- factor(pm10$pres_superam)
# pm10$dati_insuff <- factor(pm10$dati_insuff)

# adding 'year' column
pm10 <- pm10 %>% 
  mutate(year = year(pm10$data_misura))


# Example: my town --------------------------------------------------------

pm10 %>% 
  filter(
    ubicazione == "Sacile"
  ) %>% 
  group_by(ubicazione, year) %>%
  summarise(media = mean(media_giornaliera),
             max_giornaliero = max(media_giornaliera),
             min_giornaliero = min(media_giornaliera)) %>% View()



# More cleaning -----------------------------------------------------------

medie_annuali <- pm10 %>% 
  group_by(ubicazione, year) %>%
  summarise(media_annuale = mean(media_giornaliera))

# To check if a single day has exceeded the limit there's already 'pres_superam',
# but it's partly wrong in the original dataset: I guess it has to do with rounding?
# Anyway, let's create a new variable: 
pm10 <- pm10 %>% 
  mutate(
    limit = media_giornaliera > 50
  )

# Let's keep only the days with the value OVER the daily limit
pm10_excess_daily_limit <- pm10 %>% 
  filter(
    limit == TRUE
  ) %>% 
  arrange(
    year
  )

# how many times did each place go over the limit, for each year?
pm10_annual_excess <- pm10_excess_daily_limit %>% 
  group_by(ubicazione, year) %>%
  summarise(
    n_over_limit = n()
  )

# now I want only who went over the ANNUAL limit
pm10_excess_annual_limit <- pm10_annual_excess %>% 
  mutate(
    over35_volte = n_over_limit > 35
  ) %>% 
  filter(
    over35_volte == TRUE
  ) %>% 
  select(
    -(over35_volte) # I don't need this column anymore: it's always TRUE
  )




# Creating the radial bar plot --------------------------------------------

pm10_excess_annual_limit$year <- factor(pm10_excess_annual_limit$year)
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(pm10_excess_annual_limit$year), ncol(pm10_excess_annual_limit)) )
colnames(to_add) <- colnames(pm10_excess_annual_limit)
to_add$year <- rep(levels(pm10_excess_annual_limit$year), each=empty_bar)
str(to_add)
pm10_excess_annual_limit <- rbind(pm10_excess_annual_limit, to_add)
pm10_excess_annual_limit <- pm10_excess_annual_limit %>% arrange(year)
pm10_excess_annual_limit$id <- seq(1, nrow(pm10_excess_annual_limit))


label_data <- pm10_excess_annual_limit
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$label <- paste0(label_data$ubicazione, ": ", label_data$n_over_limit)


base_data <- pm10_excess_annual_limit %>% 
  group_by(year) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

p <- ggplot(pm10_excess_annual_limit, aes(x=as.factor(id), y=n_over_limit, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=.9) +
  ylim(-50,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n_over_limit+10, label=label, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=year), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)


p
ggsave(filename = "pm10.svg") # Refine it in Illustrator and you're done!
