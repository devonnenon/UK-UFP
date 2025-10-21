library(ggplot2)
library(tidyr)
library(dplyr)

sitesdf <- ufp_agg_long %>%
  filter(date >= as.Date("2003-01-01") & site %in% UB_sites)

UB_sites <- c("belfast", "birmcen", "birmtyb", "glasgow", "bloomsbury", 
                  "manchester", "kensington","honor")

firstlastlist <- lapply(UB_sites, function(msite){
  sitedata <- sitesdf %>% filter(site == msite)
  range <- range(seq(nrow(sitedata))[!is.na(sitedata$ufp)])
  return(sitedata$date[range])
}) ; names(firstlastlist) <- UB_sites

# remove missing periods at beginnings and end
sitesdf <- sitesdf %>%
  select(-area) %>%
  mutate(ufp = ifelse(is.na(ufp), 0, 1)) %>%
  pivot_wider(names_from = c(site), values_from = c(ufp)) %>%
  mutate(across(all_of(names(firstlastlist)), ~ {
    sitename <- cur_column()
    ifelse(date <= min(firstlastlist[[sitename]]) | date >= max(firstlastlist[[sitename]]),
           NA, .x)
  })) %>%
  pivot_longer(cols = !date, names_to = "site", values_to = "ufp")


# siteslabels <- c("belfast" = "Belfast\nCentre", "birmcen" = "Birmingham\nCentre",
#                  "birmtyb" = "Birmingham\nTyburn","glasgow" = "Glasgow\nCentre",
#                  "bloomsbury" = "London\nBloomsbury", "kensington" = "London\nNorth Kensington",
#                  "honor" = "London\nHonor Oak", "manchester" = "Manchester\nPicadilly")


sitesdf$site <- factor(sitesdf$site, levels = c("bloomsbury", "kensington", "honor",
                                                "birmcen", "birmtyb", "belfast", 
                                                "manchester", "glasgow"))
                       

# Plot
ggplot(sitesdf, aes(x = date, y = site, fill = factor(ufp))) +
  geom_raster() +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), na.value = NA) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_discrete(limits = rev)+
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# Compute availability range (first and last 1)
availability <- sitesdf %>%
  filter(indicator == 1) %>%
  group_by(area) %>%
  summarise(start = min(date), end = max(date), .groups = "drop")

# Plot
ggplot() +
  # Availability bar in light gray
  geom_segment(data = availability,
               aes(x = start, xend = end, y = area, yend = area),
               size = 8, color = "grey90", lineend = "round") +
  # Raster overlay of 0/1 pattern
  geom_raster(data = sitesdf,
              aes(x = date, y = area, fill = factor(indicator))) +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(ggplot2)
library(dplyr)

# Assume you already have sitesdf with columns: date, area, indicator

# Compute availability range (first and last 1 per area)
availability <- sitesdf %>%
  filter(indicator == 1) %>%
  group_by(area) %>%
  summarise(start = min(date), end = max(date), .groups = "drop")

# Plot: availability bar + black ticks for 1s
ggplot() +
  # Availability bar in light gray
  geom_segment(data = availability,
               aes(x = start, xend = end, y = area, yend = area),
               size = 8, color = "lightblue")+#, lineend = "round") +
  # Plot only the 1’s (so grey shows through for 0’s)
  geom_raster(data = subset(sitesdf, indicator == 1),
              aes(x = date, y = area),
              fill = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

