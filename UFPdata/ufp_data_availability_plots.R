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


datamissingrate <- do.call(rbind, lapply(UB_sites, function(msite){
  sitedata <- sitesdf %>% filter(site == msite)
  perc <- sum(sitedata$ufp, na.rm = T) / nrow(sitedata[!is.na(sitedata$ufp),])
  return(data.frame(site = msite, missing = perc*100))
})) #; names(datamissingrate) <- UB_sites

siteslabels <- c("bloomsbury" = "London\nBloomsbury", "kensington" = "London\nNorth Kensington",
                 "honor" = "London\nHonor Oak", "birmcen" = "Birmingham\nCentre",
                 "birmtyb" = "Birmingham\nTyburn", "belfast" = "Belfast Centre",
                 "manchester" = "Manchester\nPicadilly","glasgow" = "Glasgow\nCentre")

sitesvec <- c("bloomsbury", "kensington", "honor", "birmcen", "birmtyb", 
              "belfast", "manchester", "glasgow")


sitesdf$site <- factor(sitesdf$site, levels = c("bloomsbury", "kensington", "honor",
                                                "birmcen", "birmtyb", "belfast", 
                                                "manchester", "glasgow"))
                       
pdf(file = paste("results_figures/dataavailabilityplot", as.character(format(Sys.time(), "%m-%d_%H%M")), ".pdf", sep = ""),
    width = 8, height = 4)

# Plot
ggplot(sitesdf, aes(x = date, y = site, fill = factor(ufp))) +
  geom_raster() +
  scale_fill_manual(values = c("0" = "tomato", "1" = "dodgerblue4"),
                               labels = c("No data", "Data available"), na.translate = F) + #na.value = NA) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_discrete(limits = rev, breaks = sitesvec, labels = siteslabels) +
  # geom_text(
  #   data = datamissingrate,
  #   aes(x = as.Date("2002-06-30"), y = site, label = paste0(round(missing, 0), "%")),
  #   inherit.aes = F
  # ) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Measurement site",
    fill = "Daily data availability"
  ) +
  theme(
    legend.position = c(0.87, 0.15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.minor.x = element_line(colour = "grey85", linewidth = 0.3),
  )

dev.off()


