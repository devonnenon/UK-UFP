################################################################################
# UFP/mortality project


# Plots: results

################################################################################
library(ggplot2)
#readRDS("results_figures/results.RData")

# Create vectors of desired results and outcomes for plots
locationsplot <- c("nkens", "wmid_pool")
outcomes <- c("nonext", "cvd", "resp")

# Create labels for facets
locationlabs <- c("London", "West Midlands")
names(locationlabs) <- c("nkens", "wmid_pool")
outcomelabs <- c("Non-external", "Cariovasular", "Respiratory")
names(outcomelabs) <- c("nonext", "cvd", "resp")
PNClab<- expression(paste(PNC," (","n/",cm^3,")"))

################################################################################
# Nonlinear E-R curves

# Create data frame with predictions in long format
nonlinplotdf <- do.call(rbind, lapply(locationsplot, function(location){
  outcomes_result <- do.call(rbind, lapply(outcomes, function(outcome){
    cpspl <- nonlinlist[[location]][[outcome]][["cpspl"]]
    cplin <- nonlinlist[[location]][[outcome]][["cplin"]]
    df <- data.frame(
      pred = cpspl[["predvar"]],
      fit = cpspl[["allRRfit"]],
      upper = cpspl[["allRRhigh"]],
      lower = cpspl[["allRRlow"]],
      fitlin = cplin[["allRRfit"]],
      upperlin = cplin[["allRRhigh"]],
      lowerlin = cplin[["allRRlow"]],
      outcome = outcome,
      location = location
    )
    return(df)
  }))
  (return(outcomes_result))
})) 

# Reorder the 'outcome' variable as a factor with the desired levels
nonlinplotdf$outcome <- factor(nonlinplotdf$outcome, levels = c("nonext", "cvd", "resp"))

# Save plot
#pdf(file = paste("results_figures/nonlinplot", as.character(format(Sys.time(), "%m-%d_%H%M")), ".pdf", sep = ""),
#    width = 8, height = 5)
# Plot
ggplot(data = nonlinplotdf, aes(x = pred)) +
  geom_hline(yintercept = 1) +
  #geom_vline(xintercept = kufp)+
  geom_line(aes(y = fit), linewidth = 0.8, color = "steelblue") +
  geom_line(aes(y = fitlin), linewidth = 0.7, color = "grey30",linetype = 2)+
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2, fill = "steelblue1") +
  geom_ribbon(aes(ymax = upperlin, ymin = lowerlin), alpha = 0.2, fill = "grey70")+
  theme_bw() +
  scale_x_continuous(limits = c(0, maxpred), n.breaks = 3) +
  theme(panel.grid = element_line(linetype = 3)) +
  labs(y = "RR", x = PNClab) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  facet_grid(location ~ outcome, 
             switch = "y", 
             #scales = "free_y", 
             labeller = labeller(outcome = outcomelabs, location = locationlabs))
#dev.off()

################################################################################
# Extended lag structure 

# Create data frame
lagsplotdf <- do.call(rbind, lapply(locationsplot, function(location){
  outcomes_result <- do.call(rbind, lapply(outcomes, function(outcome){
    totalest <- extlaglist[[location]][[outcome]][["estRR"]]
    lagsest <- extlaglist[[location]][[outcome]][["tabRR"]]
    df <- data.frame(
      est = c(totalest[1], lagsest[,1]),
      est_cilow = c(totalest[2], lagsest[,2]),
      est_cihigh = c(totalest[3], lagsest[,3]),
      lag = c(-2, 0, 1, 2, 3, 4, 5),
      type = c("cumul", rep("indiv", 6)),
      location = location,
      outcome = outcome
    )
    return(df)
  }))
  (return(outcomes_result))
})) 

colors <- c("cumul" = "steelblue1", "indiv" = "black")

# save plot 
#pdf(file = paste("results_figures/lagsplot", as.character(format(Sys.time(), "%m-%d_%H%M")), ".pdf", sep = ""),
#    width = 8, height = 4)
# Plot
lagsplotdf %>%
  ggplot(aes(x = lag, color = type))+
  geom_hline(yintercept = 1, color = "grey", linetype = 3)+
  geom_point(aes(y = est), shape = 18, size = 2)+
  geom_errorbar(aes(ymin = est_cilow, ymax = est_cihigh), linewidth = 0.5, width = 0.25) + 
  scale_color_manual(values = colors) +
  theme(panel.grid = element_line(linetype = 3)) +
  labs(y = "% change per 10,000 particle/cm3 increase", x = "Lag") +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())+
 # scale_y_continuous(limits = c(-6, 6))+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4, 5), 
                     labels = c("Net", "", 0, 1, 2, 3, 4, 5))+
  theme(strip.background = element_blank(), strip.placement = "outside") +
  facet_grid(location ~ outcome, 
             switch = "y", 
             #scales = "free_y", 
             labeller = labeller(outcome = outcomelabs, location = locationlabs))
#dev.off()


################################################################################
# Interrupted - effect before and after 2008

# Locations used in this analysis
locationlabsint <- c("nkens" = "London", "birmcen" = "W. Midlands")

# Data frame
intplotdf <- do.call(rbind, lapply(names(intlist), function(location){
  outcomes_result <- do.call(rbind, lapply(outcomes, function(outcome){
    est <- intlist[[location]][[outcome]][["estRR"]]
    df <- data.frame(
      est = est[,1],
      est_cilow = est[,2],
      est_cihigh = est[,3],
      when = c("pre", "post"),
      location = location,
      outcome = outcome
    )
    return(df)
  }))
  (return(outcomes_result))
})) 


#pdf(file = paste("results_figures/intplot", as.character(format(Sys.time(), "%m-%d_%H%M")), ".pdf", sep = ""),
#    width = 8, height = 4)
# Plot
intplotdf %>%
  ggplot(aes(x = when))+
  geom_hline(yintercept = 1, color = "grey", linetype = 2)+
  geom_point(aes(y=est), shape = 18)+
  geom_errorbar(aes(ymin = est_cilow, ymax = est_cihigh), linewidth = 0.5, width = 0.25) + 
  theme_bw()+
  labs(x = "Period", y = "RR")+
  theme(strip.background = element_blank(), strip.placement = "outside") +
  facet_grid(location ~ outcome, 
             switch = "y", 
             #scales = "free_y", 
             labeller = labeller(outcome = outcomelabs, location = locationlabsint))
#dev.off()

################################################################################
# Scatterplots 

# Ensure date is in Date format
ufpdf$date <- as.Date(ufp$date)

# Create labels for this plot
variablelabs <- c("PNC\n(n/cm3)","Non-external\n(n/day)", "Cariovasular\n(n/day)", "Respiratory\n(n/day)")
names(variablelabs) <- c("ufp","nonext", "cvd", "resp")
locationlabs2 <- c("London", "West Midlands")
names(locationlabs2) <- c("london", "wmid")

# Create data frame 
scatterdf <- do.call(rbind,lapply(names(dlist), function(site){
  data <- dlist[[site]]
  data$site <- site
  data$area <- ifelse(site=="nkens", "london", "wmid")
  return(data)
})) %>%
  select(date, area, ufp, nonext, cvd, resp)%>%
  pivot_longer(cols = c(ufp, nonext, cvd, resp))%>%
  mutate(identifier = paste0(area, ".", name))%>%
  select(-area, -name)%>%
  pivot_wider(names_from = identifier, 
              values_from = value) %>%
  pivot_longer(cols = -date, 
               names_to = c("area", "type"), 
               names_pattern = "(.*)\\.(.*)", 
               values_to = "value") %>%
  filter(!(area == "wmid" & date > as.Date("2013-12-31")))

# Plot
ggplot(data = scatterdf, aes(x = date, y = value))+ 
  geom_vline(data = scatterdf[which(scatterdf$area == "wmid"),], 
             aes(xintercept = as.Date("2009-02-12")), #
             color = "steelblue", linetype = 2)+
  geom_vline(xintercept = as.Date("2008-01-01"), #
             color = "steelblue1", linetype = 2)+
  geom_point(size = 0.1) +
  theme_bw() +
  labs(x = "Date", y = "")+
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(panel.grid = element_line(linetype = 3)) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "2 year") +
  facet_grid(type~area, scales = "free", switch = "y",
             labeller = labeller(type = variablelabs, area = locationlabs2))

