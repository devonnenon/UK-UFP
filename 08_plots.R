################################################################################
# Reproducible code for the analysis of non-accidental mortality in:
# 
#   Mortality risks associated with short-term exposure to ultrafine particles 
#   in London and the West Midlands
# 
#   Nenon D, Fuller G, Masselot P, Gasparrini, A.
#   Environmental Epidemiology - 2025
#
#
# Results: plots (non-accidental mortality)
#
################################################################################

library(ggplot2)

# Select locations to include in results tables and define lavels
locationsplot <- c("kensington", "wmid_pool")
locationlabs <- c("London", "West Midlands")
names(locationlabs) <- c("kensington", "wmid_pool")

# Plotting labels
PNClab<- bquote("PNC (n/"*cm^3*")")
RRlab <- bquote("Risk ratio per"~ .(unitinc)~"n/"~cm^3~"increase")


################################################################################
# Nonlinear E-R curves

# Create data frame with predictions in long format
nonlinplotdf <- do.call(rbind, lapply(locationsplot, function(location){
  cpspl <- nonlinlist[[location]][["cpspl"]]
  cplin <- nonlinlist[[location]][["cplin"]]
  df <- data.frame(
    pred = cpspl[["predvar"]],
    fit = cpspl[["allRRfit"]],
    upper = cpspl[["allRRhigh"]],
    lower = cpspl[["allRRlow"]],
    fitlin = cplin[["allRRfit"]],
    upperlin = cplin[["allRRhigh"]],
    lowerlin = cplin[["allRRlow"]],
    location = location
  )
  return(df)
})) 

# Plot
ggplot(data = nonlinplotdf, aes(x = pred)) +
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
  geom_line(aes(y = fit), linewidth = 0.8, color = "steelblue") +
  geom_line(aes(y = fitlin), linewidth = 0.5, color = "grey50") +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2, fill = "steelblue1") +
  geom_ribbon(aes(ymax = upperlin, ymin = lowerlin), alpha = 0.2, fill = "grey70") +
  theme_bw() +
  scale_x_continuous(limits = c(0, maxpred), n.breaks = 3) +
  theme(panel.grid = element_line(linetype = 3)) +
  labs(y = "Risk ratio", x = PNClab,
       title = "Figure 1") +
  theme(strip.background = element_blank(), strip.placement = "outside",
        plot.title = element_text(hjust = 0)) +
  facet_wrap("location", labeller = labeller(location = locationlabs))


################################################################################
# Extended lag structure 

# Create data frame
lagsplotdf <- do.call(rbind, lapply(locationsplot, function(location){
  totalest <- extlaglist[[location]][["estRR"]]
  lagsest <- extlaglist[[location]][["tabRR"]]
  df <- data.frame(
    est = c(totalest[1], lagsest[,1]),
    est_cilow = c(totalest[2], lagsest[,2]),
    est_cihigh = c(totalest[3], lagsest[,3]),
    lag = c(-2, 0, 1, 2, 3, 4, 5),
    type = c("cumul", rep("indiv", 6)),
    location = location
  )
  return(df)
})) 

# Define colors
colors <- c("cumul" = "steelblue", "indiv" = "black")

# Plot
lagsplotdf %>%
  ggplot(aes(x = lag, color = type))+
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed")+
  geom_point(aes(y = est), shape = 18, size = 2)+
  geom_errorbar(aes(ymin = est_cilow, ymax = est_cihigh), linewidth = 0.5, width = 0.25) + 
  scale_color_manual(values = colors) +
  labs(y = "Risk ratio", x = "Lag",
       title = "Figure 2") +
  theme_bw() + 
  theme(panel.grid = element_line(linetype = 3)) +
  theme(legend.position = "none") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_x_continuous(breaks = c(-2, 0, 1, 2, 3, 4, 5), 
                     labels = c("Net", 0, 1, 2, 3, 4, 5))+
  theme(strip.background = element_blank(), strip.placement = "outside") +
  facet_wrap(vars(location), 
             labeller = labeller( location = locationlabs))



################################################################################
# Interrupted - effect before and after 2008

# Locations used in this analysis
locationlabsint <- c("kensington" = "London", "birmcen" = "West Midlands")

# Data frame
intplotdf <- do.call(rbind, lapply(names(intlist), function(location){
  est <- intlist[[location]][["estRR"]]
  df <- data.frame(
    est = est[,1],
    est_cilow = est[,2],
    est_cihigh = est[,3],
    when = c("pre", "post"),
    location = location
  )
  return(df)
})) 

# Reorder the pre/post to plot chronologically 
intplotdf$when <- factor(intplotdf$when, levels = c("pre", "post"))

# Define shared y axis limit
ylimint <- range(intplotdf[1:3])

londonintplot <- ggplot(intplotdf %>% filter(location == "kensington"), aes(x = when))+
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed")+
  geom_point(aes(y=est), shape = 18, size = 2)+
  geom_errorbar(aes(ymin = est_cilow, ymax = est_cihigh), linewidth = 0.5, width = 0.15) + 
  theme_bw()+
  theme(panel.grid = element_line(linetype = 3)) +
  labs(x = "", y = "")+
 # theme(axis.title.y = element_text(vjust = 1))+
  scale_x_discrete(breaks = c("pre", "post"), 
                   labels = c("2003-2007", "2008-2018"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_y_continuous(limits = ylimint)+
  theme(strip.background = element_blank(), strip.placement = "outside") +
  facet_wrap(vars(location), 
             labeller = labeller(location = locationlabsint))

wmidintplot <- ggplot(intplotdf %>% filter(location != "kensington"), aes(x = when))+
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed")+
  geom_point(aes(y=est), shape = 18, size = 2)+
  geom_errorbar(aes(ymin = est_cilow, ymax = est_cihigh), linewidth = 0.5, width = 0.15) + 
  theme_bw()+
  theme(panel.grid = element_line(linetype = 3)) +
  labs(x = "", y = "")+
  scale_x_discrete(breaks = c("pre", "post"), 
                   labels = c("2003-2007", "2008-2009"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(strip.background = element_blank(),  strip.placement = "outside") +
  scale_y_continuous(limits = ylimint)+
  facet_wrap(vars(location), 
             labeller = labeller(location = locationlabsint))

intplot <- londonintplot | wmidintplot

wrap_elements(intplot) +
  labs(tag = "Risk ratio", title = "Figure 3")+
  theme(plot.tag = element_text(size= rel(1), angle = 90),
        plot.tag.position = c(0.02,0.5), plot.title = element_text(hjust = 0))

