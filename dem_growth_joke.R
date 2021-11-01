#### Libraries --------------------

library(WDI)
library(data.table)
library(lmtest)
library(plm)
library(ggplot2)
library(showtext)

#### Global Options --------------------

font_add_google("Baloo Thambi", "baloo2")
font_add_google("Roboto", "roboto")
font_add_google("Oswald", "oswald")

#### Functions --------------------

# takes logs from positive numerics, ignores the rest
log_positives <- function(x) {
  # check if numeric
  if (is.numeric(x)) {
    # check if positive
    if (min(x, na.rm = T) > 0) {
      return(log(x))
    } else {
      return(x)
    } 
  } else {
    return(x)
  }
}

#### Data Preparation --------------------

# List of variables as in Hristos Doucouliagos and Mehmet Ulubasoglu

# Region BV: 1 = regional dummies used (region from WDI)
# Inequality BV: 1 = inequality variable included (SI.POV.GINI)
# Ecofreedom BV: 1 = economic freedom included (v2xcl_prpty)
# Instability BV: 1 = political instability control included (e_miinteco, e_miinterc )
# Inflation BV: 1 = controls for inflation included (FP.CPI.TOTL.ZG)
# Population BV: 1 = controls for population included (SP.POP.TOTL)
# Convergence BV: 1 = controls for initial income included (NY.GDP.PCAP.KD.ZG)
# Human Capital BV: 1 = controls for human capital included (SE.SEC.ENRR.FE, SP.DYN.LE00.FE.IN)
# Physical Capital BV: 1 = controls for physical capital included (NE.GDI.TOTL.KD.ZG)
# Openness BV: 1 = controls for foreign trade included (NE.TRD.GNFS.ZS)
# Govt Size BV: 1 = controls for government included (GC.XPN.TOTL.GD.ZS)

wdi_indicators <- c("NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                    "NY.GDP.PCAP.KD", # GDP per capita
                    "SI.POV.GINI", # GINI
                    "FP.CPI.TOTL.ZG", # Inflation, consumer prices (annual %)
                    "SP.POP.TOTL", # Population
                    "SP.DYN.LE00.FE.IN", # Life expectancy at birth, female (years)
                    "SE.SEC.ENRR.FE", # School enrollment, secondary, female (% gross)
                    "NE.GDI.TOTL.KD.ZG", # Gross capital formation (annual % growth)
                    "NE.TRD.GNFS.ZS", # Trade (% of GDP)
                    "GC.XPN.TOTL.GD.ZS" # Expense (% of GDP)
)

# World Bank Data
dt_wdi <- WDI(country   = "all",
              indicator = wdi_indicators,
              start     = "1960",
              end       = "2018",
              extra     = TRUE,
              cache     = NULL)

#write.csv(file = "wdi_data.csv", dt_wdi)
#dt_wdi <- fread("wdi_data.csv")[,-c("V1")]

dt_wdi <- data.table(dt_wdi)

# get rid of aggregates
dt_wdi <- dt_wdi[region != "Aggregates"]

# get V-DEM data
dt_vdem <- fread("/Users/aliaksandrkazlou/dev/V-Dem-DS-CY+Others-v7.1.csv")
# create a new 'conflict' variable 
dt_vdem[,conflict := ifelse(e_miinteco + e_miinterc > 0, 1, 0)]
dt_vdem[,freedom_house := (e_fh_pr + e_fh_cl)/2]

# select rows and columns
dt_vdem <- dt_vdem[year  >= "1960" & dt_vdem$year < "2019", .(iso3c = country_text_id, 
                                                              year,
                                                              e_polity2,
                                                              freedom_house,
                                                              economic_freedom = v2xcl_prpty,
                                                              conflict)]
# Merging two datasets
dt_main <- dt_wdi[dt_vdem, nomatch=0, on = c("iso3c", "year")]
dt_main <- dt_main[,-c("iso2c", "iso3c", "capital",
                       "longitude", "latitude", "income",
                       "lending")]

# Renaming variables
old_names <- c("NY.GDP.PCAP.KD.ZG", "NY.GDP.PCAP.KD", "SI.POV.GINI",
               "FP.CPI.TOTL.ZG", "SP.POP.TOTL", "SP.DYN.LE00.FE.IN",
               "SE.SEC.ENRR.FE", "NE.GDI.TOTL.KD.ZG", "NE.TRD.GNFS.ZS",
               "GC.XPN.TOTL.GD.ZS")

new_names <- c("economic_growth", "gdp_level", "inequality",
               "inflation", "population", "life_expectancy",
               "enrollment", "physical_capital", "trade",
               "gov_size")

setnames(dt_main, old_names, new_names)
# make all variables lag 1 year in respect to economic growth
dt_main[, economic_growth := shift(economic_growth, 1, type = "lead"), by = country]

# subsetting variables
controls <- c("gdp_level", "economic_freedom", "inequality",
              "population", "inflation", "life_expectancy",
              "enrollment", "physical_capital", "trade",
              "gov_size", "conflict") # "region", 
dependent_var <- "economic_growth"
independent_vars <- c("e_polity2", "freedom_house")

#dt_main <- dt_main[ , (controls) := lapply(.SD, log_positives), .SDcols = controls]
dt_main$year <- as.factor(as.character(dt_main$year))

controls_combinations <- lapply(5:length(controls),
                                function (x) {
                                  combn(controls, x, simplify = FALSE)
                                }
)
controls_combinations <- unlist(controls_combinations, recursive=FALSE)

# operationalizing democracy as Polity index
formulas_polity <- sapply(controls_combinations, function (x) {
  as.formula(paste(dependent_var,
                   paste(c(x, independent_vars[1]), collapse=" + "),
                   sep=" ~ "))
})

# operationalizing democracy as Freedom House index
formulas_fh <- sapply(controls_combinations, function (x) {
  as.formula(paste(dependent_var,
                   paste(c(x, independent_vars[2]), collapse=" + "),
                   sep=" ~ "))
})

# combine both operationalizations
formulas <- append(formulas_polity, formulas_fh)


#### Building a pipe-line --------

re <- c("HC0", "HC3")
to_log <- c(TRUE, FALSE)

specifications <- expand.grid(formula = formulas,
                              robust_errors = re,
                              log = to_log)

Sys.time()

t_stats <- c()
p_values <- c()
for (i in 1:nrow(specifications)) {
  specification <- specifications[i,]
  dt_tmp <- copy(dt_main)
  if (specification$log == TRUE) {
    dt_tmp[ , (controls) := lapply(.SD, log_positives), .SDcols = controls]
 }
  
  tmp_model <- plm(specification$formula[[1]],
                   data = dt_tmp,
                   index=c("country", "year"),
                   model = "within")
  r_coefs <- tryCatch(coeftest(tmp_model, 
                               vcovHC(tmp_model, type = as.character(specification$robust_errors[[1]]))),
                               error = function(e) c(NA))
  if (any(rownames(r_coefs) %in% independent_vars)) {
    # return coefs for democracy is computed
    t_stats <- c(t_stats, r_coefs[rownames(r_coefs) %in% independent_vars, c(3)])
    p_values <- c(p_values, r_coefs[rownames(r_coefs) %in% independent_vars, c(4)])
  } else {
    # return NAs if computation failed
    t_stats <- c(t_stats, r_coefs)
    p_values <- c(p_values, r_coefs)
  }
}

specifications$t <- t_stats
specifications$p <- p_values

df_results <- as.data.frame(specifications)
df_results <- na.omit(df_results)
df_results$formula <- as.character(df_results$formula)
write.csv(df_results, file = "dem_growth_results.csv", row.names = FALSE)

#df_results <- read.csv("results.csv")

#### Graphs ---------------------

showtext_auto()

p <- ggplot(df_results, aes(x = t)) + 
  geom_histogram(bins = 100, fill = "red") +
  theme_minimal(base_size = 9, base_family = "baloo2") +
  theme(plot.subtitle = element_text(color = "#666666"),
        plot.caption = element_text(color = "#AAAAAA")) +
  labs(title = "T-statistic distribution",
       subtitle = "across 8034 democracy-growth regressions",
       caption = "by Aliaksandr Kazlou — akazlou.github.io")

## On-screen device
quartz()
print(p)

## PNG device
ggsave(plot = p, "democracy_growth_tstatistic.png", dpi = "retina")

p2 <- ggplot(df_results, aes(x = p)) + 
  geom_histogram(bins = 100, fill = "red") +
  theme_minimal(base_size = 9, base_family = "baloo2") +
  theme(plot.subtitle = element_text(color = "#666666"),
        plot.caption = element_text(color = "#AAAAAA")) +
  labs(title = "P-values distribution",
       subtitle = "across 8034 democracy-growth regressions",
       caption = "by Aliaksandr Kazlou — akazlou.github.io")
quartz()
print(p2)

ggsave(plot = p2, "democracy_growth_pvalue.png", dpi = "retina")

showtext_auto(FALSE)

# negative significant
sum(estimates$p[estimates$beta < 0] < 0.05, na.rm = T)/nrow(na.omit(estimates))
# negative insignificant
sum(estimates$p[estimates$beta < 0] >= 0.05, na.rm = T)/nrow(na.omit(estimates))
# positive insignificant
sum(estimates$p[estimates$beta > 0] >= 0.05, na.rm = T)/nrow(na.omit(estimates))
# positive significant
sum(estimates$p[estimates$beta > 0] < 0.05, na.rm = T)/nrow(na.omit(estimates))

# height

n <- seq(30,300000,100)
n <- c(30,60,600,15000000)
min_effect <- c()
for (i in 1:length(n)) {
  control <- rnorm(n[[i]], 167.4, 7.97)
  experiment <- rnorm(n[[i]], 167.4 + 0.5, 7.97)
  s1 <- sd(control)
  s2 <- sd(experiment)
  se <- sqrt( (s1^2/n[[i]]) + (s2^2/n[[i]]) )
  min_effect <- cbind(min_effect, se*1.96)
}
df_length <- data.frame(n = n, min_effect = as.vector(min_effect))
