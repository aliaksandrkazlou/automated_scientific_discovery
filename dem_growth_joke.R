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
  # check if numeric and positive
  if (is.numeric(x) & (min(x, na.rm = T)) > 0) {
      return(log(x))
  } else {
    return(x)
  }
}

#### Data Preparation --------------------

# Indepedent Variables summary

# Inequality: control for inequality (SI.POV.GINI)
# Ecofreedom: control for economic freedom (v2xcl_prpty)
# Conflict: control for external and internal conflicts (e_miinteco, e_miinterc )
# Inflation: control for inflation (FP.CPI.TOTL.ZG)
# Population: controls for population size (SP.POP.TOTL)
# Convergence: controls for initial income (NY.GDP.PCAP.KD.ZG)
# Human Capital: controls for life expectancy 
# or secondary school enrollment (SE.SEC.ENRR.FE, SP.DYN.LE00.FE.IN)
# Physical Capital: controls for physical capital (NE.GDI.TOTL.KD.ZG)
# Openness: controls for import and export (NE.TRD.GNFS.ZS)
# Govt Size: controls for government expenses (GC.XPN.TOTL.GD.ZS)

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

# Download WDI data
dt_wdi <- WDI(country   = "all",
              indicator = wdi_indicators,
              start     = "1960",
              end       = "2018",
              extra     = TRUE,
              cache     = NULL)

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
# Merge two datasets
dt_main <- dt_wdi[dt_vdem, nomatch=0, on = c("iso3c", "year")]
dt_main <- dt_main[,-c("iso2c", "iso3c", "capital",
                       "longitude", "latitude", "income",
                       "lending")]

# Rename variables
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

# subset variables
controls <- c("gdp_level", "economic_freedom", "inequality",
              "population", "inflation", "life_expectancy",
              "enrollment", "physical_capital", "trade",
              "gov_size", "conflict")
dependent_var <- "economic_growth"
independent_vars <- c("e_polity2", "freedom_house")

dt_main$year <- as.factor(as.character(dt_main$year))

controls_combinations <- lapply(5:length(controls),
                                function (x) {
                                  combn(controls, x, simplify = FALSE)
                                }
)
controls_combinations <- unlist(controls_combinations, recursive=FALSE)

# operationalize democracy as Polity index
formulas_polity <- sapply(controls_combinations, function (x) {
  as.formula(paste(dependent_var,
                   paste(c(x, independent_vars[1]), collapse=" + "),
                   sep=" ~ "))
})

# operationalize democracy as Freedom House index
formulas_fh <- sapply(controls_combinations, function (x) {
  as.formula(paste(dependent_var,
                   paste(c(x, independent_vars[2]), collapse=" + "),
                   sep=" ~ "))
})

# combine both operationalizations
formulas <- append(formulas_polity, formulas_fh)


#### Building a pipe-line --------

# SE to use
re <- c("HC0", "HC3")

# Log transform all IVs including conflict
# Inspired by log NAICS
to_log <- c(TRUE, FALSE)

# grid for all possible specifications
specifications <- expand.grid(formula = formulas,
                              robust_errors = re,
                              log = to_log)

# run the pipe-line
Sys.time()

t_stats <- c()
p_values <- c()
for (i in 1:nrow(specifications)) {
  specification <- specifications[i,]
  dt_tmp <- copy(dt_main)
  if (specification$log == TRUE) {
    dt_tmp[ , (controls) := lapply(.SD, log_positives), .SDcols = controls]
 }
  
  # fit a regression model
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
