######################################
####                              ####
####    Descriptive statistics    ####
####                              ####
######################################

#### Descriptive statistics of variables - Mean ####

descriptive_mean <- data.frame(vdem3[, c("v2x_libdem_rev", "v2x_liberal_rev", "v2x_polyarchy_rev", 
                                         "delta_libdem_rev", "delta_liberal_rev", "delta_polyarchy_rev",
                                         "mean_world_change_rev_lag1", "mean_world_change1_rev_lag1", 
                                         "mean_world_change2_rev_lag1", "mean_region_change_rev_lag1", 
                                         "mean_region_change1_rev_lag1", "mean_region_change2_rev_lag1", 
                                         "delta_libdem_rev_lag1", "delta_liberal_rev_lag1", 
                                         "delta_polyarchy_rev_lag1", "e_migdpgrolns", "population", 
                                         "e_miurbani", "e_peaveduc")])
descriptive_neighbor_mean <- data.frame(neighbor_mean[, c("mean_neighbor_change_rev_lag1", 
                                                          "mean_neighbor_change1_rev_lag1", 
                                                          "mean_neighbor_change2_rev_lag1")])

## Exporting mean tables
stargazer(descriptive_mean,  
          type = "latex", 
          out = "descriptive_mean.tex",
          title = "Descriptive Statistics", 
          digits = 3, 
          covariate.labels = c("LDI", "LCI", "EDI", "Change in LDI", "Change in LCI", "Change in EDI", 
                               "Mean world change LDI lag", "Mean world change LCI lag", 
                               "Mean world change EDI lag", "Mean region change LDI lag", 
                               "Mean region change LCI lag", "Mean region change EDI lag", 
                               "Change in LDI lag", "Change in LCI lag", "Change in EDI lag",
                               "Ln GDP Growth", "Ln Population", "Urbanization", "Education"),
          summary.stat = c("n", "min", "max", "mean", "median", "sd"), 
          font.size = "footnotesize", 
          no.space = TRUE)

stargazer(descriptive_neighbor_mean, 
          type = "latex", 
          out = "descriptive_mean_neighbor.tex",
          title = c("Descriptive Statistics Mean Neighbor"),
          digits = 3, 
          covariate.labels = c("Mean neighbor change LDI lag", "Mean neighbor change LCI lag", 
                               "Mean neighbor change EDI lag"), 
          summary.stat = c("n", "min", "max", "mean", "median", "sd"), 
          font.size = "footnotesize", 
          no.space = TRUE)

## Frequency tables

# Civil war
table_civilwar <- table(vdem3$e_civil_war, useNA = "ifany")
table_civilwar 
prop.table(table_civilwar)*100

# Coup d'etat
table_coup <- table(vdem3$e_pt_coup_n, useNA = "ifany")
table_coup
prop.table(table_coup)*100 

#### Descriptive statistics of variables - Percentage ####

descriptive_pct <- data.frame(vdem4[, c("percentage_world_aut_lag1", "percentage_world_aut1_lag1", "percentage_world_aut2_lag1",
                                              "percentage_region_aut_lag1", "percentage_region_aut1_lag1", "percentage_region_aut2_lag1")])
descriptive_neighbor_pct <- data.frame(neighbor_percentage[, c("percentage_neighbor_aut_lag1", "percentage_neighbor_aut1_lag1", 
                                                               "percentage_neighbor_aut2_lag1")])

## Exporting percentage tables
stargazer(descriptive_vdem4_pct, 
          type = "latex",
          out = "descriptive_pct.tex",
          title = c("Descriptive Statistics Pct"), 
          digits = 3, 
          covariate.labels = c("Pct world aut LDI lag", "Pct world aut LCI lag", "Pct world aut EDI lag", 
                               "Pct region aut LDI lag", "Pct region aut LCI lag", "Pct region aut EDI lag"), 
          summary.stat = c("n", "min", "max", "mean", "median", "sd"), 
          font.size = "footnotesize", 
          notes = "Percentage as shares from 0 to 1")

stargazer(descriptive_neighbor_pct, 
          type = "latex", 
          out = "descriptive_pct_neighbor.tex",
          title = c("Descriptive Statistics Pct Neighbor"),
          digits = 3, 
          covariate.labels = c("Pct neighbor aut LDI lag", "Pct neighbor aut LCI lag", "Pct neighbor aut EDI lag"), 
          summary.stat = c("n", "min", "max", "mean", "median", "sd"), 
          font.size = "footnotesize", 
          notes = "Percentage as shares from 0 to 1")

#################################
####                         ####
####    Bivariate analysis   ####
####                         ####
#################################

########################################
#### Checking for Multicollinearity ####
########################################

## Correlation all control variables 
R <- cor(vdem3[, c("delta_libdem_rev", "delta_liberal_rev", "delta_polyarchy_rev", "e_migdpgrolns", 
                   "population", "e_peaveduc", "e_miurbani", "e_civil_war", "e_pt_coup_n")],
         use = "pairwise.complete.obs", # eller complete.obs
         method = "pearson")
R

stargazer(R, type = "latex", out = "multicollinearity.tex", 
          title = "Correlation Matrix", 
          covariate.labels = c(" ","Change in LDI", "Change in LCI", "Change in EDI", "Ln GDP Growth", 
                               "Ln Population", "Education", "Urbanization", "Civil War", "Coup d'etat"), 
          font.size = "footnotesize", 
          no.space = TRUE,
          column.sep.width = "-5pt",
          float.env = "sidewaystable")

## Correlation between dependent variable and two control variables ##

## Coup d'etat
R_coup <- cor(x = vdem3$delta_libdem_rev, y = vdem3$e_pt_coup_n,
              use = "complete.obs", 
              method = "pearson")
R_coup # 0.156 (weak positive correlation) 

R_coup_test <- cor.test(vdem3$delta_libdem_rev, y = vdem3$e_pt_coup_n,
                        use = "complete.obs", 
                        method = "pearson")
R_coup_test
#	95 percent confidence interval  between 0.13 and 0.17. In 95 out of 100 analyses of the same population, the 
# correlation between X and Y will be between the interval 0.17 and 0.13. We read the p-value (< 2.2e-16) as 
# very low, indicating that it is highly unlikely that a dataset with this correlation will be drawn at random. 

## Urbanization
r_urb <- cor(x = vdem3$delta_libdem_rev, y = vdem3$e_miurbani, 
             use = "complete.obs", 
             method = "pearson")
r_urb # -0.01 (svært svak positiv korrelasjon)

R_urb_test <- cor.test(vdem3$delta_libdem_rev, y = vdem3$e_miurbani,
                        use = "complete.obs", 
                        method = "pearson")
R_urb_test
# 05 percent confidence interval between -0.02 and 0.006. The p-value is 0.2, greater than 0.05, indicating 
# that the correlation can be drawn at random and is therefore not statistically significant. 

## Education
r_edu <- cor(x = vdem3$delta_libdem_rev, y = vdem3$e_peaveduc, 
             use = "complete.obs", 
             method = "pearson")
r_edu # -0.02


