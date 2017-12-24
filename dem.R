library("ggpubr")
dem=read.csv("MyData.csv")

m=cor(dem$life_expectancy,dem$radio_television_per_cap)
cat("   Corelation  lE and RTPC /n ", m)

m=cor(dem$life_expectancy,dem$education15)
cat("   Corelation  lE and EDU /n  ", m)

m=cor(dem$life_expectancy,dem$urban_population_pct)
cat("   Corelation  lE and UPPCT /n  ", m)

m=cor(dem$life_expectancy,dem$political_stability)
cat("   Corelation  lE and GE  ", m)

m=cor(dem$life_expectancy,dem$political_stability)
cat("   Corelation  lE and PS  ", m)

m=cor(dem$life_expectancy,dem$gdp_per_cap)
cat("   Corelation  lE and GDPPC  ", m)







  ggscatter(dem, x = "gdp_per_cap", y = "life_expectancy", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = "pearson",
                       xlab = "GDP", ylab = "Life_Expectancy")
ggscatter(dem, x = "political_stability", y = "life_expectancy", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                     xlab = "political_stability", ylab = "Life_Expectancy")
ggscatter(dem, x = "government_effectiveness", y = "life_expectancy", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "government_effectiveness", ylab = "Life_Expectancy")
 ggscatter(dem, x = "urban_population_pct", y = "life_expectancy",      add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                      xlab = "urban_population_pct", ylab = "Life_Expectancy")
ggscatter(dem, x = "education15", y = "life_expectancy", 
                      add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                     xlab = "education15", ylab = "Life_Expectancy")
ggscatter(dem, x = "radio_television_per_cap", y = "life_expectancy", 
                     add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "radio_television_per_cap", ylab = "Life_Expectancy")

model1=lm(life_expectancy ~ radio_television_per_cap + gdp_per_cap + 
     political_stability + urban_population_pct + government_effectiveness + 
     education15+ healthcare, data = dem
)
summary(model1)
summary(dem$life_expectancy)
summary(dem$education15)
summary(dem$political_stability)
summary(dem$radio_television_per_cap)
summary(dem$urban_population_pct)
summary(dem$gdp_per_cap)
summary(dem$government_effectiveness)


