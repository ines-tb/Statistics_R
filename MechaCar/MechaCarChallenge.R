
library(tidyverse)


# MPG REGRESSION
# *********************************

# Extract data:
mechaCarMpg <- read.csv(file="MechaCar_mpg.csv",check.names = F,stringsAsFactors = F)

# Multilinear regression:
# Rename column names with spaces for easier future use.
mechaCarMpg <- mechaCarMpg %>% 
    rename(
      vehicle_length = "vehicle length",
      vehicle_weight = "vehicle weight",
      spoiler_angle = "spoiler angle",
      ground_clearance = "ground clearance"
    )

# Generate multiple linear regression model:
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechaCarMpg)
# Generate summary statistics:
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechaCarMpg)) 

# Correlation matrix for checking:
carsMatrix <- as.matrix(mechaCarMpg[,c("vehicle_length","vehicle_weight","spoiler_angle","ground_clearance","AWD","mpg")]) #convert data frame into numeric matrix
cor(carsMatrix)




# SUSPENSION COIL SUMMARY
# **********************************

# Extract data
suspensionCoil <- read.csv(file="Suspension_Coil.csv",check.names = F,stringsAsFactors = F)

# Create summary table:
coilSummary <- suspensionCoil %>% group_by(Manufacturing_Lot) %>% 
      summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),StdDeviation_PSI=sd(PSI))

# Plot summary Coil results to display previous results:
coilLong <- coilSummary %>% gather(key="Measure",value="PSI_Value",c(Mean_PSI,Median_PSI,Variance_PSI,StdDeviation_PSI)) #convert to long format

plt <- ggplot(coilLong,aes(x=Manufacturing_Lot,y=Value,color=Measure, fill=Measure)) #import dataset into ggplot2
plt + geom_col() + 
  facet_wrap(vars(Measure)) + #create multiple bars, one for each Measure
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Lot") #rotate x-axis labels



# SUSPENSION COIL T-TEST
# **********************************

# T-test for compare sample versus population means
# Generate a random sample:
sampleCoil <- suspensionCoil %>% sample_n(30)
# Perform t-test
t.test(sampleCoil$PSI,mu=mean(suspensionCoil$PSI))


