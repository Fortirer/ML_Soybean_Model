# script tentando converter python para R da interação Tripla

setwd("C:/Users/LAFIECO/Downloads")

df1 <- read.csv("soybeanBiomassTemp.csv", sep=";")
df2 <- read.csv("soybeanBiomassDro.csv", sep=";")

# Combine the data frames
combined_data <- rbind(transform(df1, Dataset='df1'), transform(df2, Dataset='df2'))

# Create an interaction term for Elev/Drought and Elev/Temp
combined_data$Elev_Interaction <- as.integer(combined_data$Trataments %in% c('Elev/Temp'))

# Convert categorical columns to numeric
for (column in names(combined_data)[sapply(combined_data, is.factor)]) {
  combined_data[[column]] <- as.integer(as.numeric(combined_data[[column]]))
}

# Check for missing values
print(colSums(is.na(combined_data)))

# Drop rows with missing values (you may need to handle missing values differently based on your data)
combined_data <- combined_data[complete.cases(combined_data), ]

# Fit a linear model with the interaction term
model <- lm(Grain ~ Elev_Interaction-1, data = combined_data)

# Print the model summary
summary(model)

str(combined_data)


combined_data$Elev_Interaction
