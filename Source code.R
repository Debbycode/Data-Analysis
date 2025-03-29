# Script Name: Source code.R
# Author: Deborah
# Date: March 8, 2025
# Time: 15:30:00


# -------------------------------------
# Load Necessary Libraries
# -------------------------------------
library(cluster)    # For K-medoids clustering
library(ggplot2)    # For visualization
library(factoextra) # For cluster evaluation
library(dplyr)      # For data manipulation
library(openxlsx)   # For saving outputs to Excel
library(ggplot2)

# -------------------------------------
# Load and Preprocess Dataset
# -------------------------------------
df <- read.csv("dataset.csv")

# Convert categorical variables to numerical using one-hot encoding
df$roast <- as.numeric(as.factor(df$roast))  # Convert 'roast' to numeric
df$loc_country <- as.numeric(as.factor(df$loc_country))  # Convert 'loc_country' to numeric

# Select relevant variables for clustering
data_cluster <- df[, c("X100g_USD", "rating", "roast", "loc_country")]

# Handle missing values
data_cluster <- na.omit(data_cluster)

# Standardize numerical variables for better clustering performance
data_cluster <- scale(data_cluster)

# -------------------------------------
# Question 1: Perform K-Medoids Clustering (2 to 9 clusters)
# -------------------------------------
cat("\nPerforming K-Medoids Clustering...\n")

# Initialize vector to store silhouette scores
silhouette_scores <- numeric(8)

for (k in 2:9) {
  kmed <- pam(data_cluster, k)
  silhouette_scores[k - 1] <- mean(silhouette(kmed$clustering, dist(data_cluster))[, 3])
}

# Determine the best number of clusters (highest silhouette score)
best_k <- which.max(silhouette_scores) + 1  
cat("\nThe optimal number of clusters based on the silhouette score is:", best_k, "\n")

# Run K-medoids with the optimal number of clusters
final_kmed <- pam(data_cluster, best_k)

# Add cluster labels to the original dataset
df$Cluster <- as.factor(final_kmed$clustering)

# Visualize the clusters
fviz_cluster(final_kmed, data = data_cluster, geom = "point", ellipse = TRUE) +
  ggtitle("K-Medoids Clustering Results")

# -------------------------------------
# Print & Save Outputs for Question 1
# -------------------------------------
# Create an Excel file to store results
output_file <- "Kmedoids_Outputs.xlsx"

# Create a list to store outputs
outputs_q1 <- list(
  "Silhouette Scores" = data.frame(Cluster_Number = 2:9, Silhouette_Score = silhouette_scores),
  "Optimal Clustering" = data.frame(df)
)

# Save outputs to an Excel file
write.xlsx(outputs_q1, file = output_file, sheetName = "Outputs_Q1", append = FALSE)


# Create a data frame for silhouette scores
silhouette_df <- data.frame(
  Cluster_Number = 2:9,
  Silhouette_Score = silhouette_scores
)

# Plot the silhouette scores
ggplot(silhouette_df, aes(x = Cluster_Number, y = Silhouette_Score)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points for each cluster number
  ggtitle("Silhouette Scores for Different Cluster Numbers") +
  xlab("Number of Clusters (k)") +
  ylab("Silhouette Score") +
  theme_minimal()

# Print Silhouette Scores in Console
cat("\nSilhouette Scores for Each Cluster:\n")
print(data.frame(Cluster_Number = 2:9, Silhouette_Score = silhouette_scores))

cat("\nOutputs for Question 1 saved successfully in", output_file, "\n")

# -------------------------------------
# Question 2: Compare Summary Statistics for Two Cluster Solutions
# -------------------------------------
cat("\nComparing Summary Statistics for 2 Clusters and", best_k, "Clusters...\n")

# Apply K-medoids clustering for k = 2 (to compare)
kmed_2 <- pam(data_cluster, 2)

# Compute summary statistics for both cluster solutions
summary_2 <- aggregate(data_cluster, by = list(Cluster = kmed_2$clustering), FUN = mean)
#summary_best <- aggregate(data_cluster, by = list(Cluster = final_kmed$clustering), FUN = mean)

# -------------------------------------
# Print & Save Outputs for Question 2
# -------------------------------------
# Create a list to store outputs
outputs_q2 <- list(
  "Summary_2_Clusters" = summary_2
)

# Define the dynamic sheet name for the best-k summary
#best_k_name <- paste0("Summary_", best_k, "_Clusters")

# Add the best-k summary to the list using the defined name
#outputs_q2[[best_k_name]] <- summary_best

# Append outputs for Question 2 in the same Excel file
#write.xlsx(outputs_q2, file = output_file, sheetName = "Outputs_Q2", append = TRUE)

# Print Summary Statistics in Console
cat("\nSummary Statistics for 2 Clusters:\n")
print(summary_2)

#cat("\nSummary Statistics for", best_k, "Clusters:\n")
#print(summary_best)

cat("\nOutputs for Question 2 saved successfully in", output_file, "\n")





