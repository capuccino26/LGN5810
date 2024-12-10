###LGN5810 - GENÉTICA QUANTITATIVA###
###AULA 12 - ESTIMATING GENETIC VARIANCES II###

# Load required libraries
library(ggplot2)
library(dplyr)
library(lmtest)
library(cowplot)
library(emmeans)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(multcomp)
library(ggpubr)
library(grid)
library(gt)
library(gtExtras)
library(webshot)

###SIMULATE DATA###
# Set seed for reproducibility
set.seed(123)

# Parameters
## Males
s <- 5
## Females
d <- 5
## Blcoks
r <- 6
## Total data
n <- s * d * r

# Simulate Variance components
varcM <- 4
varcF <- 3
varcMXF <- 2
varcB <- 1.5
varcR <- 1

# Generate effects
male_effect <- rnorm(s, 0, sqrt(varcM))
female_effect <- rnorm(d, 0, sqrt(varcF))
interaction_effect <- matrix(rnorm(s * d, 0, sqrt(varcMXF)), nrow = s, ncol = d)
block_effect <- rnorm(r, 0, sqrt(varcB))
residual <- rnorm(n, 0, sqrt(varcR))

# Generate data frame
data <- expand.grid(Male = factor(1:s), Female = factor(1:d), Block = factor(1:r))
data$Response <- with(data, 
  50 + male_effect[as.numeric(Male)] + female_effect[as.numeric(Female)] +
    interaction_effect[cbind(as.numeric(Male), as.numeric(Female))] + 
    block_effect[as.numeric(Block)] + residual)

# Save the data table as a CSV
write.csv(data, "NCII_data.csv", row.names = FALSE)

# ANOVA
model <- aov(Response ~ Male + Female + Male:Female + Block, data = data)
anova_table <- summary(model)

# Save ANOVA table as a CSV
anova_df <- as.data.frame(anova_table[[1]])
write.csv(anova_df, "ANOVA_table.csv", row.names = TRUE)

# Treat for GTTables (Presentable dataframe)
anova_df_gt<-anova_df
anova_df_gt$Comparisons <- rownames(anova_df)
anova_df_gt <- anova_df[, c("Comparisons", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
gt(anova_df_gt) %>%
gt_theme_guardian() %>%
cols_width(.,everything()~px(150)) %>%
tab_header(title = "Anova Table") %>%
tab_options(row_group.as_column = TRUE) %>%
tab_options(heading.align = "center") %>%
cols_align(align = "center") %>%
gtsave("anova_table.html")
webshot("anova_table.html" , "anova_table.pdf", delay = 0.2,vwidth=2000)
write.csv(anova_df, "ANOVA_table.csv", row.names = TRUE)

# Extract Mean Squares
MS_Male <- anova_df["Male", "Mean Sq"]
MS_Female <- anova_df["Female", "Mean Sq"]
MS_Interaction <- anova_df["Male:Female", "Mean Sq"]
MS_Residual <- anova_df["Residuals", "Mean Sq"]

# Calculate variance components (Environment not considered)
var_M <- (MS_Male - MS_Interaction) / (d * r)
var_F <- (MS_Female - MS_Interaction) / (m * r)
var_MxF <- (MS_Interaction - MS_Residual) / r
var_E <- MS_Residual

# Compile variance components into a data frame
var_components <- data.frame(
  Component = c("Male", "Female", "Male:Female", "Residual"),
  Variance = c(var_M, var_F, var_MxF, var_E)
)

# Save variance components table as a CSV
write.csv(var_components, "Variance_Components.csv", row.names = FALSE)

# Interaction plot: Male x Female
interaction_plot <- ggplot(data, aes(x = Male, y = Response, color = Female, group = Female)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Interaction between Male and Female Parents", x = "Male Parent", y = "Response")
ggsave("Interaction_Plot.png", plot = interaction_plot, width = 8, height = 6)

# Residual analysis
residuals <- resid(model)

# Fitted values
fitted_values <- fitted(model)

# Residual variance
residual_var <- var(residuals)
cat("Variance of Residuals:", residual_var, "\n")

# Save residuals table
residuals_table <- data.frame(Fitted = fitted_values, Residuals = residuals)
write.csv(residuals_table, "residuals_table.csv")

# Visualization of residuals
png("residuals_analysis.png", width = 1200, height = 800)
layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), widths = c(1, 2), heights = c(1, 1))
# 1. Histogram
hist(residuals, breaks = 20, main = "Histogram of Residuals", 
     xlab = "Residuals", col = "lightblue", border = "white")
# 2. Q-Q Plot
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")
# 3. Residuals vs Fitted
plot(fitted_values, residuals, main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
dev.off()
png(sprintf("%s%s_resb.png", path,resposta), width=800,height=600, res = 100)
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))
plot(modelo_simples, which = 2)
boxplot(res_stud, ylab = "Studentized Residuals")
plot(modelo_simples, which = 1)
dev.off()

# Visualization of residuals comparisons
# Residuals vs Males
plot_male <- ggplot(data, aes(x = fitted(model), y = Response, color = Male)) +
  geom_point() +
  xlab("Response") +
  ylab("Studentized Residuals") +
  ggtitle("Fitted Residuals vs Males")

# Residuals vs Females
plot_female <- ggplot(data, aes(x = fitted(model), y = Response, color = Female)) +
  geom_point() +
  xlab("Response") +
  ylab("Studentized Residuals") +
  ggtitle("Fitted Residuals vs Females")

# Generate grid
combined_plot <- plot_grid(
  plot_male, plot_female, 
  ncol = 1, align = "v", 
  labels = c("a.", "b.")
)
ggsave(combined_plot, file = "combined_fitted_residuals.png", width = 8, height = 10, limitsize = FALSE)

# Normality of residuals (Shapiro-Wilk)
shapiro_test <- shapiro.test(residuals)

# Homoscedasticity (Breusch-Pagan)
bp_test <- bptest(model)

# Export normality tests
output_file <- "test_results.txt"
cat("Test Results\n", file = output_file)
cat("=============\n\n", file = output_file, append = TRUE)
cat("Shapiro-Wilk Test for Residuals Normality:\n", file = output_file, append = TRUE)
cat(paste("W =", shapiro_test$statistic, "\n"), file = output_file, append = TRUE)
cat(paste("p-value =", shapiro_test$p.value, "\n\n"), file = output_file, append = TRUE)
cat("Breusch-Pagan Test for Homoscedasticity:\n", file = output_file, append = TRUE)
cat(paste("BP =", bp_test$statistic, "\n"), file = output_file, append = TRUE)
cat(paste("Degrees of Freedom =", bp_test$parameter, "\n"), file = output_file, append = TRUE)
cat(paste("p-value =", bp_test$p.value, "\n"), file = output_file, append = TRUE)
cat("Results saved to:", output_file, "\n")

# Calculate estimated means
model_emeans_male <- emmeans(model, "Male")
model_emeans_female <- emmeans(model, "Female")
model_emeans_interaction <- emmeans(model, ~ Male:Female)

# Export emmeans
output_file <- "emmeans_results.txt"
cat("Estimated Marginal Means\n", file = output_file)
cat("=========================\n\n", file = output_file, append = TRUE)
cat("Estimated Marginal Means for Male:\n", file = output_file, append = TRUE)
capture.output(summary(model_emeans_male), file = output_file, append = TRUE)
cat("\n", file = output_file, append = TRUE)
cat("Estimated Marginal Means for Female:\n", file = output_file, append = TRUE)
capture.output(summary(model_emeans_female), file = output_file, append = TRUE)
cat("\n", file = output_file, append = TRUE)
cat("Estimated Marginal Means for Male:Female Interaction:\n", file = output_file, append = TRUE)
capture.output(summary(model_emeans_interaction), file = output_file, append = TRUE)
cat("\nResults saved to:", output_file, "\n", file = output_file, append = TRUE)

# Generate emmeans plots
plot_male <- plot(model_emeans_male, comparisons = TRUE) + ggtitle("Male Predictors")
plot_female <- plot(model_emeans_female, comparisons = TRUE) + ggtitle("Female Predictors")
plot_interaction <- plot(model_emeans_interaction, comparisons = TRUE) + ggtitle("Male:Female Interaction")
png("emmeans.png", width = 1200, height = 800)
grid.arrange(
  grobs = list(plot_male, plot_female, plot_interaction),
  layout_matrix = matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE)
)
dev.off()

# Models comparisons
male_comparisons <- pairs(model_emeans_male)
female_comparisons <- pairs(model_emeans_female)
interaction_comparisons <- pairs(model_emeans_interaction)

# Export comparisons
save_results <- function(results, filename) {
  results_clean <- results %>%
    as.data.frame() %>%
    select(contrast, estimate, SE, df, t.ratio, p.value) %>%
    mutate(p.value = ifelse(p.value < 0.0001, "< 0.0001", round(p.value, 4)))
  
  write.table(results_clean, file = filename, sep = "\t", row.names = FALSE, col.names = TRUE)
}
save_results(male_comparisons, "male_comparisons_results.tsv")
save_results(female_comparisons, "female_comparisons_results.tsv")
save_results(interaction_comparisons, "interaction_comparisons_results.tsv")

# Extract p-values
summary_comparisons <- summary(interaction_comparisons)
p_values <- summary_comparisons$p.value

# Get comparisons levels
male_levels <- unique(data$Male)
female_levels <- unique(data$Female)

# Check number of comparisons
num_comparisons <- length(p_values)

# Generate matrix
p_values_matrix <- matrix(p_values, nrow = length(male_levels), 
                          ncol = length(female_levels), 
                          byrow = TRUE, 
                          dimnames = list(male_levels, female_levels))

# Generate matrix for treatment
p_values_long <- melt(p_values_matrix)

# Generate heatmap
heatmap_plot <- ggplot(p_values_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  ggtitle("Heatmap de Valores-P") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tukey:
tukey_result<-TukeyHSD(model)
tukey_male <- as.data.frame(tukey_result$`factor(Male)`)
tukey_female <- as.data.frame(tukey_result$`factor(Female)`)
tukey_block <- as.data.frame(tukey_result$`factor(Block)`)
tukey_male$comparison <- rownames(tukey_male)
tukey_female$comparison <- rownames(tukey_female)
tukey_block$comparison <- rownames(tukey_block)
write.table(tukey_male, "tukey_results_male.tsv", sep = "\t", row.names = FALSE)
write.table(tukey_female, "tukey_results_female.tsv", sep = "\t", row.names = FALSE)
write.table(tukey_block, "tukey_results_block.tsv", sep = "\t", row.names = FALSE)

# Plot tukey male:
tkmale<-ggplot(tukey_male, aes(x = comparison, y = diff)) +
geom_point() +
geom_errorbar(aes(ymin = lwr, ymax = upr)) +
geom_text(aes(label = ifelse(`p adj` < 0.05, "*", "ns")), vjust = -0.5) +
xlab("Comparisons Male") +
ylab("Mean differences") +
ggtitle("Tukey - Comparisons between males") +
theme_minimal()
ggsave(tkmale, file="tk_male.png",limitsize=FALSE)

# Plot tukey female:
tkfmale<-ggplot(tukey_female, aes(x = comparison, y = diff)) +
geom_point() +
geom_errorbar(aes(ymin = lwr, ymax = upr)) +
geom_text(aes(label = ifelse(`p adj` < 0.05, "*", "ns")), vjust = -0.5) +
xlab("Comparisons Female") +
ylab("Mean differences") +
ggtitle("Tukey - Comparisons between females") +
theme_minimal()
ggsave(tkfmale, file="tk_female.png",limitsize=FALSE)

# Plot tukey Block:
tkblock<-ggplot(tukey_block, aes(x = comparison, y = diff)) +
geom_point() +
geom_errorbar(aes(ymin = lwr, ymax = upr)) +
geom_text(aes(label = ifelse(`p adj` < 0.05, "*", "ns")), vjust = -0.5) +
xlab("Comparisons Blocks") +
ylab("Mean differences") +
ggtitle("Tukey - Comparisons between blocks") +
theme_minimal()
ggsave(tkblock, file="tk_block.png",limitsize=FALSE)
png("tk_full.png", width = 1200, height = 800)

# Generate grid
grid.arrange(grobs = list(tkmale, tkfmale, tkblock),layout_matrix = matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE))
# Labels positions
grid.text("a.", x = unit(0.01, "npc"), y = unit(0.987, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("b.", x = unit(0.50, "npc"), y = unit(0.987, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("c.", x = unit(0.01, "npc"), y = unit(0.487, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
dev.off()

# Boxplot of individual interactions
interactind<-ggplot(data, aes(x = interaction(Male, Female), y = Response, fill = interaction(Male, Female))) +
geom_boxplot() +
labs(x = "Pairs Male x Female", y = "Response") +
ggtitle("Boxplot for each pair") +
theme_minimal()
ggsave(interactind, file="interacts_ind.png",limitsize=FALSE)

# Generate heatmaps
heatmap_plot <- ggplot(p_values_long, aes(Var1, Var2, fill = value)) +
geom_tile() +
scale_fill_gradient(low = "blue", high = "red") +
theme_minimal() +
ggtitle("Heatmap for p-values") +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
xlab("Male") +
ylab("Female")
ggsave(heatmap_plot, file="interacts_heat.png",limitsize=FALSE)
heatmap_plot_full <- ggplot(p_values_long_full, aes(Var1, Var2, fill = value)) +
geom_tile() +
scale_fill_gradient(low = "blue", high = "red") +
theme_minimal() +
ggtitle("Heatmap de Valores-P") +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
xlab("Male") +
ylab("Female")
ggsave(heatmap_plot_full, file="interacts_heat_full.png",limitsize=FALSE)

# Generate statistcs plots for Males
statm<-ggplot(data, aes(x = as.factor(Block), y = Response, fill = as.factor(Male))) +
geom_boxplot() +
xlab("Block") +
ylab("Response") +
ggtitle("Boxplot of Males per Block") +
theme_minimal() +
scale_fill_brewer(palette = "Dark2") +
labs(fill = "Males") +
stat_compare_means(aes(group = as.factor(Male)), method = "wilcox.test", label = "p.signif", hide.ns = TRUE)
ggsave(statm, file="stats_male.png",limitsize=FALSE)

# Generate statistcs plots for Males
statfm<-ggplot(data, aes(x = as.factor(Block), y = Response, fill = as.factor(Female))) +
geom_boxplot() +
xlab("Block") +
ylab("Response") +
ggtitle("Boxplot of Females per Block") +
theme_minimal() +
scale_fill_brewer(palette = "Dark2") +
labs(fill = "Females") +
stat_compare_means(aes(group = as.factor(Female)), method = "wilcox.test", label = "p.signif", hide.ns = TRUE)
ggsave(statfm, file="stats_female.png",limitsize=FALSE)

# Generate statistcs plots for Males
statfmf<-ggplot(data, aes(x = as.factor(Block), y = Response, fill = as.factor(Female))) +
geom_boxplot() +
xlab("Block") +
ylab("Response") +
ggtitle("Boxplot of Females per Block") +
theme_minimal() +
scale_fill_brewer(palette = "Dark2") +
labs(fill = "Females") +
stat_compare_means(method = "kruskal.test", label.y = max(dadosf[[resposta]]) * 1.1) +
stat_compare_means(aes(group = as.factor(Female)), method = "wilcox.test", label = "p.signif", hide.ns = TRUE)
ggsave(statfm, file="stats_female_full.png",limitsize=FALSE)

# Generate statistcs plots for Males
statmf<-ggplot(data, aes(x = as.factor(Block), y = Response, fill = as.factor(Male))) +
geom_boxplot() +
xlab("Block") +
ylab("Response") +
ggtitle("Boxplot of Males per Block") +
theme_minimal() +
scale_fill_brewer(palette = "Dark2") +
labs(fill = "Males") +
stat_compare_means(method = "kruskal.test", label.y = max(dadosf[[resposta]]) * 1.1) +
stat_compare_means(aes(group = as.factor(Female)), method = "wilcox.test", label = "p.signif", hide.ns = TRUE)
ggsave(statfm, file="stats_male_full.png",limitsize=FALSE)

# Generate grid
png("stats_full.png", width = 1200, height = 800)
grid.arrange(
grobs = list(statm, statfm, statmf, statfmf),
layout_matrix = matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE)
)
grid.text("a.", x = unit(0.01, "npc"), y = unit(0.987, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("b.", x = unit(0.50, "npc"), y = unit(0.987, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("c.", x = unit(0.01, "npc"), y = unit(0.487, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("d.", x = unit(0.50, "npc"), y = unit(0.487, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
dev.off()

# Boxplot of Response by Male
p1 <- ggplot(data, aes(x = factor(Male), y = Response, fill = factor(Male))) +
geom_boxplot() +
stat_compare_means(method = "wilcox.test", label = "p.signif", hide.ns = TRUE) +
ggtitle("Boxplot of Response by Male") +
labs(title = "Response by Male Parent", x = "Male", y = "Response",fill = "Males") +
theme_minimal()
ggsave("Boxplot_male.png", plot = p1, width = 7, height = 5)

# Boxplot of Response by Female
p2 <- ggplot(data, aes(x = factor(Female), y = Response, fill = factor(Female))) +
geom_boxplot() +
stat_compare_means(method = "wilcox.test", label = "p.signif", hide.ns = TRUE) +
ggtitle("Boxplot of Response by Female") +
labs(title = "Response by Female Parent", x = "Female", y = "Response",fill = "Females") +
theme_minimal()
ggsave("boxplot_female.png", plot = p2, width = 7, height = 5)

# Interaction plot: Male x Female
interaction_plot <- ggplot(data, aes(x = Male, y = Response, color = Female, group = Female)) +
geom_line() +
theme_minimal() +
labs(title = "Interaction between Male and Female Parents", x = "Male", y = "Response")
ggsave("Interaction_plot.png", plot = interaction_plot, width = 8, height = 6)

# Generate grid
png("boxplots_full.png", width = 1200, height = 800)
grid.arrange(
grobs = list(p1, p2),
layout_matrix = matrix(c(1, 2), ncol = 1, byrow = TRUE)
)
grid.text("a.", x = unit(0.01, "npc"), y = unit(0.987, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("b.", x = unit(0.01, "npc"), y = unit(0.487, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
dev.off()

# Visualization of residuals general
png("residuals_analysis.png", width = 1200, height = 800)
layout(matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE), widths = c(1, 1), heights = c(1, 1))
# 1. Histogram
hist(residuals, breaks = 20, main = "Histogram of Residuals",
xlab = "Residuals", col = "lightblue", border = "white")
# 2. Boxplot
boxplot(res_stud, ylab = "Studentized Residuals", main = "Studentized Residuals")
# 3. Q-Q Plot
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")
# 4. Residuals vs Fitted
plot(fitted_values, residuals, main = "Residuals vs Fitted Values",
xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
grid.text("a.", x = unit(0.01, "npc"), y = unit(0.977, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("b.", x = unit(0.50, "npc"), y = unit(0.977, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("c.", x = unit(0.01, "npc"), y = unit(0.477, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("d.", x = unit(0.50, "npc"), y = unit(0.477, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
dev.off()

# Residuals for predictors (Male and Female)
png("residuals_analysis_predictors.png", width = 1200, height = 800)
# Layout for 1 column and 2 rows
layout(matrix(c(1, 2), ncol = 2, byrow = TRUE), widths = c(1, 1), heights = c(1, 1))
# 4. Residuals vs predictors
plot(data$Male, residuals, main = "Residuals vs Male",
xlab = "Male", ylab = "Residuals", pch = 19, col = "green")
abline(h = 0, col = "red", lty = 2)
plot(data$Female, residuals, main = "Residuals vs Female",
xlab = "Female", ylab = "Residuals", pch = 19, col = "green")
abline(h = 0, col = "red", lty = 2)
grid.text("a.", x = unit(0.01, "npc"), y = unit(0.963, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("b.", x = unit(0.50, "npc"), y = unit(0.963, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
dev.off()
