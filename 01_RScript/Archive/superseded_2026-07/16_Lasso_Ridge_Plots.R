# Author: Orhun Ozel
# Date: 07/12/2025
# Scope: Create Charts for Lasso Ridge ElNet

cr_path <- function(output_file_name) {
  output_chart_path <- paste0("03_Output/Charts/lasso_ridge/", output_file_name,".png")
  return(output_chart_path)
}
  
### Create Charts
## Lambda Grid Search
lambda_search_lasso <- sapply(best_lam_lasso_all_1$all_res, function(x) c("lambda"=x$lambda, x$errors, "n"=sum(x$coef[1,] != 0)))
lambda_search_ridge <- sapply(best_lam_ridge_all_1$all_res, function(x) c("lambda"=x$lambda, x$errors, "n"=sum(x$coef[1,] != 0)))
lambda_lasso   <- as.data.table(t(lambda_search_lasso))
lambda_ridge   <- as.data.table(t(lambda_search_ridge))
best_lam_lasso <- lambda_lasso[rmse==min(rmse), lambda]
best_lam_ridge <- lambda_ridge[rmse==min(rmse), lambda]

ggplot(lambda_lasso, aes(x=lambda, y=rmse)) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_lam_lasso, linetype="dashed", color="red") +
  geom_point(size = 2) +    
  scale_x_continuous(breaks = function(x) {sort(unique(c(pretty(x), best_lam_lasso)))})+
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(filename = cr_path("lasso_1_rmse"), width=12, height=6, dpi=600)


ggplot(lambda_lasso) + aes(x=lambda, y =n   ) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_lam_lasso, linetype="dashed", color="red") +  
  geom_point(size = 2) + ylab("Number of Covariates") +
  scale_x_continuous(breaks = function(x) {sort(unique(c(pretty(x), best_lam_lasso)))})+
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(filename = cr_path("lasso_2_ncov"), width=12, height=6, dpi=600)

ggplot(lambda_ridge, aes(x=lambda, y=rmse)) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_lam_ridge, linetype="dashed", color="red") +
  geom_point(size = 2) +
  scale_x_continuous(breaks = function(x) {sort(unique(c(pretty(x), best_lam_ridge)))})+
  theme(
    axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(filename = cr_path("ridge_1_rmse"), width=12, height=6, dpi=600)

## Alpha Grid Search Table for Elastic Net
alpha_search <- sapply(best_alp_all_1$all_res, function(x) c("alpha"=x$alpha, "lambda"=x$lambda, x$errors))
alpha_search <- as.data.table(t(alpha_search))
alpha_search

best_alpha <- alpha_search[rmse==min(rmse), alpha]
ggplot(alpha_search, aes(x=alpha, y=rmse)) + geom_line() + theme_light() + 
  geom_vline(xintercept=best_alpha, linetype="dashed", color="red") + 
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = sort(unique(c(best_alpha, alpha_search$alpha)))
  ) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


table_elnet <- alpha_search[alpha>0 & alpha<1, -c("mae")] |>
  transform(best = rmse == min(rmse)) |>
  gt() |>
  tab_header(title = html("Table: Elastic Net &alpha; and &lambda; Tuning Results")) |>
  cols_align(align = "center", columns = everything()) |>
  data_color(
    columns = rmse,
    colors  = col_numeric(c("blue", "red"), range(alpha_search[alpha>0 & alpha<1,]$rmse))
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = best)
  ) |>
  cols_hide(best)
table_elnet
gtsave(table_elnet, "03_Output/Tables/ElNet_Parameter_Tuning.html")


## Variable Coefficients (Variable Importance)
coef_lasso <- data.table(coef_name=lasso_s1_l1$coef_names, coef=lasso_s1_l1$coef[1,])
coef_ridge <- data.table(coef_name=ridge_s1_l1$coef_names, coef=ridge_s1_l1$coef[1,])
coef_elnet <- data.table(coef_name=elnet_s1_l1$coef_names, coef=elnet_s1_l1$coef[1,])

# Graph Coef_Lasso
low    <- coef_lasso[order(coef)][1:5]
high   <- coef_lasso[order(coef)][(.N-14):.N]
spacer <- data.table(coef_name = c("..."), coef = NA)
coef_plot <- rbind(low, spacer, high, fill = TRUE)
coef_plot[, coef_name := factor(coef_name, levels = unique(coef_name))]
ggplot(coef_plot, aes(coef_name, coef, fill = coef > 0)) +
  geom_col(na.rm = TRUE) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(x = "Coefficient", y = "Value") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  ) + xlab("Variable") + ylab("Coefficients")
ggsave(filename = cr_path("lasso_3_varimp"), width=12, height=6, dpi=600)

# Graph Coef_Ridge
low    <- coef_ridge[order(coef)][1:5]
high   <- coef_ridge[order(coef)][(.N-14):.N]
spacer <- data.table(coef_name = c("..."), coef = NA)
coef_plot <- rbind(low, spacer, high, fill = TRUE)
coef_plot[, coef_name := factor(coef_name, levels = unique(coef_name))]
ggplot(coef_plot, aes(coef_name, coef, fill = coef > 0)) +
  geom_col(na.rm = TRUE) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(x = "Coefficient", y = "Value") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  ) + xlab("Variable") + ylab("Coefficients")
ggsave(filename = cr_path("ridge_2_varimp"), width=12, height=6, dpi=600)



# Graph Coef_Elnet
low    <- coef_elnet[order(coef)][1:5]
high   <- coef_elnet[order(coef)][(.N-14):.N]
spacer <- data.table(coef_name = c("..."), coef = NA)
coef_plot <- rbind(low, spacer, high, fill = TRUE)
coef_plot[, coef_name := factor(coef_name, levels = unique(coef_name))]
ggplot(coef_plot, aes(coef_name, coef, fill = coef > 0)) +
  geom_col(na.rm = TRUE) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick", "steelblue")) +
  labs(x = "Coefficient", y = "Value") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  ) + xlab("Variable") + ylab("Coefficients")
ggsave(filename = cr_path("elnet_1_varimp"), width=12, height=6, dpi=600)



