#' Model Variable Missingness
#'
#' @description
#' @param var A character string indicating the name of the variable
#' to explore missingness within
#' @param confounders A character vector containing the programmatic names of variables
#' to use as confounders.
#' @param confounder_names A character vector containing presentation names of variables. If not supplied will default
#' to \code{confounders}.
#' @inheritParams account_for_nested_missing
#' @return A dataframe containing data and model estimates for the specified variable and confounders.
#' @export
#' @importFrom dplyr select_at mutate_at case_when slice mutate select count add_count drop_na ungroup rename_if mutate_if group_by all_vars left_join
#' @importFrom tidyr replace_na
#' @importFrom stringr str_replace
#' @importFrom prettypublisher pretty_ci pretty_round
#' @importFrom tibble tibble
#' @importFrom purrr map2_dfr map_dbl
#' @importFrom broom tidy
#' @examples
#'
#' ## Code
#' model_variable_missingness
model_variable_missingness <- function(df = NULL, var = NULL,
                                       confounders = NULL, confounder_names = NULL) {

  if (is.null(confounder_names)) {
    confounder_names <- confounders
  }

  df <- df %>%
    select_at(.vars = c(var, confounders)) %>%
    mutate_at(.vars = var, funs(case_when(is.na(.) ~ "Missing",
                                          TRUE ~ "Complete") %>%
                                  factor(levels = c("Complete", "Missing")))) %>%
    drop_na()

  model <- glm(as.formula(paste0(var, " ~ .")), data = df, family = binomial(link="logit"))

  ## Tidy model output table
  table <- model %>%
    tidy(conf.int = TRUE) %>%
    mutate_at(.vars = c("estimate", "conf.low", "conf.high"),
              exp) %>%
    slice(-1) %>%
    mutate(`Odds Ratio` = pretty_ci(estimate, conf.low, conf.high, sep = ", "),
           `P value (Wald)` = signif(p.value, 3)) %>%
    select(-estimate, -std.error, -statistic, -p.value, -conf.low, -conf.high)


  ## Get clean variable names and demographic data
  data <- map2_dfr(confounders, confounder_names, ~ group_by(df, .dots = c(.x, var)) %>%
                     count %>%
                     group_by(.dots = .x) %>%
                     add_count(wt = n, name = "nn") %>%
                     filter_at(.vars = var, all_vars(. == "Missing")) %>%
                     select(-contains(var)) %>%
                     drop_na %>%
                     ungroup %>%
                     rename_if(is.factor, funs(paste0("Category"))) %>%
                     mutate_if(is.factor, as.character) %>%
                     mutate(Variable = .y) %>%
                     mutate(key = paste0(.x, Category)) %>%
                     mutate(Missing = pretty_round(n / nn * 100, 1)) %>%
                     mutate(Missing = paste0(Missing, "% (", n, ")")) %>%
                     select(Variable, Category, `Missing (N)` = Missing,
                            Notifications = nn, key))

  lik_tests <- tibble(Variable = confounder_names,
                      `P value (LRT)` = map_dbl(confounders, ~anova(
                        model,
                        update(model, paste0(". ~ . - ", .)),
                        test = "LRT")$`Pr(>Chi)`[2]
                      ) %>%
                        signif(3))

  ## Combine model, data and likelihood into a table
  output <- data %>%
    left_join(table, by = c("key" = "term")) %>%
    select(-key) %>%
    mutate(`Odds Ratio` = `Odds Ratio` %>% replace_na("")) %>%
    mutate(dummy = Variable) %>%
    group_by(dummy) %>%
    mutate(Variable = c(Variable[1], rep("", n() - 1))) %>%
    ungroup %>%
    select(-dummy) %>%
    left_join(lik_tests, by = "Variable") %>%
    mutate(`P value (LRT)` = `P value (LRT)` %>%
             replace_na("")) %>%
    mutate(`P value (Wald)` = `P value (Wald)` %>%
             replace_na(""))

  colnames(output) <- colnames(output) %>%
    str_replace("Notifications", paste0("Notifications (", nrow(df), ")"))

  return(output)
}
