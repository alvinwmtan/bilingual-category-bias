library(tidyverse)

theme_set(
  theme_classic(base_size = 12) +
    theme(axis.line.x = element_line(linewidth = 0),
          axis.line.y = element_line(linewidth = 0),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin())
)

CAT_SCALE <- scale_colour_manual(values = c("#4476AA", "#DECC77", "#CD6677"),
                                 labels = c("nouns", "predicates", "function words"))

make_prop_plot <- function(raw_data, fitted_data, faceting = language ~ lang_group) {
  ggplot(fitted_data, 
         aes(x = prop_total, y = prop_class, col = lex_class)) +
    geom_abline(intercept = 0, slope = 1, lty = "dashed") +
    facet_grid(faceting) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    geom_point(data = raw_data, alpha = .1, size = 0.6) +
    geom_line(linewidth = 1.5, alpha = .9) +
    labs(x = "Vocabulary size",
         y = "Proportion of category", 
         col = "Lexical class") +
    CAT_SCALE +
    scale_x_continuous(breaks = c(0, 0.5, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) 
}

make_bias_plot <- function(model_summary, faceting = lex_class ~ .,
                           shape_opt = NULL) {
  ggplot(model_summary |> 
           mutate(lex_class = ifelse(lex_class == "function_words", "function words", lex_class) |> 
                    as_factor() |> fct_relevel(c("nouns", "predicates", "function words")))) +
    geom_vline(xintercept = 0, lty = "dashed") +
    geom_pointrange(aes(x = mean, xmin = ci_lower, xmax = ci_upper,
                        y = language, col = lang_group,
                        shape = {{ shape_opt }}),
                    position = position_dodge(width = 0.6)) +
    facet_grid(faceting) +
    labs(x = "Category bias", y = "Language", col = "Language group") +
    scale_color_discrete(guide = guide_legend(reverse = TRUE))
}
