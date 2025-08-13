# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------


cs <- load_data("df_only_c_section.fst", file.path(drv_root, "final"))
any_true <- apply(cs[, c("has_pain_back","has_pain_neck","has_pain_arthritis","has_pain_neuro","has_pain_headache","has_pain_misc","has_pain_back_neck_unspecified")], 1, any)

# Use table() to count the number of TRUE and FALSE observations
(result <- prop.table(table(any_true)))


other <- load_data("df_non_c_section.fst", file.path(drv_root, "final"))

any_true <- apply(other[, c("has_pain_back","has_pain_neck","has_pain_arthritis","has_pain_neuro","has_pain_headache","has_pain_misc","has_pain_back_neck_unspecified")], 1, any)

# Use table() to count the number of TRUE and FALSE observations
(result <- prop.table(table(any_true)))

