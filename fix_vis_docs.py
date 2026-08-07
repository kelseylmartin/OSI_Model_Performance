import re

file_path = "R/visualizations.R"
with open(file_path, "r") as f:
    content = f.read()

# For plot_bland_altman, plot_confusion_matrix, plot_performance_by_threshold, plot_pr_curve, plot_roc_curve
funcs_with_title_and_model_col = [
    "plot_bland_altman",
    "plot_confusion_matrix",
    "plot_performance_by_threshold",
    "plot_pr_curve",
    "plot_roc_curve"
]

for func in funcs_with_title_and_model_col:
    # Find the rdname tag and inject the params before it
    pattern = rf"(#' @rdname {func}\n)"
    replacement = r"#' @param title Character. Plot title.\n#' @param model_col Symbol or character for model column.\n\1"
    content = re.sub(pattern, replacement, content, count=1)

# For plot_multiclass_confusion_matrix (only title)
func = "plot_multiclass_confusion_matrix"
pattern = rf"(#' @rdname {func}\n)"
replacement = r"#' @param title Character. Plot title.\n\1"
content = re.sub(pattern, replacement, content, count=1)

with open(file_path, "w") as f:
    f.write(content)

