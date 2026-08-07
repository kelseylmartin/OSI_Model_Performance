import re

files = ["R/performance.R", "R/visualizations.R"]

def fix_file(filename):
    with open(filename, 'r') as f:
        content = f.read()
    
    # plot_bland_altman
    content = re.sub(r"(#' @param aligned_df .*?\n)", r"\1#' @param model_col Character. Column name for model counts.\n#' @param title Character. Plot title.\n", content, count=1)
    
    with open(filename, 'w') as f:
        f.write(content)

# We will just write a specific python script or do manual replacements.
