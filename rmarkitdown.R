
# MarkItDown executable installed with pipx
markitdown <- "/Users/andreaobradors/.local/bin/markitdown"

# Folder containing PDFs
input_dir <- "/Users/andreaobradors/Downloads/PDFs"

# Find PDFs
pdfs <- list.files(
  input_dir,
  pattern = "\\.pdf$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Convert each PDF
for (pdf in pdfs) {
  
  output <- file.path(
    input_dir,
    paste0(
      tools::file_path_sans_ext(basename(pdf)),
      ".md"
    )
  )
  
  cat("Converting:", basename(pdf), "\n")
  
  result <- system2(
    markitdown,
    args = c(pdf, "-o", output)
  )
  
  if (file.exists(output)) {
    cat("  ✓ Created:", basename(output), "\n")
  } else {
    cat("  ✗ Failed:", basename(pdf), "\n")
  }
}
