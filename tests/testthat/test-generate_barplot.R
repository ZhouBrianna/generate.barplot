test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Create a test dataset with explicit counts
test_data <- data.frame(class = factor(rep(c("A", "B", "C"), times = c(5, 3, 2))))

# Expected ggplot
expcted_plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = class, fill = class)) +
  ggplot2::geom_bar() +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::labs(
    title = "Distribution of Class",
    x = "Class",
    y = "Count"
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    plot.title = ggplot2::element_text(size = 16, face = "bold")
  )

# Generate plot
test_that("make_barplot function works correctly", {
  plot <- generate_barplot(test_data, "class", "Class")
  # Check if the output is a ggplot object
  expect_s3_class(plot, "gg")
  # Check if axis labels is correct
  expect_equal(plot$labels$x, "Class")
  expect_equal(plot$labels$y, "Count")
  # Check if title is correctly set
  expect_equal(plot$labels$title, "Distribution of Class")
  # Extract computed counts from the plot
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  actual_counts <- setNames(plot_data$count, levels(test_data$class)[plot_data$x])
  expected_counts <- table(test_data$class)
  expected_counts <- setNames(as.numeric(expected_counts), names(expected_counts))
  # Compare expected and actual counts
  expect_equal(actual_counts, expected_counts)
})

# Edge case: Dataset with only one unique class value
test_that("Edge case - Dataset with only one unique value in class", {
  test_data_single <- data.frame(class = factor(rep("A", times = 5)))
  plot_single <- generate_barplot(test_data_single, "class", "Car Class")
  # Check if the output is a ggplot object
  expect_s3_class(plot_single, "ggplot")
  # Check if axis labels are correct
  expect_equal(plot_single$labels$x, "Car Class")
  expect_equal(plot_single$labels$y, "Count")
  # Check if title is correctly set
  expect_equal(plot_single$labels$title, "Distribution of Car Class")
  # Extract computed counts from the plot
  plot_data <- ggplot2::ggplot_build(plot_single)$data[[1]]
  actual_counts <- setNames(plot_data$count, levels(test_data_single$class)[plot_data$x])
  expected_counts <- table(test_data_single$class)
  expected_counts <- setNames(as.numeric(expected_counts), names(expected_counts))
  # Compare expected and actual counts
  expect_equal(actual_counts, expected_counts)
})

test_that("Edge case: Dataset has no count for the column", {
  test_data_zero <- data.frame(x1 = factor(c("A", "B", "A")))
  plot_zero <- generate_barplot(test_data_zero, "x1", "X1")
  expect_s3_class(plot_zero, "ggplot")
  expect_equal(plot_zero$labels$x, "X1")
  expect_equal(plot_zero$labels$y, "Count")
  expect_equal(plot_zero$labels$title, "Distribution of X1")
  plot_data <- ggplot2::ggplot_build(plot_zero)$data[[1]]
  actual_counts <- setNames(plot_data$count, levels(test_data_zero$x1)[plot_data$x])
  expected_counts <- table(test_data_zero$x1)
  expected_counts <- setNames(as.numeric(expected_counts), names(expected_counts))
  # Compare expected and actual counts
  expect_equal(actual_counts, expected_counts)
})

test_that("Edge case: Large dataset", {
  test_data_large <- data.frame(class = factor(sample(letters, 10000, replace = TRUE)))
  plot_large <- generate_barplot(test_data_large, "class", "Test Class")
  expect_s3_class(plot_large, "ggplot")
})

test_that("Edge case - Dataset with duplicate x-axis values", {
  test_data_duplicate <- data.frame(class = factor(rep(c("A", "A", "B", "B", "C", "C"), times = c(1, 1, 1, 1, 1, 1))))
  plot_duplicates <- generate_barplot(test_data_duplicate, "class", "Class")
  expect_s3_class(plot_duplicates, "ggplot")
  expect_equal(plot_duplicates$labels$x, "Class")
  expect_equal(plot_duplicates$labels$y, "Count")
  expect_equal(plot_duplicates$labels$title, "Distribution of Class")
  plot_data <- ggplot2::ggplot_build(plot_duplicates)$data[[1]]
  actual_counts <- setNames(plot_data$count, levels(test_data_duplicate$class)[plot_data$x])
  expected_counts <- table(test_data_duplicate$class)
  expected_counts <- setNames(as.numeric(expected_counts), names(expected_counts))
  # Compare expected and actual counts
  expect_equal(actual_counts, expected_counts)
})

test_that("Edge case - Column is not a factor", {
  test_data_nonfactor <- data.frame(grade = (rep(c("A", "B", "C"), times = c(1, 2, 3)))) # Not a factor
  plot_nonfactor <- generate_barplot(test_data_nonfactor, "grade", "Grade")
  expect_s3_class(plot_nonfactor, "ggplot")
  expect_equal(plot_nonfactor$labels$x, "Grade")
  expect_equal(plot_nonfactor$labels$y, "Count")
  expect_equal(plot_nonfactor$labels$title, "Distribution of Grade")
  plot_data <- ggplot2::ggplot_build(plot_nonfactor)$data[[1]]
  grade_factor <- factor(test_data_nonfactor$grade)
  actual_levels <- levels(grade_factor)
  actual_counts <- setNames(plot_data$count, actual_levels[plot_data$x])
  # actual_counts <- setNames(plot_data$count, levels(test_data_nonfactor$grade)[plot_data$x])
  expected_counts <- table(test_data_nonfactor$grade)
  expected_counts <- setNames(as.numeric(expected_counts), names(expected_counts))
  # Compare expected and actual counts
  expect_equal(actual_counts, expected_counts)
})

test_that("Invalid dataset: Dataset is not a data frame", {
  expect_error(generate_barplot(list(class = factor(c("A", "B", "C"))), "class", "Class"), "Dataset must be a data frame!")
})

test_that("Invalid dataset: Dataset is NULL", {
  expect_error(generate_barplot(NULL, "class", "Class"))
})

test_that("Invalid column: Column is missing", {
  expect_error(generate_barplot(test_data, NULL, "Class"))
})

test_that("Invalid column: Column is not in the dataset", {
  test_data <- data.frame(other_column = factor(c("A", "B", "C")))
  expect_error(generate_barplot(test_data, "class", "Class"))
})
