
map <- read_extdata("CA_tiny_map.zip") |> read_any()

# Your binary matrix
binary_matrix <- matrix(c(TRUE, TRUE, FALSE, 
                          TRUE, TRUE, FALSE,
                          FALSE, FALSE, FALSE,
                          FALSE, FALSE, FALSE,
                          FALSE, TRUE, FALSE, 
                          FALSE, TRUE, TRUE,
                          FALSE, TRUE, FALSE), ncol = 3, byrow = T)