# Load package
library(hetu)

# Test rhetu function speed
# Run all lines at once
n <- 10000
start_time <- Sys.time()
example_vector <- rhetu(n)
end_time <- Sys.time()
end_time - start_time
# Remove unneeded objects from environment
rm(start_time, end_time, n)

# Test hetu function speed
# Run all lines at once
start_time <- Sys.time()
placeholder <- hetu(example_vector)
end_time <- Sys.time()
end_time - start_time
# Remove unneeded objects from environment
rm(start_time, end_time, placeholder, example_vector)

# Function profiling
# Use smaller sample size for profiling, too large and it fails
n <- 10000
library(profvis)
profvis({
  test_vector <- rhetu(n)
})

profvis({
  hetu(test_vector)
})
# Remove unneeded objects from environment
rm(test_vector, n)
