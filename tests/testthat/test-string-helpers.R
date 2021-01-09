context("String helper functions")

# Test for split of names with default delimiter
test_that(desc = "Split names by hyphen",
          code = {
            split_goose <- SplitNames(x = "Snow Goose - Anser caerulescens")
            expect_equal(split_goose[1, 1], "Snow Goose")
            expect_equal(split_goose[1, 2], "Anser caerulescens")
          })

# Test for split of names with semicolon as delimiter
test_that(desc = "Split names by semicolon",
          code = {
            split_goose <- SplitNames(x = "Snow Goose;Anser caerulescens",
                                      delim = ";")
            expect_equal(split_goose[1, 1], "Snow Goose")
            expect_equal(split_goose[1, 2], "Anser caerulescens")
          })

# Test for removal of "hybrid" species
test_that(desc = "Drop species with 'hybrid' in name",
          code = {
            df <- data.frame(comName = c("Mallard",
                                         "Mallard x Mexican Duck hybrid",
                                         "Verdin"),
                             date = c("2021-01-09",
                                      "2021-01-09",
                                      "2021-01-09"))
            df <- DropPatterns(data = df)
            expect_equal(nrow(df), 2)
          })
