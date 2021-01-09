context("Recent nearby queries")

# Skip tests if eBird key does not exist
skip_if(!file.exists("../../ebird-key.txt"),
        message = "No eBird key file")

# Skip also if there is no internet connection
skip_if_offline()

key <- scan(file = "../../ebird-key.txt", what = "character")

# Test for recent observations from Sweetwater Wetlands
test_that(desc = "RecentNearby query for Sweetwater succeeds",
          code = {
            recentSweetwater <- RecentNearby(key = key,
                                             lat = 32.28,
                                             lng = -111.02,
                                             dist = 5)
            expect_equal(class(recentSweetwater), "recent_obs")
          })

# Test for South Pole observations (should return zero results)
test_that(desc = "RecentNearby query for South Pole returns no results",
          code = {
            recentSouthPole <- RecentNearby(key = key,
                                            lat = -90,
                                            lng = 0,
                                            dist = 5)
            expect_true(is.null(recentSouthPole$obs))
          })

# Test for recent observations for Verdin within 5k of Sweetwater Wetlands
test_that(desc = "RecentNearbySpecies query for Verdin succeeds",
          code = {
            recentVerdin <- RecentNearbySpecies(key = key,
                                                species_code = "verdin",
                                                lat = 32.28,
                                                lng = -111.02,
                                                dist = 5,
                                                back = 5)
            expect_equal(class(recentVerdin), "recent_obs")
          })

# Test for recent observations for Verdin within 5k of South Pole
test_that(desc = "RecentNearbySpecies query for Verdin returns zero results",
          code = {
            recentVerdinSouthPole <- RecentNearbySpecies(key = key,
                                                         species_code = "verdin",
                                                         lat = -90,
                                                         lng = 0,
                                                         dist = 5,
                                                         back = 5,
                                                         verbose = FALSE)
            expect_true(is.null(recentVerdinSouthPole$obs))
          })
