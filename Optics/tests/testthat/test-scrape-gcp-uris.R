test_that("scrape_gcp_uris() returns filtered URIs with expected columns", {
  local_mocked_bindings(
    .list_gcs_objects = function(bucket_name, prefix) {
      data.frame(
        name = c(
          "GFISHER/Video_data/site_001/video_a.MP4",
          "GFISHER/Video_data/site_002/image_b.jpg",
          "GFISHER/Video_data/site_003/readme.txt",
          "GFISHER/Video_data/site_004/"
        ),
        stringsAsFactors = FALSE
      )
    },
    .env = environment(scrape_gcp_uris)
  )

  result <- scrape_gcp_uris(
    bucket_name = "nmfs-dev-uc1-sefsc",
    prefix = "GFISHER/Video_data/",
    extensions = c(".mp4", ".jpg")
  )

  expect_true(is.data.frame(result))
  expect_identical(names(result), c("folder_name", "file_name", "bucket_uri"))
  expect_equal(nrow(result), 2)
  expect_identical(result$folder_name, c("site_001", "site_002"))
  expect_identical(result$file_name, c("video_a.MP4", "image_b.jpg"))
  expect_identical(
    result$bucket_uri,
    c(
      "gs://nmfs-dev-uc1-sefsc/GFISHER/Video_data/site_001/video_a.MP4",
      "gs://nmfs-dev-uc1-sefsc/GFISHER/Video_data/site_002/image_b.jpg"
    )
  )
})

test_that("scrape_gcp_uris() warns when no files match extensions", {
  local_mocked_bindings(
    .list_gcs_objects = function(bucket_name, prefix) {
      data.frame(
        name = c("GFISHER/Video_data/site_001/readme.txt"),
        stringsAsFactors = FALSE
      )
    },
    .env = environment(scrape_gcp_uris)
  )

  expect_warning(
    result <- scrape_gcp_uris(
      bucket_name = "nmfs-dev-uc1-sefsc",
      prefix = "GFISHER/Video_data/",
      extensions = c(".mp4")
    ),
    regexp = "No files matched requested extensions"
  )
  expect_true(is.data.frame(result))
  expect_identical(names(result), c("folder_name", "file_name", "bucket_uri"))
  expect_equal(nrow(result), 0)
})

test_that("scrape_gcp_uris() returns descriptive errors for bucket/auth failures", {
  local_mocked_bindings(
    .list_gcs_objects = function(bucket_name, prefix) {
      stop("403 Forbidden")
    },
    .env = environment(scrape_gcp_uris)
  )

  expect_error(
    scrape_gcp_uris(
      bucket_name = "nmfs-dev-uc1-sefsc",
      prefix = "GFISHER/Video_data/"
    ),
    regexp = "Failed to list objects from bucket"
  )
})
