test_that("Node class works", {
  node <- Node$new(id = "1", name = "root")
  expect_equal(node$name, "root")
})
