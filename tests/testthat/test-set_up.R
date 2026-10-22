test_that("split_reps divides evenly and hands out the remainder first", {
  expect_equal(split_reps(8, 4), c(2L, 2L, 2L, 2L))
  expect_equal(split_reps(1000003, 8), c(rep(125001L, 3), rep(125000L, 5)))
  expect_equal(sum(split_reps(1000003, 8)), 1000003L)
  expect_equal(split_reps(3, 5), c(1L, 1L, 1L, 0L, 0L))
  expect_error(split_reps(0, 4), "n_reps")
  expect_error(split_reps(10, 0), "n_cores")
})

test_that("make_set_up draws the seed vectors the way the book does", {
  # Appendix C: the first seed vector after set.seed(1234)
  set_up <- make_set_up(1234, 100, 4)
  expect_length(set_up, 4)
  expect_equal(set_up[[1]]$seed, c(-1622172679L, -597384213L))
  expect_equal(set_up[[1]]$n_reps, 25L)
  expect_equal(set_up[[3]]$worker_id, 3L)
  expect_equal(set_up[[3]]$n_cores, 4L)

  # the book's own sequence: seeds first, then streams, with set.seed(seed)
  set.seed(1234)
  sv_list <- dqrng::generateSeedVectors(4)
  stream_list <- dqrng::generateSeedVectors(4)
  expect_equal(lapply(set_up, `[[`, "seed"), sv_list)
  expect_equal(lapply(set_up, `[[`, "stream"), stream_list)
})

test_that("set_worker_rng seeds dqrng from a set_up element", {
  set_up <- make_set_up(1234, 100, 4)
  set_worker_rng(set_up[[2]])
  a <- dqrng::dqrunif(5)
  dqrng::dqset.seed(set_up[[2]]$seed, set_up[[2]]$stream)
  b <- dqrng::dqrunif(5)
  expect_identical(a, b)
})
