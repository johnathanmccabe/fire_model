


test_that(desc = "Test calculate loss cost code", {
  expect_equal(calculate_loss_cost(10,1,2,3,4,5,6,7), factorial(7)*10)
})
