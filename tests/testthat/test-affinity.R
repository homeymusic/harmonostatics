expected_tonic_affinity = c(16,0,4,6,7,9,3,11,5,8,2,2,14)
expected_octave_affinity = c(14,2,2,8,5,11,3,9,7,6,4,0,16)
expected_mean_affinity = c(15,1,3,7,6,10,3,10,6,7,3,1,15)

test_that("affinity at level 0 meets expectations", {
  expect_equal(harmony_affinity(), c(15,1,3,7,6,10,3,10,6,7,3,1,15))
  # the mean of the tonic and octave affinities equals rotated affinity
  mean_affinity = rbind(affinity_octave(),affinity_tonic()) %>% colMeans
  expect_equal(harmony_affinity(),mean_affinity)
})

test_that("affinity form matches expectations", {
  t = affinity_tonic()
  o = affinity_octave()
  expect_equal(t,expected_tonic_affinity)
  expect_equal(o,expected_octave_affinity)
  mean_affinity = rbind(o,t) %>% colMeans
  expect_equal(mean_affinity,expected_mean_affinity)
  expect_equal(mean_affinity,harmony_affinity())
})

test_that("affinity for chords matches our expectations",{
  expect_equal(affinity(c(5,-2)),10,tolerance=0.001)
  # above primary level
  expect_equal(affinity(14),2,tolerance=0.001)
  expect_equal(affinity(-2),2,tolerance=0.001)
  # above primary level
  # major chords
  expect_equal(affinity(c(0,4,7)),7.66,tolerance=0.001)
  expect_equal(affinity(c(5,9,12)),7.66,tolerance=0.001)
  expect_equal(affinity(c(7,11,14)),7.66,tolerance=0.001)
  # minor chords
  expect_equal(affinity(c(12,8,5)),7.66,tolerance=0.001)
  expect_equal(affinity(c(0,7,3)),7.66,tolerance=0.001)
  expect_equal(affinity(c(5,1,-2)),7.66,tolerance=0.001)
  expect_equal(affinity(7),10,tolerance=0.001)
  expect_equal(affinity(-5),9,tolerance=0.001)
  expect_equal(affinity(-17),7,tolerance=0.001)
  expect_equal( affinity(c(2,5,9)) , affinity(c(14,17,21)))
  expect_equal( affinity(c(1,5,8)) , affinity(c(13,17,20)))
})

test_that("affinity_all_levels meets expectations", {
  expect_error(affinity_all_levels(position=-1))
  expect_error(affinity_all_levels(position=13))
  # level -3
  expect_equal(affinity_all_levels(position=12,level=-3),10)
  expect_equal(affinity_all_levels(position=6,level=-3),-2)
  expect_equal(affinity_all_levels(position=11,level=-3),-4)
  expect_equal(affinity_all_levels(position=0,level=-3),10)
  # level -2
  expect_equal(affinity_all_levels(position=12,level=-2),12)
  expect_equal(affinity_all_levels(position=6,level=-2),0)
  expect_equal(affinity_all_levels(position=11,level=-2),-2)
  expect_equal(affinity_all_levels(position=0,level=-2),12)
  # level -1
  expect_equal(affinity_all_levels(position=12,level=-1),14)
  expect_equal(affinity_all_levels(position=6,level=-1),2)
  expect_equal(affinity_all_levels(position=11,level=-1),0)
  expect_equal(affinity_all_levels(position=0,level=-1),14)
  # level 0
  expect_equal(affinity_all_levels(position=0),15)
  expect_equal(affinity_all_levels(position=1),1)
  expect_equal(affinity_all_levels(position=6),3)
  expect_equal(affinity_all_levels(position=11),1)
  expect_equal(affinity_all_levels(position=12),15)
  # level 1
  expect_equal(affinity_all_levels(position=12,level=1),14)
  expect_equal(affinity_all_levels(position=1,level=1),0)
  expect_equal(affinity_all_levels(position=6,level=1),2)
  expect_equal(affinity_all_levels(position=0,level=1),14)
  # level 2
  expect_equal(affinity_all_levels(position=12,level=2),12)
  expect_equal(affinity_all_levels(position=1,level=2),-2)
  expect_equal(affinity_all_levels(position=6,level=2),0)
  expect_equal(affinity_all_levels(position=0,level=2),12)
  # level 3
  expect_equal(affinity_all_levels(position=12,level=3),10)
  expect_equal(affinity_all_levels(position=1,level=3),-4)
  expect_equal(affinity_all_levels(position=6,level=3),-2)
  expect_equal(affinity_all_levels(position=0,level=3),10)
})
test_that("affinity of positions meets expectations", {
  # level -3
  expect_equal(affinity(-24),12)
  expect_equal(affinity(-30),-2)
  expect_equal(affinity(-25),-4)
  expect_equal(affinity(-36),10)
  # level -2
  expect_equal(affinity(-12),14)
  expect_equal(affinity(-18),0)
  expect_equal(affinity(-13),-2)
  expect_equal(affinity(-24),12)
  # level -1
  expect_equal(affinity(0),15)
  expect_equal(affinity(-6),2)
  expect_equal(affinity(-1),0)
  expect_equal(affinity(-12),14)
  # level 0
  expect_equal(affinity(0),15)
  expect_equal(affinity(1),1)
  expect_equal(affinity(6),3)
  expect_equal(affinity(11),1)
  expect_equal(affinity(12),15)
  # level 1
  expect_equal(affinity(24),14)
  expect_equal(affinity(13),0)
  expect_equal(affinity(18),2)
  expect_equal(affinity(12),15)
  # level 2
  expect_equal(affinity(25),-2)
  expect_equal(affinity(30),0)
  expect_equal(affinity(36),12)
  # level 3
  expect_equal(affinity(36),12)
  expect_equal(affinity(37),-4)
  expect_equal(affinity(42),-2)
  expect_equal(affinity(48),10)
})
