# nb101
# num_params on log scale
# quantiles determined based on 20000 random architectures
# small: 0.5 quantile
# medium: 0.01, 0.05, 0.3, 0.5 quantiles
# large: 0.01, 0.02, 0.05, 0.17, 0.3, 0.4, 0.5, 0.75, 0.8 quantiles
nb101_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = log(c(0, 2793866))))
nb101_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = log(c(2793866, Inf))))
nb101_small_nb = NichesBoundaries$new("nb101_small", niches_boundaries = list(niche1 = nb101_small_n1, niche2 = nb101_small_n2))

nb101_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = log(c(0, 706433))))
nb101_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = log(c(706433, 882570))))
nb101_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(num_params = log(c(882570, 1756298))))
nb101_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(num_params = log(c(1756298, 2793866))))
nb101_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(num_params = log(c(2793866, Inf))))
nb101_medium_nb = NichesBoundaries$new("nb101_medium", niches_boundaries = list(niche1 = nb101_medium_n1, niche2 = nb101_medium_n2, niche3 = nb101_medium_n3, niche4 = nb101_medium_n4, niche5 = nb101_medium_n5))

nb101_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = log(c(0, 706442)))
nb101_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = log(c(706442, 823172)))
nb101_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(num_params = log(c(823172, 882570)))
nb101_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(num_params = log(c(882570, 967178)))
nb101_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(num_params = log(c(967178, 1756298)))
nb101_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(num_params = log(c(1756298, 2155680)))
nb101_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(num_params = log(c(2155680, 2793866)))
nb101_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(num_params = log(c(2793866, 5532810)))
nb101_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(num_params = log(c(5532810, 6667274)))
nb101_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(num_params = log(c(6667274, Inf)))
nb101_large_nb = NichesBoundaries$new("nb101_large", niches_boundaries = list(niche1 = nb101_large_n1, niche2 = nb101_large_n2, niche3 = nb101_large_n3, niche4 = nb101_large_n4, niche5 = nb101_large_n5, niche6 = nb101_large_n6, niche7 = nb101_large_n7, niche8 = nb101_large_n8, niche9 = nb101_large_n9, niche10 = nb101_large_n10))

# nb201
# latency
# quantiles determined based on 20000 random architectures on cifar10
# small: 0.5 quantile
# medium: 0.01, 0.05, 0.3, 0.5 quantiles
# large: 0.01, 0.02, 0.05, 0.17, 0.3, 0.4, 0.5, 0.75, 0.8 quantiles
nb201_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.01503411)))
nb201_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0.01503411, Inf)))
nb201_small_nb = NichesBoundaries$new("nb201_small", niches_boundaries = list(niche1 = nb201_small_n1, niche2 = nb201_small_n2))

nb201_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.008502045)))
nb201_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0.008502045, 0.010319312)))
nb201_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0.010319312, 0.013641099)))
nb201_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0.013641099, 0.015034115)))
nb201_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0.015034115, Inf)))
nb201_medium_nb = NichesBoundaries$new("nb201_medium", niches_boundaries = list(niche1 = nb201_medium_n1, niche2 = nb201_medium_n2, niche3 = nb201_medium_n3, niche4 = nb201_medium_n4, niche5 = nb201_medium_n5))

nb201_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 0.008502045)))
nb201_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0.008502045, 0.008936170)))
nb201_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0.008936170, 0.010319312)))
nb201_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0.010319312, 0.012185984)))
nb201_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0.012185984, 0.013641099)))
nb201_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(latency = c(0.013641099, 0.014302651)))
nb201_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(latency = c(0.014302651, 0.015034115)))
nb201_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(latency = c(0.015034115, 0.017073179)))
nb201_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(latency = c(0.017073179, 0.017585874)))
nb201_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(latency = c(0.017585874, Inf)))
nb201_large_nb = NichesBoundaries$new("nb201_large", niches_boundaries = list(niche1 = nb201_large_n1, niche2 = nb201_large_n2, niche3 = nb201_large_n3, niche4 = nb201_large_n4, niche5 = nb201_large_n5, niche6 = nb201_large_n6, niche7 = nb201_large_n7, niche8 = nb201_large_n8, niche9 = nb201_large_n9, niche10 = nb201_large_n10))

