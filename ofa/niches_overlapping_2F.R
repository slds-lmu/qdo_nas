# note10 latency and size
ofa_note10_small_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 20), size = c(0, 20)))
ofa_note10_small_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 35), size = c(0, 20)))
ofa_note10_small_nb = NichesBoundaries$new("ofa_note10", niches_boundaries = list(niche1 = ofa_note10_small_n1, niche2 = ofa_note10_small_n2))

ofa_note10_medium_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 20), size = c(0, 20)))
ofa_note10_medium_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 25), size = c(0, 20)))
ofa_note10_medium_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 30), size = c(0, 20)))
ofa_note10_medium_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 35), size = c(0, 20)))
ofa_note10_medium_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, 40), size = c(0, 20)))
ofa_note10_medium_nb = NichesBoundaries$new("ofa_note10", niches_boundaries = list(niche1 = ofa_note10_medium_n1, niche2 = ofa_note10_medium_n2, niche3 = ofa_note10_medium_n3, niche4 = ofa_note10_medium_n4, niche5 = ofa_note10_medium_n5))

ofa_note10_large_n1 = NicheBoundaries$new("niche1", niche_boundaries = list(latency = c(0, 20), size = c(0, 20)))
ofa_note10_large_n2 = NicheBoundaries$new("niche2", niche_boundaries = list(latency = c(0, 23), size = c(0, 20)))
ofa_note10_large_n3 = NicheBoundaries$new("niche3", niche_boundaries = list(latency = c(0, 26), size = c(0, 20)))
ofa_note10_large_n4 = NicheBoundaries$new("niche4", niche_boundaries = list(latency = c(0, 29), size = c(0, 20)))
ofa_note10_large_n5 = NicheBoundaries$new("niche5", niche_boundaries = list(latency = c(0, 32), size = c(0, 20)))
ofa_note10_large_n6 = NicheBoundaries$new("niche6", niche_boundaries = list(latency = c(0, 35), size = c(0, 20)))
ofa_note10_large_n7 = NicheBoundaries$new("niche7", niche_boundaries = list(latency = c(0, 38), size = c(0, 20)))
ofa_note10_large_n8 = NicheBoundaries$new("niche8", niche_boundaries = list(latency = c(0, 41), size = c(0, 20)))
ofa_note10_large_n9 = NicheBoundaries$new("niche9", niche_boundaries = list(latency = c(0, 44), size = c(0, 20)))
ofa_note10_large_n10 = NicheBoundaries$new("niche10", niche_boundaries = list(latency = c(0, 47), size = c(0, 20)))
ofa_note10_large_nb = NichesBoundaries$new("ofa_note10", niches_boundaries = list(niche1 = ofa_note10_large_n1, niche2 = ofa_note10_large_n2, niche3 = ofa_note10_large_n3, niche4 = ofa_note10_large_n4, niche5 = ofa_note10_large_n5, niche6 = ofa_note10_large_n6, niche7 = ofa_note10_large_n7, niche8 = ofa_note10_large_n8, niche9 = ofa_note10_large_n9, niche10 = ofa_note10_large_n10))

