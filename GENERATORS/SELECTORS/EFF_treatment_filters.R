#### FILTERS TREATMENT

full_mean[p2_1 != 3, p2_1 := 0] # ISOLATION OF CESION
full_mean[p2_1 == 3, p2_1 := 1] # ISOLATION OF CESION
full_mean[!(p2_2 %in%  c(2,3)), p2_2 := 0]# ISOLATION OF INHERITANCE
full_mean[p2_2 %in%  c(2,3), p2_2 := 1]# ISOLATION OF INHERITANCE