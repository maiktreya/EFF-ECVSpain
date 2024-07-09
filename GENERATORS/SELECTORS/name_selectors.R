
gross_var_names <- read.csv2(file = "SELECTORS/csv_lists/EFF_SELECTION.csv")[1]

gross_var_ex <- array()
gross_var_aux <- array()
gross_var_ex2 <- array()

for (i in seq_along(unlist(gross_var_names))) {
    gross_var_ex[i] <- as.character(sub(" .*$", "", gross_var_names[i, ]))
    gross_var_aux[i] <- as.character(sub("^\\S+\\s+", "", gross_var_names[i, ]))
    gross_var_ex2[i] <- as.character(sub("^\\S+\\s+", "", gross_var_aux[i]))
}

gross_var_ex_alt <- stringr::str_replace_all(gross_var_ex, "_", ".")

net_var_names <- data.frame(gross_var_ex, gross_var_ex_alt, gross_var_ex2)

print(net_var_names)
