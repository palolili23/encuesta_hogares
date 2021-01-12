# 
# location <- here()
# 
# 
# data_dir <- paste0(location, "/002_eh/persona")
# 
# sav_files <- fs::dir_ls(data_dir, regexp = "\\.sav$")
# 
# data <- sav_files %>% 
#   map(haven::read_sav) %>% 
#   map(labelled::unlabelled)

# vars1 <- data %>% pluck(1) %>% colnames()
# vars2 <- data %>% pluck(2) %>% colnames()
# vars3 <- data %>% pluck(3) %>% colnames()
# 
# unique_vars <- vars3[vars3 %in% vars1]

# data <- data %>% 
#   map(select, unique_vars)