funcs_to_load <- c("Matrix","lubridate","corpcor","zoo") # by 'strain'
sapply(rev(funcs_to_load), function(x) system.time(do.call("library",list(x))))
#
# (because of Matrix)
#
#            Matrix corpcor lubridate pborepo   zoo
# user.self   3.084   0.040     0.452   0.434 0.091
# sys.self    0.105   0.002     0.031   0.021 0.004
# elapsed     3.462   0.129     0.577   0.627 0.134
# user.child  0.000   0.000     0.000   0.000 0.000
# sys.child   0.000   0.000     0.000   0.000 0.000
#
