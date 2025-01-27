# THIS LOOP WILL BE USED TO PULL THE COLUMNS BASED ON THE FILE NAME

  if (base_filename == "Softball Throwing master") {
    data <- data[, c(1:10, 23:38, 40:43)]
  } else if (base_filename == "Softball Hitting master") {
    data <- data[, c(1:10, 23:38, 40:43)]
  } else if (base_filename == "Baseball Throwing master") {
    data <- data[, c(1:10, 24:39, 41:44)]
  } else {
    data <- data[, c(1:10, 22:37, 39:42)] 
  }
