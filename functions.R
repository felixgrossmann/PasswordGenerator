generatePassword <- function(pwd_length, lowercase, uppercase, numbers, specialchars, specialchars_string){
  # List of lowercase letters
  list_lowercase <- letters
  # List of uppercase letters
  list_uppercase <- LETTERS
  # List of numbers
  list_numbers <- seq(0, 9, 1)
  
  # Number of symbols in each array
  length_lowercase <- length(list_lowercase)
  length_uppercase <- length(list_uppercase)
  length_numbers <- length(list_numbers)
  length_specialchars <- nchar(specialchars_string)
  
  # Create an empty string
  pwd <- ""
  # Variable which counts the number of symbols in the password array
  count <- 1
  
  while(1)
  {
    # rnd01: determines if the next symbol is a...
    # 0-0.25: lowercase
    # 0.25-05: uppercase
    # 0.5-0.75: numbers
    # 0.75-1: specialchar
    rnd01 <- runif(1,0)
    
    if(rnd01 <= 0.25)
    {
      if(lowercase == TRUE)
      {
        # rnd02: determines the exact lowercase out of the array
        rnd02 <- 1 + runif(1,0)*(length_lowercase-1)
        # Paste the new symbol to the rest of the string
        pwd <- paste0(pwd, list_lowercase[rnd02])
        # Increase the counter variable count
        count <- count+1
      }
    }
    else if(rnd01 > 0.25 & rnd01 <= 0.5)
    {
      if(uppercase == TRUE)
      {
        # Same as in lines 29 to 34
        rnd02 <- 1 + runif(1,0)*(length_uppercase-1)
        pwd <- paste0(pwd, list_uppercase[rnd02])
        count <- count+1
      }
    }
    else if(rnd01 > 0.5 & rnd01 <= 0.75)
    {
      if(numbers == TRUE)
      {
        # Same as in lines 29 to 34
        rnd02 <- 1 + runif(1,0)*(length_numbers-1)
        pwd <- paste0(pwd, list_numbers[rnd02])
        count <- count+1
      }
    }
    else
    {
      if(specialchars == TRUE)
      {
        # Same as in lines 29 to 34
        rnd02 <- 1 + runif(1,0)*(length_specialchars-1)
        pwd <- paste0(pwd, substr(specialchars_string, rnd02, rnd02))
        count <- count+1
      }
    }
    
    # If the password length is reached, stop!
    if(count > pwd_length)
    {
      break
    }
  }
  
  # Return the password
  return(pwd)
}