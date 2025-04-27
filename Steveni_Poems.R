library(tidyverse) 
library(markovchain) 

steveni <- read_lines("Steveni.txt")
steveni2 <- read_lines("Steveni2.txt")
steveni3 <- read_lines("Steveni3.txt")
steveni <- c(steveni, steveni2, steveni3)

# punctuation <- c(",", ":", ";", ".", " ")
# punctuation_probs <- c(0.03, 0.01, 0.01, 0.01 )


steveni_chain <- markovchainFit(steveni)
#cat(markovchainSequence(n = 10, markovchain = steveni_chain$estimate), collapse =  " ")

write_a_line <- function(n_lines = 1, chain = steveni_chain) {
  walk(1:n_lines, function(.x) {
    # put together lines of more or less average length
    lines <- markovchainSequence(n = sample(c(5:8), 1), 
                                 markovchain = chain$estimate) %>% 
      paste(collapse = " ")
    
    cat(paste0(lines, "  \n"))
    
  })
}

psuedosonnet <- function() {
  walk(1:3, function(.x) {
    write_a_line(4)
    cat("  \n")
  })
  
  write_a_line(2)
}


set.seed(154)
psuedosonnet()

