###############################################################################
# As described in paper titled
# "The markov chain monte carlo revolution"
# citation: Diaconis, P., 2009. The markov chain monte carlo revolution. Bulletin of the American Mathematical Society, 46(2), pp.179-205.
#
# Taken from the hardcopy from my 2011 Simulation book
#
# Marcela Cespedes
# Monday 12/02/2024


rm(list = ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(purrr)

# Below are snippets of code taken from 
# https://maximilianrohde.com/posts/code-breaking-with-metropolis/

# This example helps explain the Metropolis-Hastings (M-H) algorith
# (a type of MCMC)

##########################################################################
##
## First, let's define some handy functions

# Example of how to make a secret message
chartr(
  x = "Hello Stats Club",
  old = "abcdefghijklmnopqrstuvwxyz",
  new = "hrpjwnbsaxgfizmulcektvydoq")  # <-- from above

## ------------------------------------------------
## Let's define some functions to do this

# Create new ABC mapping
generate_cipher<- function(){
  sample(letters, replace = FALSE)
}


# Encode a text with this cypher
encode_text<- function(text, cipher){
  chartr(x = text,
         old = paste(letters, collapse = ""),
         new = paste(cipher, collapse = ""))
}

# Decode the text
decode_text<- function(ciphered_text, cipher){
  chartr(x = ciphered_text,
         old = paste(cipher, collapse = ""),
         new = paste(letters, collapse = ""))
}


## -------------------------------
# Test above functions
# Give this a try
cipher.1<- generate_cipher()
cipher.1


secret.msg<- "I love stats"
secret.msg.coded<- encode_text(secret.msg, cipher.1)
secret.msg.coded


decode_text(secret.msg.coded, cipher.1)
# viola! it works


# Why not use brute force?
# ANS: permutation problem, it would take 26! permutations to try
# because the ORDER of the letters matters and we would need to check all possible combinations

factorial(26)
#4.032915e+26










############################################################
############################################################
# NOTES:
# As per link (above), even the fastest computer would take 11.6 years

# Why use probability?
# because it would seek out the likely ciphers, and ignore the ones that are unlikely to be correct.
# i.e a clever/ data-driven way to seek out likely ciphers and ignore unlikely ones

# with M-H algorithm, we will 
# 1. start somewhere random,
# 2. make a proposal, and 
# 3. with some probability we will either accept the new proposal - i.e
# with some likelihood explore something more likely, or reject the proposal - i.e
# not go there.
# 4. repeat.

##############################################################
##############################################################

##
## Preparation for de-ciphering example:
## English is a complex language, and we need a lot of English text 
##

# We'll first need information on 
# this is a book in an entire character - length 1
war_and_peace <- readr::read_file("https://www.gutenberg.org/cache/epub/2600/pg2600.txt")

# clean this text
war_and_peace<- war_and_peace %>%
  stringr::str_to_lower() %>%  # convert everything to lower case
  gsub(pattern = "[^A-Za-z]+", replacement = "", x =.) %>% # remove all non-alphabetical characters
  stringi::stri_trans_general(id = "Latin-ASCII") # remove all accent characters


# Break this text into two-character chunks
test_string<- "hello there"
test_string

starting_indices<- 1:(nchar(test_string)-1)
starting_indices

ending_indices <- starting_indices + 1
ending_indices

# Yay! it works!!!
# Estimating frequencies based on paired samples
stringi::stri_sub(test_string,
                  from = starting_indices,
                  to = ending_indices)


##
## Chuck all of this into a function
break_into_two_chars<- function(text){
  starting_indices<- 1:(nchar(text) - 1)
  ending_indices<- starting_indices + 1
  
  return(stringi::stri_sub(text,
                           from = starting_indices,
                           to = ending_indices))
}



# Let's apply this to War and Peace
war_and_peace_2_characters<- break_into_two_chars(war_and_peace)

# check
war_and_peace_2_characters[1:10]

# Look at a piece of the results (skipping the table of contents)
war_and_peace_2_characters[10000:10100]

###################################################################
## IMPORTANT!!!!!
# Now figure out the frequencies of likely words from War and Peace
probability_table<- table(war_and_peace_2_characters)/length(war_and_peace_2_characters)

dim(probability_table)

probability_table[1:40]

# sort it by the most common occuring pairs
probability_table %>%
  sort(decreasing=TRUE) %>%
  head(20)

# we can see that 'th' comes up a lot - 0.031%
# followed by 'in'  'at', etc


# Some two-character combinations did not occur in War and Peace
# Let's say these combinations occurred ONCE in the book - and assign
# suitable probability
get_prob_two_char<- function(two_char){
  prob_from_table<- probability_table[two_char]
  
  if(is.na(prob_from_table)){
    return(1/length(war_and_peace_2_characters))
  }else(
    return(prob_from_table)
  )
}

# test above function

get_prob_two_char("at") # <-- common combination
get_prob_two_char("qq") # <-- UNCOMMON combination

##
## Work done above is the set up
## For any two character combination we can estimate the probability  of such
## combination in English.

# give it a try
sample_text<- "hello there"
sample_text_two_char<- break_into_two_chars(sample_text)
sample_text_two_char


score<- purrr::map_dbl(sample_text_two_char, get_prob_two_char) %>%
  prod()

score # <-- this is a TINY number
# let's work on log-scale

score<- purrr::map_dbl(sample_text_two_char, get_prob_two_char) %>%
  log() %>%
  sum()

score # <-- that's better
# -64.25837
# this is the log-likelihood


# pop this into a function
get_log_lik_text<- function(text){
  text %>%
    break_into_two_chars() %>%
    purrr::map_dbl(get_prob_two_char) %>%
    log() %>%
    sum()
}


# Try it with rubbish text
get_log_lik_text("esto es basura y es espanol")
# -230.29   Spanish text above (translated = This is rubbish and in Spanish), 
# sees this as rubbish in our English log-likelihood

##
## Nearly ready to implement our M-H algorithm
## define function to swap two elements of a vector
swap<- function(x){
  
  # convert to a vector
  #x1<- break_into_two_chars(x)
  
  # Select two distinct indices
  rand_indices<- sample(1:length(x), 
                        size = 2,
                        replace=FALSE)
  
  element_1<- x[rand_indices[1]]
  element_2<- x[rand_indices[2]]
  
  x[rand_indices[1]]<- element_2
  x[rand_indices[2]]<- element_1
  
  # pop back as a single character
  #x2<- paste(x1, collapse = "") 
  return(x)
}

# test
swap(c("some", "random", "text", "to","test", "this", "out"))
#swap("some text")

# this will be swapping ciphers
swap(c("r", "a", "b", "t"))

########################################
########################################
##
## Implement de-ciphering MH algorithm
##
##
#########################################

# some message
plaintext<- "to be or not to be that is the question whether tis nobler in the mind to suffer the slings and arrows of outrageous fortune or to take arms against a sea of troubles"


# Generate way to cipher this
true_cipher<- generate_cipher()
true_cipher


# Encode plaintext
ciphered_text<- encode_text(text = plaintext,
                            cipher = true_cipher)

ciphered_text

# Create another random cipher: initialise
current_cipher<- generate_cipher()
current_cipher

# Counter to keep track of how many decoded texts have been accepted
i<- 0

for(iter in 1:15000){
  
  #######################################
  ## 1. Generate proposal cipher
  # pc1<-  unlist(strsplit(ciphered_text, " "))
  # pc1
  # 
  # # propose a new cipher by swapping two letters in current cipher
  # pc2<- swap(pc1)
  # pc2
  # 
  # proposed_cipher<- paste(pc2, collapse =  " ")
  # proposed_cipher
  
  proposed_cipher<- swap(current_cipher)
  proposed_cipher
  
  ##########################################################
  ## 2. Decode message using current and proposed ciphers
  
  # Text decoded from the proposal cipher
  decoded_text_proposed<- decode_text(ciphered_text,
                                      cipher = proposed_cipher)
  
  decoded_text_proposed
  
  # Text decoded from the current cipher
  decoded_text_current <- decode_text(ciphered_text,
                                      cipher = current_cipher)
  
  decoded_text_current
  
  ###########################################
  # 3. Compute the log-likelihood of both
  
  proposed_log_lik<- get_log_lik_text(decoded_text_proposed)
  proposed_log_lik
  
  current_log_lik<- get_log_lik_text(decoded_text_current)
  current_log_lik
  
  ######################################################################
  # 4. Acceptance probability of the proposal, define by M-H algorithm
  
  acceptance_prob<- min(1, exp(proposed_log_lik - current_log_lik))
  acceptance_prob
  
  # accept ot reject given probability above
  accept<- sample(c(TRUE, FALSE),
                  size = 1,
                  prob = c(acceptance_prob, 1-acceptance_prob))
  accept
  
  
  if(accept){
    current_cipher<- proposed_cipher
    
    # print text as the dcoded by current cipher
    print(glue::glue("Iteration {i}: {decoded_text_proposed}"))
    
    # increment number of times it's accepted
    i<- i+1
  }
}

i




