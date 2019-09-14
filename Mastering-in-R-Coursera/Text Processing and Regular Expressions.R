paste("Square", "Circle", "Triangle")
#By default the paste() function inserts a space between each word. 
#You can insert a different string between each word by specifying the sep argument
paste("Square", "Circle", "Triangle", sep = "+")
#A shortcut for combining all of the string arguments without any characters 
#in between each of them is to use the paste0() function
paste0("Square", "Circle", "Triangle")

shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

#You can also collapse all of the elements of a vector of strings into a single string 
#by specifying the collapse argument
paste(shapes, collapse = " ")

# The nchar()function counts the number of characters in a string
nchar("Supercalifragilisticexpialidocious")

#toupper() and tolower() functions make strings all uppercase or lowercase respectively
cases <- c("CAPS", "low", "Title")
tolower(cases)
toupper(cases)

#One of the most basic functions in R that uses regular expressions is the 
#grepl() function, which takes two arguments: a regular expression and a string 
#to be searched.
regular_expression <- "a"
string_to_search <- "Maryland"

grepl(regular_expression, string_to_search)

regular_expression <- "u"
string_to_search <- "Maryland"

grepl(regular_expression, string_to_search)

head(state.name)

# The first metacharacter that we’ll discuss is ".". 
#The metacharacter that only consists of a period represents any character 
#other than a new line
grepl(".", "Maryland")
grepl(".", "*&2[0+,%<@#~|}")
grepl(".", "") #FALSE

grepl("a.b", c("aaa", "aab", "abb", "acadb"))# FALSE  TRUE  TRUE  TRUE

# Does "Maryland" contain one or more of "a" ?
grepl("a+", "Maryland") #TRUE

# Does "Maryland" contain one or more of "x" ?
grepl("x+", "Maryland") #FALSE

# Does "Maryland" contain zero or more of "x" ?
grepl("x*", "Maryland") #TRUE

#You can also specify exact numbers of expressions using curly brackets {}. 
#For example "a{5}" specifies “ five times,”"a{2,5}" specifies “a between 2 and 5 times
# and "a{2,}"specifies “a at least 2 times.” Let’s take a look at some examples
# Does "Mississippi" contain exactly 2 adjacent "s" ?
grepl("s{2}", "Mississippi") #TRUE
# This is equivalent to the expression above:
grepl("ss", "Mississippi") #TRUE

# Does "Mississippi" contain between 1 and 3 adjacent "s" ?
grepl("s{1,3}", "Mississippi")#TRUE

# Does "Mississippi" contain between 2 and 3 adjacent "i" ?
grepl("i{2,3}", "Mississippi")#FALSE

# Does "Mississippi" contain between 2 adjacent "iss" ?
grepl("(iss){2}", "Mississippi") #TRUE

# Does "Mississippi" contain between 2 adjacent "ss" ?
grepl("(ss){2}", "Mississippi")#FALSE

# Does "Mississippi" contain the pattern of an "i" followed by 
# 2 of any character, with that pattern repeated three times adjacently?
grepl("(i.{2}){3}", "Mississippi")#TRUE

grepl("\\w", "abcdefghijklmnopqrstuvwxyz0123456789")#TRUE

grepl("\\d", "0123456789")#TRUE

# "\n" this regex for a new line and "\t" is the regex for a tab

grepl("\\s", "\n\t   ")#TRUE

grepl("\\d", "abcdefghijklmnopqrstuvwxyz")#FALSE

grepl("\\D", "abcdefghijklmnopqrstuvwxyz")#TRUE

grepl("\\w", "\n\t   ")#FALSE

#You can also specify specific character sets using straight brackets[]. 
#For example a character set of just the vowels would look like:"[aeiou]". 
#You can find the complement to a specific character by putting a carrot ^ 
#after the first bracket. 
#For example "[^aeiou]"matches all characters except the lowercase vowels. 
#You can also specify ranges of characters using a hyphen - inside of the brackets. 
#For example "[a-m]" matches all of the lowercase characters between a and m, 
#while "[5-8]" matches any digit between 5 and 8 inclusive. 
grepl("[aeiou]", "rhythms")# FALSE

grepl("[^aeiou]", "rhythms")#TRUE

grepl("[a-m]", "xyz")#FALSE

grepl("[a-m]", "ABC")#FALSE

grepl("[a-mA-M]", "ABC")#TRUE

#Putting two backslashes before a punctuation mark that is also a metacharacter 
#indicates that you are looking for the symbol and not the metacharacter meaning. 
#For example "\\." indicates you are trying to match a period in a string. 
grepl("\\+", "tragedy + time = humor")#TRUE

grepl("\\.", "http://www.jhsph.edu/")#TRUE

#There are also metacharacters for matching the beginning and the end of a string 
#which are "^" and "$" respectively.
grepl("^a", c("bab", "aab"))#FALSE  TRUE

grepl("b$", c("bab", "aab"))#TRUE TRUE

grepl("^[ab]+$", c("bab", "aab", "abc"))#TRUE  TRUE FALSE

#The last metacharacter we’ll discuss is the OR metacharacter ("|"). 
#The OR metacharacter matches either the regex on the left 
#or the regex on the right side of this character. 

grepl("a|b", c("abc", "bcd", "cde"))#TRUE  TRUE FALSE

grepl("North|South", c("South Dakota", "North Carolina", "West Virginia"))#TRUE  TRUE FALSE

#1.We match the beginning of a string.
#2.We create a character set of just capitalized vowels.
#3.We specify one instance of that set.
#4.Then any number of characters until:
#5.A character set of just lowercase vowels.
#6.We specify one instance of that set.
#7.We match the end of a string.
start_end_vowel <- "^[AEIOU]{1}.+[aeiou]{1}$"
vowel_state_lgl <- grepl(start_end_vowel, state.name)
head(vowel_state_lgl)
state.name[vowel_state_lgl]

#grepl() which stands for “grep logical.”
grepl("[Ii]", c("Hawaii", "Illinois", "Kentucky")) #TRUE  TRUE FALSE
#grep() which returns the indices of the vector that match the regex 
grep("[Ii]", c("Hawaii", "Illinois", "Kentucky"))
#1 2

#The sub() function takes as arguments a regex, a “replacement,” and a vector 
#of strings, and replace the first instance of that regex found in each string.
sub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky"))# "Hawa1i" "1llinois" "Kentucky"

#The gsub() function is nearly the same as sub() except it will replace 
#every instance of the regex that is matched in each string.
gsub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky"))#"Hawa11"   "1ll1no1s" "Kentucky"

#The strsplit() function will split up strings according to the provided regex.
#If strsplit() is provided with a vector of strings it will return 
#a list of string vectors.
two_s <- state.name[grep("ss", state.name)]
two_s #"Massachusetts" "Mississippi"   "Missouri"      "Tennessee"    

strsplit(two_s, "ss")
#[[1]]
#[1] "Ma"        "achusetts"

#[[2]]
#[1] "Mi"   "i"    "ippi"

#[[3]]
#[1] "Mi"   "ouri"

#[[4]]
#[1] "Tenne" "ee" 

library(stringr)
#The majority of the function names in stringr begin with str_.
#The str_extract() function returns the sub-string of a string that matches 
#the providied regular expression.

state_tbl <- paste(state.name, state.area, state.abb)
head(state_tbl)
str_extract(state_tbl, "[0-9]+")

#The str_order() function returns a numeric vector that corresponds to 
#the alphabetical order of the strings in the provided vector
head(state.name)
str_order(state.name)

head(state.abb)
str_order(state.abb)

#The str_pad() function pads strings with other characters which is often useful 
#when the string is going to be eventually printed for a person to read.
str_pad("Thai", width = 8, side = "left", pad = "-")# "----Thai"

str_pad("Thai", width = 8, side = "right", pad = "-")#"Thai----"

str_pad("Thai", width = 8, side = "both", pad = "-")# "--Thai--"

#The str_to_title() function acts just like tolower() andtoupper() 
#except it puts strings into Title Case.
cases <- c("CAPS", "low", "Title")
str_to_title(cases)  #"Caps"  "Low"   "Title"

#The str_trim() function deletes whitespace from both sides of a string.
to_trim <- c("   space", "the    ", "    final frontier  ")
str_trim(to_trim)#"space" "the" "final frontier"

#The str_wrap() function inserts newlines in strings so that 
#when the string is printed each line’s length is limited.
pasted_states <- paste(state.name[1:20], collapse = " ")

#用cat能把\n 分列，不然會擠在一起
cat(str_wrap(pasted_states, width = 30))
#Alabama Alaska Arizona
#Arkansas California Colorado
#Connecticut Delaware Florida
#Georgia Hawaii Idaho Illinois
#Indiana Iowa Kansas Kentucky
#Louisiana Maine Maryland

#The word() function allows you to index each word in a string as if it were a vector
a_tale <- "It was the best of times it was the worst of times it was the age of wisdom it was the age of foolishness"

word(a_tale, 2)# "was"

word(a_tale, end = 3)# "It was the"

word(a_tale, start = 11, end = 15)# "of times it was the"
