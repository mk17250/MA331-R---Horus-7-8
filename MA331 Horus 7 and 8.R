##################################### HoRus 7 & 8 ########################################################
### Case Study ###
### Selfreported Heights ###

## One of the challenges that face a data scientist involves extracting numeric data stored 
## in character strings and converting them into a sensible numeric representation required 
## to analyse them by computing summaries, creating plots, or fitting statistical models. 
## Many of the string processing challenges are unique and often unexpected. However in this 
## section, we will cover methods that are commonly used for handling string processing in most
## of the Data Science projects of real-world problems. We will use a case study on self-reported 
## heights to describe how to convert the original raw data Collected via online forms into a 
## sensible ready-to-analyse format as in the data frame shown below:

## These data were originally obtained using a web form in which students were asked to enter their 
## heights in inches. Although the instructions asked for a number, the students could enter anything.
##A total of 1,095 submissions have been compiled, but unfortunately the height column vector had 
## several non-numeric entries and - as a result - became a character vector. The raw original data 
## can be viewed from the dslabs package. They looked as follows (You may notice the different inputs 
## of heights if you scroll to next pages of the following table. You would see formats like 5' 4", 
## 165cm, etc. )

require(dslabs)
data("reported_heights")
reported_heights

## We can check the class of the height vector as follows:
class(reported_heights$height)

## As we have studied earlier, we can parse a character Vector into numbers by using the as.numeric 
## function. However, if we try to do that here we get a warning:
new_ht <- as.numeric(reported_heights$height)

## Although most values appear to be height in inches as requested, we ended up with many missing values (NAs):
sum(is.na(new_ht)) # count how many NAs in the x vector

## We can see some of the entries that are not successfully converted to numbers by using the 
## filter function - to keep only the records resulting in NAs - as follows:

require(dplyr)
mutate(reported_heights, new_ht = suppressWarnings(as.numeric(height))) %>% # supress the NAs warning
  filter(is.na(new_ht)) %>%
  head(n = 10)

## We now see why we did end up with some NAs. Some students didn't report their heights in inches 
## as requested. We could discard these entries, but many of them follow patterns that in 
## principle we can easily convert to inches. For instance, we see many cases that use the 
## format F' I'' with F and I represent feet and inches respectively. Each of these entries 
## can be converted to inches: for example, 6' 3'' is 6*12 + 3 = 75. We now need to construct 
## an automated approach to fix all the problematic entries. Such an automated approach will be 
## quite helpful to avoid human potential mistakes - if these would rather be fixed by hand - 
## and it will save time and effort if we plan on continuing to collect data using the same web form. 
## Therefore, writing a code that automatically perform such a process will be convenient.

## The first step is to survey the problematic entries and try to define specific patterns that 
## are followed by large groups of entries (the larger these groups the more entries we can 
## fix with a single programmatic approach). Our aim is to find patterns that can be accurately 
## described with a specific rule, e.g. "a digit, followed by a feet symbol, followed by one or 
## two digits, followed by an inch symbol".

## To this aim, we are going to focus only on the problematic entries defined as: the entries 
## resulted in NAs when applying the as.numeric function; the entries outside a range of 
## plausible heights (99.9999% of the adult population heights are between 50 to 84 inches inclusive).
## We will write the function to help us do this:

not_inches <- function(x, smallest = 50, tallest = 84){
  inches = suppressWarnings(as.numeric(x))
  ind = is.na(inches) | inches < smallest | inches > tallest  # logical vector: TRUE refers to a problematic entry
  return(ind)
}

## Using our defined function, we will create a vector that contains all the problematic entries:
problems <- filter(reported_heights, not_inches(height)) %>%
  pull(height)
head(problems, n = 20)

length(problems)

## We can then print all the problematic entries by typing problems. After surveying them 
## carefully, we can see there are three patterns describing three large groups within these entries.
## A pattern of the form F'I, F' I'', or F'I" with F and I representing feet and inches, 
## respectively. Here are a few examples examples:

## 5' 4"   5'7   5'7"   5'3"   5'11   5'9''   5'10''   5' 10   5'5"   5'2"

## A pattern of the form F.I or F,I. Here are a few examples:

## 5.3   5.5   6.5   5.8   5.6   5,3   5.9   6,8   5.5   6.2

## Entries that were clearly reported in cm rather than inches. Here are a few examples:

##  150   175   177   178   163   175   178   165   165   180

## Now, we can develop a plan of attack. Our plan is designed to help teach as many useful methods as possible, but surely there is a more efficient way of performing such a task.

## Plan of attack: We plan to perform the following steps: 1- Convert entries fitting the first 
## two patterns into a standardized one, e.g. F' I. 2- Use the defined standardization to extract 
## the feet and inches and convert them to inches using the formula: F*12 + I. 3- define a 
## procedure (we will see how later) for identifying entries that are in cm and convert them 
## to inches by dividing the height by 2.54. 4- Check and improve: check again to see what entries
## were not yet fixed and see if we can tweak our approach to be more comprehensive.

## To achieve our goal, we will use the stringr package which is very useful in string processing.

########################################### II. Escape special characters ################################

## Before we go any further, we will describe how to escape certain characters when defining a string in R, 
## an important technique for string processing.

## Strings in R are defined by using either double quotes or single quote:

s <- "Hello!"   # double quotes
s <- 'Hello'    # single quote - make sure you use the correct single quote, NOT the back quote (``).

## What happens if the string we want to define includes double quotes? For example, 
## if we want to write 10 inches like this 10"? In this case you can't use double quotes,
## because this would just be the string 10 followed by a double quote. R should give us an 
## error because you have an unclosed double quote:

# s <- "10""

## To avoid this, we can use the single quote. Note that when we print the s object, 
## we would see that the double quotes are escaped with the backslash \, which means 
##"escape the special function of the following character and treat it as it is":

s <- "10\""

## The function cat allows to print the string actually in the form in which it looks like

cat(s)

## What if we want to write something like 6' 10" to represent 6 feet and 10 inches? In this case,
## neither the single nor double quotes will work alone. We should escape the functions of the 
## quotes using \ as follows:

# either of these two definitions can be used:
s <- "6' 10\""
s <- '6\' 10"'
cat(s)

######################## III. Regular expressionsA regular expression (regex) ########################
## is used for defining specific patterns strings. They can be used to determine whether 
## a given string matches the defined pattern. A set of rules has been defined to precisely 
## construct patterns and to efficiently detect them in the text. We will cover these rules using 
## general examples and using examples from our case study

######################## 1. Special characters: | and \d #############################################

## To illustrate the idea of each of the following rules in regex, we would create two simple vectors
## named: 'yes' (with strings that match the pattern of interest) and 'no' (with strings that do
## NOT match the pattern of interest).

## If we want to detect which of the following strings contain the text cm or inches?
# create two simple vectors 'yes' and 'no', then concatenate them together in a single vector
yes <- c("180 cm", "70 inches")
no  <- c("180", "70''")
s <- c(yes, no)    # concatenate both vectors in a single vector
s

## We can detect which of the strings contain the cm or inches text as follows:

require(stringr)

str_detect(s, "cm") | str_detect(s, "inches")

## Another special character is \d which means any digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. 
## The backslash is used to distinguish it from the character d. In R, we have to escape the 
## backslash \ itself, so we actually have to use \\d to represent digits in regex. Here is an 
## example:

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

## Other relevant functions of the stringr package are the str_view and str_view_all 
## functions, which shows the first match and all the matches, respectively, for each string 
## (element):

str_view(s, pattern)

str_view_all(s, pattern)

################################### 2. Classes of Characters #######################################
## Classes of Characters are used for defining a series of characters. They are defined using 
## square brackets []. For instance, if we want to define a pattern that matchs only if there is 
## 5 or 6 in the expression, we can use the regex [56]:

str_detect(s, "[56]")

str_view_all(s, "[56]")

## We can use ranges within the square brackets [] to define character classes. For example, 
## if we want to match values between 4 and 7, we can use the regex [4-7]. This means that the 
## \\d - that we introduced earlier - is equivalent to [0-9]. NOTE that: in regex everything is 
## defined as a character; there are no numbers; So 7 is the character 7 not the number seven. 
## This means that the regex [1-30] simply means this character class includes the character 1 
## through 3 or the character 0, i.e. it matches with: 0, 1, 2, or 3. Keep in mind that characters 
## do have an order: the digits do follow the numeric order; all lower case letters can be defined 
## as [a-z], upper case letters as [A-Z], and both as [a-zA-z].

############################################# 3. Anchors ###########################################
## To define patterns that must start or end with a specific character, we define regex with 
## anchors, ^ and $ which represent the beginning and end of a string, respectively. For instance, 
## the regex ^\\d$ is read as "start of the string followed by one digit followed by end of string".
## This pattern detects only the strings with exactly one digit:

pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 7", "a4", "b.2")
s <- c(yes, no)
str_view_all(s, pattern)

## NOTE THAT: The 7 does not match because it does not start with the digit but rather with a 
## white space, which is actually not easy to see!

############################################ 4. Quantifiers #########################################
## In our case study, the inches part could have one or two digits. This can be specified in regex 
## with quantifiers using the {}. For instance, following a part of the pattern with {2} means that 
## the previous entry appears twice. In the following example we illustrate the pattern matching 
## one or two digits:

pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b11")
s <- c(yes, no)
str_view_all(s, pattern)

## To detect the patterns for feet and inches, we can add the symbols for feet ' and inches " 
## after the digits in the form of F'I\" as follows: ^[4-7]'\\d{1,2}\"$. The pattern is now 
## getting little complex, but you can look at it carefully and break it down:

# ^ = start of the string.
# [4-7] = one digit, either 4,5,6 or 7. (plausible values for adult heights in feet).
# ' = feet symbol.
# \\d{1,2} = one or two digits.
# \" = inches symbol (escaped with the \).
# $ = end of the string.

## Let's try this out:

pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'14\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64", "5' 9\"")
s <- c(yes, no)
str_detect(s, pattern)

## Up tp now, we permit the inches to be 12 or larger. This is implausible if a student use feet
## and inches to input their height. We will learn how to add a restriction later as the regex for 
##this is a bit more complex than we are ready to show. For now, we are accepting this.

################################## 5. White space \s ###############################################
## Our previous pattern does not match 5' 9" because there is a space between ' and 9. 
## The defined pattern does not permit that. Spaces are characters and R does not ignore them: 
identical("DS@Essex", "DS@Essex ")

## \s represents white space, but we need to escape the backslash as we have done with \d before,
## so we use \\s. To match pattern like 5' 9", we can update our pattern by ading the \\s. 
## To illustrate that, we will look for such a pattern in the problematic entries of our case 
## study example as follows:

pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$" # defining our new pattern
str_subset(problems, pattern_2) # show the entries of the vector 'problems' (defined earlier) that match the pattern.

## NOTE THAT: this will not match the patterns with no space. Do we need to define more than one 
## regex pattern? We will find out in the following section.

########################################## 6. Special quantifiers: *, ?, + ##########################
## We want our pattern to permit spaces but not require them. Even if there are several spaces, e.g.
## 5'   4, we still want it to match. There is a typical special quantifier for this purpose: 
## the symbol * which means zero or more occurrences of the previous character. There are two other 
## similar quantifiers: none or once which can be defined using ?; one or more which can be defined 
## using +. We can see how they differ with this example:

pattern_none_or_more <- "^[4-7]'\\s*\\d{1,2}\"$"  # we used '*'
pattern_none_or_once <- "^[4-7]'\\s?\\d{1,2}\"$"  # we used '?'
pattern_once_or_more <- "^[4-7]'\\s+\\d{1,2}\"$"  # we used '+'

string <- c("5'4\"", "5' 4\"", "5'  4\"")  # no spaces, one space, two spaces
data.frame(string, description = c("No spaces", "one space", "two spaces"),
           none_or_more = str_detect(string, pattern_none_or_more),
           none_or_once = str_detect(string, pattern_none_or_once),
           once_or_more = str_detect(string, pattern_once_or_more))

########################################## 7. Not ##################################################
## To specify patterns that we do not want to match, we can use the ^ symbol but only inside square 
## brackets. Remember that the ^ outside the square bracket means the start of the string. For 
## instance, if we want to detect digits that are preceded by anything except a letter we can do 
## the following:

pattern <- "[^a-zA-Z]\\d"  # A digit that is preceded by anything except a letter.
yes <- c(".3", "+2", "-0","*4")
no <- c("A3", "b2", "C0", "E4")
s <- c(yes, no)
str_detect(s, pattern)

## Another way to generate a pattern that searches for "everything except" is to use the upper 
## case of the special character. For example, \\D means anything other than a digit, \\S means 
## anything except a space, etc. 

################################## 8. Groups using () ##############################################
## Groups are a powerful feature of the regex. They are defined using () and do NOT affect the 
## pattern matching per se. It rather helps to identify and extract specific parts of the pattern. 
## For example, we have some height entries in a form of F.I:

pattern_without_groups <- "^[4-7]\\.\\d{1,2}$"  # we needed to escape the '.' character as it represent a special regex function means "any character", if it's not escaped.
str_subset(problems, pattern_without_groups)

## We want to change heights written as F.I into F'I, the standardisation defined earlier. 
## First, we need to extract the digits F and I so we can then form the standardised format. 
## First, We will define the groups as follows:

pattern_with_groups <- "^([4-7])\\.(\\d{1,2})$" # we define two groups using () for the two parts that we want to keep for later use, e.g. extract

## NOTE THAT: Adding groups does not affect the detection. It only means that we want to save what 
## is captured by the groups for later use. You can see that defining groups would return the same 
## result obtained using str_subset:

Feet.Inches_entries <- str_subset(problems, pattern_with_groups)
Feet.Inches_entries

## Once we define groups, we can use the function str_match to extract the values these groups define:

str_match(Feet.Inches_entries, pattern_with_groups) %>% head

## The str_match function returns a matrix. The first column of the above matrix was actually the part of 
## the string matching the pattern. The second and third columns contained feet and inches,
## respectively. If no match occurred at an entry, we would have seen an NA in the entire 
## corresponding row.

####################################### HoRus 8 ####################################################
 
################################ I. Search and replace #############################################

### Using regex ###

## In the previous tutorial, we have learned how to use regular expressions (regex) in R. 
## In this week, we will learn how to put them into action to process the data of reported heights 
## from our case study. As a quick reminder, the vector problems containing the problematic entries
## were as follows:

problems

## So far, we can see that not too many of our problematic entries (only 14 entries indeed) are in the form F'I":

pattern <- "^[4-7]'\\d{1,2}\"$"
require(stringr)
require(dplyr)
str_detect(problems, pattern) %>% sum

## By surveying the problems vector, we can get an indication of why this is the case. 
## Here, we will only show a few examples of why we don't have more matches:

head(problems, n=20) %>% str_view(pattern)

## An initial problem we see immediately is that some students used the words "feet" and "inches", 
## and some used two single quotes '' instead of a double quote ". We can see the entries that did 
## these with the str_subset function:

str_subset(problems, "''|inches")

## To correct this, we can replace all these different ways of representing feet and inches with 
## the standardised form of F'I. Therefore, our desired pattern would be:

pattern <- "^[4-7]'\\d{1,2}$"
## replace different ways of representing feet and inches, then detect the pattern
problems %>%
  str_replace("feet|ft|foot", "'") %>%     # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>%   # remove all inches symbols
  str_detect(pattern) %>%
  sum

## By doing the replacement before the matching (as done above), we got many more matches 
## (now 48 out of a total of 292). However, we still have many more entries to go. 
## Let's improve our pattern further by allowing white spaces in front of and after the feet 
## symbol ', i.e. to permit spaces between the feet symbol and the numbers:

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>%     # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>%   # remove all inches symbols
  str_detect(pattern) %>%
  sum

## Now we did match a few more entries.

## Another large set of problematic entries were of the form F.I, F,I and F I. We want to change all these to our standardised form F'I.

######################################### Search and replace using group values ##########################

## We now want to define a pattern that helps us convert all the F.I, F,I and F I to our 
## standardised form. We will do that by defining groups using the () so that we can later extract 
## the F and I values. We will define the following pattern that is flexible to allow capturing all
## these cases:

pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# This might be a complex pattern, but let's break it down:
#   
#   ^ = start of the string.
# [4-7] = one digit, either 4, 5, 6, or 7.
# \\s* = none or more white spaces.
# [,\\.\\s+] = feet symbol is either ,, . or at least one space.
# \\s* = none or more white spaces.
# \\d* = none or more digits.
# $ = end of the string.
# Let's show the entries matching this pattern:

str_subset(problems, pattern_with_groups)

##] We can now replace them with our desired format. The regex special character for printing 
## the i-th group is \\i. Therefore, \\1 is the value extracted from the first group, \\2 the value 
## from the second group, etc.

str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2")

######################################### II. Testing and improving #################################

### Problematic patterns in our case study ###

## Developing the optimal regex on the first try is often difficult. Trial and error is a common 
## approach. In the previous sections, we have developed powerful string processing techniques 
## that did help us catch many of the problematic entries. Here we will test our approach, search 
## for further problems, and tweak our approach for possible improvements. Let's write a function .
## to exclude the problematic entries that were given in centimeters (we will deal with those later),
## and focus only on the other problematic entries:

neither_inch_nor_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  # identify the plausible entries, either in inches or cm (dividing by 2.54 converts cm to inches)
  ind <- !is.na(inches) & (between(inches, smallest, tallest) | between(inches/2.54, smallest, tallest))
  # return the implausible entries  
  return(!ind)
}

# extract problems that are not inches nor cm form the reported heights:
problems <- reported_heights %>% 
  filter(neither_inch_nor_cm(height)) %>%
  pull(height)
length(problems)

# create a converted vector of the problematic entries after applying what we learned so far. 
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% # convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  # remove inches symbols
  str_replace(pattern_with_groups, "\\1'\\2") # change format into our standardised format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"   # already defined above - but mentioned again here for our information

# how many entries of the converted vector do match our standardised pattern?
converted %>% str_detect(pattern) %>% sum

# what proportion is that?
converted %>% str_detect(pattern) %>% mean

## The last piece of code shows that we have matched well over half of the strings. 
## Let's examine the remaining cases:

index <- str_detect(converted, pattern) # matched cases
converted[!index]

# A few clear patterns arise:
#   
#   Many heights were exactly 5 or 6 feet. The corresponding measurements were entered either in the form F or F', with no inches.
# Some of the inches were entered with decimal points. For example 5'7.5". Our pattern only looks for two digits in the inches' place.
# Some entries had spaces at the end, for example 5 ' 9.
# Although not as common, we additionally see the following issues:
# 
# Some entries were in meters, e.g. 1.6, and some of these use European decimals: e.g. 1,70.
# some students added cm.
# A student spelled out the numbers: Five ' eight ".

### Plan of Attack ###

## For case 1, we can detect the pattern F or F' and replace it to be in our standardised form: 
## F'0 as follows. NOTE THAT: The pattern has to start with a digit 5 or 6 and either end there or 
## end with the feet symbol ' (5 and 6 feet tall are quite common. But, 4 and 7 feet tall are so 
## rare: we don't accept 48 inches, and although we accept 84 as a valid inch entry, it is very 
## rare and we assume 7 was entered in error.

# illustrate how to fix problems of case 1
pattern_case1 <- "^([56])'?$"   # note the defined group using the ()
yes <- c("5", "6'", "5'", "6")
no <- c("4'", "6''", "5'4")
s <- c(yes, no)
str_replace(s, pattern_case1, "\\1'0")

## For case 2, the entries were not matched because the inches included decimals, e.g. 5'7.5" 
## and our current pattern does not permit this. We can print how our current pattern looks like:

pattern 

## In order to allow our pattern to detect these cases, we need to permit decimals, not just digits,
## in the inches part. Therefore, we can update our pattern - at the inches part - to permit zero or
## one period ., folowed by zero or more digits:

pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
yes <- c("4' 6", "5' 6.5", "6 ' 2.75")
no <- c("1.7", "1, 8")
s <- c(yes, no)
str_detect(s, pattern)

# after updating our pattern, we could detect three more entries: now the total detected = 126
str_detect(problems, pattern) %>% sum

## In case 3, there were spaces at the end of the entries, e.g. 5 ' 9. Spaces at the start or 
## end of the string are uninformative, and they sometimes can be hard to see. This is a general 
## problem and there is a function dedicated to removing them: str_trim.

str_trim(c("5 ' 9 ", "5 ' 9 ", " 5 ' 9 ", "5 ' 9"))

## For case 4, meters using commas, we can convert them from x,y into x.y to make all the entries 
## in meters consistent. NOTE THAT: we require the first digit to be either 1 or 2 because other 
## values would be implausible for adult heights. Here is an example of how to do this:

yes <- c("1,7", "1, 8", "2, ")   # examples of the entries that we want to detect.
no <- c("5,8", "5,3,2", "1.7")   # examples of the entries that won't match out target here.
s <- c(yes, no)
s

str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

## We will cover the other cases, case 5 and 6 (they are not common in our problematic entries), in the following sections.

### Chaninging Lettercase ###

## The regex is case sensitive. Often we want to match a word regardless of case. There are functions to convert 
## all cases to lower, str_to_lower, all cases to upper, str_to_upper. Furthermore, there is the 
## str_to_title function which convert the first letter to upper and the rest to lower.

######################################### III. Finalising string processing ##########################

### Final Fixing ###
 
## Now, we can show all the problematic entries and see how many of them can be currently detected 
## using the standardised pattern we defined:

length(converted)    # how many problematic entries?

index <- converted %>% str_detect(pattern)   # detected as matching the standardised pattern
sum(index)          # how many entries matching the standardised pattern

converted[!index]    # show the remaining problematic entries

## First, we will write a function that takes a string vector as an input and convert words, such as 
## "Five ' eight ", into numbers:

library(english)
words_to_numbers <- function(s){
  s <- str_to_lower(s)     # all to lower case
  for(i in 0:11){
    # replace words to their numbers: e.g. words(0) = "zero", if detected, will be replaced by "0", words(1) = "one" --> "1", so on.
    s <- str_replace_all(s, english::words(i), as.character(i))
  }
  return(s)
}
# example:
words_to_numbers(c("Five ' eight ", "4' 11"))

## Now, we will put all of what we have learned together into action to convert as many strings
## of the problematic entries of the reported heights as possible into our standardised format. 
## NOTE THAT: So far We used the problems vector and the converted vector (which are the subset of 
## the reported heights with the problematic entries) to illustrate how to apply the learned 
## techniques. However, our aim is to generate a fixed version of the the whole column height in 
## the given data frame reported_heights. We will write another function that should put all our 
## developed techniques above into an action:

convert_format <- function(s){
  s %>%
    str_replace_all("feet|foot|ft", "'") %>% # replace feet labels with '
    str_replace_all("inches|in|''|\"|cm|and", "") %>% # remove inches labels, 'cm' and 'and' characters.
    str_replace(pattern_with_groups, "\\1'\\2") %>% # convert various formats of feet and inches to standardised format.
    str_replace("^([56])'?$", "\\1'0") %>% # convert entries described in "case 1" to standardised format.
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% # "case 4": make all the entries in meters consistent.
    str_trim()   # remove any spaces potentially left at the end, after removing the last word, e.g. "5 feet and 8.11 inches" would be "5 ' 8.11 " see the 2nd line of this function
}

## Now we can see which problematic entries remain after applying these two functions:

# convert formats, and words
converted <- problems %>% words_to_numbers() %>% convert_format()
# After converting, we may detect more entries. so we apply the functions 'neither_inch_nor_cm' defined earlier to remain only the problematic entries:
remaining_problems <- converted[neither_inch_nor_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"   # already defined - but mention again here for our information

# which of the remaining problems now match the pattern
index <- str_detect(remaining_problems, pattern)
# show those that don't match the pattern
remaining_problems[!index]

## Apart from the cases reported as meters, e.g. 1.70 and 1.6, which we will fix below, they 
## all seem to be cases that are implausible entries and imposible to fix.

####################################### IV. The extract function ###################################

## Up to now, We could fix most problematic entries by converting them into a standardised format, F'I. We will need to extract the values of F and I seperately to convert 
## the entry to inches. The function extract allows us to do this. Th following example illustrate that:

# suppose we have the following entries stored in the column 's' of a data.frame named 'tab':
tab <- data.frame(s = c("5'10", "6'1","5'8"))
# we can extract the feet and inches as follows:
Names <- c("feet", "inches")  # names of the new two columns
tab

require(tidyr)
tab %>% extract(s, Names, regex = "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$") # the defined groups will be extracted and stored at the new columns

######################################## V. Putting it all together ##################################

## We are now ready to put it all together and wrangle our reported heights data. We will start by keeping the original heights column 
## (by renaming it as original) and fixing another copy of it, so we can compare before and after.

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"   # already defined - but now grouped to extract feet and inches.

smallest <- 50
tallest <- 84

new_heights <- reported_heights %>%
  # keep the original, and convert another copy of the heights
  mutate(original = height, height = words_to_numbers(height) %>% convert_format()) %>%
  # extract the feet and inches according to the defined group of our pattern, don't remove the input column (height) from the data frame
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
  # convert all entries of the 'height' and the new extracted variables into numeric
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  # create a new variable 'guess' to store the inches converted from the feet and inches entries
  mutate(guess = 12 * feet + inches) %>%
  # set the values of height in inches (by converting entries in cm and meters into inches), set the remaining as NA
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height,  #inches
    between(height/2.54, smallest, tallest) ~ height/2.54, #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    TRUE ~ as.numeric(NA))
  ) %>%
  # set the height value as the one calculated in'guess' ONLY if it was given in F'I format such that: I was <12 and the converted value (in inches) is within our accepted range
  mutate(height = ifelse(is.na(height) & inches < 12 & between(guess, smallest, tallest),
                         guess, height)) %>%
  # show heights with two decimal digits only
  mutate(height = round(height, digits = 2)) %>%
  # return all the data frame columns except the 'guess' column - as it is no longer needed
  select(-guess)

## NOTE!! Warning: NAs introduced by coercion

##This object, new_heights, is our final solution for this case study. Now, let's look at the data sorted by heights:

new_heights %>% arrange(height)

## These data are typically the final processed data stored as the heights data set in the dslabs 
## R package. We can check only all the entries we converted by:

new_heights %>%
  filter(not_inches(original)) %>%
  arrange(height)


##################################################### END ###############################################
