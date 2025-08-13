# This is to pull tokenized paragraphs using policy belief search words from hearings 
# and then code via DNA - works well with longer texts and many PDFs

# install.packages("quanteda")
# install.packages("XML")
# install.packages("pdftools")

# Load required libraries
library(dplyr)
library(tidyr)
library(tm)
library(stringr)
library(readxl)
library(quanteda)
library(tokenizers)
library(pdftools)
library(readxl)
library(tm)

### ------------------------------------------------------------------------------
### Import PDFs in a folder, export tokenized sentences/paragraphs for DNA
### ------------------------------------------------------------------------------

# Path to the folder containing PDF files 
# (PDFs will be assigned a column number based on the order loaded into the folder)
# Load files into folder with dates, make sure titles of PDFs correspond with pdf date/title

folder_path <- "/" #path to folder of PDFS

# List all PDF files in the folder
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# Define the words to filter paragraphs for (these are words indicating a policy preference)
filter_words <- c("need", "want", "like","should", "think", "must", "necessary", 
                  "necessity", "agree", "disagree", "crucial", "critical", 
                  "important", "imperative", "ought", "suggest", "advocate",
                  "support", "oppose", "endorse", "promote", "reject", "accept", 
                  "concede", "deny", "refute", "rebut", "justify", "prohibit", 
                  "permit", "condone", "condemn", "implement", "adopt", "levy",
                  "abandon", "retain", "revise", "abandon", "amend", "expedite", 
                  "facilitate","negotiate", "compromise", "concur", "dissent", 
                  "ratify", "veto", "enact", "invoke", "believe", "prefer", 
                  "prioritize", "urge","demand", "require","suggest",
                  "encourage","propose","feel", "maintain", "consider", "argue",
                  "emphasize", "highlight", "stress","assert","contend", 
                  "posit", "underscore", "insist", "recommend", "propose", 
                  "cost", "benefit", "beneficial","afford", "allow", "favor", 
                  "against", "align", "consistent", "effective", "appropriate",
                  "suit", "desirable", "desire", "detriment", "help", 
                  "positive", "negative","feasible", "viable", "realistic", 
                  "pursue", "success", "fail", "can", "do")


# Function to extract and tokenize text by SENTENCES from a PDF file------------
# Tokenized paragraph code is below and works better for congressional hearings so far 
# 
# extract_and_tokenize_sentences <- function(pdf_file_path, pdf_number) {
#   # Get the file name without the path
#   pdf_file_name <- basename(pdf_file_path)
#   
#   text <- tryCatch({
#     pdf_text(pdf_file_path)
#   }, error = function(e) {
#     warning(paste("Error reading file:", pdf_file_path))
#     return(NULL)
#   })
#   
#   # Tokenize text by sentence
#   sentences <- unlist(tokenize_sentences(text))
#   
#   # Remove newline characters and other unnecessary spaces
#   sentences <- gsub("\\n", " ", sentences)
#   sentences <- gsub("\\s+", " ", sentences)
#   
#   # Return a list with PDF file name, PDF number, and tokenized sentences
#   return(list(pdf_file_name = pdf_file_name, pdf_number = pdf_number, sentences = sentences))
# }
# 
# # Loop over each PDF file, extract and tokenize sentences, and store in a list
# pdf_sentences <- lapply(seq_along(pdf_files), function(i) {
#   extract_and_tokenize_sentences(pdf_files[i], pdf_number = i)
# })
# 
# # Function to filter sentences based on the filter words
# filter_sentences <- function(pdf_sentences, filter_words) {
#   filtered_sentences <- lapply(pdf_sentences, function(pdf_sentence) {
#     filtered <- grep(paste(filter_words, collapse = "|"), pdf_sentence$sentences, value = TRUE)
#     list(pdf_file_name = pdf_sentence$pdf_file_name, pdf_number = pdf_sentence$pdf_number, sentences = filtered)
#   })
#   return(filtered_sentences)
# }
# 
# # Filter the tokenized sentences
# filtered_pdf_sentences <- filter_sentences(pdf_sentences, filter_words)

#Check that root words and editions are being pulled - 4/15 - yes!
#print(filtered_pdf_sentences)

# Save the filtered sentences to a CSV file
# save_filtered_sentences <- function(filtered_pdf_sentences, output_file) {
#   for (i in seq_along(filtered_pdf_sentences)) {
#     file_path <- file.path(output_file, paste0("filtered_sentences_", i, ".csv"))
#     write.csv(filtered_pdf_sentences[[i]], file = file_path, row.names = FALSE)
#   }
# }
# 
# # Specify the folder path where CSV files will be saved
# output_folder <- "/"
# 
# # Save the filtered sentences
# save_filtered_sentences(filtered_pdf_sentences, output_folder)

# Begin paragraph tokenizing (Works better for congressional hearings so far) ------------------------------

# Specify path to the folder containing PDF files
folder_path <- "/"

# List all PDF files in the folder
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# Function to extract and tokenize text by paragraph from a PDF file ----------------
extract_and_tokenize_paragraphs <- function(pdf_file_path, pdf_number) {
  # Get the file name without the path
  pdf_file_name <- basename(pdf_file_path)
  
  text <- tryCatch({
    pdf_text(pdf_file_path)
  }, error = function(e) {
    warning(paste("Error reading file:", pdf_file_path))
    return(NULL)
  })
  
  # Tokenize text by paragraph
  paragraphs <- unlist(tokenize_paragraphs(text))
  
  # Remove numbers, punctuation, and other unnecessary spaces
  paragraphs <- gsub("\\d+", "", paragraphs) # remove numbers
  paragraphs <- gsub("[[:punct:]]", "", paragraphs) # remove punctuation
  paragraphs <- gsub("\\s+", " ", paragraphs) # remove extra spaces
  
  # Return a list with PDF file name, PDF number, and tokenized paragraphs
  return(list(pdf_file_name = pdf_file_name, pdf_number = pdf_number, paragraphs = paragraphs))
}

# Loop over each PDF file, extract and tokenize paragraphs, and store in a list
pdf_paragraphs <- lapply(seq_along(pdf_files), function(i) {
  extract_and_tokenize_paragraphs(pdf_files[i], pdf_number = i)
})

# Function to filter paragraphs based on the filter words
filter_paragraphs <- function(pdf_paragraphs, filter_words) {
  filtered_paragraphs <- lapply(pdf_paragraphs, function(pdf_paragraph) {
    filtered <- grep(paste(filter_words, collapse = "|"), pdf_paragraph$paragraphs, value = TRUE)
    list(pdf_file_name = pdf_paragraph$pdf_file_name, pdf_number = pdf_paragraph$pdf_number, paragraphs = filtered)
  })
  return(filtered_paragraphs)
}

# Filter the tokenized paragraphs
filtered_pdf_paragraphs <- filter_paragraphs(pdf_paragraphs, filter_words)

# Save the filtered paragraphs to a CSV file
save_filtered_paragraphs <- function(filtered_pdf_paragraphs, output_file) {
  for (i in seq_along(filtered_pdf_paragraphs)) {
    file_path <- file.path(output_file, paste0("filtered_paragraphs_", i, ".csv"))
    write.csv(filtered_pdf_paragraphs[[i]], file = file_path, row.names = FALSE)
  }
}

# Specify the folder path where CSV files will be saved
output_folder <- "/"

# Save the filtered paragraphs
save_filtered_paragraphs(filtered_pdf_paragraphs, output_folder)

# I hope you have a great day!



