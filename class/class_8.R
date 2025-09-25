#-------------------------------------------------------------------------
# POLSCI797: LLM/DSL Script
#-------------------------------------------------------------------------
# If you had an OpenAI API key, you could run code like this
library(ellmer)
library(httr)
library(jsonlite)
library(dsl)

chat <- ellmer::chat_openai("I am a researcher. Please answer my questions
                            tersely and please don't embarass me in front of
                            my cool students.",
                            model = "gpt-4.1-nano")

#-------------------------------------------------------------------------
# Instead, you can sign up for OpenRouter API Key (free) and choose one
# of the free models.
# I picked: google/gemini-2.0-flash-exp:free
# https://openrouter.ai/google/gemini-2.0-flash-exp:free
# 
# Then, tell R about the key by opening your .Renviron file
# and adding OPENROUTER_KEY="your_key."
# 
# If you don't know what an .Renviron is, you can install the 
# 'usethis' package and run usethis::edit_r_environ() to make one

# Define the API endpoint
url <- "https://openrouter.ai/api/v1/chat/completions"

# Other free options:
# openai/gpt-oss-120b:free
# google/gemini-2.0-flash-exp:free
# meta-llama/llama-3.3-70b-instruct:free

# 1. Define the request body

# 2. Make the POST request

# 3. Parse and print response

#----------------------------------------------------------------------
# Pan and Chen (2018): are online complaints accusing local officials 
# of corruption by local officials reported to upper-level governments
# in China at higher rates?
#
# For this, need to code whether a post accuses officials of corruption
# this variable is countyWrong: only some expert coded labels
# pred_countyWrong: full LLM labeled set

data("PanChen")
head(PanChen)

table(PanChen$countyWrong, PanChen$pred_countyWrong)

# They want to run regression: 
  # SendOrNot ~ countyWrong + prefecWrong + connect2b + prevalence + regionj + groupIssue
