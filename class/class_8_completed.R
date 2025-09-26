#-------------------------------------------------------------------------
# POLSCI797: LLM/DSL Script
#-------------------------------------------------------------------------
# If you had an OpenAI API key, you could run code like this
library(ellmer)
library(httr)
library(jsonlite)

chat <- ellmer::chat_openai("I am a researcher. Please answer my questions
                            tersely and please don't embarass me in front of
                            my cool students.",
                            model = "gpt-4.1-nano")

send_prompt <- function(chat, university) {
  chat$chat(interpolate("Where is {{university}} located?"))
}

send_prompt(chat, "University of Michigan")

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

# Define the request body
body <- list(
  model = "meta-llama/llama-3.3-70b-instruct:free",
  messages = list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "Is the University of Michigan political science department good?")
  )
)

# Make the POST request
res <- POST(
  url,
  add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  ),
  body = toJSON(body, auto_unbox = TRUE)
)

# Parse and print response
content <- content(res, as = "parsed", type = "application/json")
cat(content$choices[[1]]$message$content, "\n")

#----------------------------------------------------------------------
# Pan and Chen (2018): are online complaints accusing local officials 
# of corruption by local officials reported to upper-level governments
# in China at higher rates?
#
# For this, need to code whether a post accuses officials of corruption
# this variable is countyWrong: only some expert coded labels
# pred_countyWrong: full LLM labeled set

library(dsl)
data("PanChen")
head(PanChen)

table(PanChen$countyWrong, PanChen$pred_countyWrong)

# naive regression
lm(SendOrNot ~ countyWrong + prefecWrong + 
  connect2b + prevalence + regionj + groupIssue, data = PanChen) %>%
  summary()

out <- dsl(model = "logit", 
           formula = SendOrNot ~ countyWrong + prefecWrong + 
             connect2b + prevalence + regionj + groupIssue,
           predicted_var = "countyWrong",
           prediction = "pred_countyWrong",
           data = PanChen)

summary(out)

