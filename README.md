# qualtrics-toolkit
R package to process and visualize qualtrics survey data

## Getting Started

### Installation

```
## devtools needed to install R package from GitHub
install.packages("devtools")

## install qsurvey package from GitHub
devtools::install_github("jamesdunham/qsurvey")

## install this package
devtools::install_github("earthlab/qtoolkit")
```

### Setup API authentication

1. Find your API Key
   * Go to [subdomain].qualtrics.com and log in
   * Click on your user icon > Account Settings...
   * Select the Qualtrics IDs tab
   * Your API token is listed under the "API" heading
   * [Here is an easy access link to this page on the cuboulder domain](https://cuboulder.qualtrics.com/ControlPanel/?ClientAction=ChangePage&Section=QualtricsIdsSection)

2. Create a `.qualtrics_api_key` file
   * The ideal way to connect is by having a file in your home directory `.qualtrics_api_key`
   * Open your favorite text editor
   * Paste in your API key and save the file as `~/.qualtrics_api_key`
   * This way your key will be automatically loaded whenever you connect, and won't be included in plaintext in any scripts you write
   
## Example Usage

# Sample Workflow

## Connect to Qualtrics API

```{r}
## connect using key saved in ~/.qualtrics_api_key
connect_qualtrics("cuboulder")

## connect using some API key passed as string
connect_qualtrics("cuboulder", key = "abcdefghijklmnopqrstuvwxyz")
```

## List Available Surveys

```{r}
## Return DF of surveys to available with API key & Login combo

s_list <- get_surveys()        # Find all surveys available
s_list <- get_surveys("week")  # Find surveys whos name matches "week"
```

## Load Surveys

```{r}
## Load survey metadata from Qualtrics; return survey design object
surveys <- load_survey()                     # load all surveys
surveys <- load_survey("week1-survey")       # load surveys with exact name "week1-survey"
surveys <- load_survey("SV_abcdefghijklmno") # load survey with ID SV_abcdefghijklmno
surveys <- load_survey(s_list)               # load all surveys in DF s_list

surveys <- load_survey("week",
                       match.exact = FALSE) # load all surveys matching "week"

```

## Get survey responses

```{r}
survey1 <- load_survey("week1")    # grab week#1 survey

## Get survey responses as a dataframe
resp <- get_responses(survey1)      # get responses to all questions
resp <- get_responses(survey1,      # get responses to question "Q10"
                      question="Q10")
resp <- get_responses(survey1,      # get responses w/ metadata
                      metadata = TRUE)
```

Check out the vignettes/ folder for some example usage cases.

## Style Guide

http://adv-r.had.co.nz/Style.html
