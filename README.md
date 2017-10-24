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

### Setting up API authentication

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

### Connecting to Qualtrics API

```
## connect using key saved in ~/.qualtrics_api_key
connect_qualtrics("cuboulder")

## connect using some API key passed as string
connect_qualtrics("cuboulder", key = "abcdefghijklmnopqrstuvwxyz")
```

### Find and load surveys

```
## Find surveys
s_list <- get_surveys()             # get all surveys
s_list <- get_surveys(name="week")  # get all surveys who's name matches "week"
s_list <- get_surveys(id="SV_")     # get all surveys who's id matches "SV_"

## Load surveys
surveys <- load_surveys(s_list)     # load all surveys returned by get_surveys
survey1 <- surveys[[1]]             # grab first survey returned
```

### Using native qsurvey functions:

```
## Questions
questions(survey1)

## Responses
responses(survey1$id)

## Blocks
blocks(survey1)

## Choices
choices(survey1)
```

## Style Guide

http://adv-r.had.co.nz/Style.html
