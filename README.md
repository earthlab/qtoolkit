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

Check out the vignettes/ folder for some example usage cases.

## Style Guide

http://adv-r.had.co.nz/Style.html
