# README.md

Site: www.gospelanalysis.com (a re-direct of https://bryanwhiting.github.io/gospel_analysis_blog).

## workflow:

```
distill::create_post('new title')
# add categories to yaml
categories:
  - scriptures 
  - general conference
```

## rmarkdown

Creating panels:
github.com/mfherman/westchester-covid/blob/master/site/county.Rmd

```
xaringanExtra::use_panelset()
```

## Airtable, Zapier, and Instagram

1. Install `airtabler` package from GitHub
1. Create new Instagram account with new email address (don't connect it to a personal account)
1. Create airtable account and new base + table
1. Get Airtable API key from https://airtable.com/account
1. Save to .Renviron `AIRTABLE_API_KEY=keyxxxxxxxxx`
1. Restart R Console
1. Get Airtable App ID (if it's not in the URL, check help > API Documentation and the new URL should have it.)
1. Edit test_airtabler.R script with APP ID
1. Upload example data
1. Create Zapier
1. Connect Zapier to Airtable using API key
1. Build a new Zap with Airtable as trigger and Instagram Business Post Photo as action
1. Can't have a private user in the "tagged_users" field.
1. Zap: https://zapier.com/shared/b2bf1c15f536497329b88201d25786ab34ecdd1e
