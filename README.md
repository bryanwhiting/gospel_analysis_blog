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
