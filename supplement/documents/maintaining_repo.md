# Maintenance of the Repository & Meta Information

### Introduction



### Metadata Needs & Some Best Practices

### Supplementary Documents

**Website Documents & Updating**

Found under the `web` subdirectory of the supplement folder (master branch) are a few documents which are actually related to the website. When rebuilding the website, the `goals.Rmd` and `layers.Rmd` are copied over to become `local_goals.Rmd` and `local_layers.Rmd` of the gh-pages branch, and knitted together as children of the `goals` and `layers` webpages. Keeping these versions in the master branch ....makes no sense and needs to be reconsidered ....or maybe the original reason for this is just lost on me?



### Other Materials in Supplement Subdirectories

### Documentation of Testing and other Analyses

### Keeping Readmes Current & Relevant

**Functions in `management.R`**

The functions written and documented in `R/management.R` were designed to make maintenance easier. 
For example, `Rfun_readme` looks at the roxygen documentation in a script and creates a formatted chunk of text about the contents to paste into a readme, so it is possible to look up functions' locations and their use just by refering to the main readme. 

See `R/README.md` for more details.


**Main Readme as GitHub Homepage**

The Readme in the root `bhi` folder essentially acts as a homepage for visitors to the GitHub repo. 
To make this as helpful as possible, the Readme should be kept up to date with the following information:

* up-to-date introduction at top of the document, indicating aims and progress of the current assessment
* relevant links in 'Learn more about the Baltic Health Index' section
* functional urls for links throughout


### Additional References
