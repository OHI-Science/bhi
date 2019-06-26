### BHI `gh-pages` branch

<br/>

To build the website, run [`rmarkdown::render_site()`](http://rmarkdown.rstudio.com/rmarkdown_websites.html), which knits each Rmarkdown to HTML based on parameters in `_site.R` and `_site.yml` before pushing the files back to GitHub. 

Objects and folders created by running `render_site` include: Gruntfile.js, all site_libs subfolder, html versions of all Rmd documents listed in the site.yml

The majority of this website architecture was coded by [Ben Best](https://github.com/bbest) with significant inputs from [Julie Lowndes](https://github.com/jules32).
