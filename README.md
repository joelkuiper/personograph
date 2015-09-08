# Personograph

![higher_is_better=F](/man/figures/green.png?raw=true)
![higher_is_better=T](/man/figures/red.png?raw=true)

## Development
Install `devtools` and `roxygen2`.
To generate documentation run `document()` in the package folder after loading `devtools` with `library(devtools)`.
The documentation is automatically generated from the comments in the source code.
See the [Roxygen documentation](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) for details.
To convert the documentation to PDF use `R CMD Rd2pdf <package>`.

## To build
`R CMD build .` in the personograph folder.
Then do `R CMD CHECK <the generated.tar.gz>` and/or `R CMD CHECK --as-cran <the generated.tar.gz>`.

## Funding & Acknowledgments
This software was commissioned and sponsored by [Doctor Evidence](http://www.doctorevidence.com/).
The Doctor Evidence mission is to improve clinical outcomes by finding and delivering medical evidence to healthcare professionals, medical associations, policy makers and manufacturers through revolutionary solutions that enable anyone to make informed decisions and policies using medical data that is more accessible, relevant and readable.
