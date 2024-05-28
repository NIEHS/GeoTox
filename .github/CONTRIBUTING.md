# Contributing to `GeoTox`
This outlines how to (1) propose a change to `GeoTox` and (2) serves as log of potential enhancements for `GeoTox`. 

## Best Practices 

If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See tidyverse team's guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.
When adding a new function to the package, always add working examples of the new function in roxygen2 documentation under `#' @examples`.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("NIEHS/GeoTox", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header).

### Code style

*  Please abide by 80-character line rules (as default settings in `lintr`)

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## GeoTox Open Issues and Enhancements

This section serves as log of potential enhancements for `GeoTox`. Community members are welcomed and encouraged to use our contributing guide to address the open issues and enhancements to this package. Additionally, new enhancements can be proposed by using our `enhancements` template request.  If you want to address an open enchancement or suggest a new one, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 


### Scientific 

1) Non-steady state toxicokinetics. This will likely require additional usage of the `httk` package.
2) Complete integration of the multiple end-point analysis into the GeoTox objects
3) Integration of Reflected Generalized Concentration Addition ([RGCA](https://github.com/NIEHS/RGCA)), which will allow 3+ parameter hill models to be used for additive mixture predictions.
4) Benchmark Dose modeling (BMD) as a metric for risk assessment. Suggested ingetrations include [BMDexpress2.0](https://www.sciome.com/bmdexpress/) or [transcryptR](https://github.com/NIEHS/transcryptR) - note `transcryptR` is also currently a [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Computational 

1) `targets` pipeline template. If we provide a make-like targets pipeline, then new users will have a reproducible template to run their analysis with ease.
   


## Code of Conduct

Please note that the chopin project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
