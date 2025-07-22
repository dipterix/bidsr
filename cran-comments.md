## R CMD check results

0 errors | 0 warnings | 1 note

```
In https://www.stats.ox.ac.uk/pub/bdr/donttest/bidsr.out

* checking for new files in some other directories ... NOTE
Found the following files/directories:
  ‘~/.cache/R/bidsr’ ‘~/.cache/R/bidsr/bids-examples’
  ...
```

### What are those artifacts

These files are official example files provided by `BIDS`, a neuroscience format standard on file organization. These files are used by `bidsr` to build documentations and for package users to validate the parsing functions.

### The cause of these artifacts

These artifacts are caused by calling `bidsr` function `download_bids_examples` without the argument `test=TRUE`. Although I tried to call `download_bids_examples(test=TRUE)` in the examples to avoid downloading this example, I over-looked two documents, where `download_bids_examples()` is called:

* `README.Rmd`
* `vignettes/aaa-get-started.Rmd`

To mitigate this issue, I have added the following code blocks to remove the artifacts.

```
#| r

cache_root <- tools::R_user_dir(package = "bidsr", which = "cache")

if(file.exists(cache_root)) {

  unlink(cache_root, recursive = TRUE)
  
}

```

These code blocks ensure that `~/.cache/R/bidsr` is removed at the end of the document.
