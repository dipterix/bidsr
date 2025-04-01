## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

Addressing CRAN comments:

```
Please write references in the description of the DESCRIPTION file in the form
authors (year) <doi:...>
(If you want to add a title as well please put it in quotes: "Title")
For more details: <https://contributor.r-project.org/cran-cookbook/description_issues.html#references>
So please add authors and (year) as well.
```

Thanks, added `authors (year) <doi:...>`


```
The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful. Please add more details about the package functionality and implemented methods in your Description text.
For more details: <https://contributor.r-project.org/cran-cookbook/general_issues.html#description-length>
```

Added text: `... Provides query functions to extract and check the 'BIDS' entity information (such as subject, session, task, etc.) from the file names according to the specification...`. 

```
Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
For more details: <https://contributor.r-project.org/cran-cookbook/docs_issues.html#missing-value-tags-in-.rd-files>
Missing Rd-tags:
     BIDSDatasetDescription.Rd: \value
     new_bids_class.Rd: \value
     new_bids_entity_file_class.Rd: \value
```

Thanks, added value field.


```
\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest.
Please unwrap the examples if they are executable in < 5 sec, or replace dontrun{} with \donttest{}.
For more details: <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>
```

Thanks, I have removed all `\dontrun` from the examples and wrapped them with `try({})` as they are used to demonstrate incorrect usages.


```
Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.
e.g.:
...
old <- options() # code line i
on.exit(options(old)) # code line i+1
...
options(timeout = 3600)# somewhere after
...
e.g.: R/examples.R
```

I believe this could be a false positive as the code indeed called `on.exit` on line `i+1` but the code arrangement made it appear to be not... Thanks for checking it!

```
f <- tempfile(fileext = ".zip")
old_opt <- options(timeout = 3600)
on.exit({
  options(old_opt)
  unlink(f)
})
```


Also please note that I have added University of Pennsylvania to the author list `role=cph` to comply with the university IP rules.
