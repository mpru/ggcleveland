## First submission to CRAN
This is a new release to CRAN, and my first re-submission, having fixed these two things mentioned by Gregor Seyer's email:

* Please reduce the length of the title to less than 65 characters.

	Solution: done.

* Is there a doi available for the reference, that you can add to the description field in the form <doi:prefix/suffix>?

	Solution: the book does not have a doi (it was written in the 90s), so I left the description unchanged.

Thanks!

## Test environments
* local ubuntu 20.04, R 4.1.0
* win-builder (devel, release and oldrelease)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs, other than the NEW SUBMISSION note.

## Downstream dependencies
There are currently no downstream dependencies for this package
