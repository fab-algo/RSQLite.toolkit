## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a patch release. There was an issue in creating the vignette "Dealing with DSV files" caused by the use of the `piggyback` package calling the GitHub releases API anonymously and exhausting rate limits. To avoid this issue I created an utility function that downloads the example files from their static URL.
