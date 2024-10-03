## Resubmission

This is a resubmission. In this version we:

-   Enclose 'tidyverse' in single quotes in the DESCRIPTION file.

-   We do not have any references to add at this time. If references become available in the future, we will be sure to include them.

-   Remove unnecessary spaces from the description field in the DESCRIPTION file.

-   Replace `print/cat`statements with appropriate message calls.

-   Use `suppressWarnings()` instead of `options(warn = -1)` in the specified functions.

-   Implement `on.exit()` to restore any changed options or settings in the relevant functions.

-   Ensure that the seed is not set to a specific number within functions.

## R CMD check results

0 errors \| 0 warnings \| 1 note

-   This is a new release.
