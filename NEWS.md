# datetoiso 1.0.0


## Major changes

* Introduced new function `impute_date()` for imputing missing **month** and/or **day** components
  in partial date strings when the **year** is known.
  - Supports both `"ymd"` and `"dmy"` input formats.
  - Provides flag variables indicating the type of imputation performed (day, month, or both).
  - Allows control over imputation direction using `"min"` (earliest date) or `"max"` (latest date).

## Minor improvements and bug fixes

*
---
# datetoiso 0.2.0



## Major changes

* `viso()` now properly recognizes dates with 3 letters month names or full month names separated from time by 'T'

## Minor improvements and bug fixes

*

# datetoiso 0.1.0

## Major changes

* calculate probability of choosing proper date transfer function (between ymd, ydm, dym, dmy, myd, mdy)

## Minor improvements and bug fixes

* 
