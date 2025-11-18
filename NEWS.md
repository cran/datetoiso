# datetoiso 1.1.1

# datetoiso 1.1.0
## Added
- `clean_date()` exported: prepares and normalizes date-like strings before YMD conversion.

## Changed
- `viso()` updated to better recognize unusual date types.
- `impute_date()` can now handle missing year ("2024") or year+month ("01-2025", "2025-01").
- Updated documentation, examples, and unit tests.

## Fixed
- Minor bug fixes and internal improvements.

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
