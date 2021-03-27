# incase (development version)

* Added `*_case_fct()` family of functions
  - These work the same as their `*_case()` equivalents, but return factors
  - Factor levels are determined by the order of case statements.
  - `in_case_fct(x < 10 ~ "Low", x < 20 ~ "Medium", default = "High")` returns a factor with levels `"Low"`, `"Medium"`, and `"High"`.
  
* Added `*_case_list()` family
  - These work the same as their `*_case()` equivalents, but return lists
  - This allows the functions to return complex data types that would otherwise
    be broken by automatic type conversion.
    
* Implemented lazy-ish evaluation of outputs in `*_case()` family.
  - If the LHS of a formula is never true, the RHS is not evaluated.
  - However, the RHS is still evaluated for all inputs if any input is TRUE.

* Removed `cli` and `crayon` from suggests.
  All styling is now handled by `rlang`.

# incase 0.2.1

* Fixed NOTE by removing unnecessary import of `stats`.
* Uses latest version of **plu** for more informative error messages.

# incase 0.2.0

* Added `grep_case()` which works similarly to `switch_case()` but uses pattern matching instead of exact matching.
* Added `fn_case()` which applies a function to the input and each formula's left-hand side to create the logical vectors used by `in_case()`.
* Added `fn_switch_case()` which applies a function to the left-hand side of each formula to create the vectors used by `switch_case()`.

# incase 0.1.0

* Initial CRAN release.
