x <- c(1, 2, 5, NA)

# if_case() produces the same output as dplyr::if_else()
if_case(x > 3, "high", "low", "missing")
dplyr::if_else(x > 3, "high", "low", "missing")

# if_case() does not throw an error if arguments are not of the same type
if_case(x > 3, "high", "low", NA)
try(dplyr::if_else(x > 3, "high", "low", NA))

# if_case() can accept a piped input without an error or requiring braces
x %>% if_case(. > 3, "high", "low", "missing")
try(x %>% dplyr::if_else(. > 3, "high", "low", "missing"))
x %>% {dplyr::if_else(. > 3, "high", "low", "missing")}

# You can also pipe a conditional test like dplyr::if_else()
{x > 3} %>% if_case("high", "low", "missing")
{x > 3} %>% dplyr::if_else("high", "low", "missing")
