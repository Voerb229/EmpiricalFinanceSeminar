# Capital Structure and Access to Public Debt Markets

Empirical finance seminar. The question: do firms without a credit rating, and therefore without direct access to public debt markets, carry different leverage than rated firms?

The script works through the analysis in stages (tasks a to j), starting from summary statistics and ending at a specification with fixed effects and clustered standard errors.

## Data

Expects a Stata file `data.dta` in the project root. **It is not included in this repository.**

Variables used: `leverage`, `unrated`, `tangibility`, `profitability`, `market_to_book_value`, `lassets` (log assets), `lage` (log firm age), `firm_id`, plus year and sector identifiers.

## Structure

| Task | Content |
| --- | --- |
| a, b | Data preparation and descriptive statistics |
| c | Leverage on the unrated dummy, univariate |
| d | Adding tangibility |
| e, f | Interaction between tangibility and unrated status |
| g | Plots (`ggplot2`) |
| h | Full set of controls: profitability, market-to-book, log assets, log age |
| i | Year and sector dummies |
| j | Standard errors clustered by firm |

## Standard errors

Task j compares three approaches to inference on the same model:

- Conventional OLS standard errors
- Heteroskedasticity-robust (`vcovHC`, HC1)
- Clustered by firm (`vcovCL`), which is the appropriate choice here since observations repeat per firm across years

The comparison shows how much the standard errors, and with them the significance of the coefficients, depend on that choice.

## Running it

```r
install.packages(c("haven", "dplyr", "ggplot2", "jtools", "lmtest", "sandwich"))
source("Code.R")
```

Requires R 4.x.
