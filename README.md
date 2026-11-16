# What Really Drives House Prices? Changeable vs Unchangeable Factors

How much of a house's price comes from things an owner can change (living area, bathrooms, heating) compared with things they can't (land value, waterfront, age)? This project uses regression modelling to answer that question for homeowners planning renovations and for buyers deciding what to prioritise.

📄 **Final report:** [`Report/final_report.pdf`](Report/final_report.pdf)
📊 **Presentation slides:** [`Presentation/slides.html`](Presentation/slides.html)

## Dataset

**Saratoga Housing Data:** 1,734 house sales in Saratoga County, New York State (2006), with 17 variables and no missing values. It was originally collected for the case study *"How much is a fireplace worth?"*

## Approach

1. **Data cleaning:** removed invalid records (zero lot size, inconsistent age and construction values), converted categorical variables, and log-transformed skewed variables (price, lot size, age, land value).
2. **Grouping variables** into **changeable** (living area, bathrooms, bedrooms, rooms, central air, heating, fuel type, fireplaces, sewer type, new construction) and **unchangeable** (lot size, waterfront, age, land value, % college-educated neighbours).
3. **Variable selection** using forward, backward and stepwise selection based on AIC.
4. **Model evaluation** with in-sample fit, an 80/20 train-test split, and k-fold cross-validation.
5. **Model stability** checked with bootstrap variable-inclusion plots (`mplot`).
6. **Assumption checks** for linearity, homoscedasticity, normality and multicollinearity.

## Key findings

- **Changeable factors explain more of the price variation (R² ≈ 0.55)** than unchangeable ones (R² ≈ 0.38), though both matter.
- The selected model explains **about 59%** of the variation in log price (in-sample).
- **Living area** was the strongest single predictor.
- Estimated effects on price, holding other factors constant:

| Factor | Change | Impact on price |
|---|---|---|
| Waterfront | Yes | **+76.0%** |
| Bathrooms | +1 | **+10.3%** |
| Gas heating fuel | Yes | +8.7% |
| Central air | Yes | +5.8% |
| Living area | +100 sq ft | +2.9% |
| Land value | +10% | +1.2% |
| Age | +10% | −0.5% |
| New construction | Yes | −21.8% |

**Takeaways:**
- **For homeowners:** adding about 350 sq ft of living space has roughly the same effect on price as adding one bathroom, which is useful for comparing renovation options.
- **For buyers:** a waterfront location is linked to the largest price increase, so it's worth weighing unchangeable features carefully. Renovation isn't everything.

## Repository structure

| Folder | Contents |
|---|---|
| `Report/` | Final report (`final_report.pdf`) and its R Markdown source |
| `Presentation/` | Slides (`slides.html`) and Quarto source |
| `EDA/` | Exploratory data analysis |
| `app/` | Shiny app (`app.R`) |

## Limitations

The data is from 2006, around the global financial crisis, and from a suburban county, so results may not generalise to today's market or to urban areas like New York City. Some factors that affect price, such as interior condition, aren't recorded in the data.

## Tech

R, Quarto, R Markdown, `tidyverse`, `caret` (cross-validation), `mplot` (model stability), `ggplot2`, Shiny
