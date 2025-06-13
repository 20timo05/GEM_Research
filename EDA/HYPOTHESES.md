# Project Hypotheses and Feature Selection Rationale

## 1. Introduction

This document outlines the theoretical hypotheses guiding the feature selection for the machine learning model. The goal is to predict **entrepreneurial intention** (`FUTSUPNO`) based on a set of demographic, social, and psychological factors.

The initial selection of variables is grounded in established entrepreneurship theory. These hypotheses will be tested empirically using data-driven feature selection techniques like LASSO and Random Forest importance scores in the subsequent modeling phase.

## 2. Core Hypotheses

| Theoretical Area       | Hypothesis                                                                                                                              | Key Variables                                                              |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| **Human Capital**      | Higher levels of education and more life/work experience (age) are positively associated with entrepreneurial intention.                  | `GEMEDUC`, `age`                                                           |
| **Social Capital**     | Knowing other entrepreneurs (social network exposure) significantly increases the intention to start a business.                          | `KNOWENyy`                                                                 |
| **Psychological Traits** | High self-efficacy (perceived skills) and a low fear of failure are strong predictors of intention.                                     | `SUSKILyy`, `FRFAILyy`                                                     |
| **Perceived Context**  | A positive perception of business opportunities in one's environment is a key driver for intention.                                     | `OPPORTyy`                                                                 |
| **Economic Status**    | Household income level is associated with entrepreneurial intention, potentially providing the necessary financial safety net or capital. | `GEMHHINC`                                                                 |

---

## 3. Agenda: Key Variable Definitions

This section details the specific variables chosen to represent the theoretical concepts outlined above.

*   **`age`**
    *   **Represents:** Respondent's exact age in years.
    *   **Rationale:** Serves as a proxy for life and work experience. We hypothesize a potential non-linear relationship (peaking in mid-career). We will use the raw `age` variable, not the binned categories (`age7c`, `age9c`), to retain maximum information.

*   **`GEMEDUC`**
    *   **Represents:** The respondent's highest level of educational attainment, harmonized by GEM.
    *   **Rationale:** A direct measure of formal human capital.
    *   **Codes:**
        *   `111`: Some Secondary Education
        *   `1212`: Secondary Degree
        *   `1316`: Post-Secondary (e.g., vocational training, associate's degree)
        *   `1720`: Graduate Experience (Bachelor's degree or higher)

*   **`KNOWENyy`**
    *   **Represents:** Whether the respondent personally knows someone who started a business in the past two years.
    *   **Rationale:** A proxy for social capital and exposure to entrepreneurial role models.
    *   **Codes:** `1` (Yes), `0` (No).

*   **`OPPORTyy`**
    *   **Represents:** Whether the respondent perceives good opportunities for starting a business in their area in the next 6 months.
    *   **Rationale:** Measures the perceived favorability of the entrepreneurial environment.
    *   **Codes:** `1` (Yes), `0` (No).

*   **`SUSKILyy`**
    *   **Represents:** Whether the respondent believes they have the required knowledge, skill, and experience to start a new business.
    *   **Rationale:** A measure of entrepreneurial self-efficacy, a key psychological trait.
    *   **Codes:** `1` (Yes), `0` (No).

*   **`FRFAILyy`**
    *   **Represents:** Whether the fear of failure would prevent the respondent from starting a business.
    *   **Rationale:** Measures risk aversion, a significant barrier to entrepreneurship.
    *   **Codes:** `1` (Yes, fear would prevent), `0` (No).

*   **`GEMHHINC`**
    *   **Represents:** The respondent's household income, recoded by GEM into three tiers (terciles).
    *   **Rationale:** A proxy for economic status and access to initial capital or a financial safety net.
    *   **Codes:**
        *   `33`: Lowest 33% income tier
        *   `3467`: Middle 33% income tier
        *   `68100`: Upper 33% income tier
  
---

**Rationale for Using `...yy` Variables**

For our perceptual and attitudinal features (e.g., `KNOWENyy`, `OPPORTyy`, `SUSKILyy`, `FRFAILyy`), we opted for the GEM-derived `...yy` variables (e.g., `KNOWENyy` instead of `knowentR`). These variables represent a clean, binary (Yes/No) interpretation (`0` or `1`) that is specifically adapted by GEM for national-level aggregation and reporting. While the original `...L` or raw variables offer a Likert-scale response or other coding, the `...yy` counterparts provide a clear, aggregated indicator of the core perception, making them ideal for our binary classification task and aligning with GEM's own reporting standards.