# hardness.f90

## Overview

The `dftd4_data_hardness` module is a specialized data provider within the dftd4 library. Its primary role is to supply element-specific chemical hardness (η) values. In the context of DFT-D4, these hardness values are particularly used in charge scaling functions that help extrapolate C6 dispersion coefficients based on the atomic charge state.

## Key Components

*   **`get_hardness` (interface):**
    *   This public interface allows retrieval of chemical hardness values. It maps to two specific elemental functions:
        *   **`get_hardness_num(atomic_number)`:** (Elemental Function) Returns the chemical hardness for a given atomic number.
        *   **`get_hardness_sym(element_symbol)`:** (Elemental Function) Returns the chemical hardness for a given element chemical symbol (e.g., "H", "C"). It internally converts the symbol to an atomic number.

## Important Variables/Constants

*   **`max_elem` (parameter):** An integer parameter defining the maximum atomic number for which data is stored, set to `118` (Oganesson).
*   **`chemical_hardness(max_elem)` (real(wp), parameter):**
    *   A private array storing the chemical hardness values in Hartree atomic units.
    *   The source of these specific hardness values is not explicitly cited in a comment within this file, but they are noted to be "for the charge scaling function used to extrapolate the C6 coefficients in DFT-D4."
    *   Values for elements beyond Md (Mendelevium, Z=101) are zero in this dataset, indicating that this specific charge scaling might not be applied or parameterized for the heaviest elements.

## Usage Examples

The `get_hardness` function can be called with either an atomic number or an element symbol.

```fortran
MODULE example_using_hardness
  USE dftd4_data_hardness
  ! Or, more commonly, through the main dftd4_data module:
  ! USE dftd4_data, ONLY : get_hardness
  USE mctc_env, ONLY : wp
  IMPLICIT NONE

  SUBROUTINE show_hardness_values
    REAL(wp) :: hardness_Li_num, hardness_S_sym, hardness_Cl_num

    hardness_Li_num = get_hardness(3)    ! Get hardness for Lithium (Z=3)
    hardness_S_sym = get_hardness("S")  ! Get hardness for Sulfur by symbol
    hardness_Cl_num = get_hardness(17)  ! Get hardness for Chlorine (Z=17)

    PRINT *, "Chemical Hardness of Lithium (Z=3): ", hardness_Li_num, " Hartree"
    PRINT *, "Chemical Hardness of Sulfur (Symbol S): ", hardness_S_sym, " Hartree"
    PRINT *, "Chemical Hardness of Chlorine (Z=17): ", hardness_Cl_num, " Hartree"

  END SUBROUTINE show_hardness_values

END MODULE example_using_hardness
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io_symbols`:** Provides the `to_number` function to convert element symbols to atomic numbers.
*   **`dftd4_data` (Aggregator Module):** The `dftd4_data_hardness` module is primarily used by `dftd4_data`, which re-exports the `get_hardness` interface. This makes the chemical hardness data available to the rest of the dftd4 library through a unified access point.
*   **Dispersion Model Modules (e.g., `dftd4_model_d4`):** Modules involved in calculating or adjusting dispersion coefficients (like C6 coefficients) based on atomic charges would utilize these hardness values.
*   **Charge Calculation Modules (e.g., `dftd4_charge` via `multicharge`):** Chemical hardness is a fundamental parameter in some electronegativity equilibration models (EEMs) for calculating partial charges, although the primary EEM parameters are usually self-contained or sourced differently in the `multicharge` library. The hardness here is specifically mentioned for C6 extrapolation.
