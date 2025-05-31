# wfpair.f90

## Overview

The `dftd4_data_wfpair` module is a specialized data provider within the dftd4 library. It supplies pairwise weighting factors, which are described in the source code as "Pairwise CN weighting factors" and are intended to "reduce weight factor exponents to improve smoothness of the potential." This suggests these factors are exponents or parameters used in a weighting function, likely related to the calculation of coordination numbers (CN) or how interactions between specific pairs of atom types are weighted in the DFT-D4 model. The D4 model uses a weighting function `wf` which depends on these parameters.

## Key Components

*   **`get_wfpair_val` (interface):**
    *   This public interface allows retrieval of the pairwise weighting factor for a given pair of elements. It maps to two specific elemental functions:
        *   **`get_wfpair_val_num(atomic_number_i, atomic_number_j)`:** (Elemental Function) Returns the weighting factor for a pair of elements identified by their atomic numbers.
        *   **`get_wfpair_val_sym(element_symbol_i, element_symbol_j)`:** (Elemental Function) Returns the weighting factor for a pair of elements identified by their chemical symbols. It internally converts the symbols to atomic numbers.

## Important Variables/Constants

*   **`max_elem` (parameter):** An integer parameter defining the maximum atomic number for which data is stored, set to `118` (Oganesson).
*   **`weight_factors(max_elem, max_elem)` (real(wp), protected):**
    *   A private 2D array storing the pairwise weighting factors. `weight_factors(i, j)` holds the factor for the pair of elements with atomic numbers `i` and `j`.
    *   The data is initialized using `DATA` statements for each column (element `j`) paired with all elements `i` from 1 to `max_elem`.
    *   The comment "Reduces weight factor exponents to improve smoothness of the potential Head-Gordon ..." indicates the origin or purpose of these values. Many values are set to `6.000000000_wp`, which might be a default or indicate no specific adjustment for those pairs.

## Usage Examples

The `get_wfpair_val` function is used to retrieve the weighting factor for a specific pair of elements. This factor is then likely used in the calculation of the D4 model's weighting function `wf`.

```fortran
MODULE example_using_wfpair
  USE dftd4_data_wfpair
  ! Or, more commonly, through the main dftd4_data module:
  ! USE dftd4_data, ONLY : get_wfpair_val
  USE mctc_env, ONLY : wp
  IMPLICIT NONE

  SUBROUTINE show_wfpair_values
    REAL(wp) :: wf_H_C, wf_O_Fe

    ! Get weighting factor for Hydrogen (1) - Carbon (6) pair
    wf_H_C = get_wfpair_val(1, 6)

    ! Get weighting factor for Oxygen (8) - Iron (26) pair by symbol
    wf_O_Fe = get_wfpair_val("O", "Fe")

    PRINT *, "Pairwise weighting factor for H-C: ", wf_H_C
    PRINT *, "Pairwise weighting factor for O-Fe: ", wf_O_Fe

  END SUBROUTINE show_wfpair_values

END MODULE example_using_wfpair
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io_symbols`:** Provides the `to_number` function to convert element symbols to atomic numbers.
*   **`dftd4_data` (Aggregator Module):** The `dftd4_data_wfpair` module is primarily used by `dftd4_data`, which re-exports the `get_wfpair_val` interface.
*   **Dispersion Model Modules (e.g., `dftd4_model_d4`):** The D4 model specifically uses a weighting function (`wf`) that depends on these pairwise parameters. These parameters influence how the coordination dependent dispersion coefficients are calculated.
*   **Coordination Number Modules (e.g., `dftd4_ncoord`):** While the comment mentions "CN weighting factors," the direct use in `dftd4_ncoord` might be indirect, via the D4 model's parameterization which then affects coordination number dependent terms. The primary role seems to be in the weighting function `wf` of the D4 model itself.
