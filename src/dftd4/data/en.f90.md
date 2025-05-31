# en.f90

## Overview

The `dftd4_data_en` module is a specialized data provider within the dftd4 library, focused on supplying Pauling electronegativity values for chemical elements. Electronegativity is a fundamental atomic property used in various parts of the DFT-D4 model, particularly in schemes that determine atomic partial charges (like electronegativity equilibration models) and potentially in environment-dependent parameterizations.

## Key Components

*   **`get_electronegativity` (interface):**
    *   This public interface allows retrieval of Pauling electronegativity values. It maps to two specific elemental functions:
        *   **`get_electronegativity_num(atomic_number)`:** (Elemental Function) Returns the Pauling electronegativity for a given atomic number.
        *   **`get_electronegativity_sym(element_symbol)`:** (Elemental Function) Returns the Pauling electronegativity for a given element chemical symbol (e.g., "H", "C"). It internally converts the symbol to an atomic number.

## Important Variables/Constants

*   **`max_elem` (parameter):** An integer parameter defining the maximum atomic number for which data is stored, set to `118` (Oganesson).
*   **`pauling_en(max_elem)` (real(wp), parameter):**
    *   A private array storing Pauling electronegativity values.
    *   The source of these values is not explicitly cited in a comment within this specific file, but they are standard Pauling scale values.
    *   Values for elements beyond Lr (Lawrencium, Z=103) are listed as "dummies" with a value of `1.50_wp` up to Z=118. This suggests that reliable or specific Pauling electronegativities for the heaviest elements might not be used or available in this parameterization.

## Usage Examples

The `get_electronegativity` function can be called with either an atomic number or an element symbol.

```fortran
MODULE example_using_en
  USE dftd4_data_en
  ! Or, more commonly, through the main dftd4_data module:
  ! USE dftd4_data, ONLY : get_electronegativity
  USE mctc_env, ONLY : wp
  IMPLICIT NONE

  SUBROUTINE show_en_values
    REAL(wp) :: en_H_num, en_O_sym, en_Fe_num

    en_H_num = get_electronegativity(1)    ! Get EN for Hydrogen (Z=1)
    en_O_sym = get_electronegativity("O")  ! Get EN for Oxygen by symbol
    en_Fe_num = get_electronegativity(26)  ! Get EN for Iron (Z=26)

    PRINT *, "Pauling EN of Hydrogen (Z=1): ", en_H_num
    PRINT *, "Pauling EN of Oxygen (Symbol O): ", en_O_sym
    PRINT *, "Pauling EN of Iron (Z=26): ", en_Fe_num

  END SUBROUTINE show_en_values

END MODULE example_using_en
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io_symbols`:** Provides the `to_number` function to convert element symbols to atomic numbers.
*   **`dftd4_data` (Aggregator Module):** The `dftd4_data_en` module is primarily used by `dftd4_data`, which re-exports the `get_electronegativity` interface. This makes the electronegativity data available to the rest of the dftd4 library through a unified access point.
*   **Charge Calculation Modules (e.g., `dftd4_charge` via `multicharge`):** Electronegativity is a key input parameter for electronegativity equilibration methods (EEM) used to calculate atomic partial charges.
*   Other modules requiring electronegativity for parameterization or property calculation might also consume this data.
