# zeff.f90

## Overview

The `dftd4_data_zeff` module is a specialized data provider within the dftd4 library. It is responsible for supplying effective nuclear charges (Z_eff) for chemical elements. According to the source code comments, these Z_eff values are derived from the def2 series of effective core potentials (ECPs) and are specifically used in the calculation of reference atomic polarizabilities within the DFT-D4 framework. These reference polarizabilities are fundamental for deriving dispersion coefficients.

## Key Components

*   **`get_effective_charge` (interface):**
    *   This public interface allows retrieval of the effective nuclear charge for an element. It maps to two specific elemental functions:
        *   **`get_effective_charge_num(atomic_number)`:** (Elemental Function) Returns the effective nuclear charge for a given atomic number.
        *   **`get_effective_charge_sym(element_symbol)`:** (Elemental Function) Returns the effective nuclear charge for a given element chemical symbol. It internally converts the symbol to an atomic number.

## Important Variables/Constants

*   **`max_elem` (parameter):** An integer parameter defining the maximum atomic number for which data is stored, set to `118` (Oganesson).
*   **`effective_nuclear_charge(max_elem)` (real(wp), parameter):**
    *   A private array storing the effective nuclear charge values.
    *   The comment states: "Effective nuclear charges from the def2-ECPs used for calculating the reference polarizibilities for DFT-D4."
    *   For H-He (Z=1-2), Li-Ne (Z=3-10), Na-Ar (Z=11-18), and K-Kr (Z=19-36), the Z_eff values correspond to the actual nuclear charge Z (i.e., number of valence electrons for common ECPs).
    *   For Rb-Xe (Z=37-54), the Z_eff starts from 9 (for Rb, Z=37) up to 26 (for Xe, Z=54). This suggests a [core] + (Z-core_charge) type of Z_eff, likely from an ECP that keeps more electrons in the core for heavier elements.
    *   For Cs-Lu (Z=55-71), Z_eff starts from 9 (for Cs, Z=55) and goes up to 43 (for Lu, Z=71).
    *   For Hf-Rn (Z=72-86), Z_eff are 12 through 26.
    *   For Fr-Lr (Z=87-103) and Rf-Og (Z=104-118), the values are stated to be "just copy & paste from above," implying a pattern repetition or approximation for the heaviest elements, matching the Cs-Lu and Hf-Rn series respectively.

## Usage Examples

The `get_effective_charge` function is used to retrieve the Z_eff for an element, which is then used in calculating reference polarizabilities and subsequently dispersion coefficients.

```fortran
MODULE example_using_zeff
  USE dftd4_data_zeff
  ! Or, more commonly, through the main dftd4_data module:
  ! USE dftd4_data, ONLY : get_effective_charge
  USE mctc_env, ONLY : wp
  IMPLICIT NONE

  SUBROUTINE show_zeff_values
    REAL(wp) :: zeff_H_num, zeff_Ar_sym, zeff_Rb_num

    zeff_H_num = get_effective_charge(1)    ! Get Z_eff for Hydrogen (Z=1)
    zeff_Ar_sym = get_effective_charge("Ar") ! Get Z_eff for Argon by symbol (Z=18)
    zeff_Rb_num = get_effective_charge(37)  ! Get Z_eff for Rubidium (Z=37)

    PRINT *, "Effective nuclear charge of Hydrogen (Z=1): ", zeff_H_num
    PRINT *, "Effective nuclear charge of Argon (Symbol Ar): ", zeff_Ar_sym
    PRINT *, "Effective nuclear charge of Rubidium (Z=37): ", zeff_Rb_num

  END SUBROUTINE show_zeff_values

END MODULE example_using_zeff
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io_symbols`:** Provides the `to_number` function to convert element symbols to atomic numbers.
*   **`dftd4_data` (Aggregator Module):** The `dftd4_data_zeff` module is primarily used by `dftd4_data`, which re-exports the `get_effective_charge` interface.
*   **Dispersion Model Modules (e.g., `dftd4_model_d4`, `dftd4_model_d4s`):** These modules use Z_eff values in the calculation of reference atomic polarizabilities (α₀), which are then used to derive C6 dispersion coefficients. The formula for α₀ often involves Z_eff and <r²> values.
*   Any other part of the DFT-D4 model that requires an estimate of the effective nuclear charge, possibly for scaling interactions or properties.
