# covrad.f90

## Overview

The `dftd4_data_covrad` module is a specialized data provider within the dftd4 library. Its sole purpose is to supply covalent radii for chemical elements. These radii are primarily used in the calculation of coordination numbers, which are essential for determining environment-dependent dispersion coefficients and other properties in the DFT-D4 model. The radii are based on published values by Pyykkö and Atsumi (2009), with a modification for metals, and are further scaled for use in the D3-type coordination number scheme.

## Key Components

*   **`get_covalent_rad` (interface):**
    *   This public interface allows retrieval of covalent radii. It maps to two specific elemental functions:
        *   **`get_covalent_rad_num(atomic_number)`:** (Elemental Function) Returns the covalent radius for a given atomic number.
        *   **`get_covalent_rad_sym(element_symbol)`:** (Elemental Function) Returns the covalent radius for a given element chemical symbol (e.g., "H", "He", "C"). It internally converts the symbol to an atomic number.

## Important Variables/Constants

*   **`max_elem` (parameter):** An integer parameter defining the maximum atomic number for which data is stored, set to `118` (Oganesson).
*   **`covalent_rad_2009(max_elem)` (real(wp), parameter):**
    *   A private array storing covalent radii in Atomic Units (Bohr).
    *   The values are taken from Pyykkö and Atsumi, *Chem. Eur. J.* **15**, 2009, 188-197.
    *   For metallic elements, these radii are decreased by 10% from the original published values.
    *   The conversion from Ångström (as likely found in the paper) to Bohr is done using `aatoau` from `mctc_io_convert`.
*   **`covalent_rad_d3(max_elem)` (real(wp), parameter):**
    *   A private array storing the covalent radii specifically used for the D3-type coordination number calculation in DFT-D4.
    *   These values are derived by scaling the `covalent_rad_2009` values by a factor of `4.0/3.0`.
    *   The `get_covalent_rad_num` function returns values from this array.

## Usage Examples

The `get_covalent_rad` function can be called with either an atomic number or an element symbol.

```fortran
MODULE example_using_covrad
  USE dftd4_data_covrad
  ! Or, more commonly, through the main dftd4_data module:
  ! USE dftd4_data, ONLY : get_covalent_rad
  USE mctc_env, ONLY : wp
  IMPLICIT NONE

  SUBROUTINE show_radii
    REAL(wp) :: radius_H_num, radius_C_sym, radius_Au_num

    radius_H_num = get_covalent_rad(1)    ! Get radius for Hydrogen (Z=1)
    radius_C_sym = get_covalent_rad("C")  ! Get radius for Carbon by symbol
    radius_Au_num = get_covalent_rad(79)  ! Get radius for Gold (Z=79)

    PRINT *, "Covalent radius of Hydrogen (Z=1): ", radius_H_num, " Bohr"
    PRINT *, "Covalent radius of Carbon (Symbol C): ", radius_C_sym, " Bohr"
    PRINT *, "Covalent radius of Gold (Z=79): ", radius_Au_num, " Bohr"

  END SUBROUTINE show_radii

END MODULE example_using_covrad
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io_convert`:** Provides the `aatoau` function for converting Ångström to Atomic Units (Bohr).
*   **`mctc_io_symbols`:** Provides the `to_number` function to convert element symbols to atomic numbers.
*   **`dftd4_data` (Aggregator Module):** The `dftd4_data_covrad` module is primarily used by `dftd4_data`, which re-exports the `get_covalent_rad` interface, making it available to the rest of the dftd4 library through a single point of access.
*   **Coordination Number Modules (e.g., `dftd4_ncoord`):** These modules are the main consumers of the covalent radii data for determining atomic coordination numbers.
*   Other modules that might need element-specific size parameters could also use this data.
