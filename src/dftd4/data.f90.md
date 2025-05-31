# data.f90

## Overview

The `dftd4_data` module serves as a centralized access point for various fundamental atomic data parameters required by the DFT-D4 dispersion model. It does not store data itself but rather re-exports public procedures from specialized sub-modules located in the `src/dftd4/data/` subdirectory. These sub-modules contain the actual data tables and functions to retrieve element-specific properties such as covalent radii, electronegativity, atomic hardness, <r⁴>/<r²> expectation values, pairwise weighting factors for weighting functions, and effective nuclear charges.

This aggregation simplifies how other parts of the dftd4 library access these essential atomic parameters.

## Key Components

This module makes the following procedures (originally defined in sub-modules) publicly available:

*   **`get_covalent_rad(element_id)`:** (Function from `dftd4_data_covrad`)
    *   Returns the covalent radius for a given element (identified by its atomic number or symbol).
*   **`get_electronegativity(element_id)`:** (Function from `dftd4_data_en`)
    *   Returns the electronegativity for a given element.
*   **`get_hardness(element_id)`:** (Function from `dftd4_data_hardness`)
    *   Returns the atomic hardness for a given element.
*   **`get_r4r2_val(element_id)`:** (Function from `dftd4_data_r4r2`)
    *   Returns the <r⁴>/<r²> expectation value for a given element, which is crucial for calculating C8 coefficients and in some damping functions.
*   **`get_wfpair_val(element_id1, element_id2)`:** (Function from `dftd4_data_wfpair`)
    *   Returns a pairwise weighting factor used in the weighting function (wf) for the D4 model, based on the identity of two interacting elements.
*   **`get_effective_charge(element_id)`:** (Function from `dftd4_data_zeff`)
    *   Returns the effective nuclear charge (Z_eff) for a given element.

## Important Variables/Constants

The actual data tables (e.g., arrays of covalent radii indexed by atomic number) are defined as private module variables within the respective sub-modules in the `src/dftd4/data/` directory (e.g., `covrad_PBE` in `dftd4_data_covrad.f90`). The `dftd4_data` module itself does not define any top-level constants or variables other than implicitly through its `USE` statements.

## Usage Examples

Other modules within the dftd4 library can use the `dftd4_data` module to retrieve atomic properties needed for their calculations.

```fortran
! Conceptual example of retrieving data for Carbon (atomic number 6)

MODULE calculate_something_atomic
  USE dftd4_data
  USE mctc_env, ONLY : wp
  IMPLICIT NONE

  SUBROUTINE print_carbon_data
    REAL(wp) :: carbon_cov_rad, carbon_en, carbon_r4r2
    INTEGER :: carbon_z = 6

    carbon_cov_rad = get_covalent_rad(carbon_z)
    carbon_en = get_electronegativity(carbon_z)
    carbon_r4r2 = get_r4r2_val(carbon_z)

    PRINT *, "Carbon Covalent Radius: ", carbon_cov_rad
    PRINT *, "Carbon Electronegativity: ", carbon_en
    PRINT *, "Carbon <r4>/<r2>: ", carbon_r4r2

  END SUBROUTINE print_carbon_data

END MODULE calculate_something_atomic
```

## Dependencies and Interactions

*   **`dftd4_data_covrad`:** Provides `get_covalent_rad`.
*   **`dftd4_data_en`:** Provides `get_electronegativity`.
*   **`dftd4_data_hardness`:** Provides `get_hardness`.
*   **`dftd4_data_r4r2`:** Provides `get_r4r2_val`.
*   **`dftd4_data_wfpair`:** Provides `get_wfpair_val`.
*   **`dftd4_data_zeff`:** Provides `get_effective_charge`.

**Interactions:**

*   The `dftd4_data` module acts as a primary data source for many other modules in the dftd4 library.
*   **Dispersion Model Modules (e.g., `dftd4_model_d4`, `dftd4_model_d4s`):** These modules heavily rely on data retrieved via `dftd4_data` for parameterizing the dispersion model (e.g., calculating C6, C8 coefficients, reference pair interaction energies).
*   **Coordination Number Modules (e.g., `dftd4_ncoord`):** Often use covalent radii to determine bonding patterns and coordination numbers.
*   **Damping Function Modules (e.g., `dftd4_damping_rational`):** May use <r⁴>/<r²> values in the damping function parameters.
*   **Charge Calculation Modules (e.g., `dftd4_charge` via `multicharge`):** Electronegativity and hardness are fundamental parameters for electronegativity equilibration models.
