# charge.f90

## Overview

This file defines the module `dftd4_charge`, which is responsible for calculating atomic partial charges using an electronegativity equilibration model (EEM). Specifically, it utilizes the EEQ2019 model implemented in the `multicharge` library. The calculated charges are often used as input for other calculations within the DFT-D4 framework, such as determining dispersion coefficients that depend on the local chemical environment.

## Key Components

*   **`get_charges(mol, qvec, dqdr, dqdL)`:** (Subroutine)
    *   This is the primary public routine in this module.
    *   It calculates atomic partial charges (`qvec`) for a given molecular structure (`mol`).
    *   Optionally, it can also compute the derivatives of these charges with respect to atomic Cartesian coordinates (`dqdr`) and with respect to strain deformations (`dqdL`).
    *   It internally uses an electronegativity equilibration model (EEQ2019) to determine the charges.

## Important Variables/Constants

*   **`EEQ2019 Model`:** The underlying charge calculation scheme is the EEQ2019 model, which is accessed via the `multicharge` library. This model has its own set of parameters and an algorithm for charge determination based on atomic properties and molecular geometry.
*   **`cn_max` (parameter):** A real parameter used within `get_charges`, likely representing a maximum coordination number for some internal calculation, set to `8.0_wp`.
*   **`cutoff` (parameter):** A real parameter used within `get_charges`, likely a distance cutoff for interactions, set to `25.0_wp`. (Note: The direct use of these parameters in `get_charges` is not immediately visible in the provided snippet, they might be passed to or used by the `model%ncoord%get_cn` or `model%solve` methods).

## Usage Examples

The `get_charges` subroutine is typically called internally by other parts of the dftd4 library when charge-dependent properties are needed.

```fortran
! Conceptual example of how get_charges might be used:

MODULE some_other_dftd4_module
  USE dftd4_charge
  USE mctc_io, ONLY : structure_type
  USE mctc_env, ONLY : wp

  IMPLICIT NONE

  SUBROUTINE calculate_something_with_charges(mol_structure)
    TYPE(structure_type), INTENT(IN) :: mol_structure
    REAL(wp), ALLOCATABLE :: charges(:)
    ! For simplicity, derivatives are not requested here
    ! REAL(wp), ALLOCATABLE :: charge_derivatives_xyz(:,:,:), charge_derivatives_lattice(:,:,:)

    ALLOCATE(charges(mol_structure%nat))

    ! Obtain the partial charges for the given structure
    CALL get_charges(mol_structure, charges)

    ! Now 'charges' array can be used for further calculations
    ! ...

    DEALLOCATE(charges)
  END SUBROUTINE calculate_something_with_charges

END MODULE some_other_dftd4_module
```

## Dependencies and Interactions

*   **`iso_fortran_env`:** Used for `error_unit` for printing error messages.
*   **`mctc_env`:** Provides `error_type` for error handling and `wp` for defining working precision.
*   **`mctc_io`:** Provides `structure_type` for defining molecular structures.
*   **`multicharge`:** This is a crucial external dependency. The `dftd4_charge` module uses `mchrg_model_type` and `new_eeq2019_model` from this library to perform the actual electronegativity equilibration.
    *   `model%ncoord%get_cn()`: Called to get coordination numbers.
    *   `model%solve()`: Called to solve the EEM equations and obtain charges and their derivatives.
*   **Other dftd4 modules:** The charges calculated by this module can be used by various other modules within dftd4, particularly those involved in calculating dispersion coefficients (like C6 coefficients) which are often environment-dependent and can be refined using partial atomic charges.
