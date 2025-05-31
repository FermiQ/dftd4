# disp.f90

## Overview

The `dftd4_disp` module serves as a high-level wrapper for performing DFT-D4 calculations. It coordinates various components of the DFT-D4 model to compute the total dispersion energy, and optionally, its analytical derivatives (atomic gradients and virial/stress tensor). It also provides routines to extract related properties like coordination numbers, atomic partial charges, dynamic C6 coefficients, and static polarizabilities.

This module brings together functionalities from structure handling, dispersion models, damping parameterizations, coordination number calculations, and charge calculations to provide a unified interface for DFT-D4 computations.

## Key Components

*   **`get_dispersion(mol, disp, param, cutoff, energy, gradient, sigma)`:** (Subroutine)
    *   This is the main computational engine for DFT-D4.
    *   **Inputs:**
        *   `mol`: Molecular structure data (`structure_type`).
        *   `disp`: Dispersion model object (`dispersion_model`), e.g., D4 or D4S.
        *   `param`: Damping parameters object (`damping_param`), e.g., rational damping.
        *   `cutoff`: Real-space cutoff settings (`realspace_cutoff`).
    *   **Outputs:**
        *   `energy`: Total dispersion energy.
        *   `gradient` (optional): Atomic gradients of the dispersion energy.
        *   `sigma` (optional): Virial/stress tensor components.
    *   **Process:**
        1.  Calculates coordination numbers (CN) using `dftd4_ncoord`.
        2.  Calculates atomic partial charges (`q`) using `dftd4_charge`.
        3.  Weights reference C6 coefficients based on CN and `q` using `disp%weight_references`.
        4.  Calculates environment-dependent atomic C6 coefficients using `disp%get_atomic_c6`.
        5.  Calls the two-body dispersion calculation (`param%get_dispersion2`).
        6.  If derivatives are requested, adds contributions from charge derivatives (`dqdr`, `dqdL`).
        7.  Re-weights and recalculates C6 for the three-body term (ATM), often setting charges to zero for this part.
        8.  Calls the three-body dispersion calculation (`param%get_dispersion3`).
        9.  If derivatives are requested, adds contributions from CN derivatives.
        10. Sums energies to get the total dispersion energy.

*   **`get_properties(mol, disp, cutoff, cn, q, c6, alpha)`:** (Subroutine)
    *   Calculates and returns various properties related to the dispersion model for a given structure.
    *   **Inputs:** `mol`, `disp`, `cutoff`.
    *   **Outputs:**
        *   `cn`: Coordination numbers for each atom.
        *   `q`: Atomic partial charges.
        *   `c6`: Environment-dependent (dynamic) C6 coefficients for each atom pair.
        *   `alpha`: Static atomic polarizabilities.
    *   **Process:** Similar initial steps to `get_dispersion` (CN, charges, reference weighting) then calls specific model routines `disp%get_atomic_c6` and `disp%get_polarizibilities`.

*   **`get_pairwise_dispersion(mol, disp, param, cutoff, energy2, energy3)`:** (Subroutine)
    *   Calculates the pairwise decomposition of the dispersion energy.
    *   **Inputs:** `mol`, `disp`, `param`, `cutoff`.
    *   **Outputs:**
        *   `energy2`: Matrix of pairwise additive (two-body) dispersion energies.
        *   `energy3`: Matrix of pairwise non-additive (three-body, ATM) dispersion energies.
    *   **Process:** Similar to `get_dispersion` in terms of calculating CN, charges, and C6 coefficients. Then calls `param%get_pairwise_dispersion2` and `param%get_pairwise_dispersion3`.

## Important Variables/Constants

This module primarily operates on data passed via its arguments, especially the derived types:
*   `structure_type` (from `mctc_io`): Contains atomic coordinates, lattice vectors, periodicity.
*   `dispersion_model` (from `dftd4_model`): Contains parameters and methods specific to the chosen dispersion model (e.g., D4, D4S), like reference C6 values, <r⁴>/<r²> values, covalent radii scaling (`rcov`), and electronegativity scaling (`en`).
*   `damping_param` (from `dftd4_damping`): Contains parameters and methods for the chosen damping function (e.g., rational damping).
*   `realspace_cutoff` (from `dftd4_cutoff`): Contains cutoff radii for CN, two-body, and three-body terms.

Internal allocatable arrays are used for intermediate results like `cn`, `q`, `c6`, derivatives, etc.

## Usage Examples

```fortran
! Conceptual example of calling get_dispersion

MODULE calculate_dftd4_energy
  USE mctc_io, ONLY : structure_type
  USE dftd4_model_d4, ONLY : d4_model, new_d4_model ! Assuming D4 model
  USE dftd4_damping_rational, ONLY : rational_damping_param ! Assuming rational damping
  USE dftd4_cutoff, ONLY : realspace_cutoff
  USE dftd4_disp, ONLY : get_dispersion
  USE dftd4_param, ONLY : get_rational_damping ! For loading damping params

  IMPLICIT NONE

  SUBROUTINE compute_energy(mol_structure)
    TYPE(structure_type), INTENT(IN) :: mol_structure
    CLASS(dispersion_model), POINTER :: d4_disp_model
    CLASS(damping_param), POINTER :: damping_parameters
    TYPE(realspace_cutoff) :: cutoffs
    REAL(wp) :: total_disp_energy
    REAL(wp), ALLOCATABLE :: gradient(:,:), virial(:,:)

    ! 1. Initialize the dispersion model (e.g., D4)
    CALL new_d4_model(mol_structure, d4_disp_model) ! Simplified call

    ! 2. Initialize damping parameters (e.g., rational for PBE)
    ALLOCATE(rational_damping_param :: damping_parameters)
    CALL get_rational_damping("PBE", damping_parameters) ! Simplified call

    ! 3. Set cutoff distances (can use defaults or customize)
    cutoffs = realspace_cutoff() ! Default cutoffs
    ! cutoffs%disp2 = 80.0_wp ! Example customization

    ! 4. Allocate space for derivatives if needed
    ALLOCATE(gradient(3, mol_structure%nat), virial(3,3))

    ! 5. Call the main dispersion calculation routine
    CALL get_dispersion(mol_structure, d4_disp_model, damping_parameters, &
                        cutoffs, total_disp_energy, gradient, virial)

    PRINT *, "DFT-D4 Dispersion Energy: ", total_disp_energy

    DEALLOCATE(gradient, virial, d4_disp_model, damping_parameters)
  END SUBROUTINE compute_energy

END MODULE calculate_dftd4_energy
```

## Dependencies and Interactions

*   **`dftd4_blas`:** Used for matrix-vector operations (`d4_gemv`) when calculating gradient contributions from charge derivatives.
*   **`dftd4_charge`:** Provides `get_charges` to calculate atomic partial charges and their derivatives.
*   **`dftd4_cutoff`:** Provides `realspace_cutoff` type and `get_lattice_points` for handling periodic boundary conditions and interaction ranges.
*   **`dftd4_damping`:** Defines the abstract `damping_param` type. The concrete damping functions (e.g., `get_dispersion2`, `get_dispersion3`) bound to this type are called.
*   **`dftd4_data`:** Provides access to fundamental atomic data like covalent radii (`get_covalent_rad`), which are used by the `dispersion_model` (though `get_covalent_rad` is directly used in `get_coordination_number` call here, the model likely holds these values internally e.g. `disp%rcov`).
*   **`dftd4_model`:** Defines the abstract `dispersion_model` type. Methods from concrete models (e.g., `disp%weight_references`, `disp%get_atomic_c6`, `disp%get_polarizibilities`) are called.
*   **`dftd4_ncoord`:** Provides `get_coordination_number` and `add_coordination_number_derivs` for calculating coordination numbers and their contribution to gradients/virial.
*   **`mctc_env`:** Provides `wp` for working precision.
*   **`mctc_io`:** Provides `structure_type`.
*   **`mctc_io_convert`:** Provides `autoaa` (though not directly visible in these subroutines, it might be used by dependencies).

This module is central to the DFT-D4 calculation, acting as an orchestrator for many other specialized modules.
