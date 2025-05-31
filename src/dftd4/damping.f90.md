# damping.f90

## Overview

This file, `dftd4_damping.f90`, defines the generic interface for damping functions used within the DFT-D4 dispersion correction method. Damping functions are essential to attenuate the dispersion correction at short interatomic distances, preventing unphysical behavior or double-counting of correlation effects already captured by the underlying density functional.

This module introduces an abstract type `damping_param` and abstract interfaces (`dispersion_interface`, `pairwise_dispersion_interface`) for calculating the two-body and three-body dispersion energies and their pairwise representations. Concrete implementations of these damping functions (e.g., rational damping, Becke-Johnson damping for ATM) are expected to be provided by other modules that extend this abstract type and implement the deferred procedures. These implementations are typically found in the `src/dftd4/damping/` subdirectory.

## Key Components

*   **`damping_param` (abstract type):**
    *   An abstract derived type that serves as the base for all specific damping function parameterizations.
    *   It contains deferred procedures that must be implemented by concrete subtypes:
        *   `get_dispersion2`: Calculates the two-body dispersion energy and its derivatives.
        *   `get_dispersion3`: Calculates the three-body (ATM - Axilrod-Teller-Muto) dispersion energy and its derivatives.
        *   `get_pairwise_dispersion2`: Calculates the pairwise representation of the two-body dispersion energy.
        *   `get_pairwise_dispersion3`: Calculates the pairwise representation of the three-body dispersion energy.
*   **`dispersion_interface` (abstract interface):**
    *   Defines the signature for subroutines that calculate the dispersion energy (either 2-body or 3-body) and its derivatives with respect to coordination numbers, partial charges, atomic positions (gradient), and cell parameters (virial/sigma).
    *   Inputs include the `damping_param` object itself, molecular structure, lattice translations, cutoff radius, C6 coefficients (and their derivatives), and <r⁴>/<r²> expectation values.
*   **`pairwise_dispersion_interface` (abstract interface):**
    *   Defines the signature for subroutines that calculate the pairwise decomposition of the dispersion energy.
    *   Inputs are similar to `dispersion_interface` but focus on outputting a matrix of pairwise energies.

## Important Variables/Constants

This top-level module primarily defines abstract types and interfaces. Specific parameters for damping functions (e.g., s6, s8, a1, a2 for rational damping) are defined within the concrete implementations of the `damping_param` type in other modules.

## Usage Examples

A concrete type extending `damping_param` (e.g., `rational_damping_param` from `dftd4_damping_rational.f90`) would be instantiated and its parameters set. This object is then passed to higher-level routines that calculate the total dispersion energy.

```fortran
! Conceptual example:
! (Actual instantiation and usage would involve a concrete type)

MODULE dispersion_calculator
  USE dftd4_damping         ! For the abstract type and interfaces
  USE dftd4_damping_rational ! For a concrete implementation (assumed)
  USE mctc_io, ONLY : structure_type
  USE mctc_env, ONLY : wp

  IMPLICIT NONE

  SUBROUTINE calculate_total_dispersion(mol_structure, damping_parameters, &
                                        r4r2_vals, c6_coeffs, total_energy)
    TYPE(structure_type), INTENT(IN) :: mol_structure
    CLASS(damping_param), INTENT(IN) :: damping_parameters ! Polymorphic damping object
    REAL(wp), INTENT(IN) :: r4r2_vals(:)
    REAL(wp), INTENT(IN) :: c6_coeffs(:,:)
    REAL(wp), INTENT(OUT) :: total_energy

    REAL(wp) :: energy2b(1), energy3b(1)
    REAL(wp), ALLOCATABLE :: translations(:,:)
    REAL(wp) :: cutoff_radius_2b, cutoff_radius_3b

    ! ... (Setup: get translations, cutoff radii, etc.) ...
    ! For example, cutoff_radius_2b might come from a realspace_cutoff type

    energy2b = 0.0_wp
    energy3b = 0.0_wp

    ! Call the appropriate dispersion calculation using the polymorphic object
    ! The actual implementation (e.g., rational damping) gets called.
    CALL damping_parameters%get_dispersion2(mol_structure, translations, cutoff_radius_2b, &
                                            r4r2_vals, c6_coeffs, energy2b)
    ! Optionally, add three-body term
    ! CALL damping_parameters%get_dispersion3(mol_structure, translations, cutoff_radius_3b, &
    !                                         r4r2_vals, c6_coeffs, energy3b)
                                            ! (Note: 3-body C9 coeffs would be needed here)


    total_energy = energy2b(1) + energy3b(1)

    IF (ALLOCATED(translations)) DEALLOCATE(translations)

  END SUBROUTINE calculate_total_dispersion

END MODULE dispersion_calculator
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io`:** Provides `structure_type` for defining molecular structures, which is used in the interfaces.
*   **Concrete Damping Modules (e.g., `dftd4_damping_rational.f90`, `dftd4_damping_bj.f90`):** These modules provide specific implementations of `damping_param` by extending the abstract type and implementing the deferred procedures. They are the primary "users" or implementers of the interfaces defined here.
*   **Dispersion Calculation Modules (e.g., `dftd4_disp.f90`):** These modules will use objects of a concrete type derived from `damping_param` to compute the damped dispersion energy. They will call the procedures like `get_dispersion2` and `get_dispersion3` on these objects.
*   **Parameter Modules (e.g., `dftd4_param.f90`):** May be used to retrieve pre-defined parameters for various damping schemes which are then used to construct specific `damping_param` instances.
