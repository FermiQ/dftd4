# atm.f90

## Overview

This file, `dftd4_damping_atm.f90`, implements the Axilrod-Teller-Muto (ATM) triple-dipole dispersion energy term, which is a form of three-body interaction. The implementation uses a modified zero-damping function (similar to Chai-Head-Gordon) combined with critical radii parameters (a1, a2) that are typically associated with rational or Becke-Johnson damping schemes. This module provides the routines to calculate the ATM energy and its derivatives with respect to atomic positions, coordination numbers, and partial charges.

It's important to note that this module itself does not define a concrete class derived from `damping_param` (from `dftd4_damping.f90`). Instead, these ATM calculation routines are designed to be called by a specific `damping_param` implementation that wishes to include a three-body ATM contribution.

## Key Components

*   **`get_atm_dispersion(...)`:** (Subroutine)
    *   This is the main public entry point for calculating the ATM dispersion contribution.
    *   It acts as a dispatcher, calling either the energy-only or the energy-and-derivatives subroutine based on the presence of optional arguments for derivatives.
*   **`get_atm_dispersion_energy(...)`:** (Subroutine)
    *   Calculates only the ATM dispersion energy for a given molecular structure, interaction parameters, and C6 coefficients.
    *   It iterates over all unique atomic triplets (iat, jat, kat) and their periodic images within the cutoff.
    *   Applies the specified damping function to the raw C9 * R⁻⁹ term.
    *   Uses OpenMP for parallelization over the outer atom loop.
*   **`get_atm_dispersion_derivs(...)`:** (Subroutine)
    *   Calculates the ATM dispersion energy and its derivatives with respect to atomic positions (gradient), cell parameters (virial/sigma), coordination numbers (dEdcn), and partial charges (dEdq).
    *   Similar loop structure to the energy-only version but includes analytical derivative calculations.
    *   Uses OpenMP for parallelization.
*   **`triple_scale(ii, jj, kk)`:** (Elemental Function)
    *   A helper function to determine the scaling factor for the energy contribution of a triplet depending on whether the indices (ii, jj, kk) are unique or if some atoms are identical (e.g., in intra-molecular interactions or interactions involving the same atom in different unit cells). This ensures correct counting.

## Important Variables/Constants

The subroutines in this module take several key parameters as input, which define the ATM interaction and its damping:

*   **`s9` (real(wp), intent(in)):** A scaling factor for the C9 dispersion coefficients. If `s9` is close to zero, the ATM contribution is effectively turned off.
*   **`a1` (real(wp), intent(in)):** A scaling parameter used in the calculation of the critical interaction radius `r0` for the damping function. Typically derived from a base two-body damping scheme (like rational/BJ damping).
*   **`a2` (real(wp), intent(in)):** An offset parameter used in the calculation of the critical interaction radius `r0`. Also typically from the base two-body damping scheme.
*   **`alp` (real(wp), intent(in)):** The exponent used in the zero-damping function: `fdmp = 1.0 / (1.0 + 6.0 * (r0 / r_ijk)**(alp / 3.0))`.
*   **`r4r2` (real(wp), intent(in)):** Array of expectation values <r⁴>/<r²> for each atom type, used in constructing the critical radii `r0ij`, `r0ik`, `r0jk`.
*   **`c6` (real(wp), intent(in)):** Matrix of C6 dispersion coefficients for all atom pairs. The C9 coefficient for a triplet (i,j,k) is estimated as `-s9 * sqrt(|c6ij * c6ik * c6jk|)`.

## Usage Examples

These subroutines are not typically called directly by the end-user. Instead, a concrete `damping_param` object (e.g., an instance of `rational_damping_param`) would internally call `get_atm_dispersion` if it's configured to include three-body effects.

```fortran
! Conceptual usage within a concrete damping_param implementation's
! get_dispersion3 method:

SUBROUTINE some_concrete_damping_get_dispersion3(self, mol, trans, cutoff, &
                                                 r4r2_vals, c6_coeffs, c9_coeffs_unused, &
                                                 energy3b, dEdcn3b, dEdq3b, grad3b, sigma3b)
  CLASS(my_concrete_damping_type), INTENT(IN) :: self
  CLASS(structure_type), INTENT(IN) :: mol
  REAL(wp), INTENT(IN) :: trans(:,:)
  REAL(wp), INTENT(IN) :: cutoff        ! This would be self%disp3_cutoff
  REAL(wp), INTENT(IN) :: r4r2_vals(:)
  REAL(wp), INTENT(IN) :: c6_coeffs(:,:)
  REAL(wp), INTENT(IN), OPTIONAL :: c9_coeffs_unused(:,:) ! ATM calculates C9 internally
  REAL(wp), INTENT(INOUT) :: energy3b(:)
  REAL(wp), INTENT(INOUT), OPTIONAL :: dEdcn3b(:), dEdq3b(:)
  REAL(wp), INTENT(INOUT), OPTIONAL :: grad3b(:,:), sigma3b(:,:)

  ! Parameters for ATM would be part of 'self' (the concrete damping object)
  ! e.g., self%s9_atm, self%a1_atm, self%a2_atm, self%alpha_atm

  IF (ABS(self%s9_atm) < EPSILON(1.0_wp)) RETURN ! No ATM term if s9 is zero

  CALL get_atm_dispersion(mol, trans, cutoff, &
                          self%s9_atm, self%a1_atm, self%a2_atm, self%alpha_atm, &
                          r4r2_vals, c6_coeffs, &
                          dc6dcn, dc6dq,       ! These would be C6 derivatives
                          energy3b, dEdcn3b, dEdq3b, grad3b, sigma3b)
  ! Note: dc6dcn and dc6dq are derivatives of C6, not C9.
  ! The ATM routine calculates derivatives w.r.t C9 based on these C6 derivatives.

END SUBROUTINE some_concrete_damping_get_dispersion3
```

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io`:** Provides `structure_type` for defining molecular structures.
*   **`dftd4_damping` module (specifically a concrete implementation of `damping_param`):** The routines in `dftd4_damping_atm.f90` are designed to be invoked by a method of a concrete class that extends `damping_param`. For example, `rational_damping_param%get_dispersion3` might call `get_atm_dispersion` if three-body ATM interactions are enabled for that parameterization.
*   **Dispersion calculation modules (e.g., `dftd4_disp`):** The main dispersion calculation routines orchestrate calls to the two-body and three-body (if active) components of the chosen `damping_param` object.
*   **OpenMP:** The computationally intensive loops are parallelized using OpenMP directives for shared-memory parallelism.
