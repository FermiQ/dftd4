# rational.f90

## Overview

This file, `dftd4_damping_rational.f90`, implements the rational damping function for the DFT-D4 dispersion model. This damping scheme is also commonly referred to as Becke-Johnson (BJ) damping. It provides a concrete implementation of the abstract `damping_param` type defined in `dftd4_damping.f90`. The rational damping function is applied to both the R⁻⁶ and R⁻⁸ two-body dispersion terms. This module also handles the three-body Axilrod-Teller-Muto (ATM) term by calling routines from `dftd4_damping_atm.f90`, using parameters consistent with the rational damping scheme.

## Key Components

*   **`rational_damping_param` (type):**
    *   A derived type that extends `damping_param`.
    *   It holds the specific parameters for the rational damping function.
    *   It overrides the deferred procedures from `damping_param` to provide concrete implementations:
        *   `get_dispersion2`: Calculates the two-body dispersion energy (C6 and C8 terms) and its derivatives using rational damping.
        *   `get_dispersion3`: Calculates the three-body ATM dispersion energy and its derivatives, using the ATM-specific damping but with `a1`, `a2`, and `alp` parameters from this rational model.
        *   `get_pairwise_dispersion2`: Calculates the pairwise representation of the two-body dispersion energy.
        *   `get_pairwise_dispersion3`: Calculates the pairwise representation of the three-body ATM dispersion energy.
*   **`get_dispersion2(...)`:** (Subroutine, bound to `rational_damping_param`)
    *   Dispatcher routine that calls either `get_dispersion_energy` (for energy only) or `get_dispersion_derivs` (for energy and derivatives) for the two-body terms.
*   **`get_dispersion_energy(...)`:** (Subroutine)
    *   Calculates the two-body dispersion energy (C6 and C8 terms) using the rational damping formula:
        `E_disp = -s6 * C6 / (R⁶ + r0⁶) - s8 * C8 / (R⁸ + r0⁸)` where `r0 = a1 * sqrt(3*<r²>_A*<r²>_B) + a2` and `C8` is related to `C6 * 3*<r²>_A*<r²>_B`.
*   **`get_dispersion_derivs(...)`:** (Subroutine)
    *   Calculates the two-body dispersion energy and its analytical derivatives with respect to atomic positions (gradient), cell parameters (virial), coordination numbers (dEdcn), and partial charges (dEdq).
*   **`get_dispersion3(...)`:** (Subroutine, bound to `rational_damping_param`)
    *   Calls `get_atm_dispersion` from the `dftd4_damping_atm` module to compute the three-body ATM contribution. It passes its own `s9`, `a1`, `a2`, and `alp` parameters to the ATM calculation.
*   **`get_pairwise_dispersion2(...)`:** (Subroutine, bound to `rational_damping_param`)
    *   Calculates the pairwise decomposition of the two-body dispersion energy.
*   **`get_pairwise_dispersion3(...)`:** (Subroutine, bound to `rational_damping_param`)
    *   Calculates the pairwise decomposition of the three-body ATM dispersion energy.
*   **`triple_scale(ii, jj, kk)`:** (Elemental Function, also present in `atm.f90`)
    *   Helper function to correctly scale triplet contributions when atoms are not unique.

## Important Variables/Constants

The `rational_damping_param` type stores the following key parameters:

*   **`s6` (real(wp)):** Scaling factor for the R⁻⁶ dispersion term. Default is `1.0_wp`.
*   **`s8` (real(wp)):** Scaling factor for the R⁻⁸ dispersion term.
*   **`s9` (real(wp)):** Scaling factor for the C9 coefficient in the ATM three-body term. Default is `1.0_wp`. If zero, the ATM term is effectively disabled.
*   **`a1` (real(wp)):** Parameter controlling the steepness of the damping function (related to the critical radius `r0`).
*   **`a2` (real(wp)):** Parameter controlling the offset of the damping function (related to the critical radius `r0`).
*   **`alp` (real(wp)):** Exponent used in the damping function for the ATM term. Default is `16.0_wp`.

The values for `s6`, `s8`, `a1`, and `a2` are typically optimized for specific DFT functionals and are loaded via the `dftd4_param` module.

## Usage Examples

An instance of `rational_damping_param` is created and populated with parameters, often loaded based on a DFT functional name. This object is then used by the main dispersion calculation routines.

```fortran
USE dftd4_damping_rational
USE dftd4_param, ONLY : get_rational_damping_parameters ! Hypothetical
USE mctc_env, ONLY : wp
USE mctc_io, ONLY : structure_type

IMPLICIT NONE

TYPE(rational_damping_param) :: bj_params
TYPE(structure_type) :: mol
! ... (initialize mol structure) ...

! Load parameters for a specific functional, e.g., "PBE"
! This is a conceptual call; actual parameter loading might differ.
! CALL get_rational_damping_parameters("pbe", bj_params%s6, bj_params%s8, &
!                                      bj_params%a1, bj_params%a2)
! If ATM is desired, s9 might also be set, or default to 1.0.
! bj_params%s9 = 1.0 ! (or could be part of loaded params)


! This bj_params object would then be passed to higher-level dispersion
! calculation routines, which would polymorphically call its bound procedures
! (get_dispersion2, get_dispersion3, etc.).

! CLASS(damping_param), POINTER :: general_damping_ptr
! general_damping_ptr => bj_params
! CALL general_damping_ptr%get_dispersion2(...)
```

## Dependencies and Interactions

*   **`dftd4_damping`:** Extends the abstract `damping_param` type from this module.
*   **`dftd4_damping_atm`:** Uses `get_atm_dispersion` from this module to calculate the three-body ATM term.
*   **`dftd4_data`:** Uses `get_r4r2_val` (implicitly, as `r4r2` is an input to its methods) for calculating terms dependent on atomic <r⁴>/<r²> values. (Corrected: `r4r2` is passed as an argument, not directly called via `get_r4r2_val` inside this module's main routines, but the values originate from `dftd4_data`).
*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **`mctc_io`:** Provides `structure_type`.
*   **`dftd4_param` (Parameter Loading Module):** This module is responsible for providing the numerical values for `s6`, `s8`, `a1`, `a2` (and potentially `s9`, `alp`) for different DFT functionals. The `rational_damping_param` objects are typically initialized with these values.
*   **Main Dispersion Calculation Routines (e.g., in `dftd4_disp`):** These routines will take a `CLASS(damping_param)` object as input. If this object is an instance of `rational_damping_param`, its implemented methods will be called polymorphically.
*   **OpenMP:** The computationally intensive loops for pairwise sums are parallelized using OpenMP.
