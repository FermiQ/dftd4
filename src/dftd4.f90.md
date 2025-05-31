# dftd4.f90

## Overview

This file defines the main module `dftd4`, which serves as an aggregator for various components of the DFT-D4 dispersion correction method. It imports and re-exports functionalities from other modules, providing a unified interface for the DFT-D4 calculations.

## Key Components

*   **dftd4 (module):** The main module that consolidates various sub-modules of the DFT-D4 method. It makes available procedures and types from these sub-modules.

## Important Variables/Constants

(Details on important variables and constants are available in the respective used modules.)

## Usage Examples

(Usage examples are best found within the documentation of the specific sub-modules or in higher-level example codes utilizing the `dftd4` module.)

```fortran
! Example of using a procedure that might be re-exported by dftd4
! (Actual usage would depend on the specific procedure and its origin module)
!
! USE dftd4
! IMPLICIT NONE
!
! ! ... variable declarations ...
!
! ! Call a DFT-D4 procedure
! CALL some_dftd4_procedure(...)
```

## Dependencies and Interactions

This module primarily acts as an interface and depends on the following modules:

*   **mctc_io:** Used for handling structure data (e.g., `structure_type`, `new`).
*   **dftd4_cutoff:** Provides functionalities related to real-space cutoffs and lattice point generation (e.g., `realspace_cutoff`, `get_lattice_points`).
*   **dftd4_disp:** Contains procedures for calculating dispersion energies and properties (e.g., `get_dispersion`, `get_properties`, `get_pairwise_dispersion`).
*   **dftd4_ncoord:** Provides functionality for calculating coordination numbers (e.g., `get_coordination_number`).
*   **dftd4_numdiff:** Used for numerical differentiation, particularly for obtaining dispersion Hessians (e.g., `get_dispersion_hessian`).
*   **dftd4_damping:** Defines damping parameters (e.g., `damping_param`).
*   **dftd4_damping_rational:** Defines rational damping parameters (e.g., `rational_damping_param`).
*   **dftd4_model:** Defines the base dispersion model (e.g., `dispersion_model`).
*   **dftd4_model_d4:** Implements the D4 dispersion model (e.g., `d4_model`, `new_d4_model`).
*   **dftd4_model_d4s:** Implements the D4s dispersion model (e.g., `d4s_model`, `new_d4s_model`).
*   **dftd4_param:** Provides access to rational damping parameters (e.g., `get_rational_damping`).
*   **dftd4_version:** Provides the DFT-D4 version information (e.g., `get_dftd4_version`).
