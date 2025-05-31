# model.f90

## Overview

The `dftd4_model` module serves as a convenient aggregator for various dispersion model components within the DFT-D4 library. It re-exports the abstract base type `dispersion_model` (defined in `dftd4_model_type.f90`) and concrete dispersion model implementations, such as the `d4_model` (from `dftd4_model_d4.f90`) and `d4s_model` (from `dftd4_model_d4s.f90`).

This module simplifies access to different dispersion models by providing a unified point of import for higher-level modules like `dftd4_disp`.

## Key Components

This module re-exports the following key components from other modules:

*   **From `dftd4_model_type.f90`:**
    *   `dispersion_model` (Abstract Derived Type): The fundamental abstract base type for all dispersion models in DFT-D4. It defines the common interface (deferred procedures) that specific models must implement. These typically include methods for calculating C6 coefficients, atomic polarizabilities, and weighting reference data.
    *   `d4_ref` (Derived Type): Likely a type to store reference data used within the D4 model.

*   **From `dftd4_model_d4.f90`:**
    *   `d4_model` (Derived Type): A concrete implementation extending `dispersion_model`, representing the D4 dispersion model.
    *   `new_d4_model`: A constructor subroutine to create an instance of `d4_model`.

*   **From `dftd4_model_d4s.f90`:**
    *   `d4s_model` (Derived Type): A concrete implementation extending `dispersion_model`, representing the D4S (scaled D4) dispersion model.
    *   `new_d4s_model`: A constructor subroutine to create an instance of `d4s_model`.

## Important Variables/Constants

This module itself does not define new variables or constants. Any such elements are defined within the modules it re-exports (e.g., parameters specific to the D4 model would be part of the `d4_model` type definition in `dftd4_model_d4.f90`).

## Usage Examples

Modules requiring access to dispersion models can `USE dftd4_model` to get access to both the abstract type and concrete implementations.

```fortran
MODULE dftd4_calculation_example
  USE dftd4_model ! Provides access to dispersion_model, d4_model, new_d4_model etc.
  USE mctc_io, ONLY : structure_type
  IMPLICIT NONE

  SUBROUTINE perform_d4_calculation(mol)
    TYPE(structure_type), INTENT(IN) :: mol
    CLASS(dispersion_model), POINTER :: model_ptr
    TYPE(d4_model), TARGET :: specific_d4_model ! Or use CLASS(d4_model), ALLOCATABLE

    ! Create an instance of the D4 model
    CALL new_d4_model(mol, specific_d4_model) ! Simplified call
    model_ptr => specific_d4_model

    ! Now model_ptr can be used polymorphically by routines expecting
    ! a CLASS(dispersion_model) object, for example, in dftd4_disp.
    ! CALL some_dispersion_routine(mol, model_ptr, ...)

    ! Clean up if allocated
    ! IF (ALLOCATED(model_ptr)) DEALLOCATE(model_ptr) ! If model_ptr was allocatable
    ! Or if specific_d4_model was allocatable and model_ptr pointed to it.
  END SUBROUTINE perform_d4_calculation

END MODULE dftd4_calculation_example
```

## Dependencies and Interactions

*   **`dftd4_model_type`:** This module provides the core abstract `dispersion_model` type and `d4_ref` type. `dftd4_model` depends on it to re-export these fundamental definitions.
*   **`dftd4_model_d4`:** Provides the concrete D4 model implementation (`d4_model`) and its constructor.
*   **`dftd4_model_d4s`:** Provides the concrete D4S model implementation (`d4s_model`) and its constructor.
*   **`dftd4_disp`:** This is a primary consumer of the types provided by `dftd4_model`. It takes a `CLASS(dispersion_model)` object to perform the actual dispersion energy and derivative calculations.

The `dftd4_model` module acts as a façade, simplifying the use of different dispersion models by centralizing their availability.
