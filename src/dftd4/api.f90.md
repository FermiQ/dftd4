# api.f90

## Overview

This file defines the module `dftd4_api`, which exposes a C-compatible Application Programming Interface (API) for the dftd4 library. This allows C and other languages (like Python via ctypes/cffi) to interact with the Fortran-based dftd4 functionalities. The API includes routines for managing dftd4 objects (errors, structures, models, parameters), calculating dispersion energies, gradients, Hessians, and other properties.

The C header file for this API is typically found at `include/dftd4.h` in the dftd4 distribution.

## Key Components

This module provides a set of `bind(C)` functions and subroutines, effectively creating C-callable wrappers around core dftd4 functionalities. Key API routines include:

*   **`dftd4_get_version()`:** (Function) Returns the library version as an integer.
*   **Error Handling:**
    *   `dftd4_new_error()`: (Function) Creates a new error handle.
    *   `dftd4_delete_error()`: (Subroutine) Deletes an error handle.
    *   `dftd4_check_error()`: (Function) Checks the status of an error handle.
    *   `dftd4_get_error()`: (Subroutine) Retrieves the error message from an error handle.
*   **Structure Management:**
    *   `dftd4_new_structure()`: (Function) Creates a new molecular structure object from atomic numbers, positions, and optional charge/lattice/periodicity.
    *   `dftd4_delete_structure()`: (Subroutine) Deletes a molecular structure object.
    *   `dftd4_update_structure()`: (Subroutine) Updates coordinates and lattice parameters of an existing structure.
*   **Dispersion Model Management:**
    *   `dftd4_new_d4_model()`: (Function) Creates a new D4 dispersion model.
    *   `dftd4_new_d4s_model()`: (Function) Creates a new D4s dispersion model.
    *   `dftd4_custom_d4_model()`: (Function) Creates a custom D4 dispersion model with specified parameters.
    *   `dftd4_custom_d4s_model()`: (Function) Creates a custom D4s dispersion model with specified parameters.
    *   `dftd4_delete_model()`: (Subroutine) Deletes a dispersion model object.
*   **Damping Parameter Management:**
    *   `dftd4_new_rational_damping()`: (Function) Creates new rational damping parameters.
    *   `dftd4_load_rational_damping()`: (Function) Loads rational damping parameters for a given functional name.
    *   `dftd4_delete_param()`: (Subroutine) Deletes damping parameter objects.
*   **Calculation Routines:**
    *   `dftd4_get_dispersion()`: (Subroutine) Calculates the dispersion energy, and optionally the gradient and stress tensor components.
    *   `dftd4_get_numerical_hessian()`: (Subroutine) Calculates the dispersion Hessian numerically.
    *   `dftd4_get_pairwise_dispersion()`: (Subroutine) Calculates the pairwise representation of the dispersion energy.
    *   `dftd4_get_properties()`: (Subroutine) Calculates various atomic properties like coordination numbers (CN), atomic charges, C6 coefficients, and atomic polarizabilities.

## Important Variables/Constants

The API uses opaque C pointers (Fortran `type(c_ptr)`) to manage handles to internal Fortran derived types. These are abstracted away from the C user.

*   **`vp_error` (type):** Fortran derived type acting as a container for an `error_type` object, passed as an opaque pointer to C.
*   **`vp_structure` (type):** Fortran derived type acting as a container for a `structure_type` object, passed as an opaque pointer to C.
*   **`vp_model` (type):** Fortran derived type acting as a container for a `dispersion_model` class instance, passed as an opaque pointer to C.
*   **`vp_param` (type):** Fortran derived type acting as a container for a `damping_param` class instance, passed as an opaque pointer to C.
*   **`namespace` (parameter):** A character parameter `dftd4_` used to prefix all C API function names.

## Usage Examples

A typical workflow using the C API (conceptually, from a C perspective) would involve:

1.  Creating an error handle: `err_handle = dftd4_new_error();`
2.  Creating a structure: `struct_handle = dftd4_new_structure(err_handle, natoms, numbers, positions, ...);`
3.  Creating a model: `model_handle = dftd4_new_d4_model(err_handle, struct_handle);`
4.  Loading/creating damping parameters: `param_handle = dftd4_load_rational_damping(err_handle, "pbe", .false.);`
5.  Calculating dispersion energy: `dftd4_get_dispersion(err_handle, struct_handle, model_handle, param_handle, &energy, gradient, sigma);`
6.  Checking for errors using `dftd4_check_error()` and retrieving messages with `dftd4_get_error()` if necessary.
7.  Cleaning up: `dftd4_delete_param(param_handle); dftd4_delete_model(model_handle); dftd4_delete_structure(struct_handle); dftd4_delete_error(err_handle);`

```fortran
! Fortran perspective: The module defines 'bind(C)' procedures.
! Example: Exposing a Fortran subroutine to C
!
! subroutine my_fortran_sub(arg1, arg2) bind(C, name="my_c_callable_sub")
!   use iso_c_binding
!   integer(c_int), intent(in) :: arg1
!   real(c_double), intent(out) :: arg2
!   ! ... implementation ...
! end subroutine my_fortran_sub
```

## Dependencies and Interactions

This API module serves as a bridge between the Fortran core of dftd4 and external C-compatible codes.

*   **Internal Dependencies:**
    *   `iso_c_binding`: Standard Fortran module for C interoperability.
    *   `mctc_env`: For error handling (`error_type`, `fatal_error`) and precision (`wp`).
    *   `mctc_io_structure`: For molecular structure definitions (`structure_type`).
    *   `dftd4_cutoff`: For real-space cutoff calculations.
    *   `dftd4_damping`, `dftd4_damping_rational`: For damping function parameters.
    *   `dftd4_disp`: For dispersion energy, gradient, and property calculations.
    *   `dftd4_model`, `dftd4_model_d4`, `dftd4_model_d4s`: For dispersion model definitions.
    *   `dftd4_numdiff`: For numerical Hessian calculations.
    *   `dftd4_param`: For accessing stored damping parameters.
    *   `dftd4_utils`: For utility functions like `wrap_to_central_cell`.
    *   `dftd4_version`: For retrieving library version information.
*   **External Interactions:**
    *   **C/C++ Codes:** Directly call these API functions after linking against the dftd4 library.
    *   **Python Interface:** The official `dftd4` Python bindings likely use `ctypes` or `cffi` to call the functions exposed in this C API.
    *   Other languages capable of C interop can also use this API.

The API ensures that data is correctly marshalled between Fortran and C types (e.g., `integer(c_int)`, `real(c_double)`, `character(kind=c_char)`).
Memory management for the opaque handles is done on the Fortran side, with C code responsible for calling the respective `delete_*` routines.
