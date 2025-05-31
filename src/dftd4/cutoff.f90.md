# cutoff.f90

## Overview

The `dftd4_cutoff` module provides utilities for managing real-space cutoffs for pairwise and other interactions, which is particularly important for calculations in periodic systems. It includes routines to generate lists of lattice translation vectors based on specified cutoff radii or cell repetitions. This ensures that interactions are only computed within a defined spatial range, making calculations feasible for extended systems.

## Key Components

*   **`realspace_cutoff` (type):**
    *   A derived type that holds a collection of cutoff radii for different types of interactions.
    *   `cn`: Cutoff for coordination number calculation.
    *   `disp2`: Cutoff for two-body dispersion interactions.
    *   `disp3`: Cutoff for three-body dispersion interactions.
*   **`get_lattice_points` (interface):**
    *   This interface provides two ways to generate lattice translation vectors:
        *   **`get_lattice_points_cutoff(periodic, lat, rthr, trans)`:** (Subroutine) Generates lattice translation vectors (`trans`) based on the periodicity (`periodic`), lattice vectors (`lat`), and a real-space cutoff radius (`rthr`). It first determines the necessary repetitions using `get_translations`.
        *   **`get_lattice_points_rep_3d(lat, rep, origin, trans)`:** (Subroutine) Generates lattice translation vectors (`trans`) given the lattice vectors (`lat`), the number of repetitions in each direction (`rep`), and whether to include the origin (`origin`).
*   **`get_translations(lat, rthr, rep)`:** (Pure Subroutine)
    *   Calculates the minimum number of cell repetitions (`rep`) required in each lattice direction to encompass a sphere of radius `rthr`. This is used by `get_lattice_points_cutoff`.

## Important Variables/Constants

*   **`cn_default` (parameter):** Default cutoff radius for coordination number calculations (30.0 Bohr).
*   **`disp2_default` (parameter):** Default cutoff radius for two-body dispersion interactions (60.0 Bohr).
*   **`disp3_default` (parameter):** Default cutoff radius for three-body dispersion interactions (40.0 Bohr).
    *   These default values are used to initialize the `realspace_cutoff` type if no specific values are provided.

## Usage Examples

The `realspace_cutoff` type can be instantiated and its members (e.g., `disp2`) can be passed to dispersion calculation routines to control the range of interactions.

```fortran
USE dftd4_cutoff
USE mctc_env, ONLY : wp

IMPLICIT NONE

TYPE(realspace_cutoff) :: my_cutoffs
REAL(wp) :: lattice(3,3)
REAL(wp), ALLOCATABLE :: translations(:,:)
LOGICAL :: is_periodic(3)

! Initialize custom cutoffs
my_cutoffs%cn = 35.0_wp
my_cutoffs%disp2 = 70.0_wp
my_cutoffs%disp3 = 45.0_wp

! Example lattice vectors (e.g., cubic box of 20 Bohr)
lattice = 0.0_wp
lattice(1,1) = 20.0_wp
lattice(2,2) = 20.0_wp
lattice(3,3) = 20.0_wp

is_periodic = [.TRUE., .TRUE., .TRUE.]

! Generate lattice points based on the two-body cutoff
CALL get_lattice_points_cutoff(is_periodic, lattice, my_cutoffs%disp2, translations)

! 'translations' now holds all lattice vectors (including origin)
! within a 70.0 Bohr radius, to be used in periodic summations.
! For example, in a dispersion calculation:
! CALL calculate_dispersion(structure, my_cutoffs, translations, ...)

IF (ALLOCATED(translations)) DEALLOCATE(translations)

END
```
The choice of cutoff radii affects both the accuracy and computational cost of the calculations. Larger cutoffs generally lead to more accurate results but increase the number of pairs/triplets to consider, thus increasing computation time.

## Dependencies and Interactions

*   **`mctc_env`:** Provides `wp` for defining working precision.
*   **Dispersion Calculation Modules (e.g., `dftd4_disp`):** These modules will typically take a `realspace_cutoff` object and the generated `translations` array as input to correctly sum up interactions in non-periodic and periodic systems.
*   **Structure Data (e.g., `structure_type` from `mctc_io`):** The lattice vectors from the structure data are essential input for `get_lattice_points` and `get_translations`. The `periodic` logical array, also part of structure data, determines which dimensions are treated periodically.
*   The module uses a pure subroutine `crossproduct` internally within `get_translations` for vector cross product calculations.
