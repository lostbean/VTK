# vtk

A Haskell library for generating VTK (Visualization Toolkit) XML files for 3D scientific visualization.

## What is this?

VTK is a widely used file format in scientific computing for representing meshes, grids, and associated data (temperature fields, velocities, labels, etc.) for visualization in tools like [ParaView](https://www.paraview.org/). This library provides a type-safe, purely functional Haskell interface for constructing and writing VTK XML files.

This is a write-only library -- it generates VTK files from Haskell data structures but does not parse them.

## What does it support?

### Dataset types

- **Unstructured Grid** (`.vtu`) -- arbitrary topology with user-supplied points and cells. The most flexible format: can represent any mesh of triangles, tetrahedra, hexahedra, etc. This is the format used most heavily in this ecosystem.
- **Rectilinear Grid** (`.vtr`) -- axis-aligned grid with independently spaced coordinates per axis. Compact representation for logically rectangular but non-uniformly spaced grids.
- **Structured Points / ImageData** (`.vti`) -- regular grid defined by origin, spacing, and dimensions. The most compact format, for uniform grids like voxel data or regular sampling.
- **Structured Grid** (`.vts`) -- topologically regular grid with explicit point positions. Defined at the type level but rendering is not yet implemented.

### Cell types

Supports 19 standard VTK cell types: vertices, lines, triangles, quads, tetrahedra, hexahedra, wedges, pyramids, voxels, polygons, strips, and their quadratic (higher-order) variants. Custom cell types can be added by implementing the `RenderCell` type class.

### Encoding formats

- **ASCII** -- human-readable, suitable for debugging and small datasets
- **Binary** -- Base64-encoded with little-endian byte order. Significantly smaller files and faster load times in ParaView.

### Attribute system

Arbitrary data can be attached to points and cells: scalar fields, vector fields, labels, etc. Three attribute types are supported depending on whether the value depends on the point index, the point value, or the cell topology.

## Example

```haskell
import qualified Data.Vector.Unboxed as U
import Data.VTK

main :: IO ()
main = do
    let points = U.fromList [(0,0,0), (1,0,0), (0,1,0), (1,1,0)] :: U.Vector (Double, Double, Double)
        cells  = [(0,1,2), (1,3,2)] :: [(Int, Int, Int)]  -- two triangles
        tempAttr = mkPointValueAttr "temperature" (\i (_x,_y,_z) -> fromIntegral i :: Double)
        vtk    = mkUGVTK "mesh" points cells [tempAttr] []
    writeUniVTKfile "mesh.vtu" True vtk  -- True = binary encoding
```

## Where is it used?

- **hammer** -- extends this library with specialized rendering for 3D voxel grids, grain boundaries, microstructure topology, and EBSD data. Almost all VTK output in the ecosystem flows through hammer's VTK module, which builds on this library.
- The generated `.vtu` and `.vtr` files are visualized in [ParaView](https://www.paraview.org/).

## How to build

```bash
# With Nix (recommended)
nix develop
cabal build --allow-newer

# With Cabal
cabal build
```

No test suite -- VTK is integration-tested by downstream packages (primarily hammer).

## License

MIT -- see [LICENSE](./LICENSE).
