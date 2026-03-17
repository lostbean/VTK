# vtk

A Haskell library for generating VTK (Visualization Toolkit) files in XML format, suitable for 3D scientific visualization with tools such as [ParaView](http://www.paraview.org/).

## Overview

The `vtk` package provides a type-safe, purely functional interface for constructing VTK XML files from Haskell data. It supports multiple VTK dataset types, both ASCII and binary (Base64-encoded) data formats, and an extensible attribute system for attaching arbitrary point and cell data. The generated files conform to the VTK XML specification documented at <http://www.vtk.org/VTK/img/file-formats.pdf>.

## Supported VTK Dataset Types

| Dataset | VTK XML Type | File Extension | Constructor | Description |
|---|---|---|---|---|
| Unstructured Grid | `UnstructuredGrid` | `.vtu` | `mkUGVTK` | Arbitrary topology; user supplies points and cells |
| Rectilinear Grid | `RectilinearGrid` | `.vtr` | `mkRLGVTK` | Axis-aligned grid with independently spaced coordinates per axis |
| Structured Grid | `StructuredGrid` | `.vts` | `mkSGVTK` | Topologically regular grid with explicit point positions (rendering not yet implemented) |
| Structured Points (ImageData) | `ImageData` | `.vti` | `mkSPVTK` | Regular grid defined by origin, spacing, and dimensions |

## Modules

### `Data.VTK` (public API)

The sole exposed module. Re-exports everything a consumer needs:

- **Dataset constructors**: `mkUGVTK`, `mkRLGVTK`, `mkSGVTK`, `mkSPVTK`
- **File writers**: `writeUniVTKfile`, `writeMultiVTKfile`
- **Attribute builders**: `mkCellAttr`, `mkPointAttr`, `mkPointValueAttr`
- **Attribute combinators**: `addPointAttr`, `addPointValueAttr`, `addCellAttr`
- **Type classes and types**: `CellType`, `RenderCell`, `RenderPoint`, `RenderElemVTK`, `VTKNumType`
- **Internal helper class**: `FastFoldable` -- a `Foldable`-like class that also works with unboxed vectors; instances for `[]`, `Vector` (boxed and unboxed), and `IntMap`
- **Built-in `RenderCell` instances**: `(Int, Int, Int)` (triangle), `(Int, Int)` (line), `Int` (vertex)

### `Data.VTK.Types` (internal)

Core data types, type classes, and rendering primitives:

- **`VTK a`** -- top-level record holding a dataset name, a `VTKDataSet`, and lists of point/cell attributes.
- **`VTKDataSet a`** -- sum type with four constructors: `StructPoint`, `StructGrid`, `RectLinGrid`, `UnstructGrid`.
- **`CellType`** -- enumeration of 19 VTK cell types (vertex, line, triangle, quad, tetra, hexahedron, wedge, pyramid, voxel, pixel, polygon, poly-vertex, poly-line, triangle strip, and their quadratic variants). Each maps to its standard VTK integer ID via `evalCellType`.
- **`VTKNumType`** -- numeric type tags (`VTK_UInt8`, `VTK_UInt`, `VTK_Int`, `VTK_Float`) that map to VTK type strings (`UInt8`, `UInt32`, `Int32`, `Float64`).
- **`RenderPoint`** -- type class for values that can be serialized to ASCII or binary (little-endian) format. Instances: `Double`, `Int`, `Word32`, `Word8`, and triples `(a, a, a)` where `a` is a `RenderPoint`.
- **`RenderElemVTK`** -- constrains a type to be both `RenderPoint` and `Unbox`. Instances: `Word8`, `Word32`, `Int`, `Double`, `(a, a, a)`.
- **`RenderCell`** -- type class for cell shapes that can produce a connectivity vector and a `CellType`.
- **Attribute wrappers** (existentially quantified):
  - `VTKAttrPointValue a` -- attribute computed from `(index, point_value)`.
  - `VTKAttrPoint a` -- attribute computed from `(index)` alone.
  - `VTKAttrCell a` -- attribute computed from `(cell_index, cell_points, cell_type)`.

### `Data.VTK.VTKXMLTemplate` (internal)

XML rendering engine built on the `xmlgen` library. Exports two functions:

- **`renderVTKUni`** -- renders a single `VTK a` value to an `Xml Doc`.
- **`renderVTKMulti`** -- renders a `Vector (VTK a)` (multi-piece dataset) to an `Xml Doc`.

Internally constructs VTK XML elements (`VTKFile`, `Piece`, `Points`, `Cells`, `PointData`, `CellData`, `Coordinates`, `DataArray`) with the correct attributes (`type`, `Name`, `format`, `NumberOfComponents`, `Extent`, `WholeExtent`, `Origin`, `Spacing`).

## Key Data Types

```haskell
-- The top-level VTK record
data VTK a = VTK
    { name           :: Text
    , dataSet        :: VTKDataSet a
    , pointValueData :: [VTKAttrPointValue a]
    , pointData      :: [VTKAttrPoint a]
    , cellData       :: [VTKAttrCell a]
    }

-- The four dataset kinds
data VTKDataSet a
    = StructPoint  { dimSP :: (Int,Int,Int), originSP :: (Double,Double,Double), spaceSP :: (Double,Double,Double) }
    | StructGrid   { dimSG :: (Int,Int,Int) }
    | RectLinGrid  { setxRG :: Vector Double, setyRG :: Vector Double, setzRG :: Vector Double }
    | UnstructGrid { setUG :: Vector a, cellUG :: Vector Int, cellOffUG :: Vector Int, cellTypeUG :: V.Vector CellType }

-- 19 VTK cell types
data CellType
    = VTK_VERTEX | VTK_POLY_VERTEX | VTK_LINE | VTK_POLY_LINE
    | VTK_TRIANGLE | VTK_TRIANGLE_STRIP | VTK_POLYGON
    | VTK_PIXEL | VTK_QUAD | VTK_TETRA | VTK_VOXEL
    | VTK_HEXAHEDRON | VTK_WEDGE | VTK_PYRAMID
    | VTK_QUADRATIC_EDGE | VTK_QUADRATIC_TRIANGLE | VTK_QUADRATIC_QUAD
    | VTK_QUADRATIC_TETRA | VTK_QUADRATIC_HEXAHEDRON
```

## Encoding Formats

The library supports two encoding formats, selected by a `Bool` parameter passed to `writeUniVTKfile` and `writeMultiVTKfile`:

- **ASCII** (`False`) -- Human-readable; each value is rendered as decimal text separated by spaces. Doubles are formatted to 4 decimal places via `renderF 4`. Suitable for debugging and small datasets.
- **Binary** (`True`) -- Each `DataArray` element is encoded as Base64 (via `base64-bytestring`). Numeric values are written in little-endian byte order (`doubleLE`, `int32LE`, `word32LE`, `word8`). A 4-byte big-endian `Word32` size header is prepended (also Base64-encoded) before the payload. This produces significantly smaller files and faster load times in ParaView.

## Usage Examples

### Creating and writing an Unstructured Grid

```haskell
import qualified Data.Vector.Unboxed as U
import Data.VTK

main :: IO ()
main = do
    let points = U.fromList [(0,0,0), (1,0,0), (0,1,0), (1,1,0)] :: U.Vector (Double, Double, Double)
        cells  = [(0,1,2), (1,3,2)] :: [(Int, Int, Int)]  -- two triangles
        vtk    = mkUGVTK "triangle_mesh" points cells [] []
    writeUniVTKfile "mesh.vtu" False vtk   -- ASCII
    writeUniVTKfile "mesh_bin.vtu" True vtk -- Binary
```

### Adding point and cell attributes

```haskell
import qualified Data.Vector.Unboxed as U
import Data.VTK

main :: IO ()
main = do
    let points = U.fromList [(0,0,0), (1,0,0), (0,1,0)] :: U.Vector (Double, Double, Double)
        cells  = [(0,1,2)] :: [(Int, Int, Int)]

        -- Point-value attribute: temperature at each point, computed from index and point value
        tempAttr = mkPointValueAttr "temperature" (\i (_x,_y,_z) -> fromIntegral i :: Double)

        -- Cell attribute: area indicator per cell
        areaAttr = mkCellAttr "area" (\i _pts _cellType -> (1.0 :: Double))

        vtk = mkUGVTK "attributed_mesh" points cells [tempAttr] [areaAttr]
    writeUniVTKfile "attributed.vtu" True vtk
```

### Adding index-only point attributes (for structured datasets)

```haskell
import qualified Data.Vector.Unboxed as U
import Data.VTK

main :: IO ()
main = do
    let pAttr = mkPointAttr "density" (\i -> fromIntegral i :: Double)
        vtk   = mkSPVTK "volume" (10, 10, 10) (0.0, 0.0, 0.0) (1.0, 1.0, 1.0) [pAttr]
    writeUniVTKfile "volume.vti" True vtk
```

### Writing a multi-piece file

```haskell
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.VTK

main :: IO ()
main = do
    let piece1 = mkUGVTK "piece1" pts1 cells1 [] []
        piece2 = mkUGVTK "piece2" pts2 cells2 [] []
    writeMultiVTKfile "multi.vtu" True (V.fromList [piece1, piece2])
```

### Creating a Rectilinear Grid

```haskell
import qualified Data.Vector.Unboxed as U
import Data.VTK

main :: IO ()
main = do
    let xCoords = U.fromList [0.0, 1.0, 3.0, 6.0] :: U.Vector Double
        yCoords = U.fromList [0.0, 2.0, 4.0]       :: U.Vector Double
        zCoords = U.fromList [0.0, 1.0]             :: U.Vector Double
        pAttr   = mkPointAttr "field" (\i -> fromIntegral i :: Double)
        vtk     = mkRLGVTK "rect_grid" xCoords yCoords zCoords [pAttr]
    writeUniVTKfile "grid.vtr" True vtk
```

## Supported Cell Types

| Constructor | VTK ID | Description |
|---|---|---|
| `VTK_VERTEX` | 1 | Single point |
| `VTK_POLY_VERTEX` | 2 | Multiple disconnected points |
| `VTK_LINE` | 3 | Line segment (2 points) |
| `VTK_POLY_LINE` | 4 | Connected line segments |
| `VTK_TRIANGLE` | 5 | Triangle (3 points) |
| `VTK_TRIANGLE_STRIP` | 6 | Strip of triangles |
| `VTK_POLYGON` | 7 | General polygon |
| `VTK_PIXEL` | 8 | Axis-aligned quad |
| `VTK_QUAD` | 9 | General quadrilateral (4 points) |
| `VTK_TETRA` | 10 | Tetrahedron (4 points) |
| `VTK_VOXEL` | 11 | Axis-aligned hexahedron |
| `VTK_HEXAHEDRON` | 12 | General hexahedron (8 points) |
| `VTK_WEDGE` | 13 | Wedge / triangular prism (6 points) |
| `VTK_PYRAMID` | 14 | Pyramid (5 points) |
| `VTK_QUADRATIC_EDGE` | 21 | Quadratic edge (3 points) |
| `VTK_QUADRATIC_TRIANGLE` | 22 | Quadratic triangle (6 points) |
| `VTK_QUADRATIC_QUAD` | 23 | Quadratic quad (8 points) |
| `VTK_QUADRATIC_TETRA` | 24 | Quadratic tetrahedron (10 points) |
| `VTK_QUADRATIC_HEXAHEDRON` | 25 | Quadratic hexahedron (20 points) |

Custom cell types can be added by writing a `RenderCell` instance that provides `makeCell` (returning a connectivity `Vector Int`) and `getType` (returning a `CellType`).

## Dependencies

| Package | Purpose |
|---|---|
| `base` (>=4, <5) | Standard library |
| `base64-bytestring` | Base64 encoding for binary VTK data |
| `blaze-builder` | Efficient bytestring construction |
| `bytestring` | Lazy and strict bytestrings for I/O |
| `containers` | `IntMap` used as a cell container |
| `text` | `Text` type for names, XML attributes |
| `vector` | Boxed and unboxed vectors for points, cells, attributes |
| `xmlgen` | VTK XML document generation |

## Building

### With Cabal

```bash
cabal build
```

### With Nix

A `flake.nix` is provided with a development shell:

```bash
nix develop
cabal build
```

The dev shell includes GHC, `cabal-install`, `haskell-language-server`, `fourmolu` (formatter), and `cabal-fmt`.

## Limitations

- **Structured Grid rendering** (`mkSGVTK` / `StructGrid`) is defined at the type level but calling `renderSG` will throw a runtime error -- it is not yet implemented.
- The library targets the VTK XML format only; legacy VTK format is not supported.
- No parser/reader is provided; this is a write-only library.

## Author

Edgar Gomes de Araujo (<talktoedgar@gmail.com>)

## License

MIT -- see [LICENSE](./LICENSE).
