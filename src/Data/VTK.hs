{-# LANGUAGE
    ExistentialQuantification
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , NamedFieldPuns
  , OverloadedStrings
  , TypeSynonymInstances
  #-}

-- | This module renders VTK files in XML format with ASCII and binary data. VTK file
-- format is used to visualize scientific data in three (less usually in two) dimensions.
-- One of the more important viewers for VTK files is Paraview <http://www.paraview.org/>.
-- More information about the VTK format can be found here:
-- <http://www.vtk.org/VTK/img/file-formats.pdf>
module Data.VTK
  ( VTK (..)
    -- * Write to file
  , writeUniVTKfile
  , writeMultiVTKfile
    -- * Make VTK
  , mkUGVTK
  , mkRLGVTK
  , mkSGVTK
  , mkSPVTK
    -- * Add attributes
  , mkCellAttr
  , mkPointAttr
  , mkPointValueAttr
  , addPointAttr
  , addPointValueAttr
  , addCellAttr
    -- * Classes for new attributes
  , CellType    (..)
  , RenderCell  (..)
  , RenderPoint (..)
  , RenderElemVTK
  , VTKAttrPointValue
  , VTKAttrPoint
  , VTKAttrCell
  , VTKNumType (..)
  ) where

import Data.IntMap (IntMap)
import Data.Text (pack)
import Data.Vector.Unboxed (Vector)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap          as IM
import qualified Data.List            as L
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U
import qualified Text.XML.Generator   as X

import Data.VTK.Types
import Data.VTK.VTKXMLTemplate

-- =======================================================================================

-- | Write an unitary piece VTK file to disk. Set 'True' for binary format.
writeUniVTKfile :: (RenderElemVTK a) => FilePath -> Bool -> VTK a -> IO ()
writeUniVTKfile name isBinary = BSL.writeFile name . X.xrender . renderVTKUni isBinary

-- | Write an multi-piece piece VTK file to disk. Set 'True' for binary format.
writeMultiVTKfile :: (RenderElemVTK a) => FilePath -> Bool -> V.Vector (VTK a) -> IO ()
writeMultiVTKfile name isBinary = BSL.writeFile name . X.xrender . renderVTKMulti isBinary

-- | Creates an Unstructured Grid dataset. Use:
--
-- > mkUGVTK "name" [Vec3 0 0 0, Vec3 0 1 0] [(0,1)]
--
mkUGVTK :: (RenderElemVTK p, RenderCell cell, VTKFoldable t cell)
        => String -> Vector p -> t cell -> [VTKAttrPointValue p] -> [VTKAttrCell p] -> VTK p
mkUGVTK name points cells pointData cellData = let
  nameTxt = pack name
  dataset = mkUnstructGrid points cells
  in VTK nameTxt dataset pointData [] cellData

-- | Creates an Rectilinear Grid dataset. Use:
--
-- > mkRLGVTK "name" [0,1,2] [0,1,2] [(0 0 0), (0 1 0)]
--
mkRLGVTK :: (RenderElemVTK a)
         => String -> Vector Double -> Vector Double -> Vector Double -> [VTKAttrPoint a] -> VTK a
mkRLGVTK name px py pz pointData = let
  nameTxt = pack name
  dataset = mkRectLinGrid px py pz
  in VTK nameTxt dataset [] pointData []

-- | Creates an Structured Grid dataset. Use:
--
-- > mkSGVTK "name" 2 1 1 ([(0 0 0), (0 1 0)])
--
mkSGVTK :: (RenderElemVTK a) => String -> Int -> Int -> Int -> [VTKAttrPoint a] -> VTK a
mkSGVTK name nx ny nz pointData = let
  nameTxt = pack name
  dataset = StructGrid { dimSG = (nx, ny, nz) }
  in VTK nameTxt dataset [] pointData []

-- | Creates an Structured Points dataset (ImageData). Use:
--
-- > mkSGVTK "name" (2, 1, 1) (0.0, 0.0, 0.0) (5.0, 5.0, 5.0) [(0 0 0), (0 1 0)]
--
mkSPVTK :: (RenderElemVTK a)
        => String -> (Int, Int, Int) -> (Double, Double, Double)
        -> (Double, Double, Double) -> [VTKAttrPoint a] -> VTK a
mkSPVTK name (dx, dy, dz) orig spc pointData = let
  nameTxt = pack name
  dataset = StructPoint
          { dimSP    = (dx-1, dy-1, dz-1)
          , originSP = orig
          , spaceSP  = spc
          }
  in VTK nameTxt dataset [] pointData []

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function
-- 'VTKAttPoint'.
--
-- > let attr = mkPointsAttr "grainID" (\i x -> x * grainIDTable!i)
-- > addPointValueAttr vtk
--
addPointValueAttr :: (RenderElemVTK a) => VTK a -> VTKAttrPointValue a -> VTK a
addPointValueAttr vtk attr = vtk { pointValueData = attr : pointValueData vtk }

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function
-- 'VTKAttPoint'.
--
-- > let attr = mkPointsAttr "grainID" (grainIDTable !)
-- > addPointAttr vtk
--
addPointAttr :: (RenderElemVTK a) => VTK a -> VTKAttrPoint a -> VTK a
addPointAttr vtk attr = vtk { pointData = attr : pointData vtk }

-- | Adds data to all points in 'VTK'. Internally, it pass the data as a function
-- 'VTKAttPoint'.
--
-- > let attr = mkCellAttr "color" (\i x cellType -> (Vec3 1 1 1) &* 1/(evalCellType cellType))
-- > addCellAttr vtk attr
--
addCellAttr :: (RenderElemVTK a) => VTK a -> VTKAttrCell a -> VTK a
addCellAttr vtk attr = vtk { cellData = attr : cellData vtk }

-- ================================ Internal stuffs ======================================

type CellBuilder = ([Vector Int], [Int], [CellType], Int)

addCell :: (RenderCell cell) => CellBuilder -> cell -> CellBuilder
addCell set@(cellUG, cellOffUG, cellTypeUG, offCount) obj
  | cell_size <= 0 = set
  | otherwise = (cell : cellUG, cell_off : cellOffUG, cell_type : cellTypeUG , cell_off)
  where
    cell       = makeCell obj
    cell_off   = offCount + cell_size
    cell_size  = U.length cell
    cell_type  = getType obj

mkUnstructGrid :: (RenderElemVTK p, RenderCell cell, VTKFoldable t cell)
               => Vector p -> t cell -> VTKDataSet p
mkUnstructGrid points cells = let
  i = ([], [], [], 0)
  (cell, cell_off, cell_type, _) = foldr_ addCell i cells
  in UnstructGrid
     { setUG      = points
     , cellUG     = U.concat cell
     , cellOffUG  = U.reverse $ U.fromList cell_off
     , cellTypeUG = V.reverse $ V.fromList cell_type
     }

mkRectLinGrid :: (RenderElemVTK a)
              => Vector Double -> Vector Double ->  Vector Double -> VTKDataSet a
mkRectLinGrid x y z = RectLinGrid { setxRG = x, setyRG = y, setzRG = z }

-- ================================ Custom Foldable ======================================

-- | Special foldable class since `Foldable` doesn't support unboxed vectors
class VTKFoldable t b where
  foldr_ :: (a -> b -> a) -> a -> t b -> a

instance VTKFoldable [] a where
  foldr_ func = L.foldr (flip func)

instance (U.Unbox a) => VTKFoldable Vector a where
  foldr_ func = U.foldr' (flip func)

instance VTKFoldable V.Vector a where
  foldr_ func = V.foldr' (flip func)

instance VTKFoldable IntMap a where
  foldr_ func = IM.fold (flip func)

-- ================================ Basic instances ======================================

instance RenderCell (Int, Int, Int) where
  makeCell (a,b,c) = U.fromList [a,b,c]
  getType _        = VTK_TRIANGLE

instance RenderCell (Int, Int) where
  makeCell (a,b) = U.fromList [a,b]
  getType _      = VTK_LINE

instance RenderCell Int where
  makeCell a = U.singleton a
  getType _  = VTK_VERTEX
