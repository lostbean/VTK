{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.VTK.VTKXMLTemplate
  ( renderVTKUni
  , renderVTKMulti
  ) where

import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Prelude                      as P

import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Data.ByteString.Base64.Lazy  (encode)
import           Data.Text                    (Text)
import           Data.Vector.Unboxed          (Vector, (!))

import           Prelude
import           Text.XML.Generator

import           Data.VTK.Types

-- =======================================================================================

renderVTKUni::(RenderElemVTK a)=> Bool -> VTK a -> Xml Doc
renderVTKUni isBinary vtk = let
  dataSetType = renderType $ dataSet vtk
  node        = [renderVTK isBinary vtk]
  in renderDoc dataSetType node

renderVTKMulti::(RenderElemVTK a)=> Bool -> V.Vector (VTK a) -> Xml Doc
renderVTKMulti isBinary vtk = let
  dataSetType = if V.null vtk
                then ("", noAttrs)
                else (renderType . dataSet .  V.head) vtk
  nodes       = V.toList $ V.map (renderVTK isBinary) vtk
  in renderDoc dataSetType nodes

renderDoc :: (Text, Xml Attr) -> [Xml Elem] -> Xml Doc
renderDoc (dataSetType, attr) node = let
  a = xelem "VTKFile" $ (xattr "type" dataSetType)
      <#> xelem dataSetType (attr <#> xelems node)
  in doc defaultDocInfo a

renderType::(RenderElemVTK a)=> VTKDataSet a -> (Text, Xml Attr)
renderType dataSet = case dataSet of
  StructPoint dim orig spc -> let
    txt = renderWholeExtAttr dim <> renderOriginAttr orig <> renderSpaceAttr spc
    in ("ImageData", txt)
  RectLinGrid x y z -> let
    ext = (U.length x, U.length y, U.length z)
    in ("RectilinearGrid",  renderWholeExtAttr ext)
  StructGrid   {} -> ("StructuredGrid",   noAttrs)
  UnstructGrid {} -> ("UnstructuredGrid", noAttrs)

-- =============================== render VTK types ======================================

renderVTK :: (RenderElemVTK a)=> Bool -> VTK a -> Xml Elem
renderVTK isBinary vtk@VTK{..} = let
  size = getVTKSize vtk
  in case dataSet of
    StructPoint dime _ _       -> renderSP isBinary dime size pointData
    StructGrid  dime           -> renderSG isBinary dime size pointData
    RectLinGrid setX setY setZ ->
      renderRG isBinary setX setY setZ size pointData
    UnstructGrid set cell cellOff cellType ->
      renderUG isBinary set cell cellOff cellType pointValueData cellData

renderSG :: a
renderSG = error "[Data] Can't render this type of VTK file. No implemented yet."

renderSP :: (RenderElemVTK a)=> Bool -> (Int, Int, Int) -> Int
            -> [VTKAttrPoint a] -> Xml Elem
renderSP isBin ext size pointData = let
  nodes = [ renderPointData isBin size pointData ]
  in xelem "Piece" $ xattr "Extent" (renderExtent ext) <#> xelems nodes

renderRG :: (RenderElemVTK a)=> Bool -> Vector Double -> Vector Double -> Vector Double
            -> Int -> [VTKAttrPoint a] -> Xml Elem
renderRG isBin setX setY setZ size pointData = let
  ext   = (U.length setX, U.length setY, U.length setZ)
  nodes = [ renderPointData   isBin size pointData
          , renderCoordinates isBin setX setY setZ ]
  in xelem "Piece" $ xattr "Extent" (renderExtent ext) <#> xelems nodes

renderUG :: (RenderElemVTK a)=> Bool -> Vector a -> Vector Int -> Vector Int
            -> V.Vector CellType -> [VTKAttrPointValue a] -> [VTKAttrCell a] -> Xml Elem
renderUG isBin set cell cellOff cellType pointData cellData = let
  numPoints = U.length set
  numCell   = U.length cellOff
  attrs = [ xattr "NumberOfPoints" (toTxt numPoints)
          , xattr "NumberOfCells"  (toTxt numCell) ]
  nodes = [ renderPointValueData isBin set pointData
          , renderCellData       isBin set cell cellOff cellType cellData
          , renderPoints         isBin set
          , renderCells          isBin cell cellOff cellType U.empty U.empty ]
  in xelem "Piece" (xattrs attrs <#> xelems nodes)

-- =============================== render tools ==========================================

renderPointData::(RenderElemVTK a)=> Bool -> Int -> [VTKAttrPoint a] -> Xml Elem
renderPointData isBin size attrs = let
  renderAttr (VTKAttrPoint name func) = let
    points = U.generate size func
    name_t = toTxt name
    in renderDataArray isBin points name_t
  in xelem "PointData" $ xelems $ P.map renderAttr attrs

renderPointValueData::(RenderElemVTK a)=> Bool -> Vector a -> [VTKAttrPointValue a] -> Xml Elem
renderPointValueData isBin pointData attrs = let
  renderAttr (VTKAttrPointValue name func) = let
    points = U.imap func pointData
    name_t = toTxt name
    in renderDataArray isBin points name_t
  in xelem "PointData" $ xelems $ P.map renderAttr attrs

renderCellData :: (RenderElemVTK a)=> Bool -> Vector a -> Vector Int -> Vector Int
               -> V.Vector CellType -> [VTKAttrCell a] -> Xml Elem
renderCellData isBin set cell cellOff cellType attrs = let
  renderAttr (VTKAttrCell name func) = let
    arr = U.imap (mode func) cellOff
    in renderDataArray isBin arr (toTxt name)
  mode func i _ = let
    sec = U.map (set!) $
          if i == 0
          then U.slice 0 (cellOff!0) cell
          else U.slice (cellOff!i-1) (cellOff!i) cell
    tp  = cellType V.! i
    in func i sec tp
  in xelem "CellData" $ xelems $ P.map renderAttr attrs

renderCells :: Bool -> Vector Int -> Vector Int -> V.Vector CellType
            -> Vector Int -> Vector Int -> Xml Elem
renderCells isBin cellConn cellOffsets cellTypes faces faceOffsets = let
  cts  = U.generate (V.length cellTypes) (evalCellType . (cellTypes V.!))
  full = if U.null faces
         then conn : off : [types]
         else conn : off : types : fac : [facoff]

  conn   = func cellConn    "connectivity"
  off    = func cellOffsets "offsets"
  types  = func cts         "types"
  fac    = func faces       "faces"
  facoff = func faceOffsets "faceoffsets"

  func = renderDataArray isBin
  in xelem "Cells" $ xelems full

renderWholeExtAttr :: (Int, Int, Int) -> Xml Attr
renderWholeExtAttr = xattr "WholeExtent" . renderExtent

renderSpaceAttr :: (Double, Double, Double) -> Xml Attr
renderSpaceAttr = xattr "Spacing" . renderTriple

renderOriginAttr :: (Double, Double, Double) -> Xml Attr
renderOriginAttr = xattr "Origin" . renderTriple

renderTriple :: (Show a)=> (a, a, a) -> Text
renderTriple (a, b, c) = T.unwords [toTxt a, toTxt b, toTxt c]

renderExtent :: (Int, Int, Int) -> Text
renderExtent (dx, dy, dz) = let
  zr = toTxt (0 :: Int)
  in T.unwords [zr, toTxt (dx-1), zr, toTxt (dy-1), zr, toTxt (dz-1)]

renderCoordinates :: (RenderElemVTK a)=> Bool -> Vector a -> Vector a -> Vector a -> Xml Elem
renderCoordinates isBin setX setY setZ = let
  nodes =
    -- Insert points
    [ renderCoordinatePoint isBin setX "x"
      -- Insert cells
    , renderCoordinatePoint isBin setY "y"
    , renderCoordinatePoint isBin setZ "z" ]
  in xelem "Coordinates" $ xelems nodes

renderCoordinatePoint :: (RenderElemVTK a)=> Bool -> Vector a -> Text -> Xml Elem
renderCoordinatePoint isBin points dir = let
  name = dir <> "_coordinate"
  in renderDataArray isBin points name

renderPoints :: (RenderElemVTK a)=> Bool -> Vector a -> Xml Elem
renderPoints isBin points = let
  child = renderDataArray isBin points "Points"
  in xelem "Points" child

renderDataArray :: (RenderElemVTK a)=> Bool -> Vector a -> Text -> Xml Elem
renderDataArray isBin points name = let
  size = encode . BB.toLazyByteString . BB.word32BE . fromIntegral $ U.length points
  vecRender
    | isBin     = (size <>) . encode . BB.toLazyByteString . foldMap renderBinaryPoint . U.toList
    | otherwise = BB.toLazyByteString . foldMap renderPoint . U.toList
  format
    | isBin     = "binary"
    | otherwise = "ascii"
  attr = [ xattr "type"               (getNumType points)
         , xattr "Name"               name
         , xattr "format"             format
         , xattr "NumberOfComponents" (getNumComp points)
         ]
  runrender = xtextRaw . fromLazyByteString . vecRender
  in xelem "DataArray" ((xattrs attr) <#> runrender points)

getNumType :: (RenderPoint a, U.Unbox a)=> Vector a -> Text
getNumType = renderNumType . pointNumberType . U.head

getNumComp :: (RenderPoint a, U.Unbox a)=> Vector a -> Text
getNumComp =  toTxt . pointNumberComp . U.head

toTxt::(Show a)=> a -> Text
toTxt = T.pack.show
