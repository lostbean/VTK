{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Data.VTK.Types where

import qualified Data.ByteString.Lazy.Builder       as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BBA
import qualified Data.Text.Lazy.Encoding            as TE
import qualified Data.Vector.Unboxed                as U
import qualified Data.Vector                        as V

import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Text.Lazy.Builder             (toLazyText)
import           Data.Vector.Unboxed                (Unbox, Vector)
import           Data.Word                          (Word32, Word8)

import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat

-- =======================================================================================

data VTKNumType
  = VTK_UInt8
  | VTK_UInt
  | VTK_Int
  | VTK_Float
  deriving (Show, Eq)

renderNumType :: VTKNumType -> Text
renderNumType x = case x of
  VTK_UInt8 -> "UInt8"
  VTK_UInt  -> "UInt32"
  VTK_Int   -> "Int32"
  _         -> "Float64"

class RenderPoint point where
  renderPoint       :: point -> BB.Builder
  renderBinaryPoint :: point -> BB.Builder
  pointNumberType   :: point -> VTKNumType
  pointNumberComp   :: point -> Word8

class (RenderPoint a, Unbox a)=> RenderElemVTK a

instance RenderElemVTK Word8
instance RenderElemVTK Word32
instance RenderElemVTK Int
instance RenderElemVTK Double
instance (RenderElemVTK a)=> RenderElemVTK (a, a, a)

class RenderCell shape where
  makeCell :: shape -> Vector Int
  getType  :: shape -> CellType

-- | This function creates an attribute of type 'attr' for each cell.
-- It is itself a function that is given to render, where:
--
-- * Int      : is the position in the cell list
--
-- * Vector a : is the list of point the cell
--
-- * CellType : is the type of the cell
--
-- > let attr = mkCellAttr "color" (\i x cellType -> (Vec3 1 1 1) &* 1/(evalCellType cellType))
--
mkCellAttr :: (RenderElemVTK attr)=> String
           -> (Int -> Vector a -> CellType -> attr) -> VTKAttrCell a
mkCellAttr = VTKAttrCell

-- | This function creates an attribute of type 'attr' for each point.
-- It is itself a function that is given to render, where:
--
-- * Int : is the position in the point list
--
-- * a   : is the value of the point
--
-- > let attr = mkCellAttr "grainID" (\i x -> grainIDTable!i)
--
mkPointValueAttr :: (RenderElemVTK attr)=> String -> (Int -> a -> attr) -> VTKAttrPointValue a
mkPointValueAttr = VTKAttrPointValue

-- | This function creates an attribute of type 'attr' for each point.
-- It is itself a function that is given to render, where:
--
-- * Int : is the position in the point list
--
-- * a   : is the value of the point
--
-- > let attr = mkCellAttr "grainID" (\i x -> grainIDTable!i)
--
mkPointAttr :: (RenderElemVTK attr)=> String -> (Int -> attr) -> VTKAttrPoint a
mkPointAttr = VTKAttrPoint

data VTKAttrPointValue a =
  forall attr. (RenderElemVTK attr)=>
  VTKAttrPointValue String (Int -> a -> attr)

data VTKAttrPoint a =
  forall attr. (RenderElemVTK attr)=>
  VTKAttrPoint String (Int -> attr)

data VTKAttrCell  a =
  forall attr. (RenderElemVTK attr)=>
  VTKAttrCell  String (Int -> Vector a -> CellType -> attr)

type MultiPieceVTK a = Vector (VTK a)

data VTK a = VTK
  { name           :: Text
  , dataSet        :: VTKDataSet a
  , pointValueData :: [VTKAttrPointValue a]
  , pointData      :: [VTKAttrPoint a]
  , cellData       :: [VTKAttrCell a]
  }

instance (Show a, RenderElemVTK a)=> Show (VTK a) where
  show (VTK{..}) = concat [show name, " ", " ", show dataSet]

data VTKDataSet a =
    StructPoint
    { dimSP    :: (Int, Int, Int)
    , originSP :: (Double, Double, Double)
    , spaceSP  :: (Double, Double, Double)
    }

  | StructGrid
    { dimSG :: (Int, Int, Int) }

  | RectLinGrid
    { setxRG :: Vector Double
    , setyRG :: Vector Double
    , setzRG :: Vector Double
    }

  | UnstructGrid
    { setUG      :: Vector a
    , cellUG     :: Vector Int
    , cellOffUG  :: Vector Int
    , cellTypeUG :: V.Vector CellType
    }

  | PolyData
    { setPD        :: Vector a
    , cellPD       :: Vector Int
    , cellOffPD    :: Vector Int
    , cellTypePD   :: V.Vector CellType
    , polysPD      :: Vector Int
    , polysOffPD   :: Vector Int
    , linesPD      :: Vector Int
    , linesOffPD   :: Vector Int
    , stripsPD     :: Vector Int
    , stripsOffPD  :: Vector Int
    , vertsPD      :: Vector Int
    , vertsOffPD   :: Vector Int
    } deriving (Show)

getVTKSize :: (RenderElemVTK a)=> VTK a -> Int
getVTKSize vtk = case dataSet vtk of
  StructPoint (dx, dy, dz) _ _   -> dx * dy * dz
  StructGrid  (dx, dy, dz)       -> dx * dy * dz
  RectLinGrid sx sy sz           -> U.length sx * U.length sy * U.length sz
  UnstructGrid{..}               -> U.length setUG
  PolyData{..}                   -> U.length setPD

getVTKIndex :: (RenderElemVTK a)=> VTK a -> Vector Int
getVTKIndex vtk = let
  vecSerial (dx, dy, dz) = U.fromList [ i + dx * j + dx * dy * k
                                      | i <- [0..dx]
                                      , j <- [0..dy]
                                      , k <- [0..dz] ]
  in case dataSet vtk of
    StructPoint size _ _ -> vecSerial size
    StructGrid  size     -> vecSerial size
    RectLinGrid sx sy sz -> vecSerial (U.length sx, U.length sy, U.length sz)
    UnstructGrid{..}     -> U.generate (U.length setUG) id
    PolyData{..}         -> U.generate (U.length setPD) id

data CellType
  = VTK_VERTEX
  | VTK_POLY_VERTEX
  | VTK_LINE
  | VTK_POLY_LINE
  | VTK_TRIANGLE
  | VTK_TRIANGLE_STRIP
  | VTK_POLYGON
  | VTK_PIXEL
  | VTK_QUAD
  | VTK_TETRA
  | VTK_VOXEL
  | VTK_HEXAHEDRON
  | VTK_WEDGE
  | VTK_PYRAMID
  | VTK_QUADRATIC_EDGE
  | VTK_QUADRATIC_TRIANGLE
  | VTK_QUADRATIC_QUAD
  | VTK_QUADRATIC_TETRA
  | VTK_QUADRATIC_HEXAHEDRON
  deriving (Eq)

instance Show CellType where
  show = show . evalCellType

-- | Evaluates the 'CellType' according the VTK standards.
evalCellType :: CellType -> Int
evalCellType x = case x of
  VTK_VERTEX               -> 1
  VTK_POLY_VERTEX          -> 2
  VTK_LINE                 -> 3
  VTK_POLY_LINE            -> 4
  VTK_TRIANGLE             -> 5
  VTK_TRIANGLE_STRIP       -> 6
  VTK_POLYGON              -> 7
  VTK_PIXEL                -> 8
  VTK_QUAD                 -> 9
  VTK_TETRA                -> 10
  VTK_VOXEL                -> 11
  VTK_HEXAHEDRON           -> 12
  VTK_WEDGE                -> 13
  VTK_PYRAMID              -> 14
  VTK_QUADRATIC_EDGE       -> 21
  VTK_QUADRATIC_TRIANGLE   -> 22
  VTK_QUADRATIC_QUAD       -> 23
  VTK_QUADRATIC_TETRA      -> 24
  VTK_QUADRATIC_HEXAHEDRON -> 25

-- ================================= Instances RenderPoint ===============================

instance (RenderPoint a)=> RenderPoint (a, a, a) where
  renderPoint (x, y, z) =
    renderPoint x <>
    renderPoint y <>
    renderPoint z
  renderBinaryPoint (x, y, z) =
    renderBinaryPoint x <>
    renderBinaryPoint y <>
    renderBinaryPoint z
  pointNumberType (x, _, _) = pointNumberType x
  pointNumberComp           = const 3

instance RenderPoint Double where
  renderPoint       = (<> BB.char8 ' ') . renderDouble
  renderBinaryPoint = BB.doubleLE
  pointNumberType   = const VTK_Float
  pointNumberComp   = const 1

instance RenderPoint Int where
  renderPoint       = (<> BB.char8 ' ') . BBA.intDec
  renderBinaryPoint = BB.int32LE . fromIntegral
  pointNumberType   = const VTK_Int
  pointNumberComp   = const 1

instance RenderPoint Word32 where
  renderPoint       = (<> BB.char8 ' ') . BBA.word32Dec
  renderBinaryPoint = BB.word32LE
  pointNumberType   = const VTK_UInt
  pointNumberComp   = const 1

instance RenderPoint Word8 where
  renderPoint       = (<> BB.char8 ' ') . BBA.word8Dec
  renderBinaryPoint = BB.word8
  pointNumberType   = const VTK_UInt8
  pointNumberComp   = const 1

renderInt :: Int -> BB.Builder
renderInt = BB.lazyByteString . TE.encodeUtf8 . toLazyText . decimal

renderDouble :: Double -> BB.Builder
renderDouble = renderF 4

renderF :: (RealFloat a)=> Int -> a -> BB.Builder
renderF n = BB.lazyByteString . TE.encodeUtf8 . toLazyText . formatRealFloat Fixed (Just n)
