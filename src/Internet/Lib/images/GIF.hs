module GIF where

-- Based on the GIF87a & GIF98a specification by CompuServe Inc, 1987-1990.

-- The documents can be found at
-- http://www.dcs.ed.ac.uk/~mxr/gfx/2d/GIF87a.txt
-- http://www.dcs.ed.ac.uk/~mxr/gfx/2d/GIF89a.txt

data File
  = GIF { signature::String, -- GIF87a or GIF89a
          screen_descriptor::ScreenDescriptor,
	  global_color_map::Maybe ColorMap, -- if hasGlobalColorMap
	  data_blocks::DataBlocks }
  deriving (Show)

data ScreenDescriptor
  = SD { swidth,sheight::Short,
         hasGlobalMap::Bool,
	 colorResolution::BitCount,
	 sortFlag::Bool, -- used in GIF89a, always False in GIF87a
	 sbitsPerPixel::BitCount,
	 background::Pixel,
	 aspectRatio::Byte } -- GIF89a, always 0 in GIF87a
  deriving (Show)

type ColorMap = [RGB]
type RGB = (Byte,Byte,Byte)

type DataBlocks = [DataBlock]

type DataBlock = Either ExtensionBlock Image

data Image
  = Image { image_descriptor::ImageDescriptor,
            local_colorMap::Maybe ColorMap,
            raster_data::RasterData }
  deriving (Show)

data ImageDescriptor =
  ID { left,top,iwidth,iheight::Short,
       hasLocalMap::Bool,
       interlace::Bool,
       ibitsPerPixel::BitCount } -- 3 bits, used only if hasLocalMap
  deriving (Show)

type RasterData
  = Either CompressedBlocks Pixels -- GIF files always contain CompressedBlocks

data CompressedBlocks
  = CB { code_size'::Int,
         blocks::Blocks }
  deriving (Show)

type Blocks = [Block]
type Block = [Byte] -- 8 bits

type Pixel = Byte
type Pixels = [Pixel] -- 8 bits

data ExtensionBlock
  = EB { function_code::Byte, -- 8 bits
         func_data::Blocks }
  -- Extension blocks interpreted according to GIF89a:
  -- Graphic Control Extension (249):
  | GCE { disposalMethod :: Int, -- 3 bits
          userInputFlag, transparentColorFlag :: Bool,
	  delayTime :: Short,
	  transparentColorIndex :: Byte }
  -- Comment Extension (254):
  | Comment String
  -- Application Extension (255):
  | AE { applicationIdentifier :: String, -- 8 chars
         applAuthenticationCode :: (Byte,Byte,Byte),
	 applicationData :: Blocks }
  deriving (Show)

type Byte = Int -- 8 bits, range 0..255 only
type Short = Int -- 16 bits, range 0..65535 only

type BitCount = Int -- 3 bits, range 1..8, stored as 0..7 in files
