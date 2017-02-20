module GIFops where
import GIF


apRasterData f gif = gif { data_blocks=map db (data_blocks gif) }
  where
    db = either Left (Right . im)
    im image = image { raster_data = f (raster_data image) }

decompressRasterData decompr = apRasterData rd
  where
    rd = either (Right . decompr) Right

compressRasterData compr = apRasterData rd
  where
    rd = either Left (Left . compr)
