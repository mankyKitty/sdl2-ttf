module SDL.TTF.Internals where

import Foreign.C.String
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Control.Monad

import qualified SDL as SDL
import qualified SDL.Raw as Raw

--import SDL.TTF.FFI (TTFFont)
import qualified SDL.TTF.FFI as FFI
import SDL.TTF.Types

peekInts 
  :: (FFI.TTFFont -> CString -> Ptr CInt -> Ptr CInt -> IO CInt)
  -> FFI.TTFFont
  -> String
  -> IO (Int,Int)
peekInts fn fontPtr text = do
    alloca $ \wPtr ->
      alloca $ \hPtr -> do
        -- TODO: handle errors
        void $ withCString text $ \cstr -> fn fontPtr cstr wPtr hPtr
        w <- peek wPtr
        h <- peek hPtr
        return (fromIntegral w, fromIntegral h)

-- | Straight from the code of "sdl2" package, which is not exported. It will make a high level Surface from a Raw Surface. I will move this to somewhere safe, soon
unmanagedSurface :: Ptr Raw.Surface -> SDL.Surface
unmanagedSurface s = SDL.Surface s Nothing

