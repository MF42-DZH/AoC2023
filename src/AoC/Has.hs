{-# Language MultiParamTypeClasses #-}

module AoC.Has where

import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS

class HasView t v where
  viewV :: t -> v

class HasPut t v where
  putV  :: v -> t -> t

class (HasView t v, HasPut t v) => Has t v

lgetH :: (Monad m, HasView t v) => SL.StateT t m v
lgetH = SL.gets viewV

lputH :: (Monad m, HasPut t v) => v -> SL.StateT t m ()
lputH v = SL.modify (putV v)

lput'H :: (Monad m, HasPut t v) => v -> SL.StateT t m ()
lput'H v = SL.modify' (putV v)

lmodifyH :: (Monad m, Has t v) => (v -> v) -> SL.StateT t m ()
lmodifyH f = SL.modify (\ t -> putV (f (viewV t)) t)

lmodify'H :: (Monad m, Has t v) => (v -> v) -> SL.StateT t m ()
lmodify'H f = SL.modify' (\ t -> putV (f (viewV t)) t)

sgetH :: (Monad m, HasView t v) => SS.StateT t m v
sgetH = SS.gets viewV

sputH :: (Monad m, HasPut t v) => v -> SS.StateT t m ()
sputH v = SS.modify (putV v)

sput'H :: (Monad m, HasPut t v) => v -> SS.StateT t m ()
sput'H v = SS.modify' (putV v)

smodifyH :: (Monad m, Has t v) => (v -> v) -> SS.StateT t m ()
smodifyH f = SS.modify (\ t -> putV (f (viewV t)) t)

smodify'H :: (Monad m, Has t v) => (v -> v) -> SS.StateT t m ()
smodify'H f = SS.modify' (\ t -> putV (f (viewV t)) t)
