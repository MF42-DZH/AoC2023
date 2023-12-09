{-# Language MultiParamTypeClasses #-}

module AoC.Has where

import Data.Bifunctor
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

class HasView t v where
  viewV :: t -> v

class HasPut t v where
  putV  :: v -> t -> t

class (HasView t v, HasPut t v) => Has t v

readerH :: (MonadReader t m, HasView t v) => (v -> a) -> m a
readerH f = f . viewV <$> ask

askH :: (MonadReader t m, HasView t v) => m v
askH = viewV <$> ask

localH :: (MonadReader t m, Has t v) => (v -> v) -> m a -> m a
localH f = local (\ t -> putV (f (viewV t)) t)

asksH :: (MonadReader t m, HasView t v) => (v -> a) -> m a
asksH f = asks (f . viewV)

stateH :: (S.MonadState t m, Has t v) => (v -> (a, v)) -> m a
stateH f = S.state (\ t -> let (a, v) = f (viewV t) in (a, putV v t))

getH :: (S.MonadState t m, HasView t v) => m v
getH = viewV <$> S.get

putH :: (S.MonadState t m, HasPut t v) => v -> m ()
putH v = S.modify (putV v)

put'H :: (S.MonadState t m, HasPut t v) => v -> m ()
put'H v = S.modify' (putV v)

modifyH :: (S.MonadState t m, Has t v) => (v -> v) -> m ()
modifyH f = S.modify (\ t -> putV (f (viewV t)) t)

modify'H :: (S.MonadState t m, Has t v) => (v -> v) -> m ()
modify'H f = S.modify' (\ t -> putV (f (viewV t)) t)

getsH :: (S.MonadState t m, HasView t v) => (v -> a) -> m a
getsH f = S.gets (f . viewV)

writerH :: (W.MonadWriter t m, HasPut t v) => (a, v) -> m a
writerH (a, v) = W.pass (pure (a, putV v))

tellH :: (W.MonadWriter t m, HasPut t v) => v -> m ()
tellH v = writerH ((), v)

listenH :: (W.MonadWriter t m, HasView t v) => m a -> m (a, v)
listenH = fmap (second viewV) . W.listen

passH :: (W.MonadWriter t m, Has t v) => m (a, v -> v) -> m a
passH = W.pass . fmap (second (\ vf t -> putV (vf (viewV t)) t))

listensH :: (W.MonadWriter t m, HasView t v) => (v -> b) -> m a -> m (a, b)
listensH f = W.listens (f . viewV)

censorH :: (W.MonadWriter t m, Has t v) => (v -> v) -> m a -> m a
censorH f = W.censor (\ t -> putV (f (viewV t)) t)
