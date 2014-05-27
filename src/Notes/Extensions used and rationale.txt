-- To generate this list:
-- for file in *.hs; cat $file | grep LANGUAGE | sed 's/  */ /g'; end | sort | uniq

-- More expressive kinds
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}

-- More expressive typeclasses
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

-- More expressive types
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Syntactic sugar
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

-- Eliminate some trivial boilerplate
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}

-- Regrettably necessary for some type families and class instances
{-# LANGUAGE UndecidableInstances #-}
