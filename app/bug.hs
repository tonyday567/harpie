{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Harpie.Fixed as F
import Harpie.Shape as S

main =
    print (F.index (F.indexes (Dims @'[0]) (S.fins @'[2] [0]) (F.range @[2,3,4])) (UnsafeFins [0,1]))
