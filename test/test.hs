{- |
File        : test.hs
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for @timesheet@ package.
-}
import qualified WorkDay.Test

import           Test.Tasty   (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ WorkDay.Test.tests
  ]
