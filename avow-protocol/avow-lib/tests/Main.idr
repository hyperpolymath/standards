-- SPDX-License-Identifier: MPL-2.0
||| Test runner for avow-lib
module Main

import STAMP.ABI.ConsentTests
import STAMP.ABI.UnsubscribeTests

main : IO ()
main = do
  runConsentTests
  runUnsubscribeTests
  putStrLn "avow-lib tests: OK"
