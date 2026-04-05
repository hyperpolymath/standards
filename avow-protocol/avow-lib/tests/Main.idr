-- SPDX-License-Identifier: PMPL-1.0-or-later
||| Test runner for avow-lib
module Main

import STAMP.ABI.ConsentTests
import STAMP.ABI.UnsubscribeTests

main : IO ()
main = do
  runConsentTests
  runUnsubscribeTests
  putStrLn "avow-lib tests: OK"
