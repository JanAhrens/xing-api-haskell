{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Types.AddressTest
import {-@ HTF_TESTS @-} Types.AwardTest
import {-@ HTF_TESTS @-} Types.BirthDateTest hiding (main)
import {-@ HTF_TESTS @-} Types.ProfessionalExperienceTest hiding (main)
import {-@ HTF_TESTS @-} Types.User.FullUserTest hiding (main)
import {-@ HTF_TESTS @-} Types.User.MinimalUserTest hiding (main)

main :: IO ()
main = htfMain htf_importedTests
