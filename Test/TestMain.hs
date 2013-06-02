{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Test.Types.AddressTest
import {-@ HTF_TESTS @-} Test.Types.AwardTest
import {-@ HTF_TESTS @-} Test.Types.BirthDateTest hiding (main)
import {-@ HTF_TESTS @-} Test.Types.ProfessionalExperienceTest hiding (main)
import {-@ HTF_TESTS @-} Test.Types.User.FullUserTest hiding (main)
import {-@ HTF_TESTS @-} Test.Types.User.MinimalUserTest hiding (main)

main = htfMain htf_importedTests
