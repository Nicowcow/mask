{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.ByteString     hiding (any)
import Data.Makefile
import Data.Makefile.Parse

main :: IO ()
main = do
    withMakefile "test-data/basic/Makefile1" $ \m -> assertTarget "foo" m
    withMakefile "test-data/basic/Makefile2" $ \m -> do
        assertTargets [ "all"
                      , "hello"
                      , "main.o"
                      , "factorial.o"
                      , "hello.o"
                      , "clean"] m
        assertAssignment ("CC", "g++") m
    withMakefile "test-data/elfparse/Makefile" $ \m -> do
        assertTargets [ "default"
                      , "is_perl_recent_enough"
                      , "all"
                      , "clean"
                      , "clean_not_caches"
                      , "${SRCP6}/STD.pmc"
                      , "lex.is_current"
                      , "${STD_BLUE_CACHEDIR}.is_current"
                      , "${STD_RED_CACHEDIR}.is_current"
                      , "IRx1_FromAST2.pm"
                      , "elfblue"
                      , "elfrx"
                      , "nodes.pm"
                      , "rx_prelude.pm"
                      , "rx_prelude_p5.pm"
                      , "std.pm.p5"
                      , "STD_green_run.pm.p5"
                      , "STD_green_run"
                      , "elfdev"
                      , "elfdev1"
                      , "check"
                      , "check_rx_on_re"
                      , "check_std_rx_on_re"
                      , "rerun_std_rx_on_re"
                      , "check_STD_blue"
                      , "does_gimme5_memory_problem_still_exist"
                      , "elfblue_regression_debug"
                      , "have_STD_red_cache"
                      , "have_STD_blue_cache" ] m
        assertAssignments [ ("ELF", "../../elf/elf_h")
                          , ("ELFDIR", "../../elf/elf_h_src")
                          , ("SRCP6", "./pugs_src_perl6")
                          , ("STDPM", "./pugs_src_perl6/STD.pm")
                          , ("TMP", "deleteme")
                          {- ... feeling lazy -} ] m

withMakefile :: FilePath -> (Makefile -> IO ()) -> IO ()
withMakefile  f a = fromRight <$> parseAsMakefile f >>= a

assertTargets :: [Target] -> Makefile -> IO ()
assertTargets ts m = mapM_ (`assertTarget` m) ts

assertAssignments :: [(ByteString, ByteString)] -> Makefile -> IO ()
assertAssignments as m = mapM_ (`assertAssignment` m) as

assertAssignment :: (ByteString, ByteString) -> Makefile -> IO ()
assertAssignment (n, v) (Makefile m) = unless (any hasAssignment m) $
    error ("Assignment " ++ show (n, v) ++ " wasn't found in Makefile " ++ show m)
  where hasAssignment (Assignment n' v') = n == n' && v == v'
        hasAssignment _                  = False

assertTarget :: Target -> Makefile -> IO ()
assertTarget t (Makefile m) = unless (any (hasTarget t) m) $
    error ("Target " ++ show t ++ " wasn't found in Makefile " ++ show m)
  where hasTarget t (Rule t' _ _) = t == t'
        hasTarget _ _                 = False

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight"
