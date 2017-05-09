{-# LANGUAGE TemplateHaskell #-}
import           Data.Maybe       (fromJust)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Eval
import           Expr
import           Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, substTests]

parserTests = testGroup "Parse tests"
  [ testCase "Parsing variable" $
      parse "x" @?= (Just $ Var "x")
  , testCase "Parsing lambda" $
      parse "λx.x" @?= (Just $ Lam "x" (Var "x"))
  , testCase "Parsing lambda with brackets" $
      parse "λx.x(xy)" @?= (Just $ Lam "x" (App (Var "x") (App (Var "x") (Var "y"))))
  , testCase "Parsing lambda with brackets - counter example" $
      parse "λx.xxy" @?= (Just $ Lam "x" (App (App (Var "x") (Var "x")) (Var "y")))
  , testCase "Parsing application" $
      parse "(λx.xx)y" @?= (Just $ App (Lam "x" (App (Var "x") (Var "x"))) (Var "y"))
  , testCase "Parsing application with brackets" $
      parse "(xy)" @?= (Just $ App (Var "x") (Var "y"))
  , testCase "Parsing failure" $
      parse "x+y" @?= Nothing
  , testCase "A complex expression" $
      parse "λx.xzλy.xy" @?=
        (Just $ Lam "x" (App (App (Var "x") (Var "z")) (Lam "y" (App (Var "x") (Var "y")))))
  , testCase "Omega" $
      parse "(λx.xx)(λx.xx)" @?=
        (Just $ App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x"))))
  ]

-- Shorthand to get parsed expr from a string.
p = fromJust . parse

substTests = testGroup "Subst tests"
  [ testCase "Subst var - change" $
      subst "x" (p "λx.xx") (p "x") @?= (p "λx.xx")
  , testCase "Subst var - no change" $
      subst "x" (p "λx.xx") (p "y") @?= (p "y")
  , testCase "Subst lambda - substitute arg" $
      subst "x" (p "y") (p "λx.xx") @?= (p "λx.xx")
  , testCase "Subst lambda - substitute a non-free var in body" $
      subst "y" (p "z") (p "λx.λy.xy") @?= (p "λx.λy.xy")
  , testCase "Subst lambda - substitute a free var in body" $
      subst "y" (p "z") (p "λx.xy") @?= (p "λx.xz")
  , testCase "Subst lambda - substitute a free var in body with new name" $
      subst "y" (p "x") (p "λx.xy") @?= (p "λa.ax")
  ]
