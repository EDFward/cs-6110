{-# LANGUAGE TemplateHaskell #-}
import           Test.Tasty
import           Test.Tasty.HUnit

import           Expr
import           Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]

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
