module Asset where

import qualified Data.Map as M
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST

data Asset = Asset
  { aId :: Int,
    aPath :: String,
    aContent :: JSAST,
    aMapping :: M.Map String Int,
    aDependencies :: [String]
  }
  deriving (Show, Eq)

mkAsset :: String -> String -> Int -> Either String Asset
mkAsset path content id' = do
  mdl <- parseModule content path
  deps <- imports mdl
  pure $ Asset {aId = id', aPath = path, aContent = transpile mdl, aMapping = M.empty, aDependencies = deps}

importSpecifierToObjectProperty :: JSImportSpecifier -> JSObjectProperty
importSpecifierToObjectProperty (JSImportSpecifier ident) = JSPropertyIdentRef JSAnnotSpace (identToString ident)
importSpecifierToObjectProperty (JSImportSpecifierAs ident _ alias) =
  JSPropertyNameandValue
    (JSPropertyIdent JSAnnotSpace (identToString ident))
    JSNoAnnot
    [JSIdentifier JSAnnotSpace (identToString alias)]

mapCommaList :: (a -> b) -> JSCommaList a -> JSCommaList b
mapCommaList f (JSLOne x) = JSLOne (f x)
mapCommaList f (JSLCons xs _ x) = JSLCons (mapCommaList f xs) JSNoAnnot (f x)
mapCommaList _ JSLNil = JSLNil

commaListToCommaTrailing :: JSCommaList a -> JSCommaTrailingList a
commaListToCommaTrailing = JSCTLNone

transformImportDecl :: JSImportDeclaration -> [JSStatement]
transformImportDecl (JSImportDeclaration clause from _) = case clause of
  JSImportClauseDefault ident ->
    [ JSConstant
        JSNoAnnot
        ( JSLOne
            ( JSVarInitExpression
                (JSIdentifier JSAnnotSpace (identToString ident))
                ( JSVarInit
                    JSAnnotSpace
                    ( JSMemberExpression
                        (JSIdentifier JSAnnotSpace "require")
                        JSNoAnnot
                        (JSLOne (JSStringLiteral JSNoAnnot (fromClauseToString from)))
                        JSNoAnnot
                    )
                )
            )
        )
        (JSSemi JSNoAnnot)
    ]
  JSImportClauseNamed (JSImportsNamed _ specifiers _) ->
    [ JSConstant
        JSNoAnnot
        ( JSLOne
            ( JSVarInitExpression
                ( JSObjectLiteral
                    JSAnnotSpace
                    (commaListToCommaTrailing . mapCommaList importSpecifierToObjectProperty $ specifiers)
                    JSAnnotSpace
                )
                ( JSVarInit
                    JSAnnotSpace
                    ( JSMemberExpression
                        (JSIdentifier JSAnnotSpace "require")
                        JSNoAnnot
                        (JSLOne (JSStringLiteral JSNoAnnot (fromClauseToString from)))
                        JSNoAnnot
                    )
                )
            )
        )
        (JSSemi JSNoAnnot)
    ]
  JSImportClauseNameSpace (JSImportNameSpace _ _ _ident) -> error "Not implemented, JSImportClauseNameSpace"
  JSImportClauseDefaultNameSpace _ident _ (JSImportNameSpace _ _ _nsIdent) -> error "Not implemented, JSImportClauseDefaultNameSpace"
  JSImportClauseDefaultNamed _ident _ (JSImportsNamed _ _specifiers _) -> error "Not implemented, JSImportClauseDefaultNamed"
transformImportDecl (JSImportDeclarationBare _ moduleName _) =
  [ JSExpressionStatement
      ( JSCallExpression
          (JSIdentifier JSNoAnnot "require")
          JSNoAnnot
          (JSLOne (JSStringLiteral JSNoAnnot moduleName))
          JSNoAnnot
      )
      (JSSemi JSNoAnnot)
  ]

identToString :: JSIdent -> String
identToString (JSIdentName _ name) = name
identToString JSIdentNone = error "Invalid identifier"

fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLOne x) = [x]
fromCommaList (JSLCons xs _ x) = fromCommaList xs ++ [x]
fromCommaList JSLNil = []

fromClauseToString :: JSFromClause -> String
fromClauseToString (JSFromClause _ _ str) = str

transformImport :: JSModuleItem -> [JSModuleItem]
transformImport (JSModuleImportDeclaration _ decl) = JSModuleStatementListItem <$> transformImportDecl decl
transformImport (JSModuleExportDeclaration _ _) = error "Fixme: exports are not supported"
transformImport stmt = [stmt]

transpile :: JSAST -> JSAST
transpile (JSAstModule items _) = JSAstModule (concatMap transformImport items) JSNoAnnot
transpile _ = error "I'm not implemented"

imports :: JSAST -> Either String [String]
imports (JSAstModule stmts _) = mapM toImpString (filter isImport stmts)
  where
    toImpString (JSModuleImportDeclaration _ (JSImportDeclaration _ (JSFromClause _ _ res) _)) = pure res
    toImpString _ = Left "Invalid import declaration"
    isImport (JSModuleImportDeclaration _ _) = True
    isImport _ = False
imports _ = pure []