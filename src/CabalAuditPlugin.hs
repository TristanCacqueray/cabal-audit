module CabalAuditPlugin (plugin) where

import Control.Monad
import GHC.Plugins hiding ((<>))

import CabalAudit (Declaration (..), nameToDeclaration)
import Data.List (intercalate, intersperse)

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    return (todo <> [CoreDoPluginPass "Collect Dependencies" pass])

pass :: ModGuts -> CoreM ModGuts
pass guts = do
    let moduleName = guts.mg_module
    let getDependencies :: CoreBind -> [(Declaration, [Declaration])]
        getDependencies = \case
            NonRec b expr -> [(mkLocalDecl moduleName b, getExprDeps expr)]
            Rec xs -> foldMap getDependencies ((\(b, e) -> NonRec b e) <$> xs)

        getExprDeps :: Expr Var -> [Declaration]
        getExprDeps = \case
            Var var -> [varDecl moduleName var]
            Lit _lit -> mempty
            App expr arg -> foldMap getExprDeps [expr, arg]
            Lam b expr -> varDecl moduleName b : getExprDeps expr
            Let bind expr -> getBindDeps bind <> getExprDeps expr
            Case expr b _type alt -> varDecl moduleName b : getExprDeps expr <> foldMap getAltDeps alt
            Cast expr _coer -> getExprDeps expr
            Tick _ expr -> getExprDeps expr
            Type _type -> mempty
            Coercion _coer -> mempty

        getBindDeps :: CoreBind -> [Declaration]
        getBindDeps = \case
            NonRec b expr -> varDecl moduleName b : getExprDeps expr
            Rec xs -> foldMap (\(b, expr) -> varDecl moduleName b : getExprDeps expr) xs

        getAltDeps :: Alt Var -> [Declaration]
        getAltDeps = \case
            Alt _altCon bs expr -> map (varDecl moduleName) bs <> getExprDeps expr

    let dependencies :: [(Declaration, [Declaration])]
        dependencies = foldMap getDependencies guts.mg_binds
    forM_ dependencies \(decl, deps) -> do
        putMsgS $ show decl <> ": " <> intercalate ", " (show <$> deps)
    pure guts

varDecl :: Module -> Var -> Declaration
varDecl moduleName var = case nameToDeclaration (varName var) of
    Just decl -> decl
    Nothing -> mkLocalDecl moduleName var

mkLocalDecl :: Module -> Var -> Declaration
mkLocalDecl moduleName var = Declaration moduleName (nameOccName $ varName var)
