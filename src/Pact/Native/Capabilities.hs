{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Native.Capabilities
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for working with capabilities.
--

module Pact.Native.Capabilities (capDefs) where

import Control.Monad
import Pact.Eval
import Pact.Native.Internal
import Pact.Types.Runtime


capDefs :: NativeModule
capDefs =
  ("Capabilities",
   [ withCapability
   , enforceGuardDef "enforce-guard"
   , requireCapability
   , createUserGuard
   , createPactGuard
   , createModuleGuard
   , keysetRefGuard
   ])

tvA :: Type n
tvA = mkTyVar "a" []


withCapability :: NativeDef
withCapability =
  defNative (specialForm WithCapability) withCapability'
  (funType tvA [("capability",TyFun $ funType' tTyBool []),("body",TyList TyAny)])
  "Specifies and requests grant of CAPABILITY which is an application of a 'defcap' \
  \production. Given the unique token specified by this application, ensure \
  \that the token is granted in the environment during execution of BODY. \
  \'with-capability' can only be called in the same module that declares the \
  \corresponding 'defcap', otherwise module-admin rights are required. \
  \If token is not present, the CAPABILITY is applied, with successful completion \
  \resulting in the installation/granting of the token, which will then be revoked \
  \upon completion of BODY. Nested 'with-capability' calls for the same token \
  \will detect the presence of the token, and will not re-apply CAPABILITY, \
  \but simply execute BODY. \
   \`$(with-capability (update-users id) (update users id { salary: new-salary }))`"
  where
    withCapability' :: NativeFun e
    withCapability' i [c@TApp{},body@TList{}] = gasUnreduced i [] $ do
      grantedCap <- evalCap (_tApp c)
      r <- reduceBody body
      mapM_ revokeCapability grantedCap
      return r
    withCapability' i as = argsError' i as

-- | Given cap app, enforce in-module call, eval args to form capability,
-- and attempt to acquire. Return capability if newly-granted.
evalCap :: App (Term Ref) -> Eval e (Maybe Capability)
evalCap a@App{..} = requireDefcap a >>= \d@Def{..} -> do
      guardForModuleCall _appInfo _dModule $ return ()
      prep@(args,_) <- prepareUserAppArgs d _appArgs
      let cap = UserCapability _dDefName args
      acquired <- acquireCapability cap $ do
        g <- computeUserAppGas d _appInfo
        void $ evalUserAppBody d prep _appInfo g reduceBody
      return $ case acquired of
        NewlyAcquired -> Just cap
        AlreadyAcquired -> Nothing

requireDefcap :: App (Term Ref) -> Eval e (Def Ref)
requireDefcap App{..} = case _appFun of
  (TVar (Ref (TDef d@Def{..} _)) _) -> case _dDefType of
    Defcap -> return d
    _ -> evalError _appInfo $ "Can only apply defcap here, found: " ++ show _dDefType
  t -> evalError (_tInfo t) $ "Attempting to apply non-def: " ++ show _appFun

requireCapability :: NativeDef
requireCapability =
  defNative "require-capability" requireCapability'
  (funType tTyBool [("capability",TyFun $ funType' tTyBool [])])
  "Specifies and tests for existing grant of CAPABILITY, failing if not found in environment. \
  \`$(require-capability (TRANSFER src dest))`"
  where
    requireCapability' :: NativeFun e
    requireCapability' i [TApp a@App{..} _] = gasUnreduced i [] $ requireDefcap a >>= \d@Def{..} -> do
      (args,_) <- prepareUserAppArgs d _appArgs
      let cap = UserCapability _dDefName args
      granted <- capabilityGranted cap
      unless granted $ evalError' i $ "require-capability: not granted: " ++ show cap
      return $ toTerm True
    requireCapability' i as = argsError' i as


createPactGuard :: NativeDef
createPactGuard =
  defRNative "create-pact-guard" createPactGuard'
  (funType (tTyGuard (Just GTyPact)) [("name",tTyString)])
  "Defines a guard predicate by NAME that captures the results of 'pact-id'. \
  \At enforcement time, the success condition is that at that time 'pact-id' must \
  \return the same value. In effect this ensures that the guard will only succeed \
  \within the multi-transaction identified by the pact id."
  where
    createPactGuard' :: RNativeFun e
    createPactGuard' i [TLitString name] = do
      pid <- getPactId i
      return $ (`TGuard` (_faInfo i)) $ GPact $ PactGuard pid name
    createPactGuard' i as = argsError i as


createModuleGuard :: NativeDef
createModuleGuard =
  defRNative "create-module-guard" createModuleGuard'
  (funType (tTyGuard (Just GTyModule)) [("name",tTyString)])
  "Defines a guard by NAME that enforces the current module admin predicate."
  where
    createModuleGuard' :: RNativeFun e
    createModuleGuard' i [TLitString name] = findCallingModule >>= \m -> case m of
      Just mn ->
        return $ (`TGuard` (_faInfo i)) $ GModule $ ModuleGuard mn name
      Nothing -> evalError' i "create-module-guard: must call within module"
    createModuleGuard' i as = argsError i as

keysetRefGuard :: NativeDef
keysetRefGuard =
  defRNative "keyset-ref-guard" keysetRefGuard'
  (funType (tTyGuard (Just GTyKeySetName)) [("keyset-ref",tTyString)])
  "Creates a guard for the keyset registered as KEYSET-REF with 'define-keyset'. \
  \Concrete keysets are themselves guard types; this function is specifically to \
  \store references alongside other guards in the database, etc."
  where
    keysetRefGuard' :: RNativeFun e
    keysetRefGuard' i [TLitString ks] =
      return $ (`TGuard` (_faInfo i)) $ GKeySetRef (KeySetName ks)
    keysetRefGuard' i as = argsError i as


createUserGuard :: NativeDef
createUserGuard =
  defRNative "create-user-guard" createUserGuard'
  (funType (tTyGuard (Just GTyUser)) [("data",tvA),("predfun",tTyString)])
  "Defines a custom guard predicate, where DATA will be passed to PREDFUN at time \
  \of enforcement. PREDFUN is a valid name in the declaring environment. \
  \PREDFUN must refer to a pure function or enforcement will fail at runtime."
  where
    createUserGuard' :: RNativeFun e
    createUserGuard' i [udata,TLitString predfun] = case typeof udata of
      Right {} -> case parseName (_faInfo i) predfun of
        Right n -> do
          rn <- resolveRef n >>= \nm -> case nm of
            Just (Direct {}) -> return n
            Just (Ref (TDef Def{..} _)) ->
              return $ QName _dModule (asString _dDefName) _dInfo
            Just _ -> evalError' i $ "Invalid predfun, not a def: " ++ show n
            _ -> evalError' i $ "Could not resolve predfun: " ++ show n
          return $ (`TGuard` (_faInfo i)) $ GUser (UserGuard udata rn)
        Left s -> evalError' i $
          "Invalid name " ++ show predfun ++ ": " ++ s
      Left {} -> evalError' i "Data must be value"
    createUserGuard' i as = argsError i as
