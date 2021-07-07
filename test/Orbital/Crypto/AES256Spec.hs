{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Orbital.Crypto.AES256Spec where

import           Crypto.Cipher.AES                   ( AES256 )
import           Orbital.Crypto.AES256
import           RIO
import           Test.Hspec


spec :: Spec
spec = do

  describe "encrypt / decrypt" $ do

    it "encrypts and decrypts given input" $ do
      let sample = "don't use cloud, make your data orbital ;-)" :: ByteString
      secretKey <- genSecretKey (undefined :: AES256) 32
      mInitIV   <- genRandomIV (undefined :: AES256)
      case mInitIV of
        Nothing     -> error "Failed to generate and initialization vector."
        Just initIV -> do
          let encryptedMsg = encrypt secretKey initIV sample
              decryptedMsg = decrypt secretKey initIV =<< encryptedMsg
          case (,) <$> encryptedMsg <*> decryptedMsg of
            Left  err          -> error $ show err
            Right (eMsg, dMsg) -> do
              dMsg `shouldBe` sample

