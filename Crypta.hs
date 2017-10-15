{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}


module Crypta ( decode, encode, (.^), charFrequencyMap, score, breakSingleXOR, (.^^), (^-^), xorBreak, (<!!>), (>!!<),
                encryptCBC, decryptCBC, pad, splitByBlocks, generateRandomKey, verifyPad, unpad, encryptCTR, CTRmode(..), splitToBlocks,
                decryptCTR, editCTR ) where


import           Data.Bits
import qualified Data.ByteString.Lazy           as BS
import qualified Data.ByteString                as BI
import qualified Data.ByteString.Lazy.Char8     as C
import qualified Data.ByteString.Char8          as CB
import           Data.Char                      (ord)
import           Data.Foldable                  (foldl', maximumBy)
import           Data.Int                       (Int64)
import           Data.List                      (sortBy)
import           Data.List.Split
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Word
import           GHC.Word
import           Numeric
import           System.Random
import qualified Data.Binary                    as BIN
import qualified Test.QuickCheck                as QC

import qualified "cipher-aes" Crypto.Cipher.AES as CP


data CTRmode = LittleEndian | BigEndian

(.^) :: BS.ByteString -> BS.ByteString -> BS.ByteString
stringA .^ stringB = BS.pack $ BS.zipWith xor stringA stringB

decode :: String -> BS.ByteString
decode string
  | length string `mod` 2 /= 0 = error $ "non-parity found in " ++ string
  | otherwise = C.pack $ map (toEnum . fst . head . readHex) (chunksOf 2 string)

encode :: BS.ByteString -> String
encode string = concatMap toHex (C.unpack string)
                where toHex x = let y = showHex (ord x) "" in
                                  if length y < 2 then '0' : y else y

charFrequencyMap :: Map.Map Char Double
charFrequencyMap = Map.fromList $ zip (['a'..'z'] ++ " ,.?!:;-\"\'")
                       [8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153, 0.772, 4.025,
                         2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074,
                         11.0, 1.0, 1.0, 0.5, 0.5, 0.1, 0.1, 0.2, 0.2 ]


score :: Double -> BS.ByteString -> Double
score penalty = C.foldl' (\acc char -> acc + fromMaybe penalty (Map.lookup char charFrequencyMap ) ) 0

generateSingleKey :: Int -> BS.ByteString
generateSingleKey key = BS.pack $ repeat (fromIntegral key :: Word8)

generateAllPossibilites :: BS.ByteString -> [(Int, BS.ByteString)]
generateAllPossibilites string = map (\key -> (key , string .^ generateSingleKey key)) [0..255]

breakSingleXOR :: Double -> BS.ByteString -> (BS.ByteString, Int, Double)
breakSingleXOR penalty string = let generated = generateAllPossibilites string in
                            maximumBy (\(_, _, x) (_, _, y) -> if x > y then GT else LT)
                            (map (\pair -> (snd pair, fst pair, score penalty (snd pair) ) ) generated)


(.^^) :: BS.ByteString -> BS.ByteString  -> BS.ByteString --String XOR String
string .^^ key = string .^ BS.cycle key

(^-^) :: BS.ByteString -> BS.ByteString -> Int --hamming distance
stringA ^-^ stringB = let xored = BS.zipWith xor stringA stringB in
                        foldl' (\acc x -> acc + popCount x) 0 xored

findKeyLen :: BS.ByteString -> Int64 -> [(Int64, Double)]
findKeyLen string limit = sortBy (\x y -> if snd x > snd y then GT else LT) (loop 1)
                          where loop :: Int64 -> [(Int64, Double)]
                                loop n
                                  | n > limit = []
                                  | n > BS.length string `div` 4  = []
                                  | otherwise = (n, hammingDistance (getBlocks string n 4) / fromIntegral n :: Double ) : loop (n+1)

hammingDistance :: [BS.ByteString] -> Double
hammingDistance blocks = (fromIntegral (loop blocks) :: Double) / fromIntegral (length blocks) :: Double
                         where loop [] = error "empty block"
                               loop [_] = error "only one block delivered"
                               loop [x,y] = x ^-^ y
                               loop (x:y:xs) = x ^-^ y + loop (y:xs)
getBlocks :: BS.ByteString -> Int64 -> Int64 -> [BS.ByteString]
getBlocks string blockSize count
  | count == 0 = []
  |otherwise = BS.take blockSize string : getBlocks (BS.drop blockSize string) blockSize (count-1)

splitByBlocks :: BS.ByteString -> Int64 -> [BS.ByteString] --pads the last block
splitByBlocks string blockSize
  | string == BS.empty = []
  | BS.length string < blockSize = pad blockSize string : []
  | otherwise = BS.take blockSize string : splitByBlocks (BS.drop blockSize string) blockSize

splitToBlocks :: Int -> BI.ByteString -> [BI.ByteString] -- doesn't pad the last blockSize
splitToBlocks blockSize string
  | string == BI.empty = []
  | otherwise = BI.take blockSize string : splitToBlocks blockSize (BI.drop blockSize string)

takeEveryN :: BS.ByteString -> Int64 -> Int64 -> BS.ByteString
takeEveryN string n pos = string >!!< [pos, pos+n..]

chunkize :: BS.ByteString -> Int64 -> [BS.ByteString]
chunkize string keyLen = map (takeEveryN string keyLen) [0..keyLen-1]

generateKey :: Int64 -> BS.ByteString -> BS.ByteString
generateKey keyLen string = let keys = map (\x -> let (_,key,_) = breakSingleXOR (-100) x in key ) (chunkize string keyLen) in
                                      C.pack $ map toEnum keys

generateAllPossibleKeys :: Int64 -> BS.ByteString -> [BS.ByteString]
generateAllPossibleKeys limit string = let keyListSorted = findKeyLen string limit in
                                         map (\x -> generateKey (fst x) string) keyListSorted
xorBreak :: BS.ByteString -> Int -> [(BS.ByteString, BS.ByteString)]
xorBreak string count = let decoded = map (\x -> ( string .^^ x, x ) ) (generateAllPossibleKeys 40 string) in
                    take count decoded

(<!!>) :: [a] -> [Int] -> [a]
_ <!!> [] = []
list <!!> (x:xs)
  | x >= length list = []
  | otherwise = list !! x : list <!!> xs

(>!!<) :: BS.ByteString -> [Int64] -> BS.ByteString
_ >!!< [] = BS.empty
string >!!< (x:xs)
  | x >= BS.length string = BS.empty
  | otherwise = string `BS.index` x `BS.cons` string >!!< xs

pad :: Int64 -> BS.ByteString -> BS.ByteString
pad blockSize string = let remain = BS.length string `mod` blockSize
                           toPad = blockSize - BS.length string `mod` blockSize in
                         if remain == 0 then string else
                            string `BS.append` BS.replicate toPad (fromIntegral toPad)

unpad string = let lastByte = BS.last string in
                 if verifyPad string then BS.reverse $ BS.drop (fromIntegral lastByte) $ BS.reverse string
                 else error "invalid padding"


verifyPad :: BS.ByteString -> Bool
verifyPad string = let lastByte = BS.last string
                       reversed = BS.reverse string in
                     BS.take (fromIntegral lastByte) reversed == BS.replicate (fromIntegral lastByte) lastByte

encryptCBC :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
encryptCBC keyString initializeVector string
  | BS.length keyString `notElem` [16, 24, 32] = Left "invalid key size"
  | BS.length initializeVector /= 16 = Left "invalid initializeVector"
  | otherwise = do
      let key = CP.initAES $ BS.toStrict keyString
      let chunked = splitByBlocks string 16 --aes operates on 128 bits blocks
      Right $ BS.concat ( encrypt chunked key initializeVector )
        where encrypt :: [BS.ByteString] ->CP.AES -> BS.ByteString -> [BS.ByteString]
              encrypt [] _ _ = []
              encrypt (x:xs) key vector = let xored = x .^^ vector
                                              encrypted = BS.fromStrict $ CP.encryptECB key (BS.toStrict xored)  in
                                            encrypted : encrypt xs key encrypted

decryptCBC :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
decryptCBC keyString initializeVector string
  | BS.length keyString `notElem` [16, 24, 32] = Left "invalid key size"
  | BS.length initializeVector /= 16 = Left "invalid initializeVector"
  | otherwise = do
      let key = CP.initAES $ BS.toStrict keyString
      let chunked = splitByBlocks string 16 --aes operates on 128 bits blocks
      Right $ BS.concat ( decrypt chunked key initializeVector )
        where decrypt :: [BS.ByteString] ->CP.AES -> BS.ByteString -> [BS.ByteString]
              decrypt [] _ _ = []
              decrypt (x:xs) key vector = let decrypted = BS.fromStrict $ CP.decryptECB key (BS.toStrict x)
                                              xored = decrypted .^^ vector in
                                            xored : decrypt xs key x


test_CBC string = not (null string) QC.==> let key = C.pack "YELLOW SUBMARINE"
                                               iv = BS.take 16 $ BS.repeat 0
                                               encrypted = encryptCBC key iv (C.pack string)
                                               message = case encrypted of
                                                 Right a -> a
                                                 Left a  -> error a
                                               decrypted = decryptCBC key iv message
                                               output = case decrypted of
                                                 Right a -> a
                                                 Left a  -> error a in
                                             pad 16 ( C.pack string ) == output

generateRandomKey :: Int -> BI.ByteString
generateRandomKey seed = loop (0, mkStdGen seed) 16
                    where
                      loop :: (GHC.Word.Word8, StdGen) -> Int -> BI.ByteString
                      loop _ 0 = BI.empty
                      loop state n = let generator = randomR (0,255) (snd state) in
                        fst generator `BI.cons` loop generator (n-1)

encryptCTRsetCNT :: BI.ByteString -> BI.ByteString -> CTRmode -> Int64 -> BI.ByteString -> BI.ByteString
encryptCTRsetCNT key nonce mode cnt string = encryptBlocks cnt $ splitToBlocks 16 string
  where aesKey = CP.initAES key
        encryptBlocks :: Int64 -> [BI.ByteString] -> BI.ByteString
        encryptBlocks _ [] = BI.empty
        encryptBlocks counter (x:xs) = let keyStream = nonce `BI.append` binaryCounter
                                           binaryCounter = case mode of
                                                             LittleEndian -> BS.toStrict (BIN.encode counter)
                                                             BigEndian -> BS.toStrict $ BS.reverse (BIN.encode counter) in
                                         BS.toStrict (BS.fromStrict (CP.encryptECB aesKey keyStream) .^ BS.fromStrict x)
                                         `BI.append` encryptBlocks (counter+1) xs

encryptCTR key nonce mode string = encryptCTRsetCNT key nonce mode 0 string

decryptCTR = encryptCTR


editCTR :: BI.ByteString -> BI.ByteString -> Int -> BI.ByteString -> BI.ByteString -> BI.ByteString
editCTR key nonce offset cipherText newText = BI.take offset cipherText `BI.append` edit `BI.append` BI.drop (offset + BI.length edit) cipherText
  where padded = BI.replicate padSize 0 `BI.append` newText
        padSize = offset `mod` 16
        edit = BI.drop padSize $ encryptCTRsetCNT key nonce LittleEndian (fromIntegral offset `div` 16) padded
