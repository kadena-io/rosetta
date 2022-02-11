{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Rosetta
-- Copyright : (c) Kadena LLC 2020
-- License   : BSD3
-- Maintainer: Linda Ortega <linda.ortega2013@gmail.com>
--
-- Haskell types for the Rosetta API.

module Rosetta
  ( -- * Data Types
    rosettaSpecVersion
    -- ** Core
  , Amount(..)
  , Block(..)
  , Coin(..)
  , CoinAction(..)
  , CoinChange(..)
  , Currency(..)
  , CurveType(..)
  , Operation(..)
  , RosettaPublicKey(..)
  , RosettaSignature(..)
  , RosettaSignatureType(..)
  , RosettaSigningPayload(..)
  , Transaction(..)
    -- ** Identifiers
  , AccountId(..)
  , SubAccountId(..)
  , BlockId(..)
  , PartialBlockId(..)
  , CoinId(..)
  , NetworkId(..)
  , SubNetworkId(..)
  , OperationId(..)
  , TransactionId(..)
    -- * Miscellaneous
  , Allow(..)
  , RosettaError(..)
  , OperationStatus(..)
  , RosettaNodePeer(..)
  , SyncStatus(..)
  , RosettaNodeVersion(..)
    -- * Wire Types
    -- ** Accounts
  , AccountBalanceReq(..)
  , AccountBalanceResp(..)
    -- ** Blocks
  , BlockReq(..)
  , BlockResp(..)
  , BlockTransactionReq(..)
  , BlockTransactionResp(..)
    -- ** Construction
  , TransactionIdResp(..)
  , ConstructionCombineReq(..)
  , ConstructionCombineResp(..)
  , ConstructionDeriveReq(..)
  , ConstructionDeriveResp(..)
  , ConstructionHashReq(..)
  , ConstructionMetadataReq(..)
  , ConstructionMetadataResp(..)
  , ConstructionParseReq(..)
  , ConstructionParseResp(..)
  , ConstructionPayloadsReq(..)
  , ConstructionPayloadsResp(..)
  , ConstructionPreprocessReq(..)
  , ConstructionPreprocessResp(..)
  , ConstructionSubmitReq(..)
    -- ** Mempool
  , MempoolResp(..)
  , MempoolTransactionReq(..)
  , MempoolTransactionResp(..)
    -- ** Network
  , MetadataReq(..)
  , NetworkReq(..)
  , NetworkListResp(..)
  , NetworkOptionsResp(..)
  , NetworkStatusResp(..)
  ) where

------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Word (Word64)
import Data.String (fromString)
import qualified Data.Text as T

import GHC.Generics (Generic)

------------------------------------------------------------------------------

rosettaSpecVersion :: Text
rosettaSpecVersion = "1.4.4"

------------------------------------------------------------------------------
-- Identifiers --
------------------------------------------------------------------------------

-- Uniquely identifies an account within a network.
-- All provided fields are utilized to determine this uniqueness.
data AccountId = AccountId
  { _accountId_address :: Text
  -- ^ Account address
  -- ^ Examples: cryptographic public key, some encoding of it, or username

  , _accountId_subAccount :: Maybe SubAccountId
  -- ^ An account may have state specific to a contract address (ERC-20 token)
  --   and/or a stake (delegated balance).

  , _accountId_metadata :: Maybe Object
  -- ^ If blockchain allows using a username model, the public key(s) owned
  --   by this address should be specified in metadata.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON AccountId where
  toJSON (AccountId add someSub someMeta) =
    case (someSub, someMeta) of
      (Nothing, Nothing) -> object restOfPairs
      (Just s, Nothing) -> object (restOfPairs ++ (subAcctPair s))
      (Nothing, Just m) -> object (restOfPairs ++ (metaPair m))
      (Just s, Just m) -> object (restOfPairs ++ (subAcctPair s) ++ (metaPair m))
    where
      restOfPairs = [ "address" .= add ]
      subAcctPair :: SubAccountId -> [Pair]
      subAcctPair s = [ "sub_account" .= s ]
      metaPair :: Object -> [Pair]
      metaPair m = [ "metadata" .= m ]

instance FromJSON AccountId where
  parseJSON = withObject "AccountId" $ \o -> do
    add <- o .: "address"
    subAcct <- o .:? "sub_account"
    meta <- o .:? "metadata"
    return $ AccountId
      { _accountId_address = add
      , _accountId_subAccount = subAcct
      , _accountId_metadata = meta
      }

------------------------------------------------------------------------------

-- Uniquely identifies a sub-account.
-- All provided fiels are utilized to determine this uniqueness.
-- Context: An account may have state specified to a contract address (ERC-20 token)
--          and/or a stake (delegated balance).
-- NOTE: A sub-account identifier should specify which state (if applicable) an
--       account instantiation refers to.
data SubAccountId = SubAccountId
  { _subAccountId_address :: Text
  -- ^ A unique cryptographic value or other identifier (i.e. bonded).
  , _subAccountId_metadata :: Maybe Object
  -- ^ Defined when an address is not sufficient to uniquely specify a sub-account.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON SubAccountId where
  toJSON (SubAccountId add someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ (metaPair m))
    where
      restOfPairs = [ "address" .= add ]
      metaPair :: Object -> [Pair]
      metaPair m = [ "metadata" .= m ]

instance FromJSON SubAccountId where
  parseJSON = withObject "SubAccountId" $ \o -> do
    add <- o .: "address"
    meta <- o .:? "metadata"
    return $ SubAccountId
      { _subAccountId_address = add
      , _subAccountId_metadata = meta
      }

------------------------------------------------------------------------------

-- Uniquely identifies a block in a particular network
data BlockId = BlockId
  { _blockId_index :: Word64
  -- ^ The block height
  , _blockId_hash :: Text
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON BlockId where
  toJSON (BlockId idx hsh) =
    object [ "index" .= idx
           , "hash" .= hsh ]

instance FromJSON BlockId where
  parseJSON = withObject "BlockId" $ \o -> do
    idx <- o .: "index"
    hash <- o .: "hash"
    return $ BlockId
      { _blockId_index = idx
      , _blockId_hash = hash
      }

------------------------------------------------------------------------------

-- Allows for fetching block information without having to specify
-- the index or hash.
-- If both are ommitted, it's assumed that the client is requesting the
-- current block.
data PartialBlockId = PartialBlockId
  { _partialBlockId_index :: Maybe Word64
  , _partialBlockId_hash :: Maybe Text
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON PartialBlockId where
  toJSON (PartialBlockId somebhi somebhsh) =
    toJSONOmitMaybe []
    [ maybePair "index" somebhi
    , maybePair "hash" somebhsh ]

instance FromJSON PartialBlockId where
  parseJSON = withObject "PartialBlockId" $ \o -> do
    idx <- o .:? "index"
    hsh <- o .:? "hash"
    return $ PartialBlockId
      { _partialBlockId_index = idx
      , _partialBlockId_hash = hsh
      }

------------------------------------------------------------------------------

-- Uniquely identifies a Coin.
newtype CoinId = CoinId
  { _coinId_identifier :: Text
  -- ^ Should be populated with a globally unique identifier of a Coin.
  --   In Bitcoin, this identifier would be "transaction_hash:index".
  }
  deriving (Generic)
  deriving newtype (Eq, Show, NFData, Ord)
instance ToJSON CoinId where
  toJSON (CoinId i) =
    object ["identifier" .= i ]
instance FromJSON CoinId where
  parseJSON = withObject "CoinId" $ \o -> do
    i <- o .: "identifier"
    return $ CoinId i

------------------------------------------------------------------------------

-- Specifies which network a particular object is associated with
data NetworkId = NetworkId
  { _networkId_blockchain :: Text
  -- ^ Name of the blockchain
  , _networkId_network :: Text
  -- ^ Specific chain-id or network identifier
  , _networkId_subNetworkId :: Maybe SubNetworkId
  -- ^ Sharded state identifier used to query object on specific shard
  -- ^ Required for all sharded blockchains
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON NetworkId where
  toJSON (NetworkId bid netId someSubNetId) =
    case someSubNetId of
      Nothing -> object restOfPairs
      Just s -> object (restOfPairs ++ (subNetIdPair s))
    where
      restOfPairs = [ "blockchain" .= bid, "network" .= netId ]
      subNetIdPair s = [ "sub_network_identifier" .= s ]

instance FromJSON NetworkId where
  parseJSON = withObject "NetworkId" $ \o -> do
    bid <- o .: "blockchain"
    netId <- o .: "network"
    subNetId <- o .:? "sub_network_identifier"
    return $ NetworkId
      { _networkId_blockchain = bid
      , _networkId_network = netId
      , _networkId_subNetworkId = subNetId
      }

------------------------------------------------------------------------------

-- Required for blockchains with a sharded state in order to query some object
-- on a specific shard.
-- Optional for all non-sharded blockchains.
data SubNetworkId = SubNetworkId
  { _subNetworkId_network :: Text
  , _subNetworkId_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON SubNetworkId where
  toJSON (SubNetworkId sid someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ (metaPair m))
    where
      restOfPairs = [ "network" .= sid ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON SubNetworkId where
  parseJSON = withObject "SubNetworkId" $ \o -> do
    sid <- o .: "network"
    m <- o .:? "metadata"
    return $ SubNetworkId
      { _subNetworkId_network = sid
      , _subNetworkId_metadata = m
      }

------------------------------------------------------------------------------

-- Uniquely identifies an operation within a transaction
data OperationId = OperationId
  { _operationId_index :: Word64
  -- ^ Unique identifier for each operation within a transaction.
  -- ^ NOTE: This index is only relative to the transaction and NOT
  --         GLOBAL. The operations in each transaction should start
  --         from index 0.
  -- ^ NOTE: It's possible that there may be no notion of an operation
  --         index in the blockchain being described.
  , _operationId_networkIndex :: Maybe Word64
  -- ^ Optional network index associated with a given operation index
  -- ^ Example: Bitcoin uses a network index to identity which UTXO was used in a
  --            transaction.
  -- ^ Network index should not be populated if there's no notion
  --   of an operation index in a blockchain (i.e. most account-based blockchains).
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON OperationId where
  toJSON (OperationId idx someNetIdx) =
    case someNetIdx of
      Nothing -> object restOfPairs
      Just ni -> object (restOfPairs ++ (netIdxPair ni))
    where
      restOfPairs = [ "index" .= idx ]
      netIdxPair ni = [ "network_index" .= ni ]

instance FromJSON OperationId where
  parseJSON = withObject "OperationId" $ \o -> do
    idx <- o .: "index"
    netIdx <- o .:? "network_index"
    return $ OperationId
      { _operationId_index = idx
      , _operationId_networkIndex = netIdx
      }

------------------------------------------------------------------------------

-- Uniquely identifies a transaction in a particular network and block
-- or in the mempool.
newtype TransactionId = TransactionId
  { _transactionId_hash :: Text
  -- ^ Any transactions that are attributable only to a block (i.e. block event)
  --   should use the hash of the block as the identifier.
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Ord)

instance ToJSON TransactionId where
  toJSON (TransactionId h) =
    object [ "hash" .= h ]

instance FromJSON TransactionId where
  parseJSON = withObject "TransactionId" $ \o -> do
    hash <- o .: "hash"
    return $ TransactionId hash


------------------------------------------------------------------------------
-- Objects --
------------------------------------------------------------------------------

-- Specifies which Operation status, types, and possible errors this implementation
-- supports. All others will cause client validation to error during parsing.
-- Used by clients to validate the correctness of a Rosetta Server implementation.
data Allow = Allow
  { _allow_operationStatuses :: [OperationStatus]
  -- ^ All Operation.Status this implementation supports
  , _allow_operationTypes :: [Text]
  -- ^ All Operation.Type this implementation supports
  , _allow_errors :: [RosettaError]
  -- ^ All RosettaErrors that this implementation could return
  , _allow_historicalBalanceLookup :: Bool
  -- ^ Set to true if the implementation supports querying the
  --   balance of an account at any height in the past.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON Allow where
  toJSON (Allow st typ err hist) =
    object [ "operation_statuses" .= st
           , "operation_types" .= typ
           , "errors" .= err
           , "historical_balance_lookup" .= hist ]

instance FromJSON Allow where
  parseJSON = withObject "Allow" $ \o -> do
    st <- o .: "operation_statuses"
    typ <- o .: "operation_types"
    err <- o .: "errors"
    hist <- o .: "historical_balance_lookup"
    return $ Allow
      { _allow_operationStatuses = st
      , _allow_operationTypes = typ
      , _allow_errors = err
      , _allow_historicalBalanceLookup = hist
      }

------------------------------------------------------------------------------

-- Some value of a currency
data Amount = Amount
  { _amount_value :: Text
  -- ^ Value of the transaction in atomic units represented as an
  --   arbitrary-sized signed integer.
  -- ^ NOTE: When used in the context of an Operation, set this to
  --         be how much the balance increased or decreased in this
  --         operation.
  -- ^ Example: 1 BTC would be represented by a value of 100,000,000
  , _amount_currency :: Currency
  -- ^ Composed of a canonical Symbol and Decimals
  -- ^ This Decimals value is used to convert an Amount.Value from atomic
  --   units (Satoshis) to standard units (Bitcoins).
  , _amount_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON Amount where
  toJSON (Amount v c someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "value" .= v, "currency" .= c ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON Amount where
  parseJSON = withObject "Amount" $ \o -> do
    v <- o .: "value"
    c <- o .: "currency"
    m <- o .:? "metadata"
    return $ Amount
      { _amount_value = v
      , _amount_currency = c
      , _amount_metadata = m
      }

------------------------------------------------------------------------------

-- Blocks contain an array of Transactions that occurred at a particular BlockId
-- NOTE: Blocks MUST be inalterable: once a client has requested and received
--       a block identified by a specific BlockIdentifier hash, all future calls
--       for that same BlockIdentifier must return the same block contents.
data Block = Block
  { _block_blockId :: BlockId
  -- ^ A unique block in a particular network
  , _block_parentBlockId :: BlockId
  -- ^ Parent block identifier
  , _block_timestamp :: Word64
  -- ^ Timestamp of the block in milliseconds since the Unix Epoch
  , _block_transactions :: [Transaction]
  , _block_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON Block where
  toJSON (Block bi pbi t ts someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs =
        [ "block_identifier" .= bi
        , "parent_block_identifier" .= pbi
        , "timestamp" .= t
        , "transactions" .= ts ]
      metaPair m = [ "metadata" .= m]

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> do
    bi <- o .: "block_identifier"
    pbi <- o .: "parent_block_identifier"
    t <- o .: "timestamp"
    ts <- o .: "transactions"
    m <- o .:? "metadata"
    return $ Block
      { _block_blockId = bi
      , _block_parentBlockId = pbi
      , _block_timestamp = t
      , _block_transactions = ts
      , _block_metadata = m
      }

------------------------------------------------------------------------------

-- The unique id of the Coin and the amount it represents
data Coin = Coin
  { _coin_coinIdentifier :: CoinId
  , _coin_amount :: Amount
  }
  deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON Coin where
  toJSON (Coin i amt) =
    object ["coin_identifier" .= i
           , "amount" .= amt ]
instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \o -> do
    i <- o .: "coin_identifier"
    amt <- o .: "amount"
    return $ Coin
      { _coin_coinIdentifier = i
      , _coin_amount = amt
      }

------------------------------------------------------------------------------

-- Different state changes that a Coin can undergo.
-- NOTE: Assumes that single Coin cannot be created or
--       spent more than once.
data CoinAction = CoinCreated | CoinSpent
  deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON CoinAction where
  toJSON CoinCreated = "coin_created"
  toJSON CoinSpent = "coin_spent"
instance FromJSON CoinAction where
  parseJSON = withText "CoinAction" $ \t -> do
    case t of
      "coin_created" -> return CoinCreated
      "coin_spent" -> return CoinSpent
      _ -> error $ "Invalid CoinAction: " ++ show t

------------------------------------------------------------------------------

-- Represents a change in state of some coin.
-- NOTE: This type is part of the Operation model and must be populated for
--       UTXO-based blockchains. Do NOT populate this model is a transfer is
--       account-based.
-- NOTE: This abstraction of UTXOs allows for supporting both account-based
--       transfers and UTXO-based transfers on the same blockchain.
data CoinChange = CoinChange
  { _coinChange_coinIdentifier :: CoinId
  , _coinChange_coinAction :: CoinAction
  } deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON CoinChange where
  toJSON (CoinChange i act) =
    object [ "coin_identifier" .= i
           , "coin_action" .= act ]
instance FromJSON CoinChange where
  parseJSON = withObject "CoinChange" $ \o -> do
    i <- o .: "coin_identifier"
    act <- o .: "coin_action"
    return $ CoinChange
      { _coinChange_coinIdentifier = i
      , _coinChange_coinAction = act
      }

------------------------------------------------------------------------------

-- Composed of canonical Symbol and Decimals.
-- Decimals value is used to convert an Amount.Value from atomic units (Satoshis)
-- to standard units (Bitcoins)
-- NOTE: It is not possible to represent the value of some currency in atomic
--       units that is not base 10.
data Currency = Currency
  { _currency_symbol :: Text
  -- ^ Canonical symbol associated with a currency
  , _currency_decimals :: Word
  -- ^ Number of decimal places in the standard unit representation of the amount
  -- ^ Example: BTC has 8 decimals.
  , _currency_metadata :: Maybe Object
  -- ^ Any additiona information related to the currency itself.
  -- ^ Example: It would be useful to populate this object with the contract address of
  --            an ERC-20 token.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON Currency where
  toJSON (Currency s d someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "symbol" .= s, "decimals" .= d ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \o -> do
    s <- o .: "symbol"
    d <- o .: "decimals"
    m <- o .:? "metadata"
    return $ Currency
      { _currency_symbol = s
      , _currency_decimals = d
      , _currency_metadata = m
      }

------------------------------------------------------------------------------

-- Type of cryptographic curve associated with a PublicKey
data CurveType =
    CurveSecp256k1
  -- ^ SEC Compressed curve with size of 33 bytes.
  -- ^ Description: https://secg.org/sec1-v2.pdf#subsubsection.2.3.3
  | CurveSecp256r1
  -- ^ SEC compressed curve with size of 33 bytes.
  -- ^ Description: https://secg.org/sec1-v2.pdf#subsubsection.2.3.3
  | CurveEdwards25519
  -- ^ y (255-bits) + x-sign-bit (1-bit) curve with size of 32 bytes.
  -- ^ Description: https://ed25519.cr.yp.to/ed25519-20110926.pdf
  | CurveTweedle
  -- ^ Curve with size of 64 bytes and format of:
  --   1st pk : Fq.t (32 bytes) + 2nd pk : Fq.t (32 bytes)
  -- Description: https://github.com/CodaProtocol/coda/blob/develop/rfcs/0038-rosetta-construction-api.md#marshal-keys
  deriving (Show, Eq, Generic, NFData, Ord)

instance ToJSON CurveType where
  toJSON CurveSecp256k1 = "secp256k1"
  toJSON CurveSecp256r1 = "secp256r1"
  toJSON CurveEdwards25519 = "edwards25519"
  toJSON CurveTweedle = "tweedle"
instance FromJSON CurveType where
  parseJSON = withText "CurveType" $ \t -> do
    case t of
      "secp256k1" -> return CurveSecp256k1
      "secp256r1" -> return CurveSecp256r1
      "edwards25519" -> return CurveEdwards25519
      "tweedle" -> return CurveTweedle
      _ -> error $ "Invalid CurveType: " ++ show t

------------------------------------------------------------------------------

-- Operations contain all balance-changing information within a transaction.
-- They are always one-sided (only affect 1 AccountId) and can succeed
-- or fail idependently from a Transaction.
data Operation = Operation
  { _operation_operationId :: OperationId
  -- ^ Uniquely identifies an operation within a transaction
  , _operation_relatedOperations :: Maybe [OperationId]
  -- ^ Restrict referenced operations to those whose identifier index is
  --   less than (<) the current operation's indentifier index. This
  --   ensures there exists a clear DAG-structure of relations.
  -- ^ Operations are one-sized, so one could imagine relating operations in a
  --   single transfer or linking operations in a call tree.
  , _operation_type :: Text
  -- ^ Network-specific type of the operation.
  -- ^ Ensure that any type that can be returned here is also specified in the
  --   NetworkStatus. This can be useful to downstream consumers that parse all
  --   block data.
  -- Example: "Transfer"
  , _operation_status :: Text
  -- ^ The network-specific status of the operation.
  -- ^ Status is not defined on the transaction object because blockchains with
  --   smart contracts may have tranasaction that partially apply.
  -- ^ Blockchains with atomic transactions (all operations succeed or all fail)
  --   will have the same status for each operation.
  -- ^ Example: "Reverted"
  , _operation_account :: Maybe AccountId
  , _operation_amount :: Maybe Amount
  , _operation_coinChange :: Maybe CoinChange
  -- ^ Only supported for UTXO-based transfers.
  , _operation_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON Operation where
  toJSON op = toJSONOmitMaybe
    [ "operation_identifier" .= (_operation_operationId op)
    , "type" .= (_operation_type op)
    , "status" .= (_operation_status op) ]
    [ maybePair "related_operations" (_operation_relatedOperations op)
    , maybePair "account" (_operation_account op)
    , maybePair "amount" (_operation_amount op)
    , maybePair "coin_change" (_operation_coinChange op)
    , maybePair "metadata" (_operation_metadata op) ]

instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \o -> do
    oid <- o .: "operation_identifier"
    rops <- o .:? "related_operations"
    typ <- o .: "type"
    stat <- o .: "status"
    acct <- o .:? "account"
    amt <- o .:? "amount"
    coin <- o .:? "coin_change"
    m <- o .:? "metadata"
    return $ Operation
      { _operation_operationId = oid
      , _operation_relatedOperations = rops
      , _operation_type = typ
      , _operation_status = stat
      , _operation_account = acct
      , _operation_amount = amt
      , _operation_coinChange = coin
      , _operation_metadata = m
      }

------------------------------------------------------------------------------

-- Contains a public key byte array for a particular curve scheme encoded
-- in hex.
-- NOTE: There is no PrivateKey struct as this is NEVER the concern of an
--       implementation.
data RosettaPublicKey = RosettaPublicKey
  { _rosettaPublicKey_hexBytes :: Text
  -- ^ Hex-encoded public key bytes in the format specified by the
  --   CurveType.
  , _rosettaPublicKey_curveType :: CurveType
  -- ^ Type of cryptographic curve
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaPublicKey where
  toJSON (RosettaPublicKey hex typ) =
    object [ "hex_bytes" .= hex
           , "curve_type" .= typ ]
instance FromJSON RosettaPublicKey where
  parseJSON = withObject "RosettaPublicKey" $ \o -> do
    hex <- o .: "hex_bytes"
    typ <- o .: "curve_type"
    return $ RosettaPublicKey
      { _rosettaPublicKey_hexBytes = hex
      , _rosettaPublicKey_curveType = typ
      }

------------------------------------------------------------------------------

-- Contains the payload that was signed, the public keys of the keypairs used
-- to produce the signature, the signature (encded in hex), and the signature
-- type used to sign the payload.
-- NOTE: The public key is often not known during construction of the signing
--       payloads but may be needed to combine signatures properly.
data RosettaSignature = RosettaSignature
  { _rosettaSignature_signingPayload :: RosettaSigningPayload
  , _rosettaSignature_publicKey :: RosettaPublicKey
  , _rosettaSignature_signatureType :: RosettaSignatureType
  , _rosettaSignature_hexBytes :: Text
  -- ^ hex-encoded signature of the signing payload.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaSignature where
  toJSON sig = object
    [ "signing_payload" .= _rosettaSignature_signingPayload sig
    , "public_key" .= _rosettaSignature_publicKey sig
    , "signature_type" .= _rosettaSignature_signatureType sig
    , "hex_bytes" .= _rosettaSignature_hexBytes sig
    ]

instance FromJSON RosettaSignature where
  parseJSON = withObject "RosettaSignature" $ \o -> do
    payload <- o .: "signing_payload"
    pk <- o .: "public_key"
    sigTyp <- o .: "signature_type"
    hex <- o .: "hex_bytes"
    return $ RosettaSignature
      { _rosettaSignature_signingPayload = payload
      , _rosettaSignature_publicKey = pk
      , _rosettaSignature_signatureType = sigTyp
      , _rosettaSignature_hexBytes = hex
      }

------------------------------------------------------------------------------

-- Type of a cryptographic signature scheme
data RosettaSignatureType =
    RosettaEcdsa
  -- ^ r (32-bytes) + s (32-bytes) signature type with size of 64 bytes.
  | RosettaEcdsaRecovery
  -- ^ r (32-bytes) + s (32-bytes) + v (1-byte) signature type with size of 65 bytes.
  | RosettaEd25519
  -- ^ R (32-bytes) + s (32-bytes) signature type with size of 64 bytes.
  | RosettaSchnorr1
  -- ^ r (32-bytes) + s (32-bytes) signature type with size of 64 bytes.
  -- NOTE: schnorr_1 is a EC-Schnorr signature implemented by Zilliqa where
  --       both r and s are scalars encoded as 32-bytes values, most significant byte first.
  --       Refer to Zilliqa's Schnorr Library and Zilliqa Technical Whitepaper - Appendix A:
  --       Schnorr Digital Signature for details.)
  | RosettaSchnorrPoseidon
  -- ^ r (32-bytes) + s (32-bytes) where s = Hash(1st pk + 2nd pk + r) signature
  --  type with size of 64 bytes.
  -- NOTE: schnorr_poseidon is an EC-schnorr signature with Poseidon hash function
  --       implemented by O(1) Labs where both r and s are scalars encoded as 32-bytes
  --       little-endian values. Refer to Coda's signer reference implementation:
  --       https://github.com/CodaProtocol/signer-reference/blob/master/schnorr.ml#L92
  deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaSignatureType where
  toJSON RosettaEcdsa = "ecdsa"
  toJSON RosettaEcdsaRecovery = "ecdsa_recovery"
  toJSON RosettaEd25519 = "ed25519"
  toJSON RosettaSchnorr1 = "schnorr_1"
  toJSON RosettaSchnorrPoseidon = "schnorr_poseidon"
instance FromJSON RosettaSignatureType where
  parseJSON = withText "RosettaSignatureType" $ \t -> do
    case t of
      "ecdsa" -> return RosettaEcdsa
      "ecdsa_recovery" -> return RosettaEcdsaRecovery
      "ed25519" -> return RosettaEd25519
      "schnorr_1" -> return RosettaSchnorr1
      "schnorr_poseidon" -> return RosettaSchnorrPoseidon
      _ -> error $ "Invalid RosettaSignatureType: " ++ show t

------------------------------------------------------------------------------

-- Signed by the client with the keypair associated with an address using the
-- specified SignatureType.
data RosettaSigningPayload = RosettaSigningPayload
  { _rosettaSigningPayload_address :: Maybe Text
  -- ^ DEPRECATED by account_identifier in v1.4.4.
  --   The network-specific address of the account that should sign the payload.
  , _rosettaSigningPayload_accountIdentifier :: Maybe AccountId
  , _rosettaSigningPayload_hexBytes :: Text
  -- ^ Hex-encoded payload to be signed.
  , _rosettaSigningPayload_signatureType :: Maybe RosettaSignatureType
  -- ^ Optionally populated if there is a restriction on the signature scheme that
  --   can be used to sign the payload.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaSigningPayload where
  toJSON p = toJSONOmitMaybe
    [ "hex_bytes" .= _rosettaSigningPayload_hexBytes p ]
    [ maybePair "address" (_rosettaSigningPayload_address p)
    , maybePair "account_identifier" (_rosettaSigningPayload_accountIdentifier p)
    , maybePair "signature_type" (_rosettaSigningPayload_signatureType p) ]

instance FromJSON RosettaSigningPayload where
  parseJSON = withObject "RosettaSigningPayload" $ \o -> do
    someAddr <- o .:? "address"
    someAcct <- o .:? "account_identifier"
    hex <- o .: "hex_bytes"
    typ <- o .: "signature_type"
    return $ RosettaSigningPayload
      { _rosettaSigningPayload_address = someAddr
      , _rosettaSigningPayload_accountIdentifier = someAcct
      , _rosettaSigningPayload_hexBytes = hex
      , _rosettaSigningPayload_signatureType = typ
      }

------------------------------------------------------------------------------

-- Transactions contain an array of Operations that are attributable to the
-- same TransactionId
data Transaction = Transaction
  { _transaction_transactionId :: TransactionId
  , _transaction_operations :: [Operation]
  , _transaction_metadata :: Maybe Object
  -- ^ NOTE: Transactions that are related to other transactions (i.e. cross-shard
  --   transactions) should include the transaction_identifier of these transaction
  --   in the metadata.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON Transaction where
  toJSON (Transaction i ops someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "transaction_identifier" .= i, "operations" .= ops ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    i <- o .: "transaction_identifier"
    ops <- o .: "operations"
    m <- o .:? "metadata"
    return $ Transaction
      { _transaction_transactionId = i
      , _transaction_operations = ops
      , _transaction_metadata = m
      }

------------------------------------------------------------------------------
-- Miscellaneous --
------------------------------------------------------------------------------

-- Enriched HTTP node error
data RosettaError = RosettaError
  { _error_code :: Word
  -- ^ UNIQUE network-specific error code.
  -- ^ Can be equivalent to an HTTP status code
  , _error_message :: Text
  -- ^ UNIQUE network-specific error message.
  -- ^ The message must NOT change for a given code. Any contextual
  --   information should be included in the details field.
  , _error_retriable :: Bool
  -- ^ Indicates whether the request COULD succeed if submitted again
  , _error_details :: Maybe Object
  -- ^ Optional additional context specific to the request that
  --   caused the error in addition to the standard error message.
  -- ^ Example: Sample of the stack trace or impacted account.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaError where
  toJSON (RosettaError c msg b details) = toJSONOmitMaybe
    [ "code" .= c
    , "message" .= msg
    , "retriable" .= b ]
    [ maybePair "details" details]

instance FromJSON RosettaError where
  parseJSON = withObject "RosettaError" $ \o -> do
    c <- o .: "code"
    msg <- o .: "message"
    b <- o .: "retriable"
    details <- o .:? "details"
    return $ RosettaError
      { _error_code = c
      , _error_message = msg
      , _error_retriable = b
      , _error_details = details
      }

------------------------------------------------------------------------------

-- Utilized to indicate which Operation status are considered successful.
-- NOTE: Some blockchains (i.e. Bitcoin) only include successful
--       operations in blocks, but other blockchains (i.e. Ethereum) include
--       unsuccessful operations that incur a fee.
-- NOTE: Thus, it's critical to understand which Operation.Status indicate an
--       Operation is successful and should affect an Account.
data OperationStatus = OperationStatus
  { _operationStatus_status :: Text
  -- ^ Network-specific status of the operation
  , _operationStatus_successful :: Bool
  -- ^ Whether an operation is considered successful
  -- ^ Set to true if the Operation.Amount should affect the Operation.Account
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON OperationStatus where
  toJSON (OperationStatus s b) =
    object [ "status" .= s
           , "successful" .= b ]

instance FromJSON OperationStatus where
  parseJSON = withObject "OperationStatus" $ \o -> do
    s <- o .: "status"
    b <- o .: "successful"
    return $ OperationStatus
      { _operationStatus_status = s
      , _operationStatus_successful = b
      }

------------------------------------------------------------------------------

-- A node's peer
data RosettaNodePeer = RosettaNodePeer
  { _peer_peerId :: Text
  , _peer_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaNodePeer where
  toJSON (RosettaNodePeer i someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "peer_id" .= i ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON RosettaNodePeer where
  parseJSON = withObject "RosettaNodePeer" $ \o -> do
    i <- o .: "peer_id"
    m <- o .:? "metadata"
    return $ RosettaNodePeer
      { _peer_peerId = i
      , _peer_metadata = m
      }

------------------------------------------------------------------------------

-- Provides additional context about an implementation's sync status.
-- NOTE: It's often used to indicate that an implementation is healthy, but it
--       cannot be queried until some sync phase occurs. If an implementation is
--       immediately queryable, this model is often not populated.
data SyncStatus = SyncStatus
  { _syncStatus_currentIndex :: Word64
  -- ^ Index of the last synced block in the current stage.
  , _syncStatus_targetIndex :: Maybe Word64
  -- ^ Index of the block that the implementation is attempting to sync to
  --   in the current stage.
  , _syncStatus_stage :: Maybe Text
  -- ^ The phase of the sync process (i.e. "header sync").
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON SyncStatus where
  toJSON s = toJSONOmitMaybe
    [ "current_index" .= _syncStatus_currentIndex s ]
    [ maybePair "target_index" (_syncStatus_targetIndex s)
    , maybePair "stage" (_syncStatus_stage s) ]
instance FromJSON SyncStatus where
  parseJSON = withObject "SyncStatus" $ \o -> do
    curr <- o .: "current_index"
    target <- o .:? "target_index"
    stage <- o .:? "stage"
    return $ SyncStatus
      { _syncStatus_currentIndex = curr
      , _syncStatus_targetIndex = target
      , _syncStatus_stage = stage
      }

------------------------------------------------------------------------------

-- Utilized to inform the client of the versions of different components of the
-- Rosetta implementation.
data RosettaNodeVersion = RosettaNodeVersion
  { _version_rosettaVersion :: Text
  -- ^ Version of the Rosetta interface the implementation adheres to
  -- ^ Useful for clients looking to reliably parse responses
  , _version_nodeVersion :: Text
  -- ^ The canonical version of the node runtime to help clients
  --   manage deployments
  , _version_middlewareVersion :: Maybe Text
  -- ^ Middleware version, if one is used to adhere to the Rosetta interface
  , _version_metadata :: Maybe Object
  -- ^ Any other information that may be useful about versioning of dependent
  --   services
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON RosettaNodeVersion where
  toJSON (RosettaNodeVersion r n someMiddle someMeta) =
    case (someMiddle, someMeta) of
      (Nothing, Nothing) -> object restOfPairs
      (Just mi, Nothing) -> object (restOfPairs ++ middlePair mi)
      (Nothing, Just meta) -> object (restOfPairs ++ metaPair meta)
      (Just mi, Just meta) -> object $ restOfPairs ++ (middlePair mi) ++ (metaPair meta)
    where
      restOfPairs = [ "rosetta_version" .= r, "node_version" .= n ]
      middlePair :: Text -> [Pair]
      middlePair mi = [ "middleware_version" .= mi ]
      metaPair :: Object -> [Pair]
      metaPair meta = [ "metadata" .= meta ]

instance FromJSON RosettaNodeVersion where
  parseJSON = withObject "RosettaNodeVersion" $ \o -> do
    r <- o .: "rosetta_version"
    n <- o .: "node_version"
    mi <- o .:? "middleware_version"
    meta <- o .:? "metadata"
    return $ RosettaNodeVersion
      { _version_rosettaVersion = r
      , _version_nodeVersion = n
      , _version_middlewareVersion = mi
      , _version_metadata = meta
      }

------------------------------------------------------------------------------
-- Requests and Responses --
------------------------------------------------------------------------------

-- Utilized to make a balance request on the /account/balance endpoint.
-- NOTE: If a blockId is populated, a historical balance query
--       should be performed.
-- NOTE:  If an account has a balance for each AccountId describing it
--        (ex: an ERC-20 token balance on a few smart contracts), an account balance
--        request must be made with each AccountId.
data AccountBalanceReq = AccountBalanceReq
  { _accountBalanceReq_networkId :: NetworkId
  , _accountBalanceReq_accountId :: AccountId
  , _accountBalanceReq_blockId :: Maybe PartialBlockId
  -- ^ NOTE: when index and hash fields missing, it's assumed the client
  --         is making a request at the current block.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON AccountBalanceReq where
  toJSON (AccountBalanceReq nid acct someb) =
    toJSONOmitMaybe
    [ "network_identifier" .= nid
    , "account_identifier" .= acct ]
    [ maybePair "block_identifier" someb ]

instance FromJSON AccountBalanceReq where
  parseJSON = withObject "AccountBalanceReq" $ \o -> do
    netId <- o .: "network_identifier"
    acctId <- o .: "account_identifier"
    bi <- o .:? "block_identifier"
    return $ AccountBalanceReq
      { _accountBalanceReq_networkId = netId
      , _accountBalanceReq_accountId = acctId
      , _accountBalanceReq_blockId = bi
      }


-- Returned on the /account/balance endpoint
data AccountBalanceResp = AccountBalanceResp
  { _accountBalanceResp_blockId :: BlockId
  , _accountBalanceResp_balances :: [Amount]
  -- ^ A single account may have a balance in multiple currencies
  , _accountBalanceResp_coins :: Maybe [Coin]
  -- ^ UTXO-based blockchains should return all unspent Coins owned by an
  --   account_identifier. It's highly recommended to populate this field so
  --   users of Rosetta API implementation don't need to maintain their own
  --   indexer to track their UTXOs.
  , _accountBalanceResp_metadata :: Maybe Object
  -- ^ Account-based blockchains that utilize a nonce or sequence number should include
  --   that number in the metadata. This number could be unique to the identifier or global
  --   across the account address.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON AccountBalanceResp where
  toJSON (AccountBalanceResp bi bals coins someMeta) =
    toJSONOmitMaybe
    [ "block_identifier" .= bi
    , "balances" .= bals ]
    [ maybePair "metadata" someMeta
    , maybePair "coins" coins ]

instance FromJSON AccountBalanceResp where
  parseJSON = withObject "AccountBalanceResp" $ \o -> do
    bi <- o .: "block_identifier"
    bals <- o .: "balances"
    coins <- o .:? "coins"
    m <- o .:? "metadata"
    return $ AccountBalanceResp
      { _accountBalanceResp_blockId = bi
      , _accountBalanceResp_balances = bals
      , _accountBalanceResp_coins = coins
      , _accountBalanceResp_metadata = m
      }

------------------------------------------------------------------------------

-- Utilized to make a block request on the /block endpoint
data BlockReq = BlockReq
 { _blockReq_networkId :: NetworkId
 , _blockReq_blockId :: PartialBlockId
 } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON BlockReq where
  toJSON (BlockReq nid bid) =
    object [ "network_identifier" .= nid
           , "block_identifier" .= bid ]

instance FromJSON BlockReq where
  parseJSON = withObject "BlockReq" $ \o -> do
    netId <- o .: "network_identifier"
    bId <- o .: "block_identifier"
    return $ BlockReq
      { _blockReq_networkId = netId
      , _blockReq_blockId = bId
      }


-- Includes a fully-populated block or a partially-populated block with a list
-- of other transactions to fetch.
-- NOTE: As a result of the consensus algorithm of some blockchains, blocks can be
--       omitted (i.e. certain block indexes can be skipped). If a query for one of
--       these omitted indexes is made, the response should not include a Block object.
--       It is VERY important to note that blocks MUST still form a canonical, connected
--       chain of blocks where each block has a unique index. In other words, the
--       PartialBlockIdentifier of a block after an omitted block should reference
--       the last non-omitted block.
data BlockResp = BlockResp
  { _blockResp_block :: Maybe Block
  -- ^ Array of Transactions that occurred at a particular block.
  , _blockResp_otherTransactions :: Maybe [TransactionId]
  -- ^ NOTE: Some blockchains require additional transactions to be fetched
  --         that weren't returned in the block response (i.e. the block only
  --         returns transaction hashes). For blockchains with a lot of
  --         transaction in each block, this can be very useful as consumers
  --         can concurrently fetch all transactions returned.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON BlockResp where
  toJSON (BlockResp someb someOtherTxs) =
    toJSONOmitMaybe []
    [ maybePair  "block" someb
    , maybePair "other_transactions" someOtherTxs ]

instance FromJSON BlockResp where
  parseJSON = withObject "BlockResp" $ \o -> do
    b <- o .:? "block"
    ts <- o .:? "other_transactions"
    return $ BlockResp
      { _blockResp_block = b
      , _blockResp_otherTransactions = ts
      }

------------------------------------------------------------------------------

-- Used to fetch a Transaction included in a block that is not returned in
-- BlockResp on the /block/transaction endpoint.
data BlockTransactionReq = BlockTransactionReq
  { _blockTransactionReq_networkId :: NetworkId
  , _blockTransactionReq_blockId :: BlockId
  , _blockTransactionReq_transactionId :: TransactionId
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON BlockTransactionReq where
  toJSON (BlockTransactionReq nid bid tid) =
    object [ "network_identifier" .= nid
           , "block_identifier" .= bid
           , "transaction_identifier" .= tid ]

instance FromJSON BlockTransactionReq where
  parseJSON = withObject "BlockTransactionReq" $ \o -> do
    netId <- o .: "network_identifier"
    bId <- o .: "block_identifier"
    txId <- o .: "transaction_identifier"
    return $ BlockTransactionReq
      { _blockTransactionReq_networkId = netId
      , _blockTransactionReq_blockId = bId
      , _blockTransactionReq_transactionId = txId
      }


-- Contains information about a block transaction
newtype BlockTransactionResp = BlockTransactionResp
  { _blockTransactionResp_transaction :: Transaction
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Ord)

instance ToJSON BlockTransactionResp where
  toJSON (BlockTransactionResp tx) = object [ "transaction" .= tx ]

instance FromJSON BlockTransactionResp where
  parseJSON = withObject "BlockTransactionResp" $ \o -> do
    tx <- o .: "transaction"
    return $ BlockTransactionResp tx

------------------------------------------------------------------------------

-- Contains the transaction_identifier of a transaction that was submitted
-- to either /construction/hash or /construction/submit.
data TransactionIdResp = TransactionIdResp
  { _transactionIdRes_transactionIdentifier :: TransactionId
  , _transactionIdRes_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON TransactionIdResp where
  toJSON (TransactionIdResp i someMeta) = toJSONOmitMaybe
    [ "transaction_identifier" .= i ]
    [ maybePair "metadata" someMeta ]
instance FromJSON TransactionIdResp where
  parseJSON = withObject "TransactionIdRes" $ \o -> do
    i <- o .: "transaction_identifier"
    someMeta <- o .:? "metadata"
    return $ TransactionIdResp
      { _transactionIdRes_transactionIdentifier = i
      , _transactionIdRes_metadata = someMeta
      }

------------------------------------------------------------------------------

-- Utilized to create a network-specific signed transaction on
--  the /construction/combine endpoint.
-- NOTE: Contains the unsigned transaction blob returned by
--       /construction/payloads and all required signatures needed to
--       create a network transaction. 
data ConstructionCombineReq = ConstructionCombineReq
  { _constructionCombineReq_networkIdentifier :: NetworkId
  , _constructionCombineReq_unsignedTransaction :: Text
  -- ^ Unsigned transaction blob returned by /construction/payloads
  , _constructionCombineReq_signatures :: [RosettaSignature]
  -- ^ All required signatures needed to create a network transaction
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionCombineReq where
  toJSON (ConstructionCombineReq netId unsignedTx sigs) =
    object [ "network_identifier" .= netId
           , "unsigned_transaction" .= unsignedTx
           , "signatures" .= sigs ]
instance FromJSON ConstructionCombineReq where
  parseJSON = withObject "ConstructionCombineReq" $ \o -> do
    netId <- o .: "network_identifier"
    unsignedTx <- o .: "unsigned_transaction"
    sigs <- o .: "signatures"
    return $ ConstructionCombineReq
      { _constructionCombineReq_networkIdentifier = netId
      , _constructionCombineReq_unsignedTransaction = unsignedTx
      , _constructionCombineReq_signatures = sigs
      }

-- Contains a signed network transaction that can be sent directly
-- to the construction/submit endpoint.
-- Returned by the /construction/combine endpoint.
newtype ConstructionCombineResp = ConstructionCombineResp
  { _constructionCombineResp_signedTransaction :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Ord) 
instance ToJSON ConstructionCombineResp where
  toJSON (ConstructionCombineResp tx) =
    object ["signed_transaction" .= tx]
instance FromJSON ConstructionCombineResp where
  parseJSON = withObject "ConstructionCombineResp" $ \o -> do
    tx <- o .: "signed_transaction"
    return $ ConstructionCombineResp tx

------------------------------------------------------------------------------

-- Utilized to retrieve the network-specific address associated with a
-- public key on the /construction/derive endpoint.
-- NOTE: Blockchains that require an on-chain action to create an account
--       should not implement this method.
data ConstructionDeriveReq = ConstructionDeriveReq
  { _constructionDeriveReq_networkIdentifier :: NetworkId
  -- ^ Some blockchains have different address formats for different networks.
  , _constructionDeriveReq_publicKey :: RosettaPublicKey
  , _constructionDeriveReq_metadata :: Maybe Object
  -- ^ Some blockchains allow for multiple address types (i.e. different address
  --   for validators vs. normal accounts).
  } deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON ConstructionDeriveReq where
  toJSON req = toJSONOmitMaybe 
    [ "network_identifier" .= _constructionDeriveReq_networkIdentifier req
    , "public_key" .= _constructionDeriveReq_publicKey req ]
    [ maybePair "metadata" (_constructionDeriveReq_metadata req) ]
instance FromJSON ConstructionDeriveReq where
  parseJSON = withObject "ConstructionDeriveReq" $ \o -> do
    netId <- o .: "network_identifier"
    pk <- o .: "public_key"
    someMeta <- o .:? "metadata"
    return $ ConstructionDeriveReq
      { _constructionDeriveReq_networkIdentifier = netId
      , _constructionDeriveReq_publicKey = pk
      , _constructionDeriveReq_metadata = someMeta
      }

-- Returns a network-specific address
data ConstructionDeriveResp = ConstructionDeriveResp
  { _constructionDeriveResp_address :: Maybe Text
  -- ^ DEPRECATED by account_identifier in v1.4.4.
  -- ^ Address in network-specific format.
  , _constructionDeriveResp_accountIdentifier :: Maybe AccountId
  , _constructionDeriveResp_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON ConstructionDeriveResp where
  toJSON (ConstructionDeriveResp someAddr someAcct someMeta) = toJSONOmitMaybe
    []
    [ maybePair "address" someAddr
    , maybePair "account_identifier" someAcct
    , maybePair "metadata" someMeta ]
instance FromJSON ConstructionDeriveResp where
  parseJSON = withObject "ConstructionDeriveResp" $ \o -> do
    someAddr <- o .:? "address"
    someAcct <- o .:? "account_identifier"
    someMeta <- o .:? "metadata"
    return $ ConstructionDeriveResp
      { _constructionDeriveResp_address = someAddr
      , _constructionDeriveResp_accountIdentifier = someAcct
      , _constructionDeriveResp_metadata = someMeta
      }

------------------------------------------------------------------------------

-- Utilized to get the network-specific hash of a signed transaction
-- on the /construction/hash endpoint.
data ConstructionHashReq = ConstructionHashReq
  { _constructionHashReq_networkIdentifier :: NetworkId
  , _constructionHashReq_signedTransaction :: Text
  } deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON ConstructionHashReq where
  toJSON (ConstructionHashReq netId tx) =
    object [ "network_identifier" .= netId
           , "signed_transaction" .= tx ]
instance FromJSON ConstructionHashReq where
  parseJSON = withObject "ConstructionHashReq" $ \o -> do
    netId <- o .: "network_identifier"
    tx <- o .: "signed_transaction"
    return $ ConstructionHashReq
      { _constructionHashReq_networkIdentifier = netId
      , _constructionHashReq_signedTransaction = tx
      }

------------------------------------------------------------------------------

-- Utilized by the /construction/metadata endpoint to get information
-- required to construct a transaction.
-- NOTE: The request used when calling this endpoint is created by calling
--       /construction/preprocess in an offline environment.
-- NOTE: NEVER assume that the request sent to this endpoint will
--       be created by the caller or populated with any custom parameters.
--       This must occur in /construction/preprocess.
-- NOTE: This endpoint is left purposely unstructured because of the wide
--       scope of metadata that could be required.
data ConstructionMetadataReq = ConstructionMetadataReq
  { _constructionMetadataReq_networkId :: NetworkId
  , _constructionMetadataReq_options :: Object
  -- ^ Specifies which metadata to return
  -- ^ NOTE: Some blockchains require different metadata for different types of
  --         transaction construction (i.e. delegation vs transfer).
  , _constructionMetadataReq_publicKeys :: Maybe [RosettaPublicKey]
  -- ^ Optionally, the request can also include an array of PublicKeys
  -- associated with the AccountIdentifiers returned in ConstructionPreprocessResponse.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionMetadataReq where
  toJSON (ConstructionMetadataReq nid someopts somePubKeys) =
    toJSONOmitMaybe
      [ "network_identifier" .= nid
      , "options" .= someopts ]
      [ maybePair "public_keys" somePubKeys ]

instance FromJSON ConstructionMetadataReq where
  parseJSON = withObject "ConstructionMetadataReq" $ \o -> do
    netId <- o .: "network_identifier"
    opts <- o .: "options"
    somePubKeys <- o .:? "public_keys"
    return $ ConstructionMetadataReq
      { _constructionMetadataReq_networkId = netId
      , _constructionMetadataReq_options = opts
      , _constructionMetadataReq_publicKeys = somePubKeys
      }

-- Returns network-specific metadata used for transaction construction.
-- NOTE: This should not pre-construct any transactions for the client
--       (this should happen in /construction/payloads).
data ConstructionMetadataResp = ConstructionMetadataResp
  { _constructionMetadataResp_metadata :: Object
  -- ^ NOTE: It's likely that the client will not inspect this metadata before
  --         passing it to a client SDK that uses it for construction.
  , _constructionMetadataResp_suggestedFee :: Maybe [Amount]
  -- ^ Optional, suggested fee associated with the transaction being constructed.
  -- ^ NOTE: The caller may use this info to adjust the intent of the transaction
  --         or to create a transaction with a different account that can pay the
  --         suggested fee.
  -- ^ NOTE: Suggested fee is an array in case fee payment must occur in multiple
  --         currencies.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionMetadataResp where
  toJSON (ConstructionMetadataResp m fees) =
    toJSONOmitMaybe [ "metadata" .= m ]
    [ maybePair "suggested_fee" fees ]

instance FromJSON ConstructionMetadataResp where
  parseJSON = withObject "ConstructionMetadataResp" $ \o -> do
    m <- o .: "metadata"
    fees <- o .:? "suggested_fee"
    return $ ConstructionMetadataResp m fees

------------------------------------------------------------------------------

-- Utilized by the /construction/parse endpoint to understand the intent
-- of the formulated unsigned or signed transaction.
-- NOTE: This is run as a sanity check before signing (after
--       /construction/payloads), but before broadcast (after
--       /construction/combine). 
data ConstructionParseReq = ConstructionParseReq
  { _constructionParseReq_networkIdentifier :: NetworkId
  , _constructionParseReq_signed :: Bool
  -- ^ Boolean indicating whether the transaction is signed.
  , _constructionParseReq_transaction :: Text
  -- ^ This must be either the unsigned transaction blob returned
  --   by /construction/payloads or the signed transaction blob returned by
  --   /construction/combine.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionParseReq where
  toJSON req = object
    [ "network_identifier" .= _constructionParseReq_networkIdentifier req
    , "signed" .= _constructionParseReq_signed req
    , "transaction" .= _constructionParseReq_transaction req ]
instance FromJSON ConstructionParseReq where
  parseJSON = withObject "ConstructionParseReq" $ \o -> do
    netId <- o .: "network_identifier"
    signed <- o .: "signed"
    tx <- o .: "transaction"
    return $ ConstructionParseReq
      { _constructionParseReq_networkIdentifier = netId
      , _constructionParseReq_signed = signed
      , _constructionParseReq_transaction = tx
      }

-- Returns an array of operations that occur in a transaction blob. This
-- should match the array of operations proviced to /construction/preprocess
-- and /construction/payloads
data ConstructionParseResp = ConstructionParseResp
  { _constructionParseResp_operations :: [Operation]
  , _constructionParseResp_signers :: Maybe [Text]
  -- ^ DEPRECATED by account_identifier_signers in v1.4.4.
  --   All signers (addresses) of a particular transaction. If the transaction is
  --   unsigned, it should be empty.
  , _constructionParseResp_accountIdentifierSigners :: Maybe [AccountId]
  , _constructionParseResp_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionParseResp where
  toJSON req = toJSONOmitMaybe
    [ "operations" .= _constructionParseResp_operations req ]
    [ maybePair "signers" (_constructionParseResp_signers req)
    , maybePair "account_identifier_signers"
        (_constructionParseResp_accountIdentifierSigners req)
    , maybePair "metadata" (_constructionParseResp_metadata req) ]
instance FromJSON ConstructionParseResp where
  parseJSON = withObject "ConstructionParseResp" $ \o -> do
    ops <- o .: "operations"
    someSigners <- o .:? "signers"
    someAcctSigners <- o .:? "account_identifier_signers"
    someMeta <- o .:? "metadata"
    return $ ConstructionParseResp
      { _constructionParseResp_operations = ops
      , _constructionParseResp_signers = someSigners
      , _constructionParseResp_accountIdentifierSigners = someAcctSigners
      , _constructionParseResp_metadata = someMeta
      }

------------------------------------------------------------------------------

-- Utilized by the /construction/payloads endpoint to create an unsigned
-- transaction blob.
-- NOTE: The array of operations provided in transaction construction
--       often times can not specify all "effects" of a transaction
--       (consider invoked transactions in Ethereum). However, they can
--       deterministically specify the "intent" of the transaction, which
--       is sufficient for construction. For this reason, parsing the corresponding
--       transaction in the Data API (when it lands on chain) will contain a superset
--       of whatever operations were provided during construction.
data ConstructionPayloadsReq = ConstructionPayloadsReq
  { _constructionPayloadsReq_networkIdentifier :: NetworkId
  , _constructionPayloadsReq_operations :: [Operation]
  -- ^ NOTE: Subset of all operations of a transaction. 
  , _constructionPayloadsReq_metadata :: Maybe Object
  -- ^ Arbitrary metadata returned by the call to /construction/metadata.
  , _constructionPayloadsReq_publicKeys :: Maybe [RosettaPublicKey]
  -- ^ Optionally, the request can also include an array of PublicKeys
  -- associated with the AccountIdentifiers returned in ConstructionPreprocessResponse.
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionPayloadsReq where
  toJSON req = toJSONOmitMaybe
    [ "network_identifier" .= _constructionPayloadsReq_networkIdentifier req
    , "operations" .= _constructionPayloadsReq_operations req ]
    [ maybePair "metadata" (_constructionPayloadsReq_metadata req)
    , maybePair "public_keys" (_constructionPayloadsReq_publicKeys req)]
instance FromJSON ConstructionPayloadsReq where
  parseJSON = withObject "ConstructionPayloadsReq" $ \o -> do
    netId <- o .: "network_identifier"
    ops <- o .: "operations"
    someMeta <- o .:? "metadata"
    somePubKeys <- o .:? "public_keys"
    return $ ConstructionPayloadsReq
      { _constructionPayloadsReq_networkIdentifier = netId
      , _constructionPayloadsReq_operations = ops
      , _constructionPayloadsReq_metadata = someMeta
      , _constructionPayloadsReq_publicKeys = somePubKeys
      }

-- Returns an unsigned transaction blob and a collection of paylaods that
-- must be signed by particular addresses using a certain SignatureType.
data ConstructionPayloadsResp = ConstructionPayloadsResp
  { _constructionPayloadsResp_unsignedTransaction :: Text
  -- ^ Unsigned transaction blob
  -- ^ NOTE: This is usually needed to construct a network transaction from
  --         a collection of signatures.
  , _constructionPayloadsResp_payloads :: [RosettaSigningPayload]
  -- ^ An array of payloads that must be signed by the caller
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionPayloadsResp where
  toJSON req = object
    [ "unsigned_transaction" .=  _constructionPayloadsResp_unsignedTransaction req
    , "payloads" .= _constructionPayloadsResp_payloads req ]
instance FromJSON ConstructionPayloadsResp where
  parseJSON = withObject "ConstructionPayloadsResp" $ \o -> do
    unsigned <- o .: "unsigned_transaction"
    payloads <- o .: "payloads"
    return $ ConstructionPayloadsResp
      { _constructionPayloadsResp_unsignedTransaction = unsigned
      , _constructionPayloadsResp_payloads = payloads
      }

------------------------------------------------------------------------------

-- Utilized by the /construction/preprocess endpoint so that a Rosetta
-- implementation can determine which metadata it needs to request for
-- construction.
-- NOTE: Preprocess is called prior to /construction/payloads to construct a
--       request for any metadata that is needed for transaction construction.
-- NOTE: Metadata provided in this object should NEVER be a product of
--       live data (i.e. the caller must follow some network-specific data
--       strategy outside of the Construction API to populate required Metadata).
-- NOTE: If live data is required for construction, it MUST be fetched in the call
--       to /construction/metadata.
data ConstructionPreprocessReq = ConstructionPreprocessReq
  { _constructionPreprocessReq_networkIdentifier :: NetworkId
  , _constructionPreprocessReq_operations :: [Operation]
  , _constructionPreprocessReq_metadata :: Maybe Object
  -- ^ NOTE: If the Construction API implementatino has configuration options,
  --         they MUST be specified in this metadata field.
  , _constructionPreprocessReq_maxFee :: Maybe [Amount]
  -- ^ Max fee the caller is willing to pay for a transaction.
  -- ^ NOTE: An array in case fees must be paid in multiple currencies.
  , _constructionPreprocessReq_suggestedFeeMultiplier :: Maybe Double
  -- ^ Indicates how to scale the suggested fee
  -- ^ NOTE: Used to set higher fees for urgent transactions or to pay
  --         lower fees when there is less urgency.
  -- ^ NOTE: It is assumed that providing a very low multiplier (i.e. 0.0001)
  --         will never lead to a transaction being created with a fee less
  --         than the minimum network fee (if applicable).
  -- ^ NOTE: If both a max fee and a suggested fee multiplier is provided,
  --         the max fee will set an upper bound on the suggested fee
  --         (regardless of the multiplier provided).
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionPreprocessReq where
  toJSON req = toJSONOmitMaybe
    [ "network_identifier" .= _constructionPreprocessReq_networkIdentifier req
    , "operations" .= _constructionPreprocessReq_operations req]
    [ maybePair "metadata" (_constructionPreprocessReq_metadata req)
    , maybePair "max_fee" (_constructionPreprocessReq_maxFee req)
    , maybePair "suggested_fee_multiplier"
      (_constructionPreprocessReq_suggestedFeeMultiplier req) ]

instance FromJSON ConstructionPreprocessReq where
  parseJSON = withObject "ConstructionPreprocessReq" $ \o -> do
    netId <- o .: "network_identifier"
    ops <- o .: "operations"
    someMeta <- o .:? "metadata"
    maxFee <- o .:? "max_fee"
    feeMult <- o .:? "suggested_fee_multiplier"
    return $ ConstructionPreprocessReq
      { _constructionPreprocessReq_networkIdentifier = netId
      , _constructionPreprocessReq_operations = ops
      , _constructionPreprocessReq_metadata = someMeta
      , _constructionPreprocessReq_maxFee = maxFee
      , _constructionPreprocessReq_suggestedFeeMultiplier = feeMult
      }

-- Returns the request that will be sent directly and UNMODIFIED
-- to /construction/metadata (if populated) in an offline execution
-- execution environment.
data ConstructionPreprocessResp = ConstructionPreprocessResp
  { _constructionPreprocessResp_options :: Maybe Object
  -- ^ Sent directly to /construction/metadata by the caller.
  -- ^ NOTE: Set to null if it is not necessary to make a request to
  --         /construction/metadata.
  , _constructionPreprocessResp_requiredPublicKeys :: Maybe [AccountId]
  -- ^ The PublicKeys of particular AccountIdentifiers needed to construct a valid transaction.
  -- To fetch these PublicKeys, populate required_public_keys with the AccountIdentifiers
  -- associated with the desired PublicKeys. If it is not necessary to retrieve any PublicKeys
  -- for construction, required_public_keys should be omitted.
  }
  deriving (Eq, Show, Generic, NFData, Ord)
instance ToJSON ConstructionPreprocessResp where
  toJSON (ConstructionPreprocessResp opts someAccts) =
    toJSONOmitMaybe []
      [ maybePair "options" opts
      , maybePair "required_public_keys" someAccts]
instance FromJSON ConstructionPreprocessResp where
  parseJSON = withObject "ConstructionPreprocessResp" $ \o -> do
    opts <- o .:? "options"
    someAccts <- o .:? "required_public_keys"
    return $ ConstructionPreprocessResp opts someAccts

------------------------------------------------------------------------------

-- Utilized to submit a pre-signed transaction on the /construction/submit endpoint.
-- NOTE: This call should not block on the transaction being included in a block.
--       But should instead return immediately with an indication of whether or not
--       the transaction was included in the mempool (i.e. a 200 status).
data ConstructionSubmitReq = ConstructionSubmitReq
  { _constructionSubmitReq_networkId :: NetworkId
  , _constructionSubmitReq_signedTransaction :: Text
  -- ^ The signed transaction
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON ConstructionSubmitReq where
  toJSON (ConstructionSubmitReq nid ts) =
    object [ "network_identifier" .= nid
           , "signed_transaction" .= ts ]

instance FromJSON ConstructionSubmitReq where
  parseJSON = withObject "ConstructionSubmitReq" $ \o -> do
    netId <- o .: "network_identifier"
    sig <- o .: "signed_transaction"
    return $ ConstructionSubmitReq
      { _constructionSubmitReq_networkId = netId
      , _constructionSubmitReq_signedTransaction = sig
      }

------------------------------------------------------------------------------

-- NOTE: The request schema for the mempool endpoint is NetworkReq

-- Contains all transaction identifiers in the mempool for a particular network.
newtype MempoolResp = MempoolResp
  { _mempoolResp_transactionIds :: [TransactionId]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Ord)

instance ToJSON MempoolResp where
  toJSON (MempoolResp txs) =
    object [ "transaction_identifiers" .= txs ]

instance FromJSON MempoolResp where
  parseJSON = withObject "MempoolResp" $ \o -> do
    txs <- o .: "transaction_identifiers"
    return $ MempoolResp txs

------------------------------------------------------------------------------

-- Utilized to retrieve a transaction from the mempool on
-- the /mempool/transaction endpoint
data MempoolTransactionReq = MempoolTransactionReq
  { _mempoolTransactionReq_networkId :: NetworkId
  , _mempoolTransactionReq_transactionId :: TransactionId
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON MempoolTransactionReq where
  toJSON (MempoolTransactionReq nid tid) =
    object [ "network_identifier" .= nid
           , "transaction_identifier" .= tid ]

instance FromJSON MempoolTransactionReq where
  parseJSON = withObject "MempoolTransactionReq" $ \o -> do
    netId <- o .: "network_identifier"
    tx <- o .: "transaction_identifier"
    return $ MempoolTransactionReq
      { _mempoolTransactionReq_networkId = netId
      , _mempoolTransactionReq_transactionId = tx
      }


-- Contains an estimate of a mempool transaction.
-- NOTE: It may not be possible to know the full impact of a transaction in
--       the mempool (i.e. fee paid)
data MempoolTransactionResp = MempoolTransactionResp
  { _mempoolTransactionResp_transaction :: Transaction
  , _mempoolTransactionResp_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON MempoolTransactionResp where
  toJSON (MempoolTransactionResp tx someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "transaction" .= tx ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON MempoolTransactionResp where
  parseJSON = withObject "MempoolTransactionResp" $ \o -> do
    tx <- o .: "transaction"
    m <- o .: "metadata"
    return $ MempoolTransactionResp
      { _mempoolTransactionResp_transaction = tx
      , _mempoolTransactionResp_metadata = m
      }

------------------------------------------------------------------------------

-- Utilized in any request where the only argument is optional metadata
newtype MetadataReq = MetadataReq
  { _metadataReq_metadata :: Maybe Object
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Ord)

instance ToJSON MetadataReq where
  toJSON (MetadataReq someMeta) =
    toJSONOmitMaybe []
    [ maybePair "metadata" someMeta ]

instance FromJSON MetadataReq where
  parseJSON = withObject "MetadataReq" $ \o -> do
    m <- o .:? "metadata"
    return $ MetadataReq m

------------------------------------------------------------------------------

-- Contains all network identifiers that the node can server information for.
-- The response from the /network/list endpoint
newtype NetworkListResp = NetworkListResp
  { _networkListResp_networkIds :: [NetworkId]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Ord)

instance ToJSON NetworkListResp where
  toJSON (NetworkListResp netIds) =
    object [ "network_identifiers" .= netIds ]

instance FromJSON NetworkListResp where
  parseJSON = withObject "NetworkListResp" $ \o -> do
    netIds <- o .: "network_identifiers"
    return $ NetworkListResp netIds

------------------------------------------------------------------------------

-- Contains information about the versioning of the node and the allowed
-- operation statuses, operation types, and errors.
data NetworkOptionsResp = NetworkOptionsResp
  { _networkOptionsResp_version :: RosettaNodeVersion
  , _networkOptionsResp_allow :: Allow
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON NetworkOptionsResp where
  toJSON (NetworkOptionsResp v allow) =
    object [ "version" .= v
           , "allow" .= allow ]

instance FromJSON NetworkOptionsResp where
  parseJSON = withObject "NetworkOptionsResp" $ \o -> do
    v <- o .: "version"
    allow <- o .: "allow"
    return $ NetworkOptionsResp
      { _networkOptionsResp_version = v
      , _networkOptionsResp_allow = allow
      }

------------------------------------------------------------------------------

-- Utilized to retrieve some data specific exclusively to a network identifier.
data NetworkReq = NetworkReq
  { _networkReq_networkId :: NetworkId
  , _networkReq_metadata :: Maybe Object
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON NetworkReq where
  toJSON (NetworkReq nid someMeta) =
    toJSONOmitMaybe
    [ "network_identifier" .= nid ]
    [ maybePair "metadata" someMeta ]

instance FromJSON NetworkReq where
  parseJSON = withObject "NetworkReq" $ \o -> do
    netId <- o .: "network_identifier"
    m <- o .:? "metadata"
    return $ NetworkReq
      { _networkReq_networkId = netId
      , _networkReq_metadata = m
      }

------------------------------------------------------------------------------

-- Contains basic information about the node's view of a blockchain network.
-- NOTE: Assumes that any BlockIdentifier.Index less than or equal to
--       CurrentBlockIdentifier.Index can be queried.
data NetworkStatusResp = NetworkStatusResp
  { _networkStatusResp_currentBlockId :: BlockId
  , _networkStatusResp_currentBlockTimestamp :: Word64
  -- ^ Timestamp of the block in milliseconds since the Unix Epoch.
  , _networkStatusResp_genesisBlockId :: BlockId
  , _networkStatusResp_oldestBlockIdentifier :: Maybe BlockId
  -- ^ The oldest block available to query. Used when an implementation
  --   prunes historical state. If omitted, it is assumed that the
  --   genesis_block_identifier is the oldest queryable block.
  , _networkStatusResp_syncStatus :: Maybe SyncStatus
  -- ^ Helps clients monitor healthiness. Populated when an implementation
  --   performs some pre-sync before it is possible to query blocks.
  --   If omitted, it may appear that the implementation is stuck syncing and
  --   needs to be terminated.
  , _networkStatusResp_peers :: [RosettaNodePeer]
  } deriving (Eq, Show, Generic, NFData, Ord)

instance ToJSON NetworkStatusResp where
  toJSON (NetworkStatusResp blockId time genesis someOldest someSync p) =
    toJSONOmitMaybe
    [ "current_block_identifier" .= blockId
    , "current_block_timestamp" .= time
    , "genesis_block_identifier" .= genesis
    , "peers" .= p ]
    [ maybePair "oldest_block_identifier" someOldest
    , maybePair "sync_status" someSync ]

instance FromJSON NetworkStatusResp where
  parseJSON = withObject "NetworkStatusResp" $ \o -> do
    currBlockId <- o .: "current_block_identifier"
    currBlockTime <- o .: "current_block_timestamp"
    genesis <- o .: "genesis_block_identifier"
    oldest <- o .:? "oldest_block_identifier"
    sync <- o .:? "sync_status"
    peers <- o .: "peers"
    return $ NetworkStatusResp
      { _networkStatusResp_currentBlockId = currBlockId
      , _networkStatusResp_currentBlockTimestamp = currBlockTime
      , _networkStatusResp_genesisBlockId = genesis
      , _networkStatusResp_oldestBlockIdentifier = oldest
      , _networkStatusResp_syncStatus = sync
      , _networkStatusResp_peers = peers
      }

------------------------------------------------------------------------------
-- Helper function
------------------------------------------------------------------------------

maybePair :: (ToJSON a) => Text -> Maybe a -> (Text, Maybe Value)
maybePair label Nothing = (label, Nothing)
maybePair label (Just v) = (label, Just (toJSON v))

toJSONOmitMaybe :: [Pair] -> [(Text, Maybe Value)] -> Value
toJSONOmitMaybe defPairs li = object allPairs
  where
    allPairs = foldl' f defPairs li
    f :: [Pair] -> (Text, Maybe Value) -> [Pair]
    f acc (_, Nothing) = acc
    f acc (t, Just p) = acc ++ [key .= p]
      where key = fromString $ T.unpack t
