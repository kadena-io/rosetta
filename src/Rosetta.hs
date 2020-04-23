{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Rosetta where

------------------------------------------------------------------------------
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Word (Word64)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Identifiers --
------------------------------------------------------------------------------

-- Uniquely identifies an account within a network.
-- All provided fields are utilized to determine this uniqueness.
data AccountIdentifier = AccountIdentifier
  { _accountIdentifier_address :: Text
  -- ^ Account address
  -- ^ Examples: cryptographic public key, some encoding of it, or username
  -- ^ TODO: this should probably correspond to the user's key in the specific ledger.
  --         i.e. the key in the coin contract.

  , _accountIdentifier_subAccount :: Maybe SubAccountIdentifier
  -- ^ An account may have state specific to a contract address (ERC-20 token)
  --   and/or a stake (delegated balance).
  -- ^ TODO: confused by the wording of this.
  -- ^ TODO: should this be used to state which contract (i.e. coin contract,
  --         some other token contract) the account refers to?
  --         Seems to be the case based on comment here:
  --            "If an account has a balance for each AccountIdentifier describing
  --             it (ex: an ERC-20 token balance on a few smart contracts)"

  , _accountIdentifier_metadata :: Maybe AccountIdentifierMetaData
  -- ^ If blockchain allows using a username model, the public key(s) owned
  --   by this address should be specified in metadata.
  -- ^ TODO: What does it mean by "public keys owned by"?
  -- ^ TODO: Since we allow for username, we should include the keyset (and
  --         which cryptographic curve it uses (? don't think we can derive this just from key set in pact)
  --         (i.e. ED25519) or guard associated with the account.
  -- ^ TODO: How will others know how to parse meta datas?
  }

instance ToJSON AccountIdentifier where
  toJSON (AccountIdentifier add someSub someMeta) =
    case (someSub, someMeta) of
      (Nothing, Nothing) -> object restOfPairs
      (Just s, Nothing) -> object (restOfPairs ++ (subAcctPair s))
      (Nothing, Just m) -> object (restOfPairs ++ (metaPair m))
      (Just s, Just m) -> object (restOfPairs ++ (subAcctPair s) ++ (metaPair m))
    where
      restOfPairs = [ "address" .= add ]
      subAcctPair :: SubAccountIdentifier -> [Pair]
      subAcctPair s = [ "sub_account" .= s ]
      metaPair :: AccountIdentifierMetaData -> [Pair]
      metaPair m = [ "metadata" .= m ]

instance FromJSON AccountIdentifier where
  parseJSON = withObject "AccountIdentifier" $ \o -> do
    add <- o .: "address"
    subAcct <- o .:? "sub_account"
    meta <- o .:? "metadata"
    return $ AccountIdentifier
      { _accountIdentifier_address = add
      , _accountIdentifier_subAccount = subAcct
      , _accountIdentifier_metadata = meta
      }


-- TODO: what information to include here?
data AccountIdentifierMetaData = AccountIdentifierMetaData ()
instance ToJSON AccountIdentifierMetaData where
  toJSON _ = object []
instance FromJSON AccountIdentifierMetaData where
  parseJSON = withObject "AccountIdentifierMetaData" $ \_ -> do
    return $ AccountIdentifierMetaData ()


------------------------------------------------------------------------------

-- Uniquely identifies a sub-account.
-- All provided fiels are utilized to determine this uniqueness.
data SubAccountIdentifier = SubAccountIdentifier
  { _subAccountIdentifier_address :: Text
  -- ^ A cryptographic value or other identifier
  , _subAccountIdentifier_metadata :: Maybe SubAccountIdentifierMetaData
  -- ^ Defined when an address is not sufficient to uniquely specify a sub-account.
  }

instance ToJSON SubAccountIdentifier where
  toJSON (SubAccountIdentifier add someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ (metaPair m))
    where
      restOfPairs = [ "address" .= add ]
      metaPair :: SubAccountIdentifierMetaData -> [Pair]
      metaPair m = [ "metadata" .= m ]

instance FromJSON SubAccountIdentifier where
  parseJSON = withObject "SubAccountIdentifier" $ \o -> do
    add <- o .: "address"
    meta <- o .:? "metadata"
    return $ SubAccountIdentifier
      { _subAccountIdentifier_address = add
      , _subAccountIdentifier_metadata = meta
      }


-- TODO: optional?
data SubAccountIdentifierMetaData = SubAccountIdentifierMetaData ()
instance ToJSON SubAccountIdentifierMetaData where
  toJSON _ = object []
instance FromJSON SubAccountIdentifierMetaData where
  parseJSON = withObject "SubAccountIdentifierMetaData" $ \_ -> do
    return $ SubAccountIdentifierMetaData ()

------------------------------------------------------------------------------

-- Uniquely identifies a block in a particular network
-- TODO: how to define a chain?
-- TODO: check to make sure that whenever a Block is returned, ntwork idetifier is also returned.
data BlockIdentifier = BlockIdentifier
  { _blockIdentifier_index :: Word64
  -- ^ The block height
  , _blockIdentifier_hash :: Text
  }

instance ToJSON BlockIdentifier where
  toJSON (BlockIdentifier idx hsh) =
    object [ "index" .= idx
           , "hash" .= hsh ]

instance FromJSON BlockIdentifier where
  parseJSON = withObject "BlockIdentifier" $ \o -> do
    idx <- o .: "index"
    hash <- o .: "hash"
    return $ BlockIdentifier
      { _blockIdentifier_index = idx
      , _blockIdentifier_hash = hash
      }

------------------------------------------------------------------------------

-- Allows for fetching block information without having to specify
-- the index or hash.
-- If both are ommitted, assumes the client is requesting the current block.
data PartialBlockIdentifier = PartialBlockIdentifier
  { _partialBlockIdentifier_index :: Maybe Word64
  , _partialBlockIdentifier_hash :: Maybe Text
  }

instance FromJSON PartialBlockIdentifier where
  parseJSON = withObject "PartialBlockIdentifier" $ \o -> do
    idx <- o .:? "index"
    hsh <- o .:? "hash"
    return $ PartialBlockIdentifier
      { _partialBlockIdentifier_index = idx
      , _partialBlockIdentifier_hash = hsh
      }

------------------------------------------------------------------------------

-- Specifies which network a particular object is associated with
data NetworkIdentifier = NetworkIdentifier
  { _networkIdentifier_blockchain :: Text
  -- ^ Name of the blockchain
  -- ^ TODO: "kadena"

  , _networkIdentifier_network :: Text
  -- ^ Specific chain-id or network identifier
  -- ^ TODO: up to client to determine which network-specific identifier is mainnet or testnet?

  , _networkIdentifier_subNetworkIdentifier :: Maybe SubNetworkIdentifier
  -- ^ Sharded state identifier used to query object on specific shard
  -- ^ Required for all sharded blockchains
  -- ^ TODO: Is Kadena a sharded blockchain? YES
  -- ^ TODO: how are they expecting optional values? Null or just missing? They just the field to be missing!
  --         Javascript clients may assume existance of a field key is important. Without noticing if its null.
  }

instance ToJSON NetworkIdentifier where
  toJSON (NetworkIdentifier bid netId someSubNetId) =
    case someSubNetId of
      Nothing -> object restOfPairs
      Just s -> object (restOfPairs ++ (subNetIdPair s))
    where
      restOfPairs = [ "blockchain" .= bid, "network" .= netId ]
      subNetIdPair s = [ "sub_network_identifier" .= s ]

instance FromJSON NetworkIdentifier where
  parseJSON = withObject "NetworkIdentifier" $ \o -> do
    bid <- o .: "blockchain"
    netId <- o .: "network"
    subNetId <- o .:? "sub_network_identifier"
    return $ NetworkIdentifier
      { _networkIdentifier_blockchain = bid
      , _networkIdentifier_network = netId
      , _networkIdentifier_subNetworkIdentifier = subNetId
      }

------------------------------------------------------------------------------

-- TODO: optional?
data SubNetworkIdentifier = SubNetworkIdentifier
  { _subNetworkIdentifier_network :: Text
  -- ^ TODO: "1". Represent chain number. Chains will always be numbers.
  , _subNetworkIdentifier_metadata :: Maybe SubNetworkIdentifierMetaData
  -- ^ TODO: "mainnet01"? Policy question. Do the care about forks?
  }

instance ToJSON SubNetworkIdentifier where
  toJSON (SubNetworkIdentifier sid someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ (metaPair m))
    where
      restOfPairs = [ "network" .= sid ]
      metaPair m = [ "metadata" .= m ]

instance FromJSON SubNetworkIdentifier where
  parseJSON = withObject "SubNetworkIdentifier" $ \o -> do
    sid <- o .: "network"
    m <- o .:? "metadata"
    return $ SubNetworkIdentifier
      { _subNetworkIdentifier_network = sid
      , _subNetworkIdentifier_metadata = m
      }


-- TODO: optional?
data SubNetworkIdentifierMetaData = SubNetworkIdentifierMetaData ()
instance ToJSON SubNetworkIdentifierMetaData where
  toJSON _ = object []
instance FromJSON SubNetworkIdentifierMetaData where
  parseJSON = withObject "SubNetworkIdentifierMetaData" $ \_ -> do
    return $ SubNetworkIdentifierMetaData ()

------------------------------------------------------------------------------

-- Uniquely identifies an operation within a transaction
-- TODO: No idea what an operation refers to?
-- TODO: Docs mention that not all blockchains have a notion of an operation index.
-- TODO: They don't specify what Operations are. Free
data OperationIdentifier = OperationIdentifier
  { _operationIdentifier_index :: Word64
  -- ^ Unique identifier for each operation within a transaction
  , _operationIdentifier_networkIndex :: Maybe Word64
  -- ^ Optional network index associated with a given operation index
  -- ^ Example: Bitcoin uses a network index to identity which UTXO was used in a
  --            transaction.
  -- ^ TODO: docs say that nework index should not be populated if there's no notion
  --         of an operation index in a blockchain (i.e. most account-based blockchains).
  --         Does this apply to Kadena?
  }

instance ToJSON OperationIdentifier where
  toJSON (OperationIdentifier idx someNetIdx) =
    case someNetIdx of
      Nothing -> object restOfPairs
      Just ni -> object (restOfPairs ++ (netIdxPair ni))
    where
      restOfPairs = [ "index" .= idx ]
      netIdxPair ni = [ "network_index" .= ni ]

------------------------------------------------------------------------------

-- Uniquely identifies a transaction in a particular network and block
-- or in the mempool.
data TransactionIdentifier = TransactionIdentifier
  { _transactionIdentifier_hash :: Text
  -- ^ Any transactions that are attributable only to a block (i.e. block event)
  --   should use the hash of the block as the identifier.
  -- ^ TODO: Does Kadena have transactions attributable only to a block? I don't think so.
  }

instance ToJSON TransactionIdentifier where
  toJSON (TransactionIdentifier h) =
    object [ "hash" .= h ]

instance FromJSON TransactionIdentifier where
  parseJSON = withObject "TransactionIdentifier" $ \o -> do
    hash <- o .: "hash"
    return $ TransactionIdentifier hash


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
  , _allow_errors :: [Error]
  -- ^ All Errors that this implementation could return
  }

instance ToJSON Allow where
  toJSON (Allow st typ err) =
    object [ "operation_statuses" .= st
           , "operation_types" .= typ
           , "errors" .= err ]

------------------------------------------------------------------------------

-- Some value of a currency
data Amount = Amount
  { _amount_value :: Text
  -- ^ Value of the transaction in atomic units represented as an
  --   arbitrary-sized signed integer.
  -- ^ Example: 1 BTC would be represented by a value of 100,000,000
  -- ^ TODO: don't understand the example?
  -- ^ TODO: We should model gas payments as KDA not gas units. And negative amounts refers to money leaving. Hence why they have "signed integer".
  , _amount_currency :: Currency
  -- ^ Composed of a canonical Symbol and Decimals
  -- ^ This Decimals value is used to convert an Amount.Value from atomic
  --   units (Satoshis) to standard units (Bitcoins).
  -- ^ TODO: Satoshis are the smallest denomination of bitcoin. In US, cents == Satoshis.
  -- ^ TODO: Are atomic units Satoshis? What's the conversion between that and KDA/BTC?
  , _amount_metadata :: Maybe AmountMetaData
  }

instance ToJSON Amount where
  toJSON (Amount v c someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "value" .= v, "currency" .= c ]
      metaPair m = [ "metadata" .= m ]


-- TODO: optional?
data AmountMetaData
instance ToJSON AmountMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Blocks contain an array of Transactions that occurred at a
-- particular BlockIdentifier
data Block = Block
  { _block_blockIdentifier :: BlockIdentifier
  -- ^ A unique block in a particular network
  , _block_parentBlockIdentifier :: BlockIdentifier
  -- ^ Parent block identifier
  , _block_timestamp :: Word64
  -- ^ Timestamp of the block in milliseconds since the Unix Epoch
  , _block_transactions :: [Transaction]
  , _block_metadata :: Maybe BlockMetaData
  }

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


data BlockMetaData
instance ToJSON BlockMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Composed of canonical Symbol and Decimals.
-- Decimals value is used to convert an Amount.Value from atomic units (Satoshis)
-- to standard units (Bitcoins)
data Currency = Currency
  { _currency_symbol :: Text
  -- ^ Canonical symbol associated with a currency
  , _currency_decimals :: Word
  -- ^ Number of decimal places in the standard unit representation of the amount
  -- ^ Example: BTC has 8 decimals.
  -- ^ NOTE: It's not possible to represent the value of some currency in atomic units
  --         that is not base 10.
  -- ^ TODO: What's this for KDA?
  , _currency_metadata :: Maybe CurrencyMetaData
  -- ^ Any additiona information related to the currency itself.
  -- ^ Example: It would be useful to populate this object with the contract address of
  --            an ERC-20 token.
  -- ^ TODO: meaning of "contract address" in KDA terms? Just the namespace.moduleName of token?
  }

instance ToJSON Currency where
  toJSON (Currency s d someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "symbol" .= s, "decimals" .= d ]
      metaPair m = [ "metadata" .= m ]


-- TODO: optional?
data CurrencyMetaData
instance ToJSON CurrencyMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Operations contain all balance-changing information within a transaction.
-- They are always one-sided (only affect 1 AccountIdentifier) and can succeed
-- or fail idependently from a Transaction.
-- NOTE: A tx can succeed but you
-- For every transaction that occurrs, there's at least two transaction.
-- The gas payment leaving, the miner getting rewards, and coinbase.
-- coinbase (system), all of the gas limit * price withdrawn from the user (atomic), (pact) *money comes out from tx (atomic) optional*, gas payment to miner (atomic), gas refund to gas payer (optional) 0 (atomic).
data Operation = Operation
  { _operation_operationIdentifier :: OperationIdentifier
  -- ^ Uniquely identifies an operation within a transaction
  , _operation_relatedOperations :: Maybe [OperationIdentifier]
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
  -- TODO: Is "NetworkStatus" a typo? Does it mean Allow's operationTypes?
  --       They meant NetworkOptionsResponse.
  , _operation_status :: Text
  -- ^ The network-specific status of the operation.
  -- ^ Status is not defined on the transaction object because blockchains with
  --   smart contracts may have tranasaction that particually apply.
  -- ^ Blockchains with atomic transactions (all operations succeed or all fail)
  --   will have the same status for each operation.
  -- ^ Example: "Reverted"
  -- ^ TODO: Not sure what this could be set to?
  , _operation_account :: Maybe AccountIdentifier
  , _operation_amount :: Maybe Amount
  , _operation_metadata :: Maybe OperationMetaData
  }

instance ToJSON Operation where
  toJSON op = object allPairs
    where
      relatedOperationsPair :: Maybe [OperationIdentifier] -> [Pair]
      relatedOperationsPair Nothing = []
      relatedOperationsPair (Just ops) = [ "related_operations" .= ops ]

      accountPair :: Maybe AccountIdentifier -> [Pair]
      accountPair Nothing = []
      accountPair (Just acct) = [ "account" .= acct ]

      amountPair :: Maybe Amount -> [Pair]
      amountPair Nothing = []
      amountPair (Just amt) = [ "amount" .= amt ]

      metaPair :: Maybe OperationMetaData -> [Pair]
      metaPair Nothing = []
      metaPair (Just m) = [ "metadata" .= m ]

      allPairs =
        [ "operation_identifier" .= (_operation_operationIdentifier op)
        , "type" .= (_operation_type op)
        , "status" .= (_operation_status op) ]
        ++ (relatedOperationsPair (_operation_relatedOperations op))
        ++ (accountPair (_operation_account op))
        ++ (amountPair (_operation_amount op))
        ++ (metaPair (_operation_metadata op))


-- TODO: optional
data OperationMetaData
instance ToJSON OperationMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Transactions contain an array of Operations that are attributable to the
-- same TransactionIdentifier
data Transaction = Transaction
  { _transaction_transactionIdentifier :: TransactionIdentifier
  , _transaction_operations :: [Operation]
  , _transaction_metadata :: Maybe TransactionMetaData
  -- ^ NOTE: Transactions that are related to other transactions (i.e. cross-shard
  --   transactions) should include the transaction_identifier of these transaction
  --   in the metadata.
  -- ^ TODO: Does the NOTE mean that cross-chain transactions should include the
  --         the transaction id of the transaction that burned in the previous chain?
  --         Are there other examples of "related transactions"?
  --         If this tx is the receive one, include the tx id from the initial cross-chain.
  --         Pacts in general are related transactions.
  -- ^ TODO: the list of operations, we need to show the changes in account balance.
  }

instance ToJSON Transaction where
  toJSON (Transaction i ops someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "transaction_identifier" .= i, "operations" .= ops ]
      metaPair m = [ "metadata" .= m ]


-- TODO: Optional?
data TransactionMetaData
instance ToJSON TransactionMetaData where
  toJSON _ = object []


------------------------------------------------------------------------------
-- Miscellaneous --
------------------------------------------------------------------------------

-- Enriched HTTP node error
data Error = Error
  { _error_code :: Word
  -- ^ Network-specific error code
  -- ^ Can be equivalent to an HTTP status code
  , _error_message :: Text
  -- ^ Network-specific error message
  -- ^ TODO: based on Allow specifications, message should be abstract enough?
  , _error_retriable :: Bool
  -- ^ Indicates whether the request COULD succeed if submitted again
  }

instance ToJSON Error where
  toJSON (Error c msg b) =
    object [ "code" .= c
           , "message" .= msg
           , "retriable" .= b ]

------------------------------------------------------------------------------

-- Utilized to indicate which Operation status are considered successful.
-- NOTE: Some blockchains (i.e. Bitcoin) only include successful
--       operations in blocks, but other blockchains (i.e. Ethereum) include
--       unsuccessful operations that incur a fee.
-- NOTE: Thus, it's critical to understand which Operation.Status indicate an
--       Operation is successful and should affect an Account.
-- TODO: not sure I understood that last NOTE?
data OperationStatus = OperationStatus
  { _operationStatus_status :: Text
  -- ^ Network-specific status of the operation
  , _operationStatus_successful :: Bool
  -- ^ Whether an operation is considered successful
  -- ^ Set to true if the Operation.Amount should affect the Operation.Account
  -- ^ TODO: whether an operation affects the amount in an Account?
  }

instance ToJSON OperationStatus where
  toJSON (OperationStatus s b) =
    object [ "status" .= s
           , "successful" .= b ]

------------------------------------------------------------------------------

-- A node's peer
data Peer = Peer
  { _peer_peerId :: Text
  , _peer_metadata :: Maybe PeerMetaData
  }

instance ToJSON Peer where
  toJSON (Peer i someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "peer_id" .= i ]
      metaPair m = [ "metadata" .= m ]


-- TODO: optional?
data PeerMetaData
instance ToJSON PeerMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Utilized to inform the client of the versions of different components of the
-- Rosetta implementation.
data Version = Version
  { _version_rosettaVersion :: Text
  -- ^ Version of the Rosetta interface the implementation adheres to
  -- ^ Useful for clients looking to reliably parse responses
  , _version_nodeVersion :: Text
  -- ^ The canonical version of the node runtime to help clients
  --   manage deployments
  , _version_middlewareVersion :: Maybe Text
  -- ^ Middleware version, if one is used to adhere to the Rosetta interface
  , _version_metadata :: Maybe VersionMetaData
  -- ^ Any other information that may be useful about versioning of dependent
  --   services
  }

instance ToJSON Version where
  toJSON (Version r n someMiddle someMeta) =
    case (someMiddle, someMeta) of
      (Nothing, Nothing) -> object restOfPairs
      (Just mi, Nothing) -> object (restOfPairs ++ middlePair mi)
      (Nothing, Just meta) -> object (restOfPairs ++ metaPair meta)
      (Just mi, Just meta) -> object $ restOfPairs ++ (middlePair mi) ++ (metaPair meta)
    where
      restOfPairs = [ "rosetta_version" .= r, "node_version" .= n ]
      middlePair :: Text -> [Pair]
      middlePair mi = [ "middleware_version" .= mi ]
      metaPair :: VersionMetaData -> [Pair]
      metaPair meta = [ "metadata" .= meta ]


-- TODO: optional?
data VersionMetaData
instance ToJSON VersionMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------
-- Requests and Responses --
------------------------------------------------------------------------------

-- TODO:
-- All the Request types should have FromJSON.
-- All Response should have ToJSON.


-- Utilized to make a balance request on the /account/balance endpoint.
-- NOTE: If a blockIdentifier is populated, a historical balance query
--       should be performed.
-- NOTE:  If an account has a balance for each AccountIdentifier describing it
--        (ex: an ERC-20 token balance on a few smart contracts), an account balance
--        request must be made with each AccountIdentifier.
-- TODO: What does the first NOTE mean by "historical balance query"? At a particular
--       block?
data AccountBalanceRequest = AccountBalanceRequest
  { _accountBalanceRequest_networkIdentifier :: NetworkIdentifier
  , _accountBalanceRequest_accountIdentifier :: AccountIdentifier
  , _accountBalanceRequest_blockIdentifier :: Maybe PartialBlockIdentifier
  -- ^ NOTE: when index and hash fields missing, it's assumed the client
  --         is making a request at the current block.
  }

instance FromJSON AccountBalanceRequest where
  parseJSON = withObject "AccountBalanceRequest" $ \o -> do
    netId <- o .: "network_identifier"
    acctId <- o .: "account_identifier"
    bi <- o .:? "block_identifier"
    return $ AccountBalanceRequest
      { _accountBalanceRequest_networkIdentifier = netId
      , _accountBalanceRequest_accountIdentifier = acctId
      , _accountBalanceRequest_blockIdentifier = bi
      }


-- Returned on the /account/balance endpoint
data AccountBalanceResponse = AccountBalanceResponse
  { _accountBalanceResponse_blockIdentifier :: BlockIdentifier
  , _accountBalanceResponse_balances :: [Amount]
  -- ^ A single account may have a balance in multiple currencies
  -- ^ TODO: what?? Is this referring to a ledger that keeps tracks of two tokesn for ex?
  , _accountBalanceResponse_metadata :: Maybe AccountBalanceResponseMetaData
  -- ^ Account-based blockchains that utilize a nonce or sequence number should include
  --   that number in the metadata. This number could be unique to the identifier or global
  --   across the account address.
  -- ^ TODO: Is Kadena an account-based blockchain?
  }

instance ToJSON AccountBalanceResponse where
  toJSON (AccountBalanceResponse bi bals someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "block_identifier" .= bi, "balances" .= bals ]
      metaPair m = [ "metadata" .= m ]


-- TODO: Optional?
data AccountBalanceResponseMetaData
instance ToJSON AccountBalanceResponseMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Utilized to make a block request on the /block endpoint
data BlockRequest = BlockRequest
 { _blockRequest_networkIdentifier :: NetworkIdentifier
 , _blockRequest_blockIdentifier :: PartialBlockIdentifier
 }

instance FromJSON BlockRequest where
  parseJSON = withObject "BlockRequest" $ \o -> do
    netId <- o .: "network_identifier"
    bId <- o .: "block_identifier"
    return $ BlockRequest
      { _blockRequest_networkIdentifier = netId
      , _blockRequest_blockIdentifier = bId
      }


-- Includes a fully-populated block or a partially-populated block with a list
-- of other transactions to fetch.
-- TODO: What's a partially-populated block??
--       Why do the other transactions have to be fetched?
--       You can send the block with all the transactions in it, or just the transaction hash.
data BlockResponse = BlockResponse
  { _blockResponse_block :: Block
  -- ^ Array of Transactions that occurred at a particular block
  , _blockResponse_otherTransactions :: Maybe [TransactionIdentifier]
  -- ^ NOTE: Some blockchains require additional transactions to be fetched
  --         that weren't returned in the block response (i.e. the block only
  --         returns transaction hashes). For blockchains with a lot of
  --         transaction in each block, this can be very useful as consumers
  --         can concurrently fetch all transactions returned.
  -- ^ TODO: I think we do this. But then how do we populate Block's transactions
  --         field?
  }

instance ToJSON BlockResponse where
  toJSON (BlockResponse b someOtherTxs) =
    case someOtherTxs of
      Nothing -> object restOfPairs
      Just ts -> object (restOfPairs ++ otherTxsPair ts)
    where
      restOfPairs = [ "block" .= b ]
      otherTxsPair ts = [ "other_transactions" .= ts ]


------------------------------------------------------------------------------

-- Used to fetch a Transaction included in a block that is not returned in
-- BlockResponse on the /block/transaction endpoint.
data BlockTransactionRequest = BlockTransactionRequest
  { _blockTransactionRequest_networkIdentifier :: NetworkIdentifier
  , _blockTransactionRequest_blockIdentifier :: BlockIdentifier
  , _blockTransactionRequest_transactionIdentifier :: TransactionIdentifier
  }

instance FromJSON BlockTransactionRequest where
  parseJSON = withObject "BlockTransactionRequest" $ \o -> do
    netId <- o .: "network_identifier"
    bId <- o .: "block_identifier"
    txId <- o .: "transaction_identifier"
    return $ BlockTransactionRequest
      { _blockTransactionRequest_networkIdentifier = netId
      , _blockTransactionRequest_blockIdentifier = bId
      , _blockTransactionRequest_transactionIdentifier = txId
      }


-- Contains information about a block transaction
data BlockTransactionResponse = BlockTransactionResponse
  { _blockTransactionResponse_transaction :: Transaction
  }

instance ToJSON BlockTransactionResponse where
  toJSON (BlockTransactionResponse tx) = object [ "transaction" .= tx ]


------------------------------------------------------------------------------

-- Utilized to get information required to construct a transaction
-- on the /construction/metadata endpoint.
-- TODO: Still unsure of what this endpoints does. Need to review endpoints.
data ConstructionMetadataRequest = ConstructionMetadataRequest
  { _constructionMetadataRequest_networkIdentifier :: NetworkIdentifier
  , _constructionMetadataRequest_options :: ConstructionMetadataOptions
  -- ^ Specifies which metadata to return
  -- ^ NOTE: Some blockchains require different metadata for different types of
  --         transaction construction (i.e. delegation vs transfer). Instead of
  --         requiring a blockchain node to return all possible types of metadata
  --         for construction (which may require multiple node fetches), the client
  --         can populate an optios object to limit the metadata returned to only
  --         the subset required.
  }

instance FromJSON ConstructionMetadataRequest where
  parseJSON = withObject "ConstructionMetadataRequest" $ \o -> do
    netId <- o .: "network_identifier"
    opts <- o .: "options"
    return $ ConstructionMetadataRequest
      { _constructionMetadataRequest_networkIdentifier = netId
      , _constructionMetadataRequest_options = opts
      }


-- TODO
data ConstructionMetadataOptions = ConstructionMetadataOptions ()
instance FromJSON ConstructionMetadataOptions where
  parseJSON = withObject "ConstructionMetadataOptions" $ \_ -> do
    return $ ConstructionMetadataOptions ()


-- Returns network-specific metadata used for transaction construction.
data ConstructionMetadataResponse = ConstructionMetadataResponse
  { _constructionMetadataResponse_metadata :: ConstructionMetadataResponseMetaData
  -- ^ NOTE: It's likely that the client will not inspect this metadata before
  --         passing it to a client SDK that uses it for construction.
  -- TODO: How will this json object be used by client SDK??
  }

instance ToJSON ConstructionMetadataResponse where
  toJSON (ConstructionMetadataResponse m) =
    object [ "metadata" .= m ]

-- TODO
data ConstructionMetadataResponseMetaData
instance ToJSON ConstructionMetadataResponseMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Utilized to submit a signed transaction on the /construction/submit endpoint
data ConstructionSubmitRequest = ConstructionSubmitRequest
  { _constructionSubmitRequest_networkIdentifier :: NetworkIdentifier
  , _constructionSubmitRequest_signedTransaction :: Text
  -- ^ The signed transaction
  }

instance FromJSON ConstructionSubmitRequest where
  parseJSON = withObject "ConstructionSubmitRequest" $ \o -> do
    netId <- o .: "network_identifier"
    sig <- o .: "signed_transaction"
    return $ ConstructionSubmitRequest
      { _constructionSubmitRequest_networkIdentifier = netId
      , _constructionSubmitRequest_signedTransaction = sig
      }


-- Contains the transaction identifier of a submitted transaction that was
-- accepted into the mempool.
data ConstructionSubmitResponse = ConstructionSubmitResponse
  { _constructionSubmitResponse_transactionIdentifier :: TransactionIdentifier
  , _constructionSubmitResponse_metadata :: Maybe ConstructionSubmitResponseMetaData
  }

instance ToJSON ConstructionSubmitResponse where
  toJSON (ConstructionSubmitResponse txId someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "transaction_identifier" .= txId ]
      metaPair m = [ "metadata" .= m ]


-- TODO: Optional?
data ConstructionSubmitResponseMetaData
instance ToJSON ConstructionSubmitResponseMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Utilized to retrieve all transaction identifiers in the mempool for a
-- particular network on the /mempool endpoint.
data MempoolRequest = MempoolRequest
  { _mempoolRequest_networkIdentifier :: NetworkIdentifier
  }

instance FromJSON MempoolRequest where
  parseJSON = withObject "MempoolRequest" $ \o -> do
    netId <- o .: "network_identifier"
    return $ MempoolRequest netId


-- Contains all transaction identifiers in the mempool for a particular network.
data MempoolResponse = MempoolResponse
  { _mempoolResponse_transactionIdentifiers :: [TransactionIdentifier]
  }

instance ToJSON MempoolResponse where
  toJSON (MempoolResponse txs) =
    object [ "transaction_identifiers" .= txs ]

------------------------------------------------------------------------------

-- Utilized to retrieve a transaction from the mempool on
-- the /mempool/transaction endpoint
data MempoolTransactionRequest = MempoolTransactionRequest
  { _mempoolTransactionRequest_networkIdentifier :: NetworkIdentifier
  , _mempoolTransactionRequest_transactionIdentifier :: TransactionIdentifier
  }

instance FromJSON MempoolTransactionRequest where
  parseJSON = withObject "MempoolTransactionRequest" $ \o -> do
    netId <- o .: "network_identifier"
    tx <- o .: "transaction_identifier"
    return $ MempoolTransactionRequest
      { _mempoolTransactionRequest_networkIdentifier = netId
      , _mempoolTransactionRequest_transactionIdentifier = tx
      }


-- Contains an estimate of a mempool transaction.
-- NOTE: It may not be possible to know the full impact of a transaction in
--       the mempool (i.e. fee paid)
data MempoolTransactionResponse = MempoolTransactionResponse
  { _mempoolTransactionResponse_transaction :: Transaction
  , _mempoolTransactionResponse_metadata :: Maybe MempoolTransactionResponseMetaData
  }

instance ToJSON MempoolTransactionResponse where
  toJSON (MempoolTransactionResponse tx someMeta) =
    case someMeta of
      Nothing -> object restOfPairs
      Just m -> object (restOfPairs ++ metaPair m)
    where
      restOfPairs = [ "transaction" .= tx ]
      metaPair m = [ "metadata" .= m ]


-- TODO: Optional?
data MempoolTransactionResponseMetaData
instance ToJSON MempoolTransactionResponseMetaData where
  toJSON _ = object []

------------------------------------------------------------------------------

-- Utilized in any request where the only argument is optional metadata
-- TODO: Which endpoint(s) use this?? I think its only used in /network/list
data MetadataRequest = MetadataRequest
  { _metadataRequest_metadata :: Maybe MetadataRequestMetaData
  }

instance FromJSON MetadataRequest where
  parseJSON = withObject "MetadataRequest" $ \o -> do
    m <- o .:? "metadata"
    return $ MetadataRequest m

-- TODO
data MetadataRequestMetaData = MetadataRequestMetaData ()
instance FromJSON MetadataRequestMetaData where
  parseJSON = withObject "MetadataRequestMetaData" $ \_ -> do
    return $ MetadataRequestMetaData ()


------------------------------------------------------------------------------

-- Contains all network identifiers that the node can server information for.
-- The response from the /network/list endpoint
data NetworkListResponse = NetworkListResponse
  { _networkListResponse_networkIdentifiers :: [NetworkIdentifier]
  }

instance ToJSON NetworkListResponse where
  toJSON (NetworkListResponse netIds) =
    object [ "network_identifiers" .= netIds ]


------------------------------------------------------------------------------

-- Contains information about the versioning of the node and the allowed
-- operation statuses, operation types, and errors.
data NetworkOptionsResponse = NetworkOptionsResponse
  { _networkOptionsResponse_version :: Version
  , _networkOptionsResponse_allow :: Allow
  }

instance ToJSON NetworkOptionsResponse where
  toJSON (NetworkOptionsResponse v allow) =
    object [ "version" .= v
           , "allow" .= allow ]

------------------------------------------------------------------------------

-- Utilized to retrieve some data specific exclusively to a network identifier.
data NetworkRequest = NetworkRequest
  { _networkRequest_networkIdentifier :: NetworkIdentifier
  , _networkRequest_metadata :: Maybe NetworkRequestMetaData
  }

instance FromJSON NetworkRequest where
  parseJSON = withObject "NetworkRequest" $ \o -> do
    netId <- o .: "network_identifier"
    m <- o .:? "metadata"
    return $ NetworkRequest
      { _networkRequest_networkIdentifier = netId
      , _networkRequest_metadata = m
      }

-- TODO: optional?
data NetworkRequestMetaData = NetworkRequestMetaData ()
instance FromJSON NetworkRequestMetaData where
  parseJSON = withObject "NetworkRequestMetaData" $ \_ -> do
    return $ NetworkRequestMetaData ()

------------------------------------------------------------------------------

-- Contains basic information about the node's view of a blockchain network
data NetworkStatusResponse = NetworkStatusResponse
  { _NetworkStatusResponse_currentBlockIdentifier :: BlockIdentifier
  , _NetworkStatusResponse_currentBlockTimestamp :: Word64
  -- ^ Timestamp of the block in milliseconds since the Unix Epoch.
  , _NetworkStatusResponse_genesisBlockIdentifier :: BlockIdentifier
  , _NetworkStatusResponse_peers :: [Peer]
  }

instance ToJSON NetworkStatusResponse where
  toJSON (NetworkStatusResponse currBlockId currBlockTime genesis peers) =
    object [ "current_block_identifier" .= currBlockId
           , "current_block_timestamp" .= currBlockTime
           , "genesis_block_identifier" .= genesis
           , "peers" .= peers ]
