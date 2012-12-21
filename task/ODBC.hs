import Database.HDBC.Statement
import Database.HDBC
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified ConnectionImpl as Impl
import Control.Concurrent.MVar
import Control.Monad (when)


main = do
    print "yes"
-- This may be wrong -- is SqlHandle always a pointer to something?
-- but it works with hsql so I'm going to use it here until I hear of it
-- breaking.
--newtype SqlHandle = Ptr ()

type SWORD        = CShort
type SDWORD       = CLong
type SQLSMALLINT  = SWORD 
type SQLINTEGER   = SDWORD 
type SQLRETURN    = SQLSMALLINT
type UDWORD       = CULong
type UWORD        = CUShort;
type SQLUSMALLINT = UWORD

sql_HANDLE_DBC        = 2
sql_HANDLE_DESC       = 4
sql_HANDLE_ENV        = 1
sql_HANDLE_STMT       = 3
sql_DRIVER_NOPROMPT   = 0
sql_ATTR_ODBC_VERSION = 200
sql_DBMS_VER          = 18
sql_DBMS_NAME         = 17

data CEnv = CEnv
type WrappedCEnv = Ptr CEnv
type Env = ForeignPtr WrappedCEnv

data CConn = CConn
type WrappedCConn = Ptr CConn
type Conn = ForeignPtr WrappedCConn

data CStmt = CStmt
type WrappedCStmt = Ptr CStmt
type Stmt = ForeignPtr WrappedCStmt

data SqlHandleT = EnvHandle (Ptr CEnv)
                | DbcHandle (Ptr CConn)
                | StmtHandle (Ptr CStmt)

connectODBC :: String -> IO Impl.Connection
connectODBC args = B.useAsCStringLen (BUTF8.fromString args) $ \(cs, cslen) -> 
                   alloca $ \(penvptr::Ptr (Ptr CEnv)) ->
                   alloca $ \(pdbcptr::Ptr (Ptr CConn)) ->
         do -- Create the Environment Handle
            rc1 <- sqlAllocHandle sql_HANDLE_ENV
                                  nullPtr  -- {const SQL_NULL_HANDLE}
                                   (castPtr penvptr)
            envptr <- peek penvptr 

            checkError "connectODBC/alloc env" (EnvHandle envptr) rc1
            sqlSetEnvAttr envptr sql_ATTR_ODBC_VERSION
                             (getSqlOvOdbc3) 0

            -- Create the DBC handle.
            sqlAllocHandle sql_HANDLE_DBC (castPtr envptr) 
                               (castPtr pdbcptr)
                          >>= checkError "connectODBC/alloc dbc"
                                  (EnvHandle envptr)
            dbcptr <- peek pdbcptr
            wrappeddbcptr <- wrapconn dbcptr envptr nullPtr
            fdbcptr <- newForeignPtr sqlFreeHandleDbc_ptr wrappeddbcptr

            -- Now connect.
            sqlDriverConnect dbcptr nullPtr cs (fromIntegral cslen)
                             nullPtr 0 nullPtr
                             sql_DRIVER_NOPROMPT
                              >>= checkError "connectODBC/sqlDriverConnect" 
                                  (DbcHandle dbcptr)
            mkConn args fdbcptr

mkConn :: String -> Conn -> IO Impl.Connection
mkConn args iconn = withConn iconn $ \cconn -> 
                    alloca $ \plen ->
                    alloca $ \psqlusmallint ->
                    allocaBytes 128 $ \pbuf -> 
    do 
       children <- newMVar []
       sqlGetInfo cconn sql_DBMS_VER (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo sql_DBMS_VER" (DbcHandle cconn)
       len <- peek plen
       serverver <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn SQL_DRIVER_VER (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo SQL_DRIVER_VER" (DbcHandle cconn)
       len <- peek plen
       proxiedclientver <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn sql_ODBC_VER (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo sql_ODBC_VER" (DbcHandle cconn)
       len <- peek plen
       clientver <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn sql_DBMS_NAME (castPtr pbuf) 127 plen
         >>= checkError "sqlGetInfo sql_DBMS_NAME" (DbcHandle cconn)
       len <- peek plen
       clientname <- peekCStringLen (pbuf, fromIntegral len)

       sqlGetInfo cconn SQL_TXN_CAPABLE (castPtr psqlusmallint)
                      0 nullPtr
         >>= checkError "sqlGetInfo SQL_TXN_CAPABLE" (DbcHandle cconn)
       txninfo <- ((peek psqlusmallint)::IO (SQLUSMALLINT))
       let txnsupport = txninfo /= SQL_TC_NONE

       when txnsupport
         (disableAutoCommit cconn
          >>= checkError "sqlSetConnectAttr" (DbcHandle cconn)
         )
       return $ Impl.Connection {
                            Impl.getQueryInfo = fGetQueryInfo iconn children,
                            Impl.disconnect = fdisconnect iconn children,
                            Impl.commit = fcommit iconn,
                            Impl.rollback = frollback iconn,
                            Impl.run = frun iconn children,
                            Impl.prepare = newSth iconn children,
                            Impl.clone = connectODBC args,
                            -- FIXME: add clone
                            Impl.hdbcDriverName = "odbc",
                            Impl.hdbcClientVer = clientver,
                            Impl.proxiedClientName = clientname,
                            Impl.proxiedClientVer = proxiedclientver,
                            Impl.dbServerVer = serverver,
                            Impl.dbTransactionSupport = txnsupport,
                            Impl.getTables = fgettables iconn,
                            Impl.describeTable = fdescribetable iconn
                           }


checkError :: String -> SqlHandleT -> SQLRETURN -> IO ()
checkError msg o res =
        do let rc = sqlSucceeded res
           if rc == 0
               then raiseError msg res o
               else return ()

raiseError :: String -> SQLRETURN -> SqlHandleT -> IO a
raiseError msg code cconn =
    do info <- getdiag ht hp 1 
       throwSqlError $ SqlError {seState = show (map fst info),
                                 seNativeError = fromIntegral code,
                                 seErrorMsg = msg ++ ": " ++  
                                         show (map snd info)}
       where (ht, hp::(Ptr ())) = case cconn of
                          EnvHandle c -> (sql_HANDLE_ENV, castPtr c)
                          DbcHandle c -> (sql_HANDLE_DBC, castPtr c)
                          StmtHandle c -> (sql_HANDLE_STMT, castPtr c)
             getdiag ht hp irow = allocaBytes 6 $ \csstate ->
                                  alloca $ \pnaterr ->
                                  allocaBytes 1025 $ \csmsg ->
                                  alloca $ \pmsglen ->
                 do ret <- sqlGetDiagRec ht hp irow csstate pnaterr
                           csmsg 1024 pmsglen
                    if sqlSucceeded ret == 0
                       then return []
                       else do state <- peekCStringLen (csstate, 5)
                               nat <- peek pnaterr
                               msglen <- peek pmsglen
                               msgbs <- B.packCStringLen (csmsg,
                                                          fromIntegral msglen)
                               let msg = BUTF8.toString msgbs
                               next <- getdiag ht hp (irow + 1)
                               return $ (state, 
                                         (show nat) ++ ": " ++ msg) : next
isOK :: SQLRETURN -> Bool
isOK r = sqlSucceeded r /= 0

-- define SQL_SUCCEEDED(rc) (((rc)&(~1))==0)
sqlSucceeded::SQLRETURN -> CShort
sqlSucceeded ret = 
    ret .&. complement 1

foreign import stdcall unsafe "sql.h SQLGetDiagRec"
  sqlGetDiagRec :: SQLSMALLINT -> Ptr () -> 
                   SQLSMALLINT -> CString -> Ptr (SQLINTEGER)
                   -> CString -> SQLSMALLINT 
                   -> Ptr (SQLSMALLINT) -> IO SQLRETURN

foreign import stdcall unsafe "sql.h SQLSetEnvAttr"
  sqlSetEnvAttr :: Ptr CEnv -> SQLINTEGER -> 
                   Ptr () -> SQLINTEGER -> IO SQLRETURN

foreign import stdcall unsafe "sql.h SQLDriverConnect"
  sqlDriverConnect :: Ptr CConn -> Ptr () -> CString -> SQLSMALLINT
                   -> CString -> SQLSMALLINT
                   -> Ptr SQLSMALLINT -> SQLUSMALLINT
                   -> IO SQLRETURN

foreign import stdcall unsafe "sql.h SQLAllocHandle"
  sqlAllocHandle :: SQLSMALLINT -> Ptr () -> 
                    Ptr () -> IO (SQLRETURN)

foreign import ccall unsafe "hdbc-odbc-helper.h getSqlOvOdbc3"
  getSqlOvOdbc3 :: Ptr ()

foreign import ccall unsafe "hdbc-odbc-helper.h sqlFreeHandleDbc_app"
  sqlFreeHandleDbc_app :: Ptr WrappedCConn -> IO (SQLRETURN)

foreign import ccall unsafe "hdbc-odbc-helper.h &sqlFreeHandleDbc_finalizer"
  sqlFreeHandleDbc_ptr :: FunPtr (Ptr WrappedCConn -> IO ())

foreign import ccall unsafe "hdbc-odbc-helper.h wrapobjodbc_extra"
  wrapconn :: Ptr CConn -> Ptr CEnv -> Ptr WrappedCConn -> IO (Ptr WrappedCConn)


