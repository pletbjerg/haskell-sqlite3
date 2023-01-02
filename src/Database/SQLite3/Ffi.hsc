{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
-- | 'Database.SQLite3.Ffi' includes foreign wrappers for various functions and
-- constants of the C API for sqlite3.
-- 
-- For an introduction to the interface, refer to the documentation here:
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/cintro.html>
module Database.SQLite3.Ffi where

#include<sqlite3.h>

import Foreign.Ptr (Ptr, FunPtr, WordPtr (WordPtr))
import qualified Foreign.Ptr as Ptr
import Foreign.C.Types
    ( CInt (CInt)
    , CDouble(CDouble)
    , CUChar
    )
import Foreign.Storable (Storable)
import Foreign.C.String (CString)
import Data.Bits (Ior (Ior))

-- * Result Codes
-- $resultCodesDoc
--
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/c_abort.html>
#{enum CInt,,
  SQLITE_OK,
  SQLITE_ERROR,
  SQLITE_INTERNAL,
  SQLITE_PERM,
  SQLITE_ABORT,
  SQLITE_BUSY,
  SQLITE_LOCKED,
  SQLITE_NOMEM,
  SQLITE_READONLY,
  SQLITE_INTERRUPT,
  SQLITE_IOERR,
  SQLITE_CORRUPT,
  SQLITE_NOTFOUND,
  SQLITE_FULL,
  SQLITE_CANTOPEN,
  SQLITE_PROTOCOL,
  SQLITE_EMPTY,
  SQLITE_SCHEMA,
  SQLITE_TOOBIG,
  SQLITE_CONSTRAINT,
  SQLITE_MISMATCH,
  SQLITE_MISUSE,
  SQLITE_NOLFS,
  SQLITE_AUTH,
  SQLITE_FORMAT,
  SQLITE_RANGE,
  SQLITE_NOTADB,
  SQLITE_NOTICE,
  SQLITE_WARNING,
  SQLITE_ROW,
  SQLITE_DONE}

-- * Open

-- | Wrapper for a pointer to the opaque @sqlite3@ structure.
-- file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/sqlite3.html
newtype Sqlite3 = Sqlite3 (Ptr Sqlite3)
  deriving stock (Show)
  deriving newtype (Eq, Ord, Storable)

-- | file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/open.html
foreign import ccall unsafe "sqlite3_open_v2" c_sqlite3_open_v2 :: 
    CString ->
    Ptr Sqlite3 ->
    SqliteOpenFlag ->
    IO CInt

-- ** Flags For File Open Operations
--
-- $flagsForFileOpenOperationsDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/c_open_autoproxy.html>

-- | 'SqliteOpenFlag' is a newtype wrapper around
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/c_open_autoproxy.html>
--
-- Merging flags is done using the 'Semigroup' instance e.g.,
--
-- @
--  'sqliteOpenReadonly' '<>' 'sqliteOpenWal'
-- @
--
-- does the bitwise OR to include both the @SQLITE_OPEN_READONLY@ and
-- @SQLITE_OPEN_WAL@ flags.
newtype SqliteOpenFlag = SqliteOpenFlag CInt
  deriving newtype (Eq, Ord, Storable)
  deriving (Semigroup, Monoid) via (Ior CInt)

#{enum SqliteOpenFlag, SqliteOpenFlag,
 SQLITE_OPEN_READONLY,
 SQLITE_OPEN_READWRITE,
 SQLITE_OPEN_CREATE,
 SQLITE_OPEN_DELETEONCLOSE,
 SQLITE_OPEN_EXCLUSIVE,
 SQLITE_OPEN_AUTOPROXY,
 SQLITE_OPEN_URI,
 SQLITE_OPEN_MEMORY,
 SQLITE_OPEN_MAIN_DB,
 SQLITE_OPEN_TEMP_DB,
 SQLITE_OPEN_TRANSIENT_DB,
 SQLITE_OPEN_MAIN_JOURNAL,
 SQLITE_OPEN_TEMP_JOURNAL,
 SQLITE_OPEN_SUBJOURNAL,
 SQLITE_OPEN_SUPER_JOURNAL,
 SQLITE_OPEN_NOMUTEX,
 SQLITE_OPEN_FULLMUTEX,
 SQLITE_OPEN_SHAREDCACHE,
 SQLITE_OPEN_PRIVATECACHE,
 SQLITE_OPEN_WAL,
 SQLITE_OPEN_NOFOLLOW,
 SQLITE_OPEN_EXRESCODE}

-- * Compiling an SQL statement
--
-- $compilingAnSqlStatementDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/prepare.html>

-- | Wrapper for a pointer to an @sqlite3_stmt@ structure.
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/stmt.html>
newtype Sqlite3Stmt = Sqlite3Stmt (Ptr Sqlite3Stmt)
  deriving stock (Show)
  deriving newtype (Eq, Ord, Storable)

-- | <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/prepare.html>
foreign import ccall unsafe "sqlite3_prepare_v2" c_sqlite3_prepare_v2 :: 
    Sqlite3 ->
    CString ->
    CInt ->
    Ptr Sqlite3Stmt ->
    Ptr CString ->
    IO CInt

-- * Binding Values to Prepared Statements
--
-- $bindingValuesToPreparedStatementsDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/bind_blob.html>

-- | @int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int n, void(*)(void*));@
foreign import ccall unsafe "sqlite3_bind_blob" c_sqlite3_bind_blob :: 
    Sqlite3Stmt ->
    CInt ->
    CString ->
    CInt -> 
    Sqlite3DestructorType -> 
    CInt

-- | @int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int n, void(*)(void*));@
-- N.B. this is a @safe@ variant of 'c_sqlite3_bind_blob' which may call back
-- to the Haskell RTS with the 'Sqlite3DestructorType'
foreign import ccall safe "sqlite3_bind_blob" c_sqlite3_bind_blob_safe :: 
    Sqlite3Stmt ->
    CInt ->
    CString ->
    CInt -> 
    Sqlite3DestructorType -> 
    CInt

-- int sqlite3_bind_blob64(sqlite3_stmt*, int, const void*, sqlite3_uint64, void(*)(void*));

-- | @int sqlite3_bind_double(sqlite3_stmt*, int, double);@
foreign import ccall unsafe "sqlite3_bind_double" c_sqlite3_bind_double :: 
    Sqlite3Stmt ->
    CInt ->
    CDouble ->
    CInt

-- | @int sqlite3_bind_int(sqlite3_stmt*, int, int);@
foreign import ccall unsafe "sqlite3_bind_int" c_sqlite3_bind_int :: 
    Sqlite3Stmt ->
    CInt ->
    CInt ->
    CInt

-- int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64);

-- | @int sqlite3_bind_null(sqlite3_stmt*, int);@
foreign import ccall unsafe "sqlite3_bind_null" c_sqlite3_bind_null :: 
    Sqlite3Stmt ->
    CInt ->
    CInt

-- | @int sqlite3_bind_text(sqlite3_stmt*,int,const char*,int,void(*)(void*));@
foreign import ccall unsafe "sqlite3_bind_text" c_sqlite3_bind_text :: 
    Sqlite3Stmt ->
    CInt ->
    CString ->
    CInt ->
    Sqlite3DestructorType ->
    CInt

-- | @int sqlite3_bind_text(sqlite3_stmt*,int,const char*,int,void(*)(void*));@
-- N.B. this is a @safe@ variant of 'c_sqlite3_bind_text' which may call back
-- to the Haskell RTS with the 'Sqlite3DestructorType'
foreign import ccall safe "sqlite3_bind_text" c_sqlite3_bind_text_safe :: 
    Sqlite3Stmt ->
    CInt ->
    CString ->
    CInt ->
    Sqlite3DestructorType ->
    CInt

-- int sqlite3_bind_text16(sqlite3_stmt*, int, const void*, int, void(*)(void*));

-- int sqlite3_bind_text64(sqlite3_stmt*, int, const char*, sqlite3_uint64, void(*)(void*), unsigned char encoding);

-- int sqlite3_bind_value(sqlite3_stmt*, int, const sqlite3_value*);

-- int sqlite3_bind_pointer(sqlite3_stmt*, int, void*, const char*,void(*)(void*));

-- int sqlite3_bind_zeroblob(sqlite3_stmt*, int, int n);

-- int sqlite3_bind_zeroblob64(sqlite3_stmt*, int, sqlite3_uint64);


-- * Evaluate An SQL Statement

-- | <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/step.html>
foreign import ccall unsafe "sqlite3_step" c_sqlite3_step :: 
    Sqlite3Stmt ->
    CInt

-- * Result Values From A Query
--
-- $resultValuesFromAQueryDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/column_blob.html>

-- | @const void *sqlite3_column_blob(sqlite3_stmt*, int iCol);@
foreign import ccall unsafe "sqlite3_column_blob" c_sqlite3_column_blob :: 
    Sqlite3Stmt ->
    CInt -> 
    Ptr CString

-- | @double sqlite3_column_double(sqlite3_stmt*, int iCol);@
foreign import ccall unsafe "sqlite3_column_double" c_sqlite3_column_double :: 
    Sqlite3Stmt ->
    CInt -> 
    CDouble

-- |@int sqlite3_column_int(sqlite3_stmt*, int iCol);@
foreign import ccall unsafe "sqlite3_column_int" c_sqlite3_column_int :: 
    Sqlite3Stmt ->
    CInt -> 
    CInt

-- sqlite3_int64 sqlite3_column_int64(sqlite3_stmt*, int iCol);

-- | @const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol);@
foreign import ccall unsafe "sqlite3_column_text" c_sqlite3_column_text :: 
    Sqlite3Stmt ->
    CInt -> 
    Ptr CUChar

-- const void *sqlite3_column_text16(sqlite3_stmt*, int iCol);

-- sqlite3_value *sqlite3_column_value(sqlite3_stmt*, int iCol);

-- int sqlite3_column_bytes(sqlite3_stmt*, int iCol);

-- int sqlite3_column_bytes16(sqlite3_stmt*, int iCol);

-- int sqlite3_column_type(sqlite3_stmt*, int iCol);

-- * Reset A Prepared Statement Object
--
-- $resetAPreparedStatementObjectDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/reset.html>

-- | int sqlite3_reset(sqlite3_stmt *pStmt);
foreign import ccall unsafe "sqlite3_reset" c_sqlite3_reset :: 
    Sqlite3Stmt ->
    CInt

-- * Destroy A Prepared Statement Object
--
-- $destroyAPreparedStatementObjectDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/finalize.html>

-- | @int sqlite3_finalize(sqlite3_stmt *pStmt);@
foreign import ccall unsafe "sqlite3_finalize" c_sqlite3_finalize :: 
    Sqlite3Stmt ->
    CInt

-- * Closing A Database Connection
--
-- $closingADatabaseConnectionDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/close.html>

-- | @int sqlite3_close(sqlite3*);@
foreign import ccall unsafe "sqlite3_close" c_sqlite3_close :: 
    Sqlite3 ->
    CInt

-- | @int sqlite3_close_v2(sqlite3*);@
foreign import ccall unsafe "sqlite3_close_v2" c_sqlite3_close_v2 :: 
    Sqlite3 ->
    CInt

-- * Error Codes And Messages
--
-- $errorCodesAndMessagesDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/errcode.html>

-- | @int sqlite3_errcode(sqlite3 *db);@
foreign import ccall unsafe "sqlite3_errcode" c_sqlite3_errcode :: 
    Sqlite3 ->
    IO CInt

-- | @int sqlite3_extended_errcode(sqlite3 *db);@
foreign import ccall unsafe "sqlite3_extended_errcode" c_sqlite3_extended_errcode :: 
    Sqlite3 ->
    IO CInt

-- | @const char *sqlite3_errmsg(sqlite3*);@
foreign import ccall unsafe "sqlite3_errmsg" c_sqlite3_errmsg :: 
    Sqlite3 ->
    IO CString

-- | @const char *sqlite3_errmsg16(sqlite3*);@
foreign import ccall unsafe "sqlite3_errmsg16" c_sqlite3_errmsg16 :: 
    Sqlite3 ->
    IO CString

-- | @const char *sqlite3_errstr(int);@
foreign import ccall unsafe "sqlite3_errstr" c_sqlite3_errstr :: 
    CInt ->
    CString

-- | @int sqlite3_error_offset(sqlite3 *db);@
foreign import ccall unsafe "sqlite3_error_offset" c_sqlite3_error_offset :: 
    Sqlite3 ->
    IO CInt

-- * Constants Defining Special Destructor Behavior
--
-- $constantsDefiningSpecialDestructorBehaviorDoc
-- <file:///nix/store/8vjwnkfabz6x4rknypgmzw48q210krlr-sqlite3-doc-3.40.00/share/doc/c3ref/c_static.html>

type Sqlite3DestructorType = FunPtr (CString -> IO ())

#{enum Sqlite3DestructorType,
 (Ptr.castPtrToFunPtr . Ptr.wordPtrToPtr . WordPtr),
 SQLITE_STATIC,
 SQLITE_TRANSIENT}
