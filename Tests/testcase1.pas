unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, SQLDB, mysql55conn, SQLite3Conn,
  Helper;

const
  SQLScriptsBaseDir = 'db-updates';

type

  TDBEngine = (dbSqlite3=1, dbMysql55);

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  protected
    Engine: TDBEngine;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParams;
    procedure TestDatabaseUpgrade;
    procedure TestDatabaseUpgradeFailures;
    procedure TestDifferentFileNames;
    procedure TestBuiltInScripts;
    procedure TestResourceScripts;
  private
    Conn: TSQLConnection;
    Query: TSQLQuery;
    Trans: TSQLTransaction;
    DBHlp: TDBHelper;
    DBVer: TDBVersioningHelper;

    procedure UpgradeToIncorrectVersion;
  end;

  { TSqlite3Test }

  TSqlite3Test = class(TTestCase1)
  public
    constructor Create; override;
  end;

  { TMysql55Test }

  TMysql55Test = class(TTestCase1)
  public
    constructor Create; override;
  end;

implementation

uses
  Forms, FileUtil, DatabaseVersioning, IniFiles;

{ TSqlite3Test }

constructor TSqlite3Test.Create;
begin
  Engine:=dbSqlite3;

  inherited Create;
end;

{ TMysql55Test }

constructor TMysql55Test.Create;
begin
  Engine:=dbMysql55;

  inherited Create;
end;

procedure TTestCase1.TestDatabaseUpgrade;
var
  Num: Integer;
begin
  //DBVer.SQLDir := ConcatPaths([SQLScriptsBaseDir, 'test01']);
  FreeAndNil(DBVer);
  DBVer := TDBVersioningHelper.Create(Conn, Trans, ConcatPaths([SQLScriptsBaseDir, 'test01']));


  AssertEquals(0, DBVer.GetCurrentDBVersion);
  AssertEquals(0, DBHlp.TablesCount);
  AssertTrue(DBVer.UpgradeNeeded);
  AssertFalse(DBVer.IsInitialized);

  DBVer.UpgradeTo(5); // --> 5
  AssertEquals(5, DBVer.GetCurrentDBVersion);
  AssertFalse(DBHlp.TableExists('t'));
  AssertTrue(DBHlp.TableExists('t1'));
  AssertTrue(DBHlp.ColumnExists('t1', 'flag'));
  AssertTrue(DBHlp.ColumnExists('t1', 'x'));
  AssertTrue(DBHlp.ColumnExists('t1', 'y'));
  AssertEquals(5, DBHlp.ColumnsCount('t1'));
  AssertTrue(DBHlp.TableExists('t2'));
  AssertEquals(3, DBHlp.ColumnsCount('t2'));
  AssertTrue(DBVer.UpgradeNeeded);
  AssertTrue(DBVer.IsInitialized);

  DBVer.UpgradeTo(6); // 5 --> 6
  AssertEquals(6, DBVer.GetCurrentDBVersion);
  AssertTrue(DBHlp.TableExists('t3'));
  AssertTrue(DBHlp.ColumnExists('t3', 'field'));
  AssertTrue(DBVer.UpgradeNeeded);

  DBVer.UpgradeToLatest; // 6 --> 8
  AssertEquals(8, DBVer.GetCurrentDBVersion);
  AssertFalse(DBHlp.TableExists('t3'));
  AssertTrue(DBHlp.TableExists('t4'));
  AssertEquals(3, DBHlp.TablesCount);

  AssertEquals(3 + 1, DBHlp.RowsCount('t1'));
  AssertEquals(2, DBHlp.RowsCount('t2'));
  AssertFalse(DBVer.UpgradeNeeded);
  AssertTrue(DBVer.IsInitialized);

  with Query do
  begin
    Close;
    SQL.Text := 'SELECT count(*) FROM t1 WHERE `str` = "new";';
    Open;
    First;
    Num := Fields[0].AsInteger;
    Close;
  end;
  AssertEquals(1, Num);

  with Query do
  begin
    Close;
    SQL.Text := 'SELECT `id` FROM t2 WHERE a=9 AND b=6;';
    Open;
    First;
    Num := Fields[0].AsInteger;
    Close;
  end;
  AssertEquals(2, Num);
end;

procedure TTestCase1.TestDatabaseUpgradeFailures;
begin
  //DBVer.SQLDir := ConcatPaths([SQLScriptsBaseDir, 'test02']);
  FreeAndNil(DBVer);
  DBVer := TDBVersioningHelper.Create(Conn, Trans, ConcatPaths([SQLScriptsBaseDir, 'test02']));

  DBVer.UpgradeTo(1); // 0 --> 1
  AssertTrue(DBHlp.TableExists('t0'));
  AssertEquals(1, DBHlp.TablesCount);

  AssertException({Exception} ESQLDatabaseError, @DBVer.UpgradeToLatest); // 1 --> 4 (should fail)
  AssertTrue(DBHlp.TableExists('t0'));
  AssertFalse(DBHlp.TableExists('t1'));
  AssertFalse(DBHlp.TableExists('t2'));
  AssertFalse(DBHlp.TableExists('t3'));
  AssertEquals(1, DBHlp.TablesCount);
  AssertEquals(1, DBVer.CurrentVersion);

  // Try to upgrade to non-existent version
  AssertException(EDBVersioningException, @UpgradeToIncorrectVersion);
end;

procedure TTestCase1.TestDifferentFileNames;
begin
  //DBVer.SQLDir := ConcatPaths([SQLScriptsBaseDir, 'test03']);
  FreeAndNil(DBVer);
  DBVer := TDBVersioningHelper.Create(Conn, Trans, ConcatPaths([SQLScriptsBaseDir, 'test03']));

  DBVer.UpgradeToLatest;

  AssertEquals(4, DBVer.CurrentVersion);
  AssertEquals(4, DBHlp.RowsCount('managers'));
  AssertEquals(2, DBHlp.RowsCount('customers'));
  AssertTrue(DBHlp.ColumnExists('managers', 'salary'));
end;

procedure TTestCase1.TestBuiltInScripts;
const
  SQLScripts: array [1..4] of string = (
    'CREATE TABLE `t` (`id` INTEGER PRIMARY KEY /*! AUTO_INCREMENT */, `first_name` VARCHAR(128));',
    'ALTER TABLE `t` RENAME TO `persons`;',
    'ALTER TABLE `persons` ADD COLUMN `last_name` VARCHAR(128);',
    'INSERT INTO `persons` (`first_name`, `last_name`) VALUES ("John", "Doe");' + sLineBreak +
        'INSERT INTO `persons` (`first_name`, `last_name`) VALUES ("Mary", "Smith");' + sLineBreak +
        'INSERT INTO `persons` (`first_name`, `last_name`) VALUES ("Samanta", "Brown");'
  );
begin
  FreeAndNil(DBVer);
  DBVer := TDBVersioningHelper.Create(Conn, Trans, SQLScripts);

  AssertEquals(4, DBVer.LatestVersion);
  AssertTrue(DBVer.UpgradeNeeded);
  DBVer.UpgradeToLatest;
  AssertEquals(4, DBVer.GetCurrentDBVersion);
  AssertFalse(DBVer.UpgradeNeeded);
  AssertTrue(DBHlp.TableExists('persons'));
  AssertEquals(3, DBHlp.RowsCount('persons'));

  // Check possible errors
  AssertException(EDBVersioningException, @UpgradeToIncorrectVersion);
end;

procedure TTestCase1.TestResourceScripts;
begin
  FreeAndNil(DBVer);
  DBVer := TDBVersioningHelper.CreateFromResources(Conn, Trans{, 'SQL_SCRIPT_'});

  AssertEquals(4, DBVer.LatestVersion);

  DBVer.UpgradeToLatest;
  AssertFalse(DBVer.UpgradeNeeded);
  AssertEquals(2, DBHlp.TablesCount);
  AssertEquals(4, DBHlp.RowsCount('managers'));
  AssertEquals(2, DBHlp.RowsCount('customers'));
end;

procedure TTestCase1.UpgradeToIncorrectVersion;
begin
  DBVer.UpgradeTo(9999);
end;

procedure TTestCase1.TestParams;
const
  CurrVerParam = 'current_version';
  Param2 = 'param2';
  WrongParam = 'unknown';
begin
  // One parameter
  {AssertFalse(DBVer.HasParam(CurrVerParam));
  AssertEquals('', DBVer.GetParam(CurrVerParam));}
  DBVer.SetParam(CurrVerParam, '555');
  AssertTrue(DBVer.HasParam(CurrVerParam));
  AssertEquals('555', DBVer.GetParam(CurrVerParam));
  AssertEquals(555, DBVer.GetCurrentDBVersion);

  // Add second parameter
  DBVer.SetParam(Param2, 'hello world!');
  AssertTrue(DBVer.HasParam(CurrVerParam));
  AssertEquals('555', DBVer.GetParam(CurrVerParam));
  AssertTrue(DBVer.HasParam(Param2));
  AssertEquals('hello world!', DBVer.GetParam(Param2));

  // Change value of first parameter
  DBVer.SetParam(CurrVerParam, '321');
  AssertEquals('321', DBVer.GetParam(CurrVerParam));
  AssertEquals('hello world!', DBVer.GetParam(Param2));

  // Recreate DBVer and check saved values
  FreeAndNil(DBVer);
  DBVer := TDBVersioningHelper.Create(Conn, Trans);
  AssertEquals('321', DBVer.GetParam(CurrVerParam));
  AssertEquals('hello world!', DBVer.GetParam(Param2));

  // Check for wrong parameter
  AssertFalse(DBVer.HasParam(WrongParam));
  AssertEquals('', DBVer.GetParam(WrongParam));
end;

procedure TTestCase1.SetUp;
var
  SqliteDBFile: String;
  Ini: TIniFile;
begin
  if Engine = dbSqlite3 then
  begin
    // Drop exists database file
    SqliteDBFile := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'test.db';
    if FileExists(SqliteDBFile) then
      DeleteFile(SqliteDBFile);
  end;

  // Prepare database
  Trans := TSQLTransaction.Create(Nil);
  //Trans.DataBase := ;

  case Engine of
    dbSqlite3:
      begin
        Conn := TSQLite3Connection.Create(Nil);
        Conn.DatabaseName := SqliteDBFile;
        Conn.CharSet := 'UTF8';
        Conn.Transaction := Trans;
        Conn.Open;
      end;

    dbMysql55:
      begin
        if not FileExists('db-config.ini') then
          CopyFile('db-config.sample.ini', 'db-config.ini');

        Ini := TIniFile.Create('db-config.ini');
        try
          Conn := TMySQL55Connection.Create(Nil);
          Conn.DatabaseName := Ini.ReadString('mysql', 'DatabaseName', '');
          Conn.HostName := Ini.ReadString('mysql', 'HostName', '');
          (Conn as TMySQL55Connection).Port := Ini.ReadInteger('mysql', 'Port', 3306);
          Conn.UserName := Ini.ReadString('mysql', 'UserName', '');
          Conn.Password := Ini.ReadString('mysql', 'Password', '');
          Conn.CharSet:='utf8';

          Conn.Transaction := Trans;
          Conn.Open;
        finally
          Ini.Free;
        end;
      end;
  end;

  Query := TSQLQuery.Create(Nil);
  Query.SQLConnection := Conn;

  Trans.Active := True;

  // Create helper
  DBHlp := TDBHelper.Create(Conn, Trans);

  if Engine = dbMysql55 then
    DBHlp.ClearDatabase;

  // Create TDBVersioning instance
  DBVer := TDBVersioningHelper.Create(Conn, Trans);
end;

procedure TTestCase1.TearDown;
begin
  FreeAndNil(DBHlp);
  FreeAndNil(DBVer);
  FreeAndNil(Query);
  FreeAndNil(Conn);
  FreeAndNil(Trans);
end;

initialization

  RegisterTest(TSqlite3Test);
  RegisterTest(TMysql55Test);
end.

