unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, {testutils,} testregistry, SQLDB, SQLite3Conn,
  Helper;

type

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParams;
    procedure TestDatabaseUpgrade;
    procedure TestDatabaseUpgradeFailures;
  private
    Conn: TSQLite3Connection;
    Query: TSQLQuery;
    Trans: TSQLTransaction;
    DBHlp: TDBHelper;
    DBVer: TDBVersioningHelper;
  end;

implementation

uses
  Forms;

procedure TTestCase1.TestDatabaseUpgrade;
var
  Num: Integer;
begin
  DBVer.SQLDir := ConcatPaths(['db-updates', 'test01']);


  AssertEquals(0, DBVer.GetCurrentDBVersion);
  AssertEquals(0, DBHlp.TablesCount);
  AssertTrue(DBVer.UpgradeNeeded);

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
  DBVer.SQLDir := ConcatPaths(['db-updates', 'test02']);

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
  DBFile: String;
begin
  // Drop exists database file
  DBFile := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'test.db';
  if FileExists(DBFile) then
    DeleteFile(DBFile);

  // Prepare database
  Trans := TSQLTransaction.Create(Nil);
  //Trans.DataBase := ;

  Conn := TSQLite3Connection.Create(Nil);
  Conn.DatabaseName := DBFile;
  Conn.CharSet := 'UTF8';
  Conn.Transaction := Trans;
  Conn.Open;

  Query := TSQLQuery.Create(Nil);
  Query.SQLConnection := Conn;

  Trans.Active := True;

  // Create TDBVersioning instance
  DBVer := TDBVersioningHelper.Create(Conn, Trans);

  // Create helper
  DBHlp := TDBHelper.Create(Conn, Trans);
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

  RegisterTest(TTestCase1);
end.

