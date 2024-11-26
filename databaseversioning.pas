unit DatabaseVersioning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB;

type

  TDBUpgradeProvider = class;

  { TDBVersioning }

  { This is the class for managing database schema upgrades }
  TDBVersioning = class
    //private
    protected
      SQLQuery: TSQLQuery;
      Provider: TDBUpgradeProvider;

      procedure CreateDBInfoTable;

      function GetCurrentDBVersion: Integer;
      procedure SetCurrentDBVersion(AVer: Integer);

      function GetParam(AParam: String): String;
      procedure SetParam(AParam, AValue: String);
      function HasParam(AParam: String): Boolean;

      function GetLatestVersion: Integer;

      function IsSqlite: Boolean;
      function IsMysql: Boolean;

    public
      { @param(ASQLDir Directory where *.sql files will be searched. If empty
        string passed @italic(ProgramDir)/db-updates/ will be used.) }
      constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction;
        ASQLDir: String = '');

      { @param(ASQLScripts Array of SQL commands. Each item may contains more than one
        command separated with semicolon (@bold(;)). Multiline strings allowed. Array
        indexes linked with versions (first item --- version 1, second item ---
        version 2, etc...).) }
      constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction;
        const ASQLScripts: array of String);
      constructor CreateFromResources(AConnection: TSQLConnection; ATransaction: TSQLTransaction;
        AResourcePrefix: String = 'SQL_SCRIPT_');
      destructor Destroy; override;

      { Returns the version of the database it is in }
      property CurrentVersion: Integer read GetCurrentDBVersion;

      { Returns last available for upgrade version }
      property LatestVersion: Integer read GetLatestVersion;

      { Upgrade database to specified version. If upgrade fails it rollback all changes
        to previous state.
        @param(AVer New version to upgrade (> 1))
        @raises(EDBVersioningException If SQL commands for specified version
        (or any intermediate) not found)
        @raises(ESQLDatabaseError If error occured on executing SQL statements) }
      procedure UpgradeTo(AVer: Integer);

      { Upgrade database to latest possible version
        @seealso(UpgradeTo) }
      procedure UpgradeToLatest;

      { Checks if database can be upgraded to new version (has any not applied upgrades). }
      function UpgradeNeeded: Boolean;

      { Checks if database not empty and has any upgrades applied.
        For the first run should return @false. }
      function IsInitialized: Boolean;

      //function IsSqlite: Boolean;
      //function IsMysql: Boolean;
  end;

  EDBVersioningException = class(Exception);

  TDBUpgradeProvider = class
    public
      function LatestVersion: Integer; virtual; abstract;
      function SQLCommands(AVer: Integer): String; virtual; abstract;
  end;

  { TDBFileUpgradeProvider }

  TDBFileUpgradeProvider = class(TDBUpgradeProvider)
    private
      SQLDir: String;

      function DefaultSQLDir: String;

    public
      constructor Create(ASQLDir: String = '');

      function LatestVersion: Integer; override;
      function SQLCommands(AVer: Integer): String; override;
  end;

  { TDBArrayUpgradeProvider }

  TDBArrayUpgradeProvider = class(TDBUpgradeProvider)
    private
      SQLScripts: array of String;

    public
      constructor Create(const ASQLScripts: array of String);
      destructor Destroy; override;

      function LatestVersion: Integer; override;
      function SQLCommands(AVer: Integer): String; override;
  end;

  { TDBResourceUpgradeProvider }

  TDBResourceUpgradeProvider = class(TDBUpgradeProvider)
    private
      ResourcePrefix: String;

    public
      constructor Create(AResourcePrefix: String {= 'SQL_SCRIPT_'});

      function LatestVersion: Integer; override;
      function SQLCommands(AVer: Integer): String; override;
  end;

implementation

uses
  FileUtil, LazFileUtils, Math, RegExpr, StrUtils, DBVerUtils
  {$IfDef Windows}
  , Windows
  {$EndIf}
  ;

const
  DBInfoTable = '_db_info';

{ TDBResourceUpgradeProvider }

constructor TDBResourceUpgradeProvider.Create(AResourcePrefix: String);
begin
  ResourcePrefix := AResourcePrefix;
end;

function TDBResourceUpgradeProvider.LatestVersion: Integer;
var
  Idx: Integer = 1;
  ResName: String;
begin
  while True do
  begin
    ResName := ResourcePrefix + IntToStr(Idx);
    if FindResource(HINSTANCE, PChar(ResName), RT_RCDATA) = 0 then
      Exit(Idx - 1);

    Inc(Idx);
  end;
end;

function TDBResourceUpgradeProvider.SQLCommands(AVer: Integer): String;
var
  ResName: String = '';
  ResStream: TResourceStream = Nil;
  StringStream: TStringStream = Nil;
begin
  ResName := ResourcePrefix + IntToStr(AVer);
  if FindResource(HINSTANCE, PChar(ResName), RT_RCDATA) <> 0 then
  begin
    ResStream := TResourceStream.Create(HINSTANCE, ResName, RT_RCDATA);
    StringStream := TStringStream.Create;
    try
      StringStream.LoadFromStream(ResStream);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
      ResStream.Free;
    end;
  end;
end;

{ TDBArrayUpgradeProvider }

constructor TDBArrayUpgradeProvider.Create(const ASQLScripts: array of String);
var
  I: Integer;
begin
  // ToDo: optimize, remake without array copying
  SetLength(SQlScripts, Length(ASQLScripts));
  for I := Low(ASQLScripts) to High(ASQLScripts) do
    SQlScripts[I] := ASQLScripts[I];
end;

destructor TDBArrayUpgradeProvider.Destroy;
begin
   SetLength(SQLScripts, 0);
end;

function TDBArrayUpgradeProvider.LatestVersion: Integer;
begin
  Result := High(SQLScripts) + 1;
end;

function TDBArrayUpgradeProvider.SQLCommands(AVer: Integer): String;
var
  Idx: Integer;
begin
  Idx := AVer - 1;
  if (Idx < Low(SQLScripts)) or (Idx > High(SQLScripts)) then
    raise EDBVersioningException.CreateFmt('Unable to find SQL script for version %d', [AVer]);

  Result := SQLScripts[Idx];
end;

{ TDBFileUpgradeProvider }

function TDBFileUpgradeProvider.DefaultSQLDir: String;
begin
  Result := ConcatPaths([ExtractFileDir({Application.ExeName} ParamStr(0)), 'db-updates', '']);
end;

constructor TDBFileUpgradeProvider.Create(ASQLDir: String);
begin
  SQLDir := ASQLDir;
  if SQLDir = '' then
    SQLDir := DefaultSQLDir;
end;

function TDBFileUpgradeProvider.LatestVersion: Integer;
var
  FileList: TStringList;
  FileName: String;
  I, Version: Integer;
  Re: TRegExpr;
begin
  Result := 0;
  FileList := FindAllFiles(SQLDir, '*.sql', False);
  Re := TRegExpr.Create('^(\d+)');
  try
    for I := 0 to FileList.Count - 1 do
    begin
      FileName := ExtractFileName(FileList[I]);
      try
        if Re.Exec(FileName) then
          Version := StrToInt(Re.Match[1]);
        Result := Max(Result, Version);
      except
        // Skip SQL-files without leading number
      end;
    end;
  finally
    FileList.Free;
    Re.Free;
  end;
end;

function TDBFileUpgradeProvider.SQLCommands(AVer: Integer): String;
  function FindSQLFileForVersion(AVer: Integer): String;
  var
    FileList: TStringList;
    Re: TRegExpr;
    I: Integer;
    FileName: String;
  begin
    Result := '';

    FileList := FindAllFiles(SQLDir, Format('*%d*.sql', [AVer]), False);
    Re := TRegExpr.Create('^(\d+)');
    try
      for I := 0 to FileList.Count - 1 do
      begin
        FileName := ExtractFileName(FileList[I]);
        if Re.Exec(FileName) and (Re.Match[1].ToInteger = AVer) then
          Exit(FileList[I]);
      end;
    finally
      FileList.Free;
      Re.Free;
    end;
  end;

var
  SqlFileName: String;
  Tmp: TStringList;
begin
  Result := '';
  Tmp := TStringList.Create;
  try
    SqlFileName := FindSQLFileForVersion(AVer);
    if SqlFileName.IsEmpty then
      raise {EFileNotFoundException}EDBVersioningException.CreateFmt('Unable to find SQL script file for version %d', [AVer]);

    Tmp.LoadFromFile(SqlFileName);
    Result := Tmp.Text;
  finally
    Tmp.Free;
  end;
end;

{ TDBVersioning }

constructor TDBVersioning.Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction;
  ASQLDir: String);
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.SQLConnection := AConnection;
  SQLQuery.Transaction := ATransaction;

  CreateDBInfoTable;

  Provider := TDBFileUpgradeProvider.Create(ASQLDir);
end;

constructor TDBVersioning.Create(AConnection: TSQLConnection;
  ATransaction: TSQLTransaction; const ASQLScripts: array of String);
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.SQLConnection := AConnection;
  SQLQuery.Transaction := ATransaction;

  CreateDBInfoTable;

  Provider := TDBArrayUpgradeProvider.Create(ASQLScripts);
end;

constructor TDBVersioning.CreateFromResources(AConnection: TSQLConnection;
  ATransaction: TSQLTransaction; AResourcePrefix: String);
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.SQLConnection := AConnection;
  SQLQuery.Transaction := ATransaction;

  CreateDBInfoTable;

  Provider := TDBResourceUpgradeProvider.Create(AResourcePrefix);
end;

destructor TDBVersioning.Destroy;
begin
  Provider.Free;
  SQLQuery.Free;

  inherited;
end;

procedure TDBVersioning.UpgradeTo(AVer: Integer);
var
  Ver, OldVer: Integer;
  SQLScript: TSQLScript;
begin
  // Fixme: в Mysql транзакции (точнее ROLLBACK) не работают с командами CREATE TABLE, ALTER TABLE и пр...
  // см.: https://dev.mysql.com/doc/refman/5.7/en/cannot-roll-back.html
  // как же тогда сделать откат изменеия схемы бд к предыдущему состоянию в случае ошибки?
  // И непонятно, почему тогда проходят тесты из TTestCase1.TestDatabaseUpgradeFailures?

  if {not UpgradeNeeded} CurrentVersion = AVer then
    Exit;

  SQLScript := TSQLScript.Create(Nil);
  try
    try
      SQLScript.DataBase := SQLQuery.DataBase;
      SQLScript.Transaction := SQLQuery.Transaction;

      for Ver := GetCurrentDBVersion + 1 to AVer do
      begin
        SQLScript.Script.{AddStrings}AddText(AddTrailingSemicolon(Provider.SQLCommands(Ver)));
      end;

      SQLScript.Execute;
    except
      (SQLQuery.Transaction as TSQLTransaction).RollbackRetaining;
      raise;
    end;
  finally
    SQLScript.Free;
  end;

  (SQLQuery.Transaction as TSQLTransaction).CommitRetaining;

  OldVer := CurrentVersion;
  SetCurrentDBVersion(AVer);
  SetParam('last_upgrade', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  SetParam('previous_version', IntToStr(OldVer));
end;

procedure TDBVersioning.UpgradeToLatest;
begin
  UpgradeTo(GetLatestVersion);
end;

function TDBVersioning.UpgradeNeeded: Boolean;
begin
  Result := GetCurrentDBVersion < GetLatestVersion;
end;

function TDBVersioning.IsInitialized: Boolean;
begin
  Result := CurrentVersion > 0;
end;

procedure TDBVersioning.CreateDBInfoTable;
begin
  with SQLQuery do
  begin
    Close;
    SQL.Clear;
    SQL.Append('CREATE TABLE IF NOT EXISTS `' + DBInfoTable + '` (');
    if IsSqlite then
    begin
      SQL.Append('`key` TEXT PRIMARY KEY,');
      SQL.Append('`value` TEXT');
    end
    else if IsMysql then
    begin
      SQL.Append('`key` VARCHAR(32) PRIMARY KEY,');
      SQL.Append('`value` VARCHAR(512)');
    end;
    SQL.Append(')');
    ExecSQL;
    Close;

    if not HasParam('current_version') then
      SetParam('current_version', '0');

    (Transaction as TSQLTransaction).CommitRetaining;
  end;
end;

function TDBVersioning.GetCurrentDBVersion: Integer;
begin
  Result := StrToInt(GetParam('current_version'));
end;

procedure TDBVersioning.SetCurrentDBVersion(AVer: Integer);
begin
  SetParam('current_version', IntToStr(AVer));
end;

function TDBVersioning.GetParam(AParam: String): String;
begin
  Result := '';

  with SQLQuery do
  begin
    Close;
    SQL.Text :=
      'SELECT `value` FROM `' + DBInfoTable + '` ' +
        'WHERE `key` = :key';
    ParamByName('key').AsString := AParam;
    Open;
    First;
    if not EOF then
      Result := FieldByName('value').AsString;
    Close;
  end;
end;

procedure TDBVersioning.SetParam(AParam, AValue: String);
begin
  with SQLQuery do
  begin
    Close;
    SQL.Clear;
    if IsSqlite then
      SQL.Append('INSERT OR ');
    SQL.Append('REPLACE INTO `' + DBInfoTable + '` (`key`, `value`)');
    SQL.Append('VALUES (:key, :value)');
    ParamByName('key').AsString := AParam;
    ParamByName('value').AsString := AValue;
    ExecSQL;
    Close;

    (Transaction as TSQLTransaction).CommitRetaining;
  end;
end;

function TDBVersioning.HasParam(AParam: String): Boolean;
begin
  with SQLQuery do
  begin
    Close;
    SQL.Text :=
      'SELECT COUNT(*) FROM `' + DBInfoTable + '` ' +
        'WHERE `key` = :key';
    ParamByName('key').AsString := AParam;
    Open;
    First;
    Result := Fields[0].AsBoolean;
    Close;
  end;
end;

function TDBVersioning.GetLatestVersion: Integer;
begin
  Result := Provider.LatestVersion;
end;

function TDBVersioning.IsSqlite: Boolean;
begin
  Result := StartsText('TSqlite', SQLQuery.SQLConnection.ClassName);
end;

function TDBVersioning.IsMysql: Boolean;
begin
  Result := StartsText('TMysql', SQLQuery.SQLConnection.ClassName);
end;

end.

