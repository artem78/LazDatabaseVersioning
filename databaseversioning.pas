unit DatabaseVersioning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB;

type

  { TDBVersioning }

  { This is the class for managing database schema upgrades }
  TDBVersioning = class
    //private
    protected
      SQLQuery: TSQLQuery;

      procedure CreateDBInfoTable;

      function GetCurrentDBVersion: Integer;
      procedure SetCurrentDBVersion(AVer: Integer);

      function GetParam(AParam: String): String;
      procedure SetParam(AParam, AValue: String);
      function HasParam(AParam: String): Boolean;

      function GetLatestVersion: Integer;

      function DefaultSQLDir: String;

    public
      SQLDir: String;
      SQLScripts: array of String;

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
  end;

  EDBVersioningException = class(Exception);

implementation

uses
  FileUtil, LazFileUtils, Math, RegExpr;

const
  DBInfoTable = '_db_info';

{ TDBVersioning }

constructor TDBVersioning.Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction;
  ASQLDir: String);
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.SQLConnection := AConnection;
  SQLQuery.Transaction := ATransaction;

  CreateDBInfoTable;

  SQLDir := ASQLDir;
  if SQLDir = '' then
    SQLDir := DefaultSQLDir;
end;

constructor TDBVersioning.Create(AConnection: TSQLConnection;
  ATransaction: TSQLTransaction; const ASQLScripts: array of String);
var
  I: Integer;
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.SQLConnection := AConnection;
  SQLQuery.Transaction := ATransaction;

  CreateDBInfoTable;

  // ToDo: optimize, remake without array copying
  SetLength(SQlScripts, Length(ASQLScripts));
  for I := Low(ASQLScripts) to High(ASQLScripts) do
    SQlScripts[I] := ASQLScripts[I];
end;

destructor TDBVersioning.Destroy;
begin
  SetLength(SQlScripts, 0);
  SQLQuery.Free;

  inherited;
end;

procedure TDBVersioning.UpgradeTo(AVer: Integer);
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
  Ver, OldVer, Idx: Integer;
  SqlFileName: String;
  SQLScript: TSQLScript;
  Tmp: TStringList;
begin
  if {not UpgradeNeeded} CurrentVersion = AVer then
    Exit;

  Tmp := TStringList.Create;
  SQLScript := TSQLScript.Create(Nil);
  try
    try
      SQLScript.DataBase := SQLQuery.DataBase;
      SQLScript.Transaction := SQLQuery.Transaction;

      for Ver := GetCurrentDBVersion + 1 to AVer do
      begin
        if Length(SQLScripts) = 0 then
        begin
          SqlFileName := FindSQLFileForVersion(Ver);
          if SqlFileName.IsEmpty then
            raise {EFileNotFoundException}EDBVersioningException.CreateFmt('Unable to find SQL script file for version %d', [Ver]);

          Tmp.LoadFromFile(SqlFileName);
        end
        else
        begin
          Idx := Ver - 1;
          if (Idx < Low(SQLScripts)) or (Idx > High(SQLScripts)) then
            raise EDBVersioningException.CreateFmt('Unable to find SQL script for version %d', [Ver]);

          Tmp.Text := SQLScripts[Idx];
        end;
        SQLScript.Script.AddStrings(Tmp);
      end;

      SQLScript.Execute;
    except
      (SQLQuery.Transaction as TSQLTransaction).RollbackRetaining;
      raise;
    end;
  finally
    SQLScript.Free;
    Tmp.Free;
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
    SQL.Text :=
      'CREATE TABLE IF NOT EXISTS `' + DBInfoTable + '` (' +
        '`key` TEXT PRIMARY KEY,' +
        '`value` TEXT' +
      ')';
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
    SQL.Text :=
      'INSERT OR REPLACE INTO `' + DBInfoTable + '` (`key`, `value`) ' +
        'VALUES (:key, :value)';
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
var
  FileList: TStringList;
  FileName: String;
  I, Version: Integer;
  Re: TRegExpr;
begin
  if Length(SQLScripts) = 0 then
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
  end
  else
  begin
    Result := High(SQlScripts) + 1;
  end;
end;

function TDBVersioning.DefaultSQLDir: String;
begin
  Result := ConcatPaths([ExtractFileDir({Application.ExeName} ParamStr(0)), 'db-updates', '']);
end;

end.

