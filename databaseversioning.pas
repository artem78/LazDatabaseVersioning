unit DatabaseVersioning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB;

type

  { TDBVersioning }

  TDBVersioning = class
    //private
    protected
      SQLQuery: TSQLQuery;
      SQLDir: String;

      procedure CreateDBInfoTable;

      function GetCurrentDBVersion: Integer;
      procedure SetCurrentDBVersion(AVer: Integer);

      function GetParam(AParam: String): String;
      procedure SetParam(AParam, AValue: String);
      function HasParam(AParam: String): Boolean;

      function GetLatestVersion: Integer;

    public
      constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
      destructor Destroy;

      procedure UpgradeTo(AVer: Integer);
      procedure UpgradeToLatest;
      function UpgradeNeeded: Boolean;
  end;

implementation

uses
  FileUtil, LazFileUtils, Math;

const
  DBInfoTable = '_db_info';

{ TDBVersioning }

constructor TDBVersioning.Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
begin
  SQLQuery := TSQLQuery.Create(Nil);
  SQLQuery.SQLConnection := AConnection;
  SQLQuery.Transaction := ATransaction;

  CreateDBInfoTable;

  SQLDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFileDir({Application.ExeName} ParamStr(0))) + 'db-updates');
end;

destructor TDBVersioning.Destroy;
begin
  SQLQuery.Free;
end;

procedure TDBVersioning.UpgradeTo(AVer: Integer);
var
  Ver: Integer;
  SqlFileName: String;
  SQLScript: TSQLScript;
  Tmp: TStringList;
begin
  Tmp := TStringList.Create;
  SQLScript := TSQLScript.Create(Nil);
  try
    SQLScript.DataBase := SQLQuery.DataBase;
    SQLScript.Transaction := SQLQuery.Transaction;

    for Ver := GetCurrentDBVersion + 1 to AVer do
    begin
      SqlFileName := SqlDir + IntToStr(Ver) + '.sql';
      Tmp.LoadFromFile(SqlFileName);
      SQLScript.Script.Append(Tmp.Text);
    end;

    SQLScript.Execute;
  finally
    SQLScript.Free;
    Tmp.Free;
  end;

  (SQLQuery.Transaction as TSQLTransaction).CommitRetaining;

  SetCurrentDBVersion(AVer);
end;

procedure TDBVersioning.UpgradeToLatest;
begin
  UpgradeTo(GetLatestVersion);
end;

function TDBVersioning.UpgradeNeeded: Boolean;
begin
  Result := GetCurrentDBVersion < GetLatestVersion;
end;

procedure TDBVersioning.CreateDBInfoTable;
begin
  with SQLQuery do
  begin
    Close;
    SQL.Text :=
      'CREATE TABLE IF NOT EXISTS `' + DBInfoTable + '` (' +
        '`id` INTEGER PRIMARY KEY,' +
        '`key` TEXT UNIQUE,' +
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
begin
  Result := 0;
  FileList := FindAllFiles(SQLDir, '*.sql', False);
  try
    for I := 0 to FileList.Count - 1 do
    begin
      FileName := ExtractFileName(FileList[I]);
      try
        Version := StrToInt(ExtractFileNameWithoutExt(FileName));
        Result := Max(Result, Version);
      except
        // Skip SQL-files with illegal names
      end;
    end;
  finally
    FileList.Free;
  end;
end;

end.

