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

      constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction;
        ASQLDir: String = '');
      destructor Destroy; override;

      property CurrentVersion: Integer read GetCurrentDBVersion;
      property LatestVersion: Integer read GetLatestVersion;

      procedure UpgradeTo(AVer: Integer);
      procedure UpgradeToLatest;
      function UpgradeNeeded: Boolean;
  end;

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

destructor TDBVersioning.Destroy;
begin
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
  Ver, OldVer: Integer;
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
        SqlFileName := FindSQLFileForVersion(Ver);

        Tmp.LoadFromFile(SqlFileName);
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

function TDBVersioning.DefaultSQLDir: String;
begin
  Result := ConcatPaths([ExtractFileDir({Application.ExeName} ParamStr(0)), 'db-updates', '']);
end;

end.

