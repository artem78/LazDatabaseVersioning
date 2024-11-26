unit Helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DatabaseVersioning, SQLDB;

type

  { TDBVersioningHelper }

  TDBVersioningHelper = class(TDBVersioning)
    { Make some methods public for testing }
    public
      function GetCurrentDBVersion: Integer;
      function GetParam(AParam: String): String;
      procedure SetParam(AParam, AValue: String);
      function HasParam(AParam: String): Boolean;
  end;

  { TDBHelper }

  TDBHelper = class
    private
      Query: TSQLQuery;

      function IsSqlite: Boolean;
      function IsMysql: Boolean;
    public
      constructor Create(AConn: TSQLConnection; ATrans: TSQLTransaction);
      destructor Destroy; override;

      function TableExists(ATbl: String): Boolean;
      function ColumnExists(ATbl, ACol: String): Boolean;
      function TablesCount: Integer;
      function ColumnsCount(ATbl: String): Integer;
      function RowsCount(ATbl: String): Integer;
      procedure ClearDatabase;
  end;

implementation

uses StrUtils;

{ TDBHelper }

function TDBHelper.IsSqlite: Boolean;
begin
  Result := StartsText('TSqlite', Query.SQLConnection.ClassName);
end;

function TDBHelper.IsMysql: Boolean;
begin
  Result := StartsText('TMysql', Query.SQLConnection.ClassName);
end;

constructor TDBHelper.Create(AConn: TSQLConnection; ATrans: TSQLTransaction);
begin
  Query := TSQLQuery.Create(Nil);
  Query.SQLConnection := AConn;
  Query.SQLTransaction := ATrans;
end;

destructor TDBHelper.Destroy;
begin
  Query.Free;

  inherited;
end;

function TDBHelper.TableExists(ATbl: String): Boolean;
begin
  with Query do
  begin
    Close;
    if IsSqlite then
    begin
      SQL.Text := 'SELECT COUNT(*) FROM `sqlite_master` ' +
        'WHERE type="table" AND name = :tbl';
    end
    else if IsMysql then
    begin
      SQL.TEXT := 'SELECT count(*) ' +
          'FROM information_schema.TABLES ' +
          'WHERE (TABLE_SCHEMA = :db) AND (TABLE_NAME = :tbl)';

      ParamByName('db').AsString := SQLConnection.DatabaseName;
    end;
    ParamByName('tbl').AsString := ATbl;
    Open;
    First;
    Result := Fields[0].AsBoolean;
    Close;
  end;
end;

function TDBHelper.ColumnExists(ATbl, ACol: String): Boolean;
begin
  with Query do
  begin
    Close;

    if IsSqlite then
    begin
      SQL.Text := 'SELECT count(*) FROM PRAGMA_TABLE_INFO(:tbl) ' +
        'WHERE name=:col;';
    end
    else if IsMysql then
    begin
      SQL.Clear;
      SQL.Append('SELECT COUNT(*)');
      SQL.Append('FROM information_schema.COLUMNS');
      SQL.Append('WHERE TABLE_SCHEMA = :db AND TABLE_NAME = :tbl AND COLUMN_NAME = :col');

      ParamByName('db').AsString := SQLConnection.DatabaseName;
    end;

    ParamByName('tbl').AsString := ATbl;
    ParamByName('col').AsString := ACol;
    Open;
    First;
    Result := Fields[0].AsBoolean;
    Close;
  end;
end;

function TDBHelper.TablesCount: Integer;
begin
  with Query do
  begin
    Close;

    if IsSqlite then
    begin
      SQL.Text := 'SELECT COUNT(*) FROM `sqlite_master` ' +
        'WHERE type="table" AND name <> "_db_info"';
    end
    else if IsMysql then
    begin
      SQL.Text := 'SELECT COUNT(*) FROM information_schema.tables '+
                  'WHERE table_schema = :db and TABLE_TYPE=''BASE TABLE'' '+
                  'AND TABLE_NAME <> "_db_info"';

      ParamByName('db').AsString:=SQLConnection.DatabaseName;
    end;

    Open;
    First;
    Result := Fields[0].AsInteger;
    Close;
  end;
end;

function TDBHelper.ColumnsCount(ATbl: String): Integer;
begin
  with Query do
  begin
    Close;

    if IsSqlite then
    begin
      SQL.Text := 'SELECT count(*) FROM PRAGMA_TABLE_INFO(:tbl);'
    end
    else if IsMysql then
    begin
      SQL.Text:='SELECT COUNT(*) '+
                'FROM INFORMATION_SCHEMA.COLUMNS '+
                'WHERE table_schema = :db '       +
                'AND table_name = :tbl';

      ParamByName('db').AsString:=SQLConnection.DatabaseName;
    end;

    ParamByName('tbl').AsString := ATbl;
    Open;
    First;
    Result := Fields[0].AsInteger;
    Close;
  end;
end;

function TDBHelper.RowsCount(ATbl: String): Integer;
begin
  with Query do
  begin
    Close;
    //SQL.Text := 'SELECT count(*) FROM :tbl';
    //ParamByName('tbl').AsString := ATbl;
    SQL.Text := 'SELECT count(*) FROM `' + ATbl + '`';
    Open;
    First;
    Result := Fields[0].AsInteger;
    Close;
  end;
end;

procedure TDBHelper.ClearDatabase;
var
  Tables: TStringList;
  TableName: String;
begin
  if IsMysql then
  begin
    Tables := TStringList.Create;
    try
      with Query do
      begin
        // Gets list of all tables
        Close;
        SQL.Text:='SHOW TABLES;';
        Open;
        First;
        while not EOF do
        begin
          Tables.Append(Fields[0].AsString);
          Next;
        end;
        Close;

        // Delete tables one by one
        for TableName in Tables do
        begin
          Close;
          SQL.Text:='DROP TABLE IF EXISTS `' + TableName + '`;';
          ExecSQL;
        end;
        Close;
      end;

    finally
      Tables.Free;
    end;
  end
  else
    raise Exception.Create('Not implemented yet...');
end;

{ TDBVersioningHelper }

function TDBVersioningHelper.GetCurrentDBVersion: Integer;
begin
  Result := inherited;
end;

function TDBVersioningHelper.GetParam(AParam: String): String;
begin
  Result := inherited;
end;

procedure TDBVersioningHelper.SetParam(AParam, AValue: String);
begin
  inherited SetParam(AParam, AValue);
end;

function TDBVersioningHelper.HasParam(AParam: String): Boolean;
begin
  Result := inherited HasParam(AParam);
end;

end.

