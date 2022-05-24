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
    public
      constructor Create(AConn: TSQLConnection; ATrans: TSQLTransaction);
      destructor Destroy;

      function TableExists(ATbl: String): Boolean;
      function ColumnExists(ATbl, ACol: String): Boolean;
      function TablesCount: Integer;
      function ColumnsCount(ATbl: String): Integer;
  end;

implementation

{ TDBHelper }

constructor TDBHelper.Create(AConn: TSQLConnection; ATrans: TSQLTransaction);
begin
  Query := TSQLQuery.Create(Nil);
  Query.SQLConnection := AConn;
  Query.SQLTransaction := ATrans;
end;

destructor TDBHelper.Destroy;
begin
  Query.Free;
end;

function TDBHelper.TableExists(ATbl: String): Boolean;
begin
  with Query do
  begin
    Close;
    SQL.Text := 'SELECT COUNT(*) FROM `sqlite_master` ' +
      'WHERE type="table" AND name = :tbl';
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
    SQL.Text := 'SELECT count(*) FROM PRAGMA_TABLE_INFO(:tbl) ' +
      'WHERE name=:col;';
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
    SQL.Text := 'SELECT COUNT(*) FROM `sqlite_master` ' +
      'WHERE type="table" AND name <> "_db_info"';
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
    SQL.Text := 'SELECT count(*) FROM PRAGMA_TABLE_INFO(:tbl);';
    ParamByName('tbl').AsString := ATbl;
    Open;
    First;
    Result := Fields[0].AsInteger;
    Close;
  end;
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

