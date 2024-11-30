unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ComCtrls, DBCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddContactButton: TButton;
    EditContactButton: TButton;
    DeleteContactButton: TButton;
    AddGroupButton: TButton;
    EditGroupButton: TButton;
    DeleteGroupButton: TButton;
    GroupsDBGrid: TDBGrid;
    GroupsDataSource: TDataSource;
    GroupsSQLQuery: TSQLQuery;
    ContactsDataSource: TDataSource;
    ContactsDBGrid: TDBGrid;
    Label0: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LogMemo: TMemo;
    SQLite3Connection1: TSQLite3Connection;
    ContactsSQLQuery: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure AddContactButtonClick(Sender: TObject);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure AddGroupButtonClick(Sender: TObject);
    procedure DeleteGroupButtonClick(Sender: TObject);
    procedure EditGroupButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Log(const AStr: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  {$IfDef Unix}
  clocale,
  {$EndIf}
  DatabaseVersioning, uContactEditForm;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  DBVer: TDBVersioning;
begin
  SQLite3Connection1.Open;

  // Upgrade database scheme if needed
  DBVer := TDBVersioning.Create(SQLite3Connection1, SQLTransaction1, 'sql');
  Log(Format('db initialized: %s', [BoolToStr(DBVer.IsInitialized, 'yes', 'no')]));
  if DBVer.CurrentVersion > DBVer.LatestVersion then
  begin
    MessageDlg('Seems you use database file made with more recent version of '
             + 'application! Please update your appication or delete '
             + 'database.sqlite3 file.'
             , mtError, [mbOK], 0);
    Application.Terminate;
  end
  else if DBVer.UpgradeNeeded then
  begin
    Log(Format('current db version: %d', [DBVer.CurrentVersion]));
    Log(Format('latest available version: %d', [DBVer.LatestVersion]));
    Log('db needs upgrade');
    try
      DBVer.UpgradeToLatest;
    except
      on E: Exception do
      begin
        MessageDlg('Database upgrade failed!' + sLineBreak + E.ToString, mtError, [mbOK], 0);
        Application.Terminate;
      end;
    end;
    Log('db upgrade finished!');
  end;

  Log(Format('current db version: %d', [DBVer.CurrentVersion]));
  DBVer.Free;

  // SQLQueries should be activated after their tables created on previous step
  ContactsSQLQuery.Open;
  GroupsSQLQuery.Open;

  ContactsDBGrid.AutoAdjustColumns;
  GroupsDBGrid.AutoAdjustColumns;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Saves changes and commits transaction
  SQLTransaction1.Commit;
  GroupsSQLQuery.Close;
  ContactsSQLQuery.Close;
  SQLTransaction1.Active := False;
  SQLite3Connection1.Connected := False;
end;

procedure TMainForm.DeleteContactButtonClick(Sender: TObject);
begin
  ContactsSQLQuery.Delete;
  ContactsSQLQuery.ApplyUpdates;
end;

procedure TMainForm.DeleteGroupButtonClick(Sender: TObject);
begin
  GroupsSQLQuery.Delete;
  GroupsSQLQuery.ApplyUpdates;
end;

procedure TMainForm.EditContactButtonClick(Sender: TObject);
begin
  ContactsSQLQuery.Edit;
  if EditContactForm.ShowModal = mrOK then
  begin
    ContactsSQLQuery.Post;
    ContactsSQLQuery.ApplyUpdates;
  end
  else
    ContactsSQLQuery.Cancel;
end;

procedure TMainForm.EditGroupButtonClick(Sender: TObject);
var
  S: String;
begin
  S := InputBox('Rename group', 'Group name', GroupsSQLQuery.FieldByName('name').AsString);
  if S <> '' then
  begin
    GroupsSQLQuery.Edit;
    GroupsSQLQuery.FieldByName('name').AsString := S;
    GroupsSQLQuery.Post;
    GroupsSQLQuery.ApplyUpdates;
  end;
end;

procedure TMainForm.AddContactButtonClick(Sender: TObject);
begin
  ContactsSQLQuery.Append;
  if EditContactForm.ShowModal = mrOK then
  begin
    ContactsSQLQuery.Post;
    ContactsSQLQuery.ApplyUpdates;
  end
  else
    ContactsSQLQuery.Cancel;
end;

procedure TMainForm.AddGroupButtonClick(Sender: TObject);
var
  S: String;
begin
  S := InputBox('Create group', 'Group name', '');
  if S <> '' then
  begin
    GroupsSQLQuery.Append;
    GroupsSQLQuery.FieldByName('name').AsString := S;
    GroupsSQLQuery.Post;
    GroupsSQLQuery.ApplyUpdates;
  end;
end;

procedure TMainForm.Log(const AStr: String);
begin
  LogMemo.Append(AStr);
  LogMemo.SelStart := Length(LogMemo.Text);
end;

end.

