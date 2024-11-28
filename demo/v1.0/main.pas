unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddContactButton: TButton;
    EditContactButton: TButton;
    DeleteContactButton: TButton;
    ContactsDataSource: TDataSource;
    ContactsDBGrid: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    LogMemo: TMemo;
    SQLite3Connection1: TSQLite3Connection;
    ContactsSQLQuery: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure AddContactButtonClick(Sender: TObject);
    procedure DeleteContactButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
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

  ContactsDBGrid.AutoAdjustColumns;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Saves changes and commits transaction
  SQLTransaction1.Commit;
  ContactsSQLQuery.Close;
  SQLTransaction1.Active := False;
  SQLite3Connection1.Connected := False;
end;

procedure TMainForm.DeleteContactButtonClick(Sender: TObject);
begin
  ContactsSQLQuery.Delete;
  ContactsSQLQuery.ApplyUpdates;
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

procedure TMainForm.Log(const AStr: String);
begin
  LogMemo.Append(AStr);
  LogMemo.SelStart := Length(LogMemo.Text);
end;

end.

