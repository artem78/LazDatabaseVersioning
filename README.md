# LazDatabaseVersioning

This will help you to auto upgrade database scheme.

## Limitations

- Works with SQLite and MySQL only (not tested with other databases)

## Quickstart

Create `db-upgrades` directory in your application folder and place SQL scripts named `1.sql`, `2.sql` and etc. into it.

Typicaly usage:

```pascal
uses
  ..., DatabaseVersioning, ...;



procedure TMainForm.FormCreate(Sender: TObject);
var
  DBVer: TDBVersioning;
begin
  SQLite3Connection1.Open;

  DBVer := TDBVersioning.Create(SQLite3Connection1, SQLTransaction1);
  if DBVer.CurrentVersion > DBVer.LatestVersion then
  begin
    MessageDlg('Seems you use database file made with more recent version of application!', mtError, [mbOK], 0);
    Application.Terminate;
  end
  else if DBVer.UpgradeNeeded then
  begin
    try
      // Upgrade database schema
      DBVer.UpgradeToLatest;
    except
      on E: Exception do
      begin
        MessageDlg('db upgrade failed!' + sLineBreak + E.ToString, mtError, [mbOK], 0);
        Application.Terminate;
      end;
    end;
  end;
  DBVer.Free;

  // SQLQueries should be activated after their tables created on previous step
  ContactsSQLQuery.Open;
  GroupsSQLQuery.Open;
end;  
```

## How to use and examples

*coming soon...*

See [test cases](/Tests/testcase1.pas).

## Documentation

Go to https://artem78.github.io/LazDatabaseVersioning/

## ToDos
- [ ] Write description and usage instructions
- [x] Allow to place SQL commands in code in addition to use separate files
- [x] Use transactions for each db upgrade, check if upgrade finished succesfully
- [ ] Rollback upgrades (downgrade DB)
- [ ] Use docker container with mysql for testing
