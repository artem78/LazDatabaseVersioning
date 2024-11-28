unit uContactEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  ButtonPanel, DBExtCtrls, ExtCtrls;

type

  { TEditContactForm }

  TEditContactForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DBDateEdit1: TDBDateEdit;
    DBLookupComboBox1: TDBLookupComboBox;
    EmailDBEdit: TDBEdit;
    Label5: TLabel;
    Label6: TLabel;
    PhoneDBEdit: TDBEdit;
    FirstNameDBEdit: TDBEdit;
    Label3: TLabel;
    Label4: TLabel;
    LastNameDBEdit: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    GenderRadioGroup: TRadioGroup;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  EditContactForm: TEditContactForm;

implementation

{$R *.lfm}

uses
  main;

{ TEditContactForm }

procedure TEditContactForm.FormShow(Sender: TObject);
var
  Idx: integer = -1;
begin
  FirstNameDBEdit.SetFocus;

  case MainForm.ContactsSQLQuery.FieldByName('gender').AsString of
    'M': Idx := 0;
    'F': Idx := 1;
  end;
  GenderRadioGroup.ItemIndex:=Idx;
end;

procedure TEditContactForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    with MainForm.ContactsSQLQuery.FieldByName('gender') do
    begin
      case GenderRadioGroup.ItemIndex of
        0: AsString := 'M';
        1: AsString := 'F';
      else
        Clear;
      end;
    end;
  end;
end;

end.

