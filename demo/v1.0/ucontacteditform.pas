unit uContactEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  ButtonPanel;

type

  { TEditContactForm }

  TEditContactForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EmailDBEdit: TDBEdit;
    PhoneDBEdit: TDBEdit;
    FirstNameDBEdit: TDBEdit;
    Label3: TLabel;
    Label4: TLabel;
    LastNameDBEdit: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  EditContactForm: TEditContactForm;

implementation

{$R *.lfm}

{ TEditContactForm }

procedure TEditContactForm.FormShow(Sender: TObject);
begin
  FirstNameDBEdit.SetFocus;
end;

end.

