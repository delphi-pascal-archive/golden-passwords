unit EditPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, Cryptosystem;

type
  TEditForm = class(TForm)
    PassLbl: TLabel;
    PassEdit: TEdit;
    CommentLbl: TLabel;
    CommentEdit: TEdit;
    ApplyBtn: TButton;
    ColorLbl: TLabel;
    ColorList: TComboBoxEx;
    ColourCodes: TImageList;
    CancelBtn: TButton;
    LoginLbl: TLabel;
    LoginEdit: TEdit;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  EditForm: TEditForm;
  Index: Integer;
  NewPwd: Boolean;

implementation

uses Main;

{$R *.dfm}

procedure TEditForm.CancelBtnClick(Sender: TObject);
begin
 Close;
end;

procedure TEditForm.FormCreate(Sender: TObject);
begin
 { On evite les scintillements }
 DoubleBuffered := True;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
 { Si c'est un nouveau mot de passe, on l'indique dans la barre de titre, sinon ... }
 if NewPwd then Caption := 'Nouveau mot de passe' else Caption := 'Personnaliser le mot de passe';

 with MainForm.List.Items[Index] do
  begin
   { On met les valeurs préalablement définies, pour une modification plus fluide et intuitive }
   LoginEdit.Text := Caption;
   PassEdit.Text := SubItems.Strings[0];
   CommentEdit.Text := SubItems.Strings[1];
   ColorList.ItemIndex := ImageIndex;
  end;
end;

procedure TEditForm.ApplyBtnClick(Sender: TObject);
begin
 { On met à jour le fichier }
 Data.Strings[Index] := Format('%s%s%s%s%s%s', [chr(ColorList.ItemIndex + 65), LoginEdit.Text, PARSE_CHAR, PassEdit.Text, PARSE_CHAR, CommentEdit.Text]);
 { On enregistre le fichier }
 if not Cryptosystem.Save(CurrentFile, Pwd) then raise Exception.Create(RES_SAVE_ERROR);
 { On met à jour }
 MainForm.ShowPasswords;
 MainForm.UpdateDisplay;
 { Et on ferme bien sûr }
 Close;
end;

end.
