unit SearchUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Cryptosystem;

type
  TSearchForm = class(TForm)
    InfoLbl: TLabel;
    CloseBtn: TButton;
    GoToBtn: TButton;
    ResultBox: TListBox;
    SearchEdit: TEdit;
    procedure CloseBtnClick(Sender: TObject);
    procedure GoToBtnClick(Sender: TObject);
    procedure ResultBoxClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure Search;
  end;

var
  SearchForm: TSearchForm;
  L: TList;

implementation

uses Main;

{$R *.dfm}

procedure TSearchForm.CloseBtnClick(Sender: TObject);
begin
 Close;
end;

procedure TSearchForm.Search;
Var
 I: Integer;
begin
 try
  { On va chercher toutes les lignes du fichier qui contiennent le texte recherché }
  { L sert à contenir les index (de fichier) des mots de passe retenus }
  L.Clear;
  ResultBox.Items.BeginUpdate;
  ResultBox.Items.Clear;
  if SearchEdit.Text <> '' then
   begin
    for I := 0 to Data.Count - 1 do
     if Pos(Lowercase(SearchEdit.Text), Lowercase(Data.Strings[I])) > 0 then
      with MainForm.List.Items[I] do
       begin
        ResultBox.Items.Add(Format('%s - %s [%s]', [Caption, SubItems.Strings[0], SubItems.Strings[1]]));
        L.Add(Ptr(I));
       end;
   end;
 finally
  ResultBox.Items.EndUpdate;
  ResultBox.ItemIndex := -1;
  GoToBtn.Enabled := (ResultBox.ItemIndex > -1);
 end;
end;

procedure TSearchForm.GoToBtnClick(Sender: TObject);
begin
 { On se place sur le mot de passe choisi }
 MainForm.List.ItemIndex := Longword(L.Items[ResultBox.ItemIndex]);
 { Puis on s'en va }
 Close;
end;

procedure TSearchForm.ResultBoxClick(Sender: TObject);
begin
 { Il faut sélectionner un truc }
 GoToBtn.Enabled := (ResultBox.ItemIndex > -1);
end;

procedure TSearchForm.SearchEditChange(Sender: TObject);
begin
 { Quand on change le texte à rechercher, on relance la recherche }
 Search;
end;

initialization
 L := TList.Create;

finalization
 L.Free;

end.
