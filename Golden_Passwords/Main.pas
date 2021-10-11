unit Main;

{
 "Exemple.gpf" password: DelphiSources
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, ImgList, Menus, ExtCtrls, Cryptosystem, StdCtrls, TrayIcon;

type
  TMainForm = class(TForm)
    ToolImgs: TImageList;
    ToolDisabled: TImageList;
    Menu: TMainMenu;
    FileHeader: TMenuItem;
    NewMenu: TMenuItem;
    OpenMenu: TMenuItem;
    CloseMenu: TMenuItem;
    QuitMenu: TMenuItem;
    List: TListView;
    EditMenu: TPopupMenu;
    ContainerPanel: TPanel;
    Divider4: TToolButton;
    Toolbar: TToolBar;
    QuitBtn: TToolButton;
    Divider1: TToolButton;
    NewBtn: TToolButton;
    OpenBtn: TToolButton;
    CloseBtn: TToolButton;
    Divider2: TToolButton;
    AddBtn: TToolButton;
    EditBtn: TToolButton;
    DeleteBtn: TToolButton;
    Divider3: TToolButton;
    ChangeBtn: TToolButton;
    SaveDlg: TSaveDialog;
    OpenDlg: TOpenDialog;
    PasswordsHeader: TMenuItem;
    AddMenu: TMenuItem;
    EditMMenu: TMenuItem;
    DeleteMenu: TMenuItem;
    N2: TMenuItem;
    ChangeMenu: TMenuItem;
    SortMMenu: TMenuItem;
    DeletePMenu: TMenuItem;
    N3: TMenuItem;
    HintBar: TStatusBar;
    AboutBtn: TToolButton;
    AboutHeader: TMenuItem;
    DownMenu: TMenuItem;
    UpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    N4: TMenuItem;
    PersonalizeMenu: TMenuItem;
    ColourCodes: TImageList;
    SearchMenu: TMenuItem;
    SearchBtn: TToolButton;
    TrayPopup: TPopupMenu;
    ShowMenu: TMenuItem;
    N1: TMenuItem;
    QuitTMenu: TMenuItem;
    StatusBar: TStatusBar;    
    procedure FormCreate(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure ListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure DeleteBtnClick(Sender: TObject);
    procedure EditMenuPopup(Sender: TObject);
    procedure ChangeBtnClick(Sender: TObject);
    procedure FileHeaderClick(Sender: TObject);
    procedure PasswordsHeaderClick(Sender: TObject);
    procedure SortMMenuClick(Sender: TObject);
    procedure ListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure AboutBtnClick(Sender: TObject);
    procedure UpMenuClick(Sender: TObject);
    procedure DownMenuClick(Sender: TObject);
    procedure EditMMenuClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SearchBtnClick(Sender: TObject);
    procedure QuitTMenuClick(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
    procedure ShowMenuClick(Sender: TObject);
    procedure FormMinimize(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    procedure ShowPasswords;
    procedure UpdateDisplay;
    procedure OpenOnRun(FilePath: String);
    procedure LogIt(S: String);
  end;

var
  MainForm: TMainForm;
  Tm: Longword;
  Tipped: Boolean;
  Log: TStringList;
  Tray: TTrayIcon;

const
 PARSE_CHAR = #1; { Le caract�re qui s�parera le login, le mot de passe et le commentaire }
 { J'ai choisi le caract�re #1 car il est improbable qu'un texte contienne ce caract�re }

resourcestring { Quelques cha�nes ressources pour afficher les erreurs }
 RES_NEW_ERROR = 'Erreur lors de la cr�ation du fichier.';
 RES_LOAD_ERROR = 'Erreur lors de l''ouverture du fichier.';
 RES_SAVE_ERROR = 'Erreur lors de la sauvegarde du fichier.';
 RES_CLOSE_ERROR = 'Erreur lors de la fermeture du fichier.';
 RES_CHECK_ERROR = 'Mot de passe incorrect.';
 RES_GET_ERROR = 'Erreur lors de la lecture du fichier.';

implementation

uses EditPassword, About, SearchUnit;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
 { On r�cup�re le temps du d�but (pour savoir combien de temps a dur� la session }
 Tm := GetTickCount;
 { On d�finit la proc�dure qui doit �tre appell�e lorsqu'on minimise }
 Application.OnMinimize := FormMinimize;
 { On cr�e notre ic�ne dans la zone de notification et on la param�tre }
 Tray := TTrayIcon.Create(self);
 Tray.Icon := Icon;
 Icon := Application.Icon;
 Tray.Hint := 'Golden Passwords';
 Tray.PopupMenu := TrayPopup;
 Tray.Active := True;
 { On �crit quelques trucs dans le log }
 Log.Add('[Log du ' + DateToStr(now) + ']');
 LogIt('');
 LogIt('Golden Passwords lanc�');
 LogIt('');
 { On evite les scintillements }
 { Notez que j'ai mis la Toolbar dans un TPanel, pour contrer au bug qui affiche la toolbar tout en
   noir avec DoubleBuffered et le th�me XP en m�me temps ... }
 DoubleBuffered := True;
 List.DoubleBuffered := True;
 Toolbar.DoubleBuffered := True;
 StatusBar.DoubleBuffered := True;
 { On redimensionne ... }
 List.Column[0].Width := 180;
 List.Column[1].Width := 180;
 List.Column[2].Width := 452;
 { Si l'on a pass� un fichier en ligne de commande, on va le chercher et l'ouvrir }
 if FindCmdLineSwitch('f', ['/', '-'], True) then
  if FileExists(ParamStr(2)) then OpenOnRun(ParamStr(2));
end;

{ Lors d'une minimisation, la fiche doit dispara�tre et NE PAS appara�tre dans la barre des t�ches }
procedure TMainForm.FormMinimize(Sender: TObject);
begin
 { On rend invisible }
 Hide;
 { On affiche une petite bulle d'aide dans la petite ic�ne pour informations }
 if not Tipped then { On ne l'affiche qu'une fois durant toute l'ex�cution du programme }
  begin
   Tray.BalloonHint('Golden Passwords', 'L''application est r�duite dans la zone de notification, et prend tr�s peu de ressources. Vous pouvez continuer � travailler, en ayant toutefois un acc�s rapide � vos mots de passe.', 15, biInfo);
   Tipped := True;
  end;
end;

{ Ajoute une ligne dans le log }
procedure TMainForm.LogIt(S: String);
Var
 T: String;
begin
 DateTimeToString(T, 'hh:nn:ss', now);
 if S <> '' then Log.Add(Format('[%s] %s.', [T, S])) else Log.Add(S);
end;

{ Si la ligne de commande contient un fichier, on l'ouvre directement dans cette routine }
procedure TMainForm.OpenOnRun(FilePath: String);
Var
 S: String;
 E: Integer;
begin
 E := 0;
 { On demande le mot de passe de ce fichier }
 if InputQuery('Ouvrir un fichier', 'Saisissez le mot de passe de ce fichier :', S) then
  try
   { On essaye d'ouvrir le fichier }
   if not Cryptosystem.Load(FilePath) then begin E := 1; raise Exception.Create(RES_LOAD_ERROR); end;
   { On v�rifie le mot de passe }
   if not Cryptosystem.CheckPassword(S) then begin E := 2; raise Exception.Create(RES_CHECK_ERROR); end;
   { Enfin, on r�cup�re les infos }
   if not Cryptosystem.GetData(Data, S) then begin E := 3; raise Exception.Create(RES_GET_ERROR); end;
   { On d�finit le fichier en cours }
   LogIt(Format('Ouverture du fichier "%s" : Succ�s', [ExtractFileName(FilePath)]));
   CurrentFile := FilePath;
   { On affiche les mots de passe }
   ShowPasswords;
   { On met � jour l'affichage (boutons, barre de status, ...) }
   UpdateDisplay;
  except
   LogIt(Format('Ouverture du fichier "%s" : Echec', [ExtractFileName(FilePath)]));
   case E of
    1: raise Exception.Create(RES_LOAD_ERROR);
    2: raise Exception.Create(RES_CHECK_ERROR);
    3: raise Exception.Create(RES_GET_ERROR);
   end;
  end;
end;

{ Pierre angulaire de l'application, cette routine s�pare une cha�ne fichier en trois parties :
  - le code couleur du mot de passe
  - le login
  - le mot de passe
  - son commentaire                                                                           }
procedure ParseStr(Str: String; var Clr: Integer; var A, B, C: String; SepChar: Char);
begin
 { Pour le code couleur ... rien de tr�s dur }
 Clr := ord(Str[1]);
 { On supprime le premier caract�re de la cha�ne }
 Str := Copy(Str, 2, Length(Str));
 { On r�cup�re le login : il se trouve entre le d�but et PARSE_CHAR }
 A := Copy(Str, 1, Pos(SepChar, Str) - 1);
 Str := Copy(Str, Length(A) + 2, Length(Str));
 { On r�cup�re le mot de passe }
 B := Copy(Str, 1, Pos(SepChar, Str) - 1);
 { Puis on r�cup�re le reste moins PARSE_CHAR pour avoir le commentaire }
 C := Copy(Str, Length(B) + 2, Length(Str) - Length(B));
end;

{ Cette routine affiche les mots de passe dans la liste }
procedure TMainForm.ShowPasswords;
Var
 I, Clr, N: Integer;
 A, B, C: String;
begin
 { On m�morise l'index avant traitement }
 N := List.ItemIndex;
 try
  { On vide la liste }
  List.Items.BeginUpdate;
  List.Items.Clear;
  { Pour chaque �l�ment du fichier }
  for I := 0 to Data.Count - 1 do
   begin
    { On parse la cha�ne fichier }
    ParseStr(Data.Strings[I], Clr, A, B, C, PARSE_CHAR);
    with List.Items.Add do { On ajoute un nouvel �l�ment dans la liste }
     begin
      { On affiche le mot de passe, le commentaire ... }
      Caption := A;
      SubItems.Add(B);
      SubItems.Add(C);
      { Dans le fichier, les codes couleur commen�ent � 65 pour �viter des caract�res g�nants }
      ImageIndex := Clr - 65;
     end;
   end;
 finally
  { On r�autorise la liste � s'afficher }
  List.Items.EndUpdate;
  { Si l'index m�moris� au d�but existe toujours, on se place dessus }
  if N > List.Items.Count - 1 then List.ItemIndex := - 1 else List.ItemIndex := N;
 end;
end;

procedure TMainForm.UpdateDisplay;
Var
 Opened, Selecting: Boolean;
begin
 { On actualise les boutons et tout }
 Opened := (CurrentFile <> '');
 Selecting := (List.ItemIndex > -1);
 NewBtn.Enabled := not Opened;
 OpenBtn.Enabled := not Opened;
 CloseBtn.Enabled := Opened;
 AddBtn.Enabled := Opened;
 SearchBtn.Enabled := Opened and (List.Items.Count > 0);
 EditBtn.Enabled := Opened and Selecting;
 DeleteBtn.Enabled := Opened and Selecting;
 ChangeBtn.Enabled := Opened;
 { On affiche le fichier en cours dans le libell� de la fiche }
 if CurrentFile = '' then Caption := 'Golden Passwords' else Caption := Format('Golden Passwords [%s]', [ExtractFileName(CurrentFile)]);
 with StatusBar.Panels[0] do
  begin
   { On formate la barre de status sup�rieure }
   if CurrentFile = '' then Text := 'No file' else Text := Format('%s [Password: "%s"]', [ExtractFileName(CurrentFile), Pwd]);
   if Data.Count = 0 then Text := Text + ' - No records.' else Text := Text + Format(' - %u records.', [Data.Count]);
  end;
end;

procedure TMainForm.QuitBtnClick(Sender: TObject);
begin
 Close; { On ferme l'application }
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
Var
 H, M, S: Longword;
begin
 { Petit message de confirmation }
 if MessageDlg('Do you really want exit from application?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
  begin
   Action := caNone;
   Exit;
  end;

 if CurrentFile <> '' then CloseBtnClick(self); { Si on a un fichier en cours }

 { On calcule la dur�e de la session }
 Tm := (GetTickCount - Tm) div 1000;
 H := Tm div 3600;
 Tm := Tm - (H * 3600);
 M := Tm div 60;
 Tm := Tm - (M * 60);
 S := Tm;

 { On ach�ve le log }
 LogIt(Format('Fermeture de Golden Passwords (Dur�e de la session : %uh, %um %us)', [H, M, S]));
 Log.SaveToFile(ExtractFilePath(Application.ExeName) + 'Log.txt');
 { On lib�re l'ic�ne dans la zone de notification }
 Tray.Free;
end;

{ Cette routine cr�e un nouveau fichier }
procedure TMainForm.NewBtnClick(Sender: TObject);
Var
 S: String;
begin
 if SaveDlg.Execute then
  if InputQuery('New file', 'Enter password for new file:', S) then
   try
    { On appelle CreateNew ... }
    if not Cryptosystem.CreateNew(SaveDlg.FileName, S) then raise Exception.Create(RES_NEW_ERROR);
    { On d�finit CurrentFile }
    LogIt(Format('Creation file "%s": Success', [ExtractFileName(SaveDlg.FileName)]));
    CurrentFile := SaveDlg.FileName;
    { On affiche et on met � jour }
    ShowPasswords;
    UpdateDisplay;
   except
    LogIt(Format('Creation file "%s": Error', [ExtractFileName(SaveDlg.FileName)]));
    raise Exception.Create(RES_NEW_ERROR);
   end;
end;

{ Cette routine ouvre un fichier existant }
procedure TMainForm.OpenBtnClick(Sender: TObject);
Var
 S: String;
 E: Integer;
begin
 E := 0;
 if OpenDlg.Execute then
  if InputQuery('Open file', 'Enter password:', S) then
   try
    { On ouvre, on v�rifie le mot de passe, on lit le fichier, et on affiche }
    if not Cryptosystem.Load(OpenDlg.FileName) then begin E := 1; raise Exception.Create(RES_LOAD_ERROR); end;
    if not Cryptosystem.CheckPassword(S) then begin E := 2; raise Exception.Create(RES_CHECK_ERROR); end;
    if not Cryptosystem.GetData(Data, S) then begin E := 3; raise Exception.Create(RES_GET_ERROR); end;
    LogIt(Format('Open file "%s": Success', [ExtractFileName(OpenDlg.FileName)]));
    CurrentFile := OpenDlg.FileName;
    ShowPasswords;
    UpdateDisplay;
   except
    LogIt(Format('Open file "%s": Error', [ExtractFileName(OpenDlg.FileName)]));
    case E of
     1: raise Exception.Create(RES_LOAD_ERROR);
     2: raise Exception.Create(RES_CHECK_ERROR);
     3: raise Exception.Create(RES_GET_ERROR);
    end;
   end;
end;

{ Bouton pour fermer le fichier en cours }
procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
 { On tente de fermer le fichier }
 try
  if not Cryptosystem.Close then raise Exception.Create(RES_CLOSE_ERROR);
  LogIt(Format('Clear file "%s": Success', [ExtractFileName(CurrentFile)]));
  LogIt('');
 except
  LogIt(Format('Clear file "%s": Error', [ExtractFileName(CurrentFile)]));
  LogIt('');
  raise Exception.Create(RES_CLOSE_ERROR);
 end;
 { Aucun fichier maintenant }
 CurrentFile := '';
 { On met � jour }
 ShowPasswords;
 UpdateDisplay;
end;

{ Bouton pour ajouter un mot de passe }
procedure TMainForm.AddBtnClick(Sender: TObject);
begin
 LogIt('Ajout d''un mot de passe');
 { On ajoute un nouveau mot de passe � la fin du fichier }
 Data.Add(chr(65) + 'Nouveau login' + PARSE_CHAR + 'Nouveau mot de passe' + PARSE_CHAR + 'Placez un commentaire ici.');
 ShowPasswords;
 { On affiche la fen�tre de personnalisation }
 NewPwd := True;
 Index := Data.Count - 1;
 EditForm.ShowModal;
end;

procedure TMainForm.ListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 { Quand on clique sur la vue liste, on met � jour les boutons selon leur disponibilit� }
 EditBtn.Enabled := (CurrentFile <> '') and (List.ItemIndex > -1);
 DeleteBtn.Enabled := (CurrentFile <> '') and (List.ItemIndex > -1);
end;

{ Bouton pour supprimer un mot de passe }
procedure TMainForm.DeleteBtnClick(Sender: TObject);
begin
 if List.ItemIndex = -1 then Exit;
 { Demande de confirmation }
 if MessageDlg('�tes-vous s�r de vouloir supprimer ce mot de passe ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
   LogIt(Format('Suppression du mot de passe "%s - %s"', [List.Items[List.ItemIndex].Caption, List.Items[List.ItemIndex].SubItems.Strings[0]]));
   { On supprime du fichier }
   Data.Delete(List.ItemIndex);
   { On enregistre }
   if not Cryptosystem.Save(CurrentFile, Pwd) then raise Exception.Create(RES_SAVE_ERROR);
   { On met � jour }
   ShowPasswords;
   UpdateDisplay;
  end;
end;

procedure TMainForm.EditMenuPopup(Sender: TObject);
begin
 { Quand le menu surgissant appara�t, il met � jour ses �l�ments selon leur disponibilit� }
 DeletePMenu.Enabled := (CurrentFile <> '') and (List.ItemIndex > -1);
 PersonalizeMenu.Enabled := (CurrentFile <> '') and (List.ItemIndex > -1);
 DownMenu.Enabled := (CurrentFile <> '') and (List.ItemIndex < List.Items.Count - 1) and (List.ItemIndex > -1);
 UpMenu.Enabled := (CurrentFile <> '') and (List.ItemIndex > 0);
end;

{ Permet de changer le mot de passe pour acc�der au fichier }
procedure TMainForm.ChangeBtnClick(Sender: TObject);
Var
 S: String;
begin
 S := Pwd; { On met le mot de passe actuel dans l'InputQuery }
 if InputQuery('change password', 'Enter new password:', S) then
  begin
   { On d�finit le mot de passe }
   LogIt(Format('Modification du mot de passe du fichier (%s)', [S]));
   Pwd := S;
   { On enregistre }
   if not Cryptosystem.Save(CurrentFile, Pwd) then raise Exception.Create(RES_SAVE_ERROR);
   { On affiche }
   UpdateDisplay;
  end;
end;

procedure TMainForm.FileHeaderClick(Sender: TObject);
begin
 { Rien de tr�s dur ... }
 NewMenu.Enabled := NewBtn.Enabled;
 OpenMenu.Enabled := OpenBtn.Enabled;
 CloseMenu.Enabled := CloseBtn.Enabled;
end;

procedure TMainForm.PasswordsHeaderClick(Sender: TObject);
begin
 { Idem ... }
 AddMenu.Enabled := AddBtn.Enabled;
 DeleteMenu.Enabled := DeleteBtn.Enabled;
 EditMMenu.Enabled := EditBtn.Enabled;
 ChangeMenu.Enabled := ChangeBtn.Enabled;
 SortMMenu.Enabled := (CurrentFile <> '');
 SearchMenu.Enabled := SearchBtn.Enabled;
end;

procedure TMainForm.SortMMenuClick(Sender: TObject);
Var
 I: Integer;
begin
 try
  LogIt('Tri par couleur des mots de passe');
  List.Items.BeginUpdate;
  { On appelle le tri par couleur en passant le param�tre $12345678 }
  List.CustomSort(nil, $12345678);
  { Apr�s le tri, on va totalement r��crire le fichier }
  for I := 0 to List.Items.Count - 1 do
   with List.Items[I] do Cryptosystem.Data.Strings[I] := chr(65 + ImageIndex) + Caption + PARSE_CHAR + SubItems.Strings[0] + PARSE_CHAR + SubItems.Strings[1];
  { Puis on enregistre ! }
  if not Cryptosystem.Save(CurrentFile, Pwd) then raise Exception.Create(RES_SAVE_ERROR);
 finally
  List.Items.EndUpdate;
 end;
end;

procedure TMainForm.ListCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
 { Si le param�tre est $12345678, il s'agit d'un tri par couleur }
 if Data = $12345678 then Compare := Item1.ImageIndex - Item2.ImageIndex;
end;

procedure TMainForm.AboutBtnClick(Sender: TObject);
begin
 AboutBox.ShowModal; { On affiche la bo�te � propos }
end;

procedure TMainForm.UpMenuClick(Sender: TObject);
begin
 if List.ItemIndex > 0 then
  begin
   { Si l'on peut, on fait monter l'�l�ment d'un cran }
   Data.Exchange(List.ItemIndex, List.ItemIndex - 1);
   if not Cryptosystem.Save(CurrentFile, Pwd) then raise Exception.Create(RES_SAVE_ERROR);
   ShowPasswords;
   List.ItemIndex := List.ItemIndex - 1;
  end;
end;

procedure TMainForm.DownMenuClick(Sender: TObject);
begin
 if (List.ItemIndex < List.Items.Count - 1) and (List.ItemIndex > -1) then
  begin
   { Si l'on peut, on fait descendre l'�l�ment d'un cran }
   Data.Exchange(List.ItemIndex, List.ItemIndex + 1);
   if not Cryptosystem.Save(CurrentFile, Pwd) then raise Exception.Create(RES_SAVE_ERROR);
   ShowPasswords;
   List.ItemIndex := List.ItemIndex + 1;
  end;
end;

procedure TMainForm.EditMMenuClick(Sender: TObject);
begin
 { On ouvre la fen�tre de personnalisation }
 if List.ItemIndex = -1 then Exit;
 NewPwd := False;
 Index := List.ItemIndex;
 EditForm.ShowModal;
end;

procedure TMainForm.EditBtnClick(Sender: TObject);
begin
 { Idem ... }
 EditMMenuClick(self);
end;

procedure TMainForm.ListDblClick(Sender: TObject);
begin
 { Quand on double-clique sur la liste, �a revient � ouvrir la personnalisation }
 if CurrentFile <> '' then EditMMenuClick(self);
end;

procedure TMainForm.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 { Page pr�c�dente : on remonte l'�l�ment. Page suivante : on le descend }
 case Key of
  VK_PRIOR: begin UpMenuClick(self); Key := 0; end;
  VK_NEXT: begin DownMenuClick(self); Key := 0; end;
 end;
 { le "Key := 0" sert � eviter que la liste ne reprenne un ItemIndex � 0 (comportement par d�faut) }
end;

procedure TMainForm.SearchBtnClick(Sender: TObject);
begin
 { On affiche la fen�tre de recherche }
 SearchForm.ShowModal;
end;

procedure TMainForm.QuitTMenuClick(Sender: TObject);
begin
 { Je devrais m�me plus commenter ces routines }
 Close;
end;

procedure TMainForm.TrayPopupPopup(Sender: TObject);
begin
 { Selon la visibilit� de la fiche, on �crit logiquement l'action que va effectuer le menu }
 case Visible of
  False: ShowMenu.Caption := 'Show';
  True:  ShowMenu.Caption := 'Hide';
 end;
end;

procedure TMainForm.ShowMenuClick(Sender: TObject);
begin
 { On inverse la visibilit� }
 case Visible of
  False: begin Visible := True; Application.Restore; end;
  True: FormMinimize(self);
 end;
end;

{ Le log se cr�e au d�but et se lib�re � la fin }
initialization
 Log := TStringList.Create;

finalization
 Log.Free;

end.
