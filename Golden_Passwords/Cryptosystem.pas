{
Ceci est le cryptosyst�me de Golden Passwords. C'est cette unit� qui g�re les acc�s aux fichiers,
la v�rification des mots de passe, l'encryptage/d�cryptage des donn�es, et la lecture/�criture des
donn�es.
}

unit Cryptosystem;

interface

uses SysUtils, Classes, LEA, SEA;

var
 F: TFileStream;    { Contient le fichier en cours }
 Data: TStringList; { Contient les donn�es (elles sont toujours d�crypt�es l�-dedans }
 CurrentFile, Pwd: String; { Contient le fichier actuel et le mot de passe actuel }
 { Attention : CurrentFile n'est pas g�r� par le cryptosyst�me, et Pwd l'est � moiti� -,- }

{ Les routines ... cr�ation de nouveau fichier, ouverture de fichier, v�rification de mot de passe,
r�cup�ration des donn�es, enregistrement du fichier et fermeture }
function CreateNew(FilePath, Password: String): Boolean;
function Load(FilePath: String): Boolean;
function CheckPassword(Password: String): Boolean;
function GetData(var Data: TStringList; Password: String): Boolean;
function Save(FilePath: String; Password: String): Boolean;
function Close: Boolean;

implementation

function CreateNew(FilePath, Password: String): Boolean;
Var
 H: THash;
begin
 Result := True;
 try
  { Rien de tr�s dur ... }
  F := TFileStream.Create(FilePath, fmCreate or fmShareExclusive);
  H := ObtainKey(Password);
  Encrypt(H, SizeOf(THash), H, OP_ENCRYPT);
  F.WriteBuffer(H, SizeOf(THash));
  Pwd := Password;
 except
  Result := False;
  F.Free;
 end;
end;

function Load(FilePath: String): Boolean;
begin
 Result := True;
 try
  { On ne fait que l'ouvrir }
  F := TFileStream.Create(FilePath, fmOpenReadWrite or fmShareExclusive);
 except
  Result := False;
  F.Free;
 end;
end;

function CheckPassword(Password: String): Boolean;
Var
 A, B: THash;
begin
 try
  { Dans A, on stocke l'empreinte du mot de passe saisi par l'utilisateur }
  A := ObtainKey(Password);
  F.Seek(0, soFromBeginning);
  { Dans B, on stocke l'empreinte du mot de passe du fichier }
  F.ReadBuffer(B, SizeOf(THash));
  { On d�crypte l'empreinte }
  Encrypt(B, SizeOf(THash), A, OP_DECRYPT);
  { On compare }
  Result := SameHash(A, B);
  { Si c'est diff�rent, on s'en va !! }
  if Result = False then raise Exception.Create('');
  Pwd := Password;
 except
  Result := False;
  F.Free;
 end;
end;

function GetData(var Data: TStringList; Password: String): Boolean;
Var
 Str: String;
begin
 Result := True;
 try
  { En fait, le fichier est structur� de la forme "Empreinte" + "Donn�es". Ce qui permet une lecture
    rapide et efficace. }
  { On se place apr�s l'empreinte du mot de passe }
  F.Seek(SizeOf(THash), soFromBeginning);
  { On initialise les donn�es lues � z�ro }
  Str := '';
  if F.Size > 64 then { Si il y a des donn�es }
   begin
    SetLength(Str, F.Size - F.Position); { On fait de la place pour �crire les donn�es }
    { On lit les donn�es }
    F.ReadBuffer(PAnsiChar(Str)^, Length(Str));
    { On les d�crypte }
    Encrypt(PAnsiChar(Str)^, Length(Str), ObtainKey(Password), OP_DECRYPT);
   end;
  try
   Data.BeginUpdate;
   Data.Clear;
   { On r�cup�re les donn�es d�crypt�es dans le texte de Data }
   Data.Text := Str;
  finally
   Data.EndUpdate;
  end;
 except
  Result := False;
 end;
end;

function Save(FilePath: String; Password: String): Boolean;
Var
 Str: String;
 H: THash;
begin
 Result := True;
 try
  { Pour l'enregistrement, on �crit l'empreinte crypt�e du mot de passe "Password" }
  H := ObtainKey(Password);
  Encrypt(H, SizeOf(THash), H, OP_ENCRYPT);
  F.Size := 0;
  F.Position := 0;
  F.WriteBuffer(H, SizeOf(THash));
  if Length(Data.Text) > 0 then { Si on a des donn�es � �crire }
   begin
    Str := Data.Text;
    { On les place dans Str, on encrypte Str, puis on �crit � la suite du fichier ... }
    Encrypt(PAnsiChar(Str)^, Length(Str), ObtainKey(Password), OP_ENCRYPT);
    F.WriteBuffer(PAnsiChar(Str)^, Length(Str));
   end;
 except
  Result := False;
 end;
end;

function Close: Boolean;
begin
 Result := True;
 try
  { On vide Data et on lib�re F }
  Data.Clear;
  F.Free;
 except
  Result := False;
 end;
end;

{ Data n'est jamais lib�r� pendant l'ex�cution, seulement � la fin }

initialization
 Data := TStringList.Create;

finalization
 Data.Free;

end.
