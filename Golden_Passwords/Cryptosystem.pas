{
Ceci est le cryptosystème de Golden Passwords. C'est cette unité qui gère les accès aux fichiers,
la vérification des mots de passe, l'encryptage/décryptage des données, et la lecture/écriture des
données.
}

unit Cryptosystem;

interface

uses SysUtils, Classes, LEA, SEA;

var
 F: TFileStream;    { Contient le fichier en cours }
 Data: TStringList; { Contient les données (elles sont toujours décryptées là-dedans }
 CurrentFile, Pwd: String; { Contient le fichier actuel et le mot de passe actuel }
 { Attention : CurrentFile n'est pas géré par le cryptosystème, et Pwd l'est à moitié -,- }

{ Les routines ... création de nouveau fichier, ouverture de fichier, vérification de mot de passe,
récupération des données, enregistrement du fichier et fermeture }
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
  { Rien de très dur ... }
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
  { On décrypte l'empreinte }
  Encrypt(B, SizeOf(THash), A, OP_DECRYPT);
  { On compare }
  Result := SameHash(A, B);
  { Si c'est différent, on s'en va !! }
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
  { En fait, le fichier est structuré de la forme "Empreinte" + "Données". Ce qui permet une lecture
    rapide et efficace. }
  { On se place après l'empreinte du mot de passe }
  F.Seek(SizeOf(THash), soFromBeginning);
  { On initialise les données lues à zéro }
  Str := '';
  if F.Size > 64 then { Si il y a des données }
   begin
    SetLength(Str, F.Size - F.Position); { On fait de la place pour écrire les données }
    { On lit les données }
    F.ReadBuffer(PAnsiChar(Str)^, Length(Str));
    { On les décrypte }
    Encrypt(PAnsiChar(Str)^, Length(Str), ObtainKey(Password), OP_DECRYPT);
   end;
  try
   Data.BeginUpdate;
   Data.Clear;
   { On récupère les données décryptées dans le texte de Data }
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
  { Pour l'enregistrement, on écrit l'empreinte cryptée du mot de passe "Password" }
  H := ObtainKey(Password);
  Encrypt(H, SizeOf(THash), H, OP_ENCRYPT);
  F.Size := 0;
  F.Position := 0;
  F.WriteBuffer(H, SizeOf(THash));
  if Length(Data.Text) > 0 then { Si on a des données à écrire }
   begin
    Str := Data.Text;
    { On les place dans Str, on encrypte Str, puis on écrit à la suite du fichier ... }
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
  { On vide Data et on libère F }
  Data.Clear;
  F.Free;
 except
  Result := False;
 end;
end;

{ Data n'est jamais libéré pendant l'exécution, seulement à la fin }

initialization
 Data := TStringList.Create;

finalization
 Data.Free;

end.
