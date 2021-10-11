{ Algorithme d'encryptage/décryptage SEA (Sequential Encoding Algorithm) }
{ Auteur : Bacterius (www.delphifr.com) }
{ Copyright : NE PAS MODIFIER CE FICHIER SANS ACCORD DE L'AUTEUR/
              DO NOT MODIFY THIS FILE WITHOUT AUTHOR PERMISSION  }

unit SEA;

interface

uses Windows, LEA;

type
 { Le type du callback : bloc n°Index sur Count }
 { Pour éviter de massacrer le temps d'execution du code, veillez à ne traiter qu'un certain nombre
   de callbacks (tous les 256 blocs par exemple) }
 TSEACallback = procedure (Index, Count: Longword);
 { Oui, c'est toujours plus lisible et ça change rien au niveau de la compilation }
 TSEAKey = THash;

 { Les buffers qui contiendront les blocs d'information }
 PSEABuf = ^TSEABuf;
 TSEABuf = array [$0..$F] of Longword;

const
 OP_ENCRYPT  = $1; { Encryptage }
 OP_DECRYPT  = $2; { Décryptage }

function ObtainKey(Str: String): TSEAKey;
function Encrypt(var Buffer; const Size: Longword; Key: TSEAKey; const Operation: Longword; Callback: TSEACallback = nil): Boolean;
function EncryptFile(const FilePath: String; const Key: TSEAKey; const Operation: Longword; Callback: TSEACallback = nil): Boolean;

implementation

{ Fonction pour obtenir une clef d'encryptage 512 bits à partir d'une chaîne }
function ObtainKey(Str: String): TSEAKey;
Var
 H: THash;
begin
 { Ajoute un salt devant la chaîne - mis à jour pour Golden Passwords }
 H := HashStr(HashToString(HashStr(Str)) + Str);
 Result := Hash(H, SizeOf(THash));
end;

{ Fonction de rotation de bits }
function RShl(A, B: Longword): Longword;
begin
 Result := (A shl B) or (A shr ($20 - B));
end;

{ Fonction d'encryptage }
procedure SEAEncrypt(var Buf: TSEABuf; const Key: TSEAKey);
begin
 Buf[$0] := Buf[$0] + Key[$8];
 Buf[$1] := Buf[$1] + Key[$E];
 Buf[$2] := Buf[$2] + Key[$7];
 Buf[$3] := Buf[$3] + Key[$3];
 Buf[$4] := Buf[$4] + Key[$C];
 Buf[$5] := Buf[$5] + Key[$1];
 Buf[$6] := Buf[$6] + Key[$D];
 Buf[$7] := Buf[$7] + Key[$9];
 Buf[$8] := Buf[$8] + Key[$2];
 Buf[$9] := Buf[$9] + Key[$5];
 Buf[$A] := Buf[$A] + Key[$B];
 Buf[$B] := Buf[$B] + Key[$0];
 Buf[$C] := Buf[$C] + Key[$F];
 Buf[$D] := Buf[$D] + Key[$4];
 Buf[$E] := Buf[$E] + Key[$6];
 Buf[$F] := Buf[$F] + Key[$A];
end;

{ Fonction de décryptage }
procedure SEADecrypt(var Buf: TSEABuf; const Key: TSEAKey);
begin
 Buf[$0] := Buf[$0] - Key[$8];
 Buf[$1] := Buf[$1] - Key[$E];
 Buf[$2] := Buf[$2] - Key[$7];
 Buf[$3] := Buf[$3] - Key[$3];
 Buf[$4] := Buf[$4] - Key[$C];
 Buf[$5] := Buf[$5] - Key[$1];
 Buf[$6] := Buf[$6] - Key[$D];
 Buf[$7] := Buf[$7] - Key[$9];
 Buf[$8] := Buf[$8] - Key[$2];
 Buf[$9] := Buf[$9] - Key[$5];
 Buf[$A] := Buf[$A] - Key[$B];
 Buf[$B] := Buf[$B] - Key[$0];
 Buf[$C] := Buf[$C] - Key[$F];
 Buf[$D] := Buf[$D] - Key[$4];
 Buf[$E] := Buf[$E] - Key[$6];
 Buf[$F] := Buf[$F] - Key[$A];
end;

{ La fonction d'encryptage : le Buffer sert d'entrée de données et de sortie de données }
function Encrypt(var Buffer; const Size: Longword; Key: TSEAKey; const Operation: Longword; Callback: TSEACallback = nil): Boolean;
Var
 P: PSEABuf;
 E: Pointer;
 Buf: TSEABuf;
 Sz, Cnt, BlockCount: Longword;
 H: TSEAKey;
 D: array [0..$1F] of Longword;
begin
 Result := False;
 Move(Key, H, SizeOf(THash));

 Cnt := $0;
 Sz := Size;
 while Sz mod $40 <> 0 do Inc(Sz);

 BlockCount := Size div $40;
 P := @Buffer;
 E := Ptr(Longword(@Buffer) + Sz);

 repeat
   Move(Key, D, SizeOf(THash));
   Move(H, D[$10], SizeOf(THash));
   Key := Hash(D, SizeOf(D));

   ZeroMemory(@Buf, $40);
   if Size - Cnt > $3F then Move(P^, Buf, $40) else Move(P^, Buf, Size - Cnt);

   case Operation of
    $1: SEAEncrypt(Buf, Key);
    $2: SEADecrypt(Buf, Key);
   end;

   if Size - Cnt > $3F then Move(Buf, P^, $40) else Move(Buf, P^, Size - Cnt);

   if Assigned(Callback) then Callback(Cnt div $40, BlockCount);

   Inc(P);
   Inc(Cnt, $40);
 until P = E;

 Result := True;
end;

function EncryptFile(const FilePath: String; const Key: TSEAKey; const Operation: Longword; Callback: TSEACallback = nil): Boolean;
Var
 H, M: Longword;
 P: Pointer;
begin
 Result := False;

 { On ouvre le fichier avec droits exclusifs }
 H := CreateFile(PChar(FilePath), GENERIC_READ or GENERIC_WRITE, 0,
                 nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);

 if H = INVALID_HANDLE_VALUE then Exit;

 { CreateFileMapping rejette les fichiers de taille 0. Notre fonction d'encryptage/décryptage les
   rejette également. Alors autant arrêter là plutôt que de bousiller des cycles inutiles. }
 if GetFileSize(H, nil) = 0 then
  begin
   CloseHandle(H);
   Exit;
  end;

 try
  { On crée une image du fichier en mémoire (lecture/écriture) }
  M := CreateFileMapping(H, nil, PAGE_READWRITE, 0, 0, nil);
  try
   if M = 0 then Exit;
   { On mappe le fichier en mémoire en lecture/écriture }
   P := MapViewOfFile(M, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
   try
    if P = nil then Exit;
    { Et on envoie le pointeur sur le fichier à la fonction. Attention, le fichier crypté/décrypté
      remplace l'original ! }
    Result := Encrypt(P^, GetFileSize(H, nil), Key, Operation, Callback);
   finally
   { On fait le ménage derrière nous ... }
   UnmapViewOfFile(P);
   end;
  finally
   CloseHandle(M);
  end;
 finally
  CloseHandle(H);
 end;
end;

end.
