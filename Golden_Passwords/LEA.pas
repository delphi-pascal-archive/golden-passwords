{
               _              ____________            ____
              | |            |  __________|          /  _ \
              | |            | |                    / /  \ \
              | |            | |                   / /    \ \
              | |            | |_______           / /      \ \
              | |            |  _______|         / _________\ \
              | |            | |                / ____________ \
              | |            | |               / /            \ \
              | |_________   | |__________    / /              \ \
              |___________|  |____________|  /_/                \_\

                                 ______________
                               |               |
                               |   Bacterius   |
                               |_______________|

________________________________________________________________________________


LEA hash algorithm implementation for 32, 64, 96, 128, 160, 192, 224, 256, 384 and 512 bits.
Author : Bacterius.
Remark : the hash is always computed on 512 bits, and the algorithm only returns the
required number of bits. The default version is 128-bit.
Note : some versions of the algorithm must not be cryptographically used ! Here is a list :
32-bit  : only for file checksums.
64-bit  : only for file checksums.
96-bit  : can be used as a cryptographic key for encryption algorithms.
128-bit : can be used cryptographically for personal use.
160-bit and more : can be used cryptographically for any use.

Copyrighted : you can use this source code for personal use, and can modify the header at
your convenance for personal use. However, you need written agreement of the author for
commercial use. If you have any improvements, I would love to get a copy of your work to
update my own code (stating your name/pseudo/whatever you want in the updated header, of course).
Anyway, feel free to use it, and if you need a written agreement, don't be afraid, I don't bite :-)

________________________________________________________________________________

Implémentation de l'algorithme de hash LEA sous 32, 64, 96, 128, 160, 192, 224, 256, 384 et 512 bits.
Auteur : Bacterius.
Remarque : le hash est toujours calculé sur 512 bits, et l'algorithme renvoie uniquement
le nombre requis de bits. La version par défaut est 128 bits.
Note : certaines versions de l'algorithme ne doivent pas être utilisées à des fins cryptographiques !
Voici une liste :
32 bits  : uniquement pour des sommes de contrôle de fichiers.
64 bits  : uniquement pour des sommes de contrôle de fichiers.
96 bits  : peut être utilisée comme une clef cryptographique pour des algorithmes de chiffrement.
128 bits : peut être utilisée cryptographiquement à des fins personnelles.
160 bits et plus : peut être utilisée cryptographiquement pour toute utilisation.

Copyrighté : vous pouvez utiliser ce code source pour votre utilisation personnelle, et vous pouvez
modifier l'en-tête à votre convenance pour une utilisation personnelle. Cependant, vous devez
obtenir une autorisation écrite de l'auteur pour une utilisation commerciale. Si vous avez quelque
amélioration à porter sur ce code, je serai ravi d'obtenir une copie de vos travaux pour mettre à
jour mon propre code (mentionnant votre nom/pseudo/comme vous voudrez dans l'en-tête mis à jour,
evidemment). Dans tous les cas, sentez-vous libre d'utiliser ce code, et si vous avez besoin d'un
accord écrit, n'ayez pas peur, je ne mords pas :-)
________________________________________________________________________________

Contact : thomas.beneteau@yahoo.fr
________________________________________________________________________________

Thanks to : Cirec (www.delphifr.com) for UNICODE compatibility for newer Delphi versions.
Merci à : Cirec (www.delphifr.com) pour la compatilibité UNICODE pour les versions de Delphi récentes.

In date of : Wednesday, August 5th, 2009.
En date de : Mercredi 5 Août 2009.

}

unit LEA;

interface

uses Windows, SysUtils;

const
  { The initialization vectors. These values are the signature of the LEA algorithm, do not modify.
    Les vecteurs d'initialisation. Ces valeurs sont la signature de l'algorithme LEA, ne pas modifier. }
  HashInit: array [$0..$F] of Longword = ($2C8715EE, $C2CAA0E7, $EF51B6D5, $1BD8CCC6,
                                          $74E6F89E, $A16E0E92, $FA7C3A6E, $2703505A,
                                          $80117C32, $05A6BE16, $322DD3F3, $B7C315BF,
                                          $10D141A9, $3D585799, $96668386, $1BFBC539);

  { The hash table. These values offer a faster avalanche effect. Do not modify.
    La table de hashage. Ces valeurs offrent un effet d'avalanche plus rapide, ne pas modifier. }
  HashTable: array [$0..$FF] of Longword = (
  $3919F7FE, $55A6F3FF, $8EC0EBFF, $C7DAE3FF, $3A0ED3FF, $7328CBFE, $E55CBBFA, $1E76B3F6,
  $90AAA3F4, $3BF88BF2, $751283F0, $20606BE1, $92945BF9, $CBAE53D8, $3DE243E3, $E9302BCC,
  $947E13CF, $CD980BD1, $78E5F3E5, $EB19E3D5, $2433DBED, $CF81C3CF, $41B5B3DF, $ED039BCE,
  $D16B7BEE, $439F6BA4, $7CB963FC, $EEED53C6, $28074B9C, $9A3B3BA6, $29F10392, $9C24F3E0,
  $4772DB7C, $808CD3D2, $9E0EABE7, $D728A36C, $82768BC1, $2DC47370, $9FF86361, $4B464B93,
  $F69433B6, $2FAE2BC5, $4D3003D5, $8649FBAD, $F87DEB62, $3197E363, $8833B3CD, $DECF8355,
  $5103734B, $8A1D6B24, $FC515BDE, $A79F43CE, $E0B93B88, $FE3B13E8, $A988FB73, $54D6E3DA,
  $0024CB0B, $393EC38D, $E48CAB9F, $56C09AEE, $8FDA9311, $AD5C6BB5, $3D1233C1, $AF4622DE,
  $E8601B72, $5A940AD4, $EA49D3AA, $9597BBA5, $B31992BC, $EC338B35, $5E677AAD, $09B56302,
  $EE1D433D, $996B2B68, $44B91334, $B6ED03E0, $623AEB5C, $46A2CAAD, $B8D6BB89, $9D3E9A7D,
  $BAC073D9, $F3DA6A63, $115C435C, $4A763ABF, $F5C423AF, $67F81383, $1345FB7C, $F7ADDB2F,
  $69E1CA3D, $A2FBC370, $152FB2D3, $6BCB82C3, $50336270, $C26752BB, $A6CF3223, $190322F1,
  $C4510A1A, $1AECDBBA, $5406D209, $55F08AAA, $013E72E4, $1EC04BEC, $CA0E3216, $755C1B10,
  $AE7613E9, $59C3FB70, $7745D284, $2293BBAF, $CDE1A37C, $06FB9B2F, $B24982C9, $5D976AE9,
  $CFCB5B4B, $08E55267, $5F8121AB, $7D02FA76, $B61CF1A0, $2850E2BE, $D39ECBEB, $7EECB235,
  $B806A97C, $0EA27BA2, $80D66AEF, $2C24517C, $108C32DC, $2E0E0B8F, $1275EB25, $2FF7C1FD,
  $145FA1F2, $BFAD893D, $6AFB722F, $DD2F639D, $C197422A, $6CE52913, $DF191931, $C380FBD6,
  $35B4EB25, $C56AB246, $E2EC8B49, $39885BEC, $72A25372, $90242B4E, $C93E23DA, $3B721393,
  $748C0A6C, $920DE2E7, $21C3A9D9, $93F799D6, $CD1190AB, $3F458202, $CEFB4BF1, $412F3918, 
  $7A4932DA, $EC7D21C0, $2780D1F8, $99B4C285, $7E1CA20C, $9B9E7BA5, $80065AD7, $F23A4AB0, 
  $9D883229, $48D61BE6, $D88BE08C, $4ABFD22B, $F60DBBF9, $A15BA1E5, $85C38317, $31116BB8, 
  $87AD3835, $F9E129FC, $A52F1112, $DE4909D9, $FBCAE017, $34E4DA0B, $E032C2FD, $FDB49B84, 
  $36CE93C5, $5450697C, $8D6A63EC, $38B84B3B, $3AA20357, $ACD5F0A4, $E5EFEB46, $5823DB60, 
  $0371C25F, $AEBFA810, $93278A76, $3E7571C1, $E9C35A98, $5DE100A1, $96FAF92F, $B47CD08B,
  $98E4B185, $B6668B41, $61B47283, $0D025800, $F16A3A4B, $48060760, $BA39F7C6, $6587DFFD,
  $10D5C781, $49EFC33B, $F53DA95E, $4BD9778A, $695B53C5, $6B4507B3, $A45F01F0, $1692F3DC,
  $C1E0D7A7, $FAFAD137, $A648B71B, $187CA9BB, $5196A116, $C3CA90C1, $1A665F73, $53805861,
  $FECE42E5, $C987B822, $74D5A163, $202387FA, $048B680B, $0675219C, $23F6F988, $B3ACC385,
  $25E0B06A, $5EFAA98A, $D12E9B6E, $7C7C7F47, $60E462FE, $D3184FD2, $0C324B3F, $B7802F30,
  $0E1C037D, $2B9DD757, $64B7CE43, $D6EBBF1E, $1005B901, $8239A9BF, $2D8790AB, $84236201, 
  $DABF334F, $BF2710B8, $15C2DF83, $C110CAC2, $3344B7DA, $DE929F81, $C2FA81F0, $352E700A, 
  $19965155, $8BCA3DFF, $1B800828, $8DB3F5C9, $3901E397, $721BD869, $E44FC6CB, $8F9DB1AA);

type
  { A LEA-512 structure. All longs are computed, but some are ignored depending on the bitlength. }
  { Une structure LEA-512. Tous les entiers sont calculés, mais certains sont ignorés selon la taille. }
  THash = array [$0..$F] of Longword;

  { A LEA callback. The BlockIndex parameter indicates the current block (starting on 0), and the
    BlockCount parameter indicates the total number of blocks. }
  { Un callback LEA. Le paramètre BlockIndex indique le bloc actuel (commençant à 0), et le paramètre
    BlockCount indique le nombre total de blocs. }
  TLEACallback = procedure (BlockIndex: Longword; BlockCount: Longword); stdcall;

{ Hash computing functions - Fonctions de calcul de hash }
function Hash         (const Buffer; const Size: Longword; Callback: TLEACallback = nil): THash;
function HashStr      (const Str     : AnsiString  ): THash;
function HashFile     (const FilePath: String; Callback: TLEACallback = nil): THash;
{ Hash textual management - Gestion textuelle des hashs }
function HashToString (const Hash    : THash   ): AnsiString;
function StringToHash (const Str     : AnsiString  ): THash;
{ Hash compare - Comparaison de hashs }
function SameHash     (const A, B    : THash   ): Boolean;
function Same         (A, B: Pointer; SzA, SzB: Longword): Boolean;
{ Hash security features - Fonctions de sécurité de hash }
function HashCrypt    (const Hash    : THash;  Key: AnsiString): THash;
function HashUncrypt  (const Hash    : THash;  Key: AnsiString): THash;
{ Hash evaluation - Evaluation de hashs }
function IsHash       (const Hash    : AnsiString  ): Boolean;

type
 { Enumerated type containing each bitlength supported by LEA }
 { Type énuméré contenant chaque taille de bit supporté par LEA }
 TLEASize = (ls32, ls64, ls96, ls128, ls160, ls192, ls224, ls256, ls384, ls512);

const
 { The string size corresponding at the bitlength - La taille de chaîne associée à la longueur de bit }
 HashLen: array [TLEASize] of Longword = ($08, $10, $18, $20, $28, $30, $38, $40, $60, $80);
 { The byte size corresponding at the bitlength - La taille en octets associée à la longueur de bit }
 HashSz : array [TLEASize] of Longword = ($04, $08, $0C, $10, $14, $18, $1C, $20, $30, $40);

var
 { The current bitlength (default 128) - La longueur de bits actuelle (128 par défaut) }
 { Modify this variable at any time  - Modifiez cette valeur à tout moment }
 LEASize: TLEASize = ls128;

implementation

type
 PLEABuf = ^TLEABuf;
 TLEABuf = array [$0..$F] of Longword;

function RShl(A, B: Longword): Longword;
begin
 Result := (A shl B) or (A shr ($20 - B));
end;

function E(A, B, C, D: Longword): Longword;
begin
 Result := (A xor B) + (C or D);
end;

function F(A, B, C, D: Longword): Longword;
begin
 Result := ((A + C) and RShl(B, $5)) xor not D;
end;

function G(A, B, C, D: Longword): Longword;
begin
 Result := RShl(A or D + C, B mod $20);
end;

function H(A, B, C, D: Longword): Longword;
begin
 Result := not A xor (B and (C or not D));
end;

procedure LEAInternal(var Hash: THash; Buf: TLEABuf);
type
 LongBytes = record A, B, C, D: Byte; end;
Var
 I: Longword;
begin
 for I := $0 to $F do with LongBytes(Buf[I]) do
  begin
   Inc(Hash[$0], Buf[$0]  + HashTable[A]  + E(Hash[$0], Buf[$3], HashTable[C], Hash[$A]));
   Hash[$0] := RShl(Hash[$0], $1) + Hash[$8];
   Inc(Hash[$1], Buf[$1]  + HashTable[B]  + F(Hash[$1], Buf[$F], HashTable[D], Hash[$9]));
   Hash[$1] := RShl(Hash[$1], $9) + Hash[$A];
   Inc(Hash[$2], Buf[$2]  + HashTable[C]  + G(Hash[$2], Buf[$2], HashTable[C], Hash[$4]));
   Hash[$2] := RShl(Hash[$2], $6) + Hash[$2];
   Inc(Hash[$3], Buf[$3]  + HashTable[D]  + H(Hash[$3], Buf[$C], HashTable[B], Hash[$3]));
   Hash[$3] := RShl(Hash[$3], $8) + Hash[$0];
   Inc(Hash[$4], Buf[$4]  + HashTable[C]  + G(Hash[$4], Buf[$5], HashTable[A], Hash[$E]));
   Hash[$4] := RShl(Hash[$4], $4) + Hash[$F];
   Inc(Hash[$5], Buf[$5]  + HashTable[B]  + F(Hash[$5], Buf[$E], HashTable[B], Hash[$5]));
   Hash[$5] := RShl(Hash[$5], $B) + Hash[$9];
   Inc(Hash[$6], Buf[$6]  + HashTable[A]  + E(Hash[$6], Buf[$0], HashTable[C], Hash[$B]));
   Hash[$6] := RShl(Hash[$6], $0) + Hash[$7];
   Inc(Hash[$7], Buf[$7]  + HashTable[B]  + F(Hash[$7], Buf[$7], HashTable[D], Hash[$7]));
   Hash[$7] := RShl(Hash[$7], $A) + Hash[$1];
   Inc(Hash[$8], Buf[$8]  + HashTable[C]  + G(Hash[$8], Buf[$1], HashTable[C], Hash[$F]));
   Hash[$8] := RShl(Hash[$8], $5) + Hash[$6];
   Inc(Hash[$9], Buf[$9]  + HashTable[D]  + H(Hash[$9], Buf[$6], HashTable[B], Hash[$C]));
   Hash[$9] := RShl(Hash[$9], $F) + Hash[$4];
   Inc(Hash[$A], Buf[$A]  + HashTable[C]  + G(Hash[$A], Buf[$9], HashTable[A], Hash[$6]));
   Hash[$A] := RShl(Hash[$A], $E) + Hash[$E];
   Inc(Hash[$B], Buf[$B]  + HashTable[B]  + F(Hash[$B], Buf[$B], HashTable[B], Hash[$1]));
   Hash[$B] := RShl(Hash[$B], $D) + Hash[$B];
   Inc(Hash[$C], Buf[$C]  + HashTable[A]  + E(Hash[$C], Buf[$A], HashTable[C], Hash[$2]));
   Hash[$C] := RShl(Hash[$C], $C) + Hash[$D];
   Inc(Hash[$D], Buf[$D]  + HashTable[B]  + F(Hash[$D], Buf[$4], HashTable[D], Hash[$D]));
   Hash[$D] := RShl(Hash[$D], $3) + Hash[$5];
   Inc(Hash[$E], Buf[$E]  + HashTable[C]  + G(Hash[$E], Buf[$8], HashTable[C], Hash[$0]));
   Hash[$E] := RShl(Hash[$E], $7) + Hash[$C];
   Inc(Hash[$F], Buf[$F]  + HashTable[D]  + H(Hash[$F], Buf[$D], HashTable[B], Hash[$8]));
   Hash[$F] := RShl(Hash[$F], $2) + Hash[$3];
  end;
end;

function Hash(const Buffer; const Size: Longword; Callback: TLEACallback = nil): THash;
Var
 V: PLEABuf;
 Sz, Cnt: Longword;
 Buf: TLEABuf;
 E: Pointer;
 BlockCount: Longword;
begin
 Move(HashInit, Result, $40);

 Cnt := $0;
 Sz := Size;
 repeat Inc(Sz) until Sz mod $40 = $0;

 BlockCount := Sz div $40;
 V := @Buffer;
 E := Ptr(Longword(@Buffer) + Sz);

 repeat
  begin
   ZeroMemory(@Buf, $40);
   if Size - Cnt > $3F then Move(V^, Buf, $40) else
    begin
     Move(V^, Buf, Size - Cnt);
     FillMemory(Ptr(Longword(@Buf) + Size - Cnt), 1, $80);
    end;

   LEAInternal(Result, Buf);

   if Assigned(Callback) then Callback(Cnt div $40, BlockCount);

   Inc(V);
   Inc(Cnt, $40);
  end
 until V = E;
end;

function HashStr(const Str: AnsiString): THash;
begin
 Result := Hash(PAnsiChar(Str)^, Length(Str));
end;

function HashFile(const FilePath: String; Callback: TLEACallback = nil): THash;
Var
 H, M: Longword;
 P: Pointer;
begin
 ZeroMemory(@Result, $40);

 H := CreateFile(PChar(FilePath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
                 nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);

 if H = INVALID_HANDLE_VALUE then Exit;

 if GetFileSize(H, nil) = 0 then
  begin
   Move(HashInit, Result, $40);
   CloseHandle(H);
   Exit;
  end;

 try
  M := CreateFileMapping(H, nil, PAGE_READONLY, 0, 0, nil);
  try
   if M = 0 then Exit;
   P := MapViewOfFile(M, FILE_MAP_READ, 0, 0, 0);
   try
    if P = nil then Exit;
    Result := Hash(P^, GetFileSize(H, nil), Callback);
   finally
   UnmapViewOfFile(P);
   end;
  finally
   CloseHandle(M);
  end;
 finally
  CloseHandle(H);
 end;
end;

function HashToString(const Hash: THash): AnsiString;
Var
 I: Longword;
begin
 Result := '';
 for I := $1 to HashLen[LEASize] shr $3 do Result := Result + Format('%.8x', [Hash[Pred(I)]]);
end;

function StringToHash(const Str: AnsiString): THash;
Var
 I: Longword;
begin
 if IsHash(Str) then
  for I := $1 to HashLen[LEASize] shr $3 do
   Result[Pred(I)] := StrToInt(Format('$%s', [Copy(Str, (I shl $3) - $7, $8)]))
 else ZeroMemory(@Result, $80);
end;

function SameHash(const A, B: THash): Boolean;
begin
 Result := CompareMem(@A, @B, HashSz[LEASIZE]);
end;

function Same(A, B: Pointer; SzA, SzB: Longword): Boolean;
begin
 Result := SameHash(Hash(A, SzA), Hash(B, SzB));
end;

function HashCrypt(const Hash: THash; Key: AnsiString): THash;
Var
 I: Integer;
 K: THash;
begin
 K := HashStr(Key);
 for I := $0 to $F do Result[I] := Hash[I] + K[I];
end;

function HashUncrypt(const Hash: THash; Key: AnsiString): THash;
Var
 I: Integer;
 K: THash;
begin
 K := HashStr(Key);
 for I := $0 to $F do Result[I] := Hash[I] - K[I];
end;

function IsHash(const Hash: AnsiString): Boolean;
Var
 I: Integer;
begin
 Result := False;
 if Longword(Length(Hash)) <> HashLen[LEASize] then Exit;
 {$ifndef unicode}
 for I := 1 to HashLen[LEASize] do if not (Hash[I] in ['0'..'9', 'A'..'F', 'a'..'f']) then Exit;
 {$else}
 for I := 1 to HashLen[LEASize] do if not CharInSet(Hash[I], ['0'..'9', 'A'..'F', 'a'..'f']) then Exit;
 {$endif}
 Result := True;
end;

end.
