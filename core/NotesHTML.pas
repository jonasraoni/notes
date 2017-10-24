//
//    NotesHTML - várias funções para lidar com HTML
//
//    Notes, https://github.com/jonasraoni/notes
//    Copyright (C) 2003-2004, Equipe do Notes.
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//


(*
  @abstract(NotesHTML - várias funções para lidar com HTML.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesHTML;

interface

{Converte HTML para texto.<BR>
 @code(EsPtFrSupport) - ativa suporte para línguas latinas como portuguêse francês.<BR>
 @code(SimbolsSupport) - ativa suporte a símbolos como copryright, marca registrada, etc.<BR>
 @code(OtherLangSupport) - ativa suporte para outras linguas. }
function HTML2TXT(S : string; EsPtFrSupport : Boolean; SimbolsSupport : Boolean; OtherLangSupport : Boolean) : string;
{Converte texto para HTML, incluíndo caracteres especiais.}
function TXT2HTML(S : string) : string;

implementation

uses
  FastStrings;

function HTML2TXT(S : string; EsPtFrSupport : Boolean; SimbolsSupport : Boolean; OtherLangSupport : Boolean) : string;
var
  X, TagCnt: Integer;
begin
  //this is the Support for english, and the base for other languages
  S := FastReplace(S,'&nbsp;',' ',False);
  S := FastReplace(S,'&amp;','&', False);
  S := FastReplace(S,#13#10,'', False);
  S := FastReplace(S,'<table>',#13#10, False);
  S := FastReplace(S,'</table>',#13#10, False);
  S := FastReplace(S,'<div>',#13#10, False);
  S := FastReplace(S,'</div>',#13#10, False);
  S := FastReplace(S,'<br>',#13#10, False);
  S := FastReplace(S,'<p>',#13#10, False);
  S := FastReplace(S,'</p>',#13#10, False);  //</td></tr>
  S := FastReplace(S,'</tr>',#13#10, False);
  S := FastReplace(S,'<tr>',#13#10, False);
  S := FastReplace(S,'&quot;','"', False);
  S := FastReplace(S,'&copy;','© ',False);
  S := FastReplace(S,'&reg;','®',False);

  //support for spanish, french and portuguese
  if  EsPtFrSupport then begin
    S := FastReplace(S,'&uuml;','ü',False);
    S := FastReplace(S,'&uacute;','ú',False);
    S := FastReplace(S,'&iexcl;','¡',False);
    S := FastReplace(S,'&ouml;','ö',False);
    S := FastReplace(S,'&otilde;','õ',False);
    S := FastReplace(S,'&ocirc;','ô',False);
    S := FastReplace(S,'&oacute;','ó',False);
    S := FastReplace(S,'&ograve;','ò',False);
    S := FastReplace(S,'&ntilde;','ñ',False);
    S := FastReplace(S,'&ecirc;','ê',False);
    S := FastReplace(S,'&iuml;','ï',False);
    S := FastReplace(S,'&iacute;','í',False);
    S := FastReplace(S,'&eacute;','é',False);
    S := FastReplace(S,'&ccedil;','ç',False);
    S := FastReplace(S,'&auml;','ä',False);
    S := FastReplace(S,'&atilde;','ã',False);
    S := FastReplace(S,'&aacute;','á',False);
    S := FastReplace(S,'&agrave;','à',False);
    S := FastReplace(S,'&Uuml;','Ü',False);
    S := FastReplace(S,'&Ucirc;','Û',False);
    S := FastReplace(S,'&Uacute;','Ú',False);
    S := FastReplace(S,'&Ouml;','Ö',False);
    S := FastReplace(S,'&Otilde;','Õ',False);
    S := FastReplace(S,'&Ocirc;','Ô',False);
    S := FastReplace(S,'&Oacute;','Ó',False);
    S := FastReplace(S,'&Ntilde;','Ñ',False);
    S := FastReplace(S,'&euml;','ë',False);
    S := FastReplace(S,'&Iuml;','Ï',False);
    S := FastReplace(S,'&Icirc;','Î',False);
    S := FastReplace(S,'&Iacute;','Í',False);
    S := FastReplace(S,'&Euml;','Ë',False);
    S := FastReplace(S,'&Ecirc;','Ê',False);
    S := FastReplace(S,'&Eacute;','É',False);
    S := FastReplace(S,'&Ccedil;','Ç',False);
    S := FastReplace(S,'&uml;','¨',False);
    S := FastReplace(S,'&die;','¨',False);
    S := FastReplace(S,'&Auml;','Ä',False);
    S := FastReplace(S,'&Atilde;','Ã',False);
    S := FastReplace(S,'&Acirc;','Â',False);
    S := FastReplace(S,'&Aacute;','Á',False);
    S := FastReplace(S,'&iquest;','¿',False);
    S := FastReplace(S,'&bdquo;','„',False);
    S := FastReplace(S,'&rdquo;','”',False);
    S := FastReplace(S,'&ldquo;','“',False);
    S := FastReplace(S,'&sbquo;','‚',False);
    S := FastReplace(S,'&rsquo;','’',False);
    S := FastReplace(S,'&lsquo;','‘',False);
    S := FastReplace(S,'&shy;','­',False);
    S := FastReplace(S,'&rsaquo;','›',False);
    S := FastReplace(S,'&lsaquo;','‹',False);
    S := FastReplace(S,'&Ograve;','Ò',False);
    S := FastReplace(S,'&Egrave;','È',False);
    S := FastReplace(S,'&Igrave;','Ì',False);
    S := FastReplace(S,'&ugrave;','ù',False);
    S := FastReplace(S,'&icirc;','î',False);
    S := FastReplace(S,'&igrave;','ì',False);
    S := FastReplace(S,'&egrave;','è',False);
    S := FastReplace(S,'&Agrave;','À',False);
  end;

  //Support for simbols....
  if SimbolsSupport then begin
    S := FastReplace(S,'&ordf;','ª',False);
    S := FastReplace(S,'&deg;','°',False);
    S := FastReplace(S,'&para;','¶',False);
    S := FastReplace(S,'&micro;','µ',False);
    S := FastReplace(S,'&acute;','´',False);
    S := FastReplace(S,'&sup3;','³',False);
    S := FastReplace(S,'&sup2;','²',False);
    S := FastReplace(S,'&plusmn;','±',False);
    S := FastReplace(S,'&yen;','¥',False);
    S := FastReplace(S,'&curren;','¤',False);
    S := FastReplace(S,'&pound;','£',False);
    S := FastReplace(S,'&cent;','¢',False);
    S := FastReplace(S,'&frac34;','¾',False);
    S := FastReplace(S,'&frac12;','½',False);
    S := FastReplace(S,'&frac14;','¼',False);
    S := FastReplace(S,'&raquo;','»',False);
    S := FastReplace(S,'&ordm;','º',False);
    S := FastReplace(S,'&sup1;','¹',False);
    S := FastReplace(S,'&divide;','÷',False);
    S := FastReplace(S,'&middot;','·',False);
    S := FastReplace(S,'&macr;','¯',False);
    S := FastReplace(S,'&hibar;','¯',False);
    S := FastReplace(S,'&not;','¬',False);
    S := FastReplace(S,'&laquo;','«',False);
    S := FastReplace(S,'&brvbar;','¦',False);
    S := FastReplace(S,'&brkbar;','¦',False);
    S := FastReplace(S,'&mdash;','—',False);
    S := FastReplace(S,'&ndash;','–',False);
    S := FastReplace(S,'&permil;','‰',False);
    S := FastReplace(S,'&Dagger;','‡',False);
    S := FastReplace(S,'&dagger;','†',False);
  End;

  //Support for other languages....
  if OtherLangSupport then begin
    S := FastReplace(S,'&oslash;','ø',False);
    S := FastReplace(S,'&eth;','ð',False);
    S := FastReplace(S,'&aelig;','æ',False);
    S := FastReplace(S,'&aring;','å',False);
    S := FastReplace(S,'&szlig;','ß',False);
    S := FastReplace(S,'&THORN;','Þ',False);
    S := FastReplace(S,'&Yacute;','Ý',False);
    S := FastReplace(S,'&Ugrave;','Ù',False);
    S := FastReplace(S,'&Oslash;','Ø',False);
    S := FastReplace(S,'&times;','×',False);
    S := FastReplace(S,'&ETH;','Ð',False);
    S := FastReplace(S,'&AElig;','Æ',False);
    S := FastReplace(S,'&Aring;','Å',False);
    S := FastReplace(S,'&cedil;','¸',False);
  end;

  TagCnt := 0;
  Result := '';
  for X := 1 to Length( S ) do
    case S[X] of
      '<' : Inc(TagCnt);
      '>' : Dec(TagCnt);
    else
      if TagCnt <= 0 then begin
        Result := Result + S[X];
        TagCnt := 0;
      end;
    end;
    Result := FastReplace(Result,'&lt;','<', False);
    Result := FastReplace(Result,'&gt;','>', False);
end;

function TXT2HTML(S : string) : string;
begin
  S := FastReplace(S,'<','&lt;', True);
  S := FastReplace(S,'>','&gt;', True);
  S := FastReplace(S,'ü','&uuml;',True);
  S := FastReplace(S,'ú','&uacute;',True);
  S := FastReplace(S,'¡','&iexcl;',True);
  S := FastReplace(S,'ö','&ouml;',True);
  S := FastReplace(S,'õ','&otilde;',True);
  S := FastReplace(S,'ô','&ocirc;',True);
  S := FastReplace(S,'ó','&oacute;',True);
  S := FastReplace(S,'ò','&ograve;',True);
  S := FastReplace(S,'ñ','&ntilde;',True);
  S := FastReplace(S,'ê','&ecirc;',True);
  S := FastReplace(S,'ï','&iuml;',True);
  S := FastReplace(S,'í','&iacute;',True);
  S := FastReplace(S,'é','&eacute;',True);
  S := FastReplace(S,'ç','&ccedil;',True);
  S := FastReplace(S,'ä','&auml;',True);
  S := FastReplace(S,'ã','&atilde;',True);
  S := FastReplace(S,'á','&aacute;',True);
  S := FastReplace(S,'à','&agrave;',True);
  S := FastReplace(S,'â','&acirc;',True);
  S := FastReplace(S,'Ü','&Uuml;',True);
  S := FastReplace(S,'Û','&Ucirc;',True);
  S := FastReplace(S,'Ú','&Uacute;',True);
  S := FastReplace(S,'Ö','&Ouml;',True);
  S := FastReplace(S,'Õ','&Otilde;',True);
  S := FastReplace(S,'Ô','&Ocirc;',True);
  S := FastReplace(S,'Ó','&Oacute;',True);
  S := FastReplace(S,'Ñ','&Ntilde;',True);
  S := FastReplace(S,'ë','&euml;',True);
  S := FastReplace(S,'Ï','&Iuml;',True);
  S := FastReplace(S,'Î','&Icirc;',True);
  S := FastReplace(S,'Í','&Iacute;',True);
  S := FastReplace(S,'Ë','&Euml;',True);
  S := FastReplace(S,'Ê','&Ecirc;',True);
  S := FastReplace(S,'É','&Eacute;',True);
  S := FastReplace(S,'Ç','&Ccedil;',True);
  S := FastReplace(S,'¨','&uml;',True);
  S := FastReplace(S,'¨','&die;',True);
  S := FastReplace(S,'Ä','&Auml;',True);
  S := FastReplace(S,'Ã','&Atilde;',True);
  S := FastReplace(S,'Â','&Acirc;',True);
  S := FastReplace(S,'Á','&Aacute;',True);
  S := FastReplace(S,'„','&bdquo;',True);
  S := FastReplace(S,'”','&rdquo;',True);
  S := FastReplace(S,'“','&ldquo;',True);
  S := FastReplace(S,'‚','&sbquo;',True);
  S := FastReplace(S,'’','&rsquo;',True);
  S := FastReplace(S,'‘','&lsquo;',True);
  S := FastReplace(S,'­','&shy;',True);
  S := FastReplace(S,'"','&quot;', True);
  S := FastReplace(S,'©','&copy;',True);
  S := FastReplace(S,'®','&reg;',True);
  S := FastReplace(S,'  ','&nbsp; ',True);
  S := FastReplace(S,'&amp;','&', True);
  Result := S;
End;

end.

