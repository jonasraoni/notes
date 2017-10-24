//
//    NotesTimers - permite usar infinitos Timer no Notes
//                  usando apenas um componente TTimer
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
  @abstract(NotesTimers - permite usar infinitos Timer no Notes usando apenas um componente TTimer.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Para usar esta unit, use a variável @code(timers) que é criada na
  inicialização e destruída automaticmaente na finalização.
*)

unit NotesTimers;

interface

uses classes, ExtCtrls;

type
  // Evento disparado quando um evento de tempo ocorre
  TNotesTimerEvent = procedure of object;

  // Cada item de link(TNotesTimers) é guardado como um TNotesTimerItem
  TNotesTimerItem = class
  public
    // procedure a ser chamado
    proc: TNotesTimerEvent;
    // intervalo do timer
    interval: cardinal;
    // intervalo já passado
    intervalPassed: cardinal;
    // número de vezes que o timer deve ser executado
    times: integer;
    // número de vezes que o timer já foi executadi
    timesPassed: integer;
  end;


  TNotesTimerItems = array of TNotesTimerItem;

  { Esta classe usa um componente TTimer para prover infinitos
    timers usando um array de @code(TNotesTimerItem). Isto permite
    um overhead muito menor, consumindo muito menos recursos
    do sistema operacional. Além disso a classe é suficientemente
    esperta para destruir o timer que ela usa toda vez que não
    houver nenhum item a ser executado.
  }
  TNotesTimers = class(TObject)
  private
    fItems: TNotesTimerItems;
    fCount: integer;
    fInternalTimer: TTimer;
    { Executa os timers. }
    procedure DoOnTimer(Sender: TObject);
    procedure Remove(Id: integer);
  public
    destructor destroy; override;
    { Ajusta o intervalo do Timer interno de acordo
      com os valores dos timers do array e deleta os timers
      que não estão mais em uso. }
    procedure Update;
    { O Evento passado em @code(onTimeOut) será chamado após o tempo passado
     em @code(Inteval) apenas uma vez. }
    function  setTimeOut(Interval: cardinal; onTimeOut: TNotesTimerEvent): TNotesTimerItem;
    { O Evento passado em @code(onTimeOut) será chamado após o tempo passado em
     @code(Inteval) até que o timer seja destruido usando @code(ClearTimer). }
    function  setInterval(Interval: cardinal; onTimeOut: TNotesTimerEvent): TNotesTimerItem; overload;
    { O Evento passado em @code(onTimeOut) será chamado após o tempo passado em
     @code(Inteval) pelo número de vezes passado em @code(TimesToRepeat) ou até
     que o timer seja destruido usando @code(ClearTimer). }
    function  setInterval(Interval: cardinal; ontimeOut: TNotesTimerEvent; TimesToRepeat: integer): TNotesTimerItem; overload;
    { Destrói um timer. Em @code(Id) passe o valo que foi retornado por
      @code(setTimeOut) ou @code(setInterval). }
    procedure ClearTimer(var Handle: TNotesTimerItem);
  end;

Var
  timers: TNotesTimers;

implementation

uses Math, SysUtils;

const
  NEED_1 = [1, 5, 7];
  NEED_2 = [2, 4, 8];
  NEED_3 = [3, 9];

// dois cálculos:
  // calculo de unidade: se precisa ser um múltiplo de 1,2 ou 3
  // calculo de dezena: vendo qual é o menor valor, vê-se se podemos usar
  // unidade * 100, unidade * 1000, unidade * 10000, unidade * 100000,
  // unidade * 1000000, unidade * 10000000, unidade * 100000000,
  // unidade * 1000000000 e unidade * 10000000000,

function ToUnit(const I: cardinal): cardinal;
begin
  Result:= I;
  While Result > 9 do
    Result:= Result div 10;

  if Result in NEED_3 then
    Result:= 3
  else if Result in NEED_2 then
    Result:= 2
  else
    Result:= 1;

end;

function ToTen (const I: cardinal): cardinal;
begin
  // retorna no mínimo 100 para compatibilidade com
  // a classe TNotesTimers
  if I < 1000 then
    result:= 100
  else if I < 10000 then
    result:= 1000
  else if I < 100000 then
    result:= 10000
  else if I < 1000000 then
    result:= 100000
  else if I < 10000000 then
    result:= 1000000
  else if I < 100000000 then
    result:= 10000000
  else if I < 1000000000 then
    result:= 100000000
  else
    result:= 1000000000;
end;

{ TNotesTimers }

procedure TNotesTimers.Update;
Var
  I: integer;
  MinUnit: Cardinal;
  MinTen: Cardinal;
  tmpUnit: Cardinal;
  tmpTen: Cardinal;
begin
  if fCount > 0 then
  begin
    if not Assigned(fInternalTimer) then
    begin
      fInternalTimer:= TTimer.Create(nil);
      fInternalTimer.OnTimer:= DoOnTimer;
    end;
  end else
  begin
    if Assigned(fInternalTimer) then
    begin
      fInternalTimer.Free;
      fInternalTimer:= nil;
    end;
    Exit;
  end;

  MinUnit:= 3;
  MinTen:= 1000000000;

  for I:= 0 to fCount -1 do
  begin

    if assigned(fItems[I]) then
    begin
      // unidade mínima
      if MinUnit > 1 then
      begin
        // tempo necessário para executar o intervalo cheio
        tmpUnit:= ToUnit(fItems[I].interval);
        if tmpUnit < MinUnit then
          MinUnit:= tmpUnit;
       // tempo para executar o resto do intervalo
        tmpUnit:= ToUnit(fItems[I].interval - fItems[I].intervalPassed);
        if tmpUnit < MinUnit then
          MinUnit:= tmpUnit;
      end;

      if MinTen > 100 then
      begin
        // tempo necessário para executar o intervalo cheio
        tmpTen:= ToTen(fItems[I].interval);
        if tmpTen < MinTen then
          MinTen:= tmpTen;
       // tempo para executar o resto do intervalo
        tmpTen:= ToTen(fItems[I].interval - fItems[I].intervalPassed);
        if tmpTen < MinTen then
          MinTen:= tmpTen;
      end;
    end;
  end;

  fInternalTimer.Interval:= MinUnit * MinTen;

  if Assigned(fInternalTimer) then
    fInternalTimer.Enabled:= true;

end;

procedure TNotesTimers.DoOnTimer(Sender: TObject);
Var
  I, J, C: integer;
begin
  // temos que parar o timer para o caso dos procedimentos que estamos
  // chamando forem mt demorados. Ativamos ele novamente ao final
  fInternalTimer.Enabled:= false;

  // executamos os timers
  for I:= 0 to fCount-1 do
  begin
    fItems[I].intervalPassed:= fItems[I].intervalPassed + fInternalTimer.Interval;

    if fItems[I].intervalPassed >= fItems[I].interval then
    begin
      if assigned(fItems[I].proc) then
        fItems[I].proc;
      inc(fItems[I].timesPassed);
      fItems[I].intervalPassed:= 0;
    end;
  end;

  C:= fCount;

  // limpamos os timers que já podem ser deletados
  for J:= 0 to fCount -1 do
  begin
    for I:= 0 to fCount-1 do
    begin
      if ( assigned(fItems[I]) = false ) or ( (fItems[I].times <> 0)
                    and (fItems[I].times <= fItems[I].timesPassed) ) then
      begin
        Remove(I);
        break;
      end;
    end;
    // se nada tiver sido removido, paramos pois não há
    // nada a limpar :)
    if fCount = C then
      Break;
  end;

  if fCount = 0 then
    FreeAndNil(fInternalTimer)
  else if fCount < C then
    Update
  else if assigned(fInternalTimer) then
    fInternalTimer.Enabled:= true;
end;

procedure TNotesTimers.ClearTimer(var Handle: TNotesTimerItem);
var
  I: integer;
begin
  if Handle = nil then Exit;

  for I:= 0 to fCount -1 do
  begin
    if fItems[I] = Handle then
      break;
  end;

  Remove(I);
  Update;
  Handle:= nil;
end;

function TNotesTimers.setInterval(Interval: cardinal;
  ontimeOut: TNotesTimerEvent; TimesToRepeat: integer): TNotesTimerItem;
Var
  I: cardinal;
begin
  // O windows tem problemas qdo usamos timers com intervalo < 100ms
  Interval:= Max(Interval, 100);
  // permitimos apenas valores múltiplos de 100
  I:= Interval mod 100;
  Interval:= Interval - I;

  setLength(fItems, fCount+1);
  fItems[fCount]:= TNotesTimerItem.Create;
  fItems[fCount].proc:= ontimeOut;
  fItems[fCount].interval:= Interval;
  fItems[fCount].times:= TimesToRepeat;
  Result:= fItems[fCount];
  Inc(fCount);
  Update;
end;

function TNotesTimers.setInterval(Interval: cardinal;
  onTimeOut: TNotesTimerEvent): TNotesTimerItem;
begin
  Result:= setInterval(Interval, onTimeout, 0);
end;

function TNotesTimers.setTimeOut(Interval: cardinal;
  onTimeOut: TNotesTimerEvent): TNotesTimerItem;
begin
  Result:= setInterval(Interval, onTimeout, 1);
end;

destructor TNotesTimers.destroy;
Var
  I: integer;
begin
  if Assigned(fInternalTimer) then
    fInternalTimer.Free;

  for I:= 0 to fCount -1 do
    fItems[I].Free;

  setLength(fItems, 0);
  inherited;
end;

procedure TNotesTimers.Remove(Id: integer);
Var
  tmph: TNotesTimerItem;
begin
  tmph:= fItems[fCount-1];
  fItems[fCount-1]:= fItems[Id];
  fItems[Id]:= tmph;
  if assigned(fItems[fCount - 1]) then
    fItems[fCount-1].Free;
  Dec(fCount);
  SetLength(fItems, fCount);
end;

initialization
  timers:= TNotesTimers.Create;

finalization
  timers.Free;

end.
