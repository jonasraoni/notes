unit frm_Statistics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type

  TNotesStatistics = record
    StringList: TStrings;
    Bytes:   integer;
    Words: integer;
    Lines: integer;
  end;

type
  TfrmStatistics = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    btOk: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
  private
    procedure counter(var var1: TNotesStatistics);
    { Private declarations }
  public
    { Public declarations }
end;

var
  frmStatistics: TfrmStatistics;

implementation

uses NotesGlobals;

{$R *.dfm}

procedure TfrmStatistics.counter(var var1: TNotesStatistics);
var
  total_del: integer;  // armazena o total de espaços do texto
  total_lines: integer;  // armazena o total de linhas
  cv, varx, vary: integer;
  hun, hun2: integer;
  Str_test: string;
//  Bar_value: real;
begin
     //form2.show;
     total_del:=0;
//     total_lines:=0;

//     Bar_value:=var1.StringList.count/100;

     varx:=0;
     vary:=0;

     Str_test:='';
     cv:=1;

     total_lines:= var1.StringList.count;

     for hun:=0 to total_lines-1 do  // laço para todas as linhas do texto
     begin

     Str_test:=var1.StringList.Strings[hun];

     if (length(Str_test)>0) then
     begin

          for hun2:=1 to length(Str_test)-1 do  // laço para todos os caracteres da linha
          begin
               if IsDelimiter(#32#9#61#40#45#43#91,Str_test,hun2) then
               begin
                  vary:=vary+cv;
                  cv:=0;
               end
               else
               begin
                   cv:=1;
               end;

               if IsDelimiter(#32#9,Str_test,hun2) then
                  inc(total_del);
          end;

          inc(Varx);

          Str_test:='';
          //form2.ProgressBar1.Position:=round(hun/Bar_value);

     end;

     end;
     //form2.hide;

     var1.Bytes:=(length(var1.StringList.text)-(total_lines-1)*2)-total_del;
     var1.Words:=vary+Varx;
end;



procedure TfrmStatistics.FormCreate(Sender: TObject);
var
  Var_vals: TNotesStatistics;
begin
  with ActiveEditorTab.editor do
  begin
    Var_vals.StringList:= TstringList.create;
    try
      Var_vals.StringList.text:=SelText;
      counter(Var_vals);

      label5.caption:=inttostr(var_vals.StringList.Count);
      label12.caption:=inttostr(Var_vals.Bytes-2);
      label8.caption:=inttostr(Var_vals.Words);
    finally
      Var_vals.StringList.Free;
    end;

    Var_vals.StringList:=Lines;

    counter(Var_vals);

    label4.caption:=inttostr(lines.count);
    label10.caption:=inttostr(Var_vals.Bytes);
    label6.caption:=inttostr(Var_vals.Words);
  end;
end;

end.
