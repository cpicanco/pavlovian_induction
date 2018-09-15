{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit TabDelimitedReport.Custom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TabDelimitedReport;

type

  TCustomReport = class sealed (TTabDelimitedReport)
  private
    FStartTime: LongWord;
  public
    procedure WriteFooter;
    procedure WriteHeader;
    procedure WriteRow(ACondition : string; AX: integer = -1; AY : integer = -1); overload;
    property StartTime : LongWord read FStartTime write FStartTime;
  end;


resourcestring
  // header, column names of the report
  RSCondition = 'Condição';
  RSMouseX = 'MouseX';
  RSMouseY = 'MouseY';
  RSTime = 'Tempo(ms)';

  // footer
  RSBegin = 'Início:';
  RSEnd = 'Final:';
var Report : TCustomReport;

implementation

{ TReport }

procedure TCustomReport.WriteFooter;
begin
  WriteRow([RSEnd, TimeToStr(Now)]);
  CloseFile;
end;

procedure TCustomReport.WriteHeader;
begin
  WriteRow([RSBegin, TimeToStr(Now)]);
  WriteRow([RSTime, RSCondition, RSMouseX, RSMouseY]);
end;

procedure TCustomReport.WriteRow(ACondition: string; AX: integer; AY: integer);
var
  LX : string = 'NA';
  LY : string = 'NA';
  LTime : string = '';
begin
  if AX = -1 then { do nothing } else WriteStr(LX, AX);
  if AY = -1 then { do nothing } else WriteStr(LY, AY);
  WriteStr(LTime, GetTickCount64-FStartTime);
  WriteRow([LTime, ACondition, LX, LY]);
end;

initialization
  Report := TCustomReport.Create;

finalization
  Report.Free;

end.

