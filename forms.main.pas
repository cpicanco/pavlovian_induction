unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, XMLPropStorage;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonStartExperiment: TButton;
    EditParticipantName: TEdit;
    ImageCoin: TImage;
    ImageClow: TImage;
    LabelConsequenceTime: TLabel;
    LabelClickDiameter: TLabel;
    LabelMilliseconds: TLabel;
    LabelMilliseconds1: TLabel;
    LabelParticipantName: TLabel;
    Panel: TPanel;
    RadioGroupCondition: TRadioGroup;
    SpinEditConsequenceTime: TSpinEdit;
    SpinEditClickDiameter: TSpinEdit;
    Timer: TTimer;
    XMLPropStorage: TXMLPropStorage;
    procedure ButtonStartExperimentClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure TimerNextObservation(Sender: TObject);
    procedure TimerFinishTest(Sender: TObject);
  private
    function ConditionAsString : string;
  public
    {$IFDEF WINDOWS}
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    procedure SwitchFullScreen;
    {$ENDIF}
  end;

var
  Form1: TForm1;

implementation

uses experiment, TabDelimitedReport.Custom;

{$R *.lfm}

{ TForm1 }

var IsRunning : Boolean = False;

procedure TForm1.ButtonStartExperimentClick(Sender: TObject);
begin
  Panel.Hide;
  //ShowMessage(
  //  'O notebook dará a você todas as instruções para a realização da tarefa. '+
  //  'Você será informado quando a tarefa terminar, então não levante '+
  //  'enquanto não for informado.'
  //);
  //ShowMessage(
  //  'Clique na tela e encontre o palhaço.'
  //);
  {$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  {$IFDEF LINUX}WindowState := wsFullScreen;{$ENDIF}
  Report.Filename := Application.ExeName;
  InitializeCondition(ConditionAsString);
  StartExperiment(Self,
    Timer,
    @TimerNextObservation,
    @TimerFinishTest,
    SpinEditConsequenceTime.Value,
    SpinEditClickDiameter.Value,
    EditParticipantName.Text);
  Invalidate;
  IsRunning:=True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ResizeStimuli(ImageCoin, ImageClow);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if IsRunning then
  begin
    if Key = #32 then
      case CurrentScreen of
        BlackBackground: NextScreen;
      end;
  end else ShowMessage('A sessão acabou, você pode fechar a janela.');
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CursorPos : TPoint;
begin
  if IsRunning then
  begin
    CursorPos := Point(X, Y);
    Report.WriteRow(X, Y);
    case CurrentScreen of
      StimuliAtScreenLeft..StimuliAtScreenRight:
        if InCircle(CoinPoint, CursorPos) then
        begin
          Report.WriteRow(['+']);
          Move(ImageCoin, CoinPoint);
          Move(ImageClow, ClowPoint);
          ImageCoin.Show;
          ImageClow.Show;
          Timer.Enabled := True;
        end;
    end;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  //DebugView;
end;

procedure TForm1.TimerNextObservation(Sender: TObject);
begin
  Timer.Enabled := False;
  ImageCoin.Hide;
  ImageClow.Hide;
  Invalidate;
  CentralizeCursorPos;
  NextScreen;
end;

procedure TForm1.TimerFinishTest(Sender: TObject);
begin
  Timer.Enabled := False;
  ImageCoin.Show;
  ImageClow.Show;
  NextScreen;
  IsRunning := False;

  //Timer.Interval:= 5000;
  //Timer.OnTimer := @TimerNextObservation;
  //Timer.Enabled := True;
end;

function TForm1.ConditionAsString: string;
begin
  with RadioGroupCondition do Result := Items[ItemIndex];
end;

{$IFDEF WINDOWS}
// http://wiki.freepascal.org/Application_full_screen_mode
procedure TForm1.SwitchFullScreen;
begin
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
  end;
end;
{$ENDIF}


end.

