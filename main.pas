unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, lNetComponents, lNet;

type

  TChargeStatus = (st_Init,st_ready,st_connected,st_charging,st_error,st_standby,st_Invalid);

  { TFormMain }

  TFormMain = class(TForm)
    pause: TButton;
    set20A: TButton;
    set16A: TButton;
    set12A: TButton;
    current_act: TLabel;
    charging_time: TLabel;
    current_set: TLabel;
    energy: TLabel;
    energy_total: TLabel;
    temperature: TLabel;
    set_10A: TButton;
    ButtonDiconnect: TButton;
    ButtonConnect: TButton;
    Connected: TImage;
    Error: TImage;
    wallbox_state: TLabel;
    Standby: TImage;
    Ready: TImage;
    Charging: TImage;
    Status: TImage;
    LTCP: TLTCPComponent;
    EditPort: TEdit;
    EditIP: TEdit;
    LabelPort: TLabel;
    LabelHostName: TLabel;
    ButtonSend: TButton;
    EditSend: TEdit;
    MemoText: TMemo;
    TimerUpdate: TTimer;
    TimerQuit: TTimer;
    procedure pauseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure LTCPComponentConnect(aSocket: TLSocket);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DiconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LTCPComponentError(const msg: string; aSocket: TLSocket);
    procedure LTCPComponentAccept(aSocket: TLSocket);
    procedure LTCPComponentReceive(aSocket: TLSocket);
    procedure LTcpComponentDisconnect(aSocket: TLSocket);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
    procedure set12AClick(Sender: TObject);
    procedure set16AClick(Sender: TObject);
    procedure set20AClick(Sender: TObject);
    procedure set_10AClick(Sender: TObject);
    procedure TimerQuitTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);


  private
    FIsServer: Boolean;
    FChargeStatus : TChargeStatus;
    procedure SendToAll(const aMsg: string);
    procedure SetChargeStatus(AValue: TChargeStatus);

  public
    property ChargeStatus:TChargeStatus read FChargeStatus write SetChargeStatus;
    { public declarations }
  end; 

var
  FormMain: TFormMain;
  FNet: TLConnection;
  last_cmd: string;
  set_current: string;
  act_state: string;
  act_current: string;
  act_temp: string;
  energy_ses: string;
  elapsed_time: string;
  energy_tot:string;
  temp: string;
  WB_connected: Boolean;

  function CRC(Data: AnsiString): AnsiString; // 8-bit XOR Checksum
  function build_cmd_str(cmd_in: AnsiString): AnsiString; // generate command string
  procedure send_cmd(str: AnsiString);
  procedure MyDelay(milliSecondsDelay: int64);
  procedure waitans();


implementation

uses
  lCommon, DateUtils;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNet := LTCP;
  LTCP.SocketNet := LAF_INET;
  if EditIP.Text = '::1' then
    EditIP.Text := 'localhost';
  FIsServer := False;
  WB_connected := False;
  last_cmd := '';
  FChargeStatus := st_ready;
  FNet.Connect(EditIP.Text, StrToInt(EditPort.Text)); // connect to wallbox
end;


procedure TFormMain.ConnectButtonClick(Sender: TObject);
begin
  FNet.Connect(EditIP.Text, StrToInt(EditPort.Text))
end;


procedure TFormMain.LTCPComponentConnect(aSocket: TLSocket);
begin
  MemoText.Append('Connected to Wallbox');
  WB_connected := True;
end;


procedure TFormMain.LTcpComponentDisconnect(aSocket: TLSocket);
begin
  MemoText.Append('Wallbox connection lost');
  WB_connected := False;
end;


procedure TFormMain.LTCPComponentError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;


procedure TFormMain.SendToAll(const aMsg: string);
var
  n: Integer;
begin
   // TCP
    FNet.IterReset; // start at server socket
    while FNet.IterNext do begin // skip server socket, go to clients only
      n := FNet.SendMessage(aMsg, FNet.Iterator);
      if n < Length(aMsg) then
        MemoText.Append('Error on send [' + IntToStr(n) + ']');
    end;
 end;


procedure TFormMain.LTCPComponentAccept(aSocket: TLSocket);
begin
  MemoText.Append('Connection accepted');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;


procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('EVSE TCP Control V0.25',
             mtInformation, [mbOK], 0);
end;


procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;


procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if FNet.Connected then begin
    CloseAction := caNone; // make sure we quit gracefuly
    FNet.Disconnect; // call disconnect (soft)
    TimerQuit.Enabled := True; // if time runs out, quit ungracefully
  end;
end;


procedure TFormMain.TimerQuitTimer(Sender: TObject);
begin
  Close;
end;


procedure MyDelay(milliSecondsDelay: int64);
var
  stopTime : TDateTime;
begin
  stopTime := IncMilliSecond(Now,milliSecondsDelay);
  while (Now < stopTime) and (not Application.Terminated) do
    Application.ProcessMessages;
end;


function CRC(Data: AnsiString): AnsiString; // 8-bit XOR Checksum
var
 i,iSum : Integer;
begin
 iSum := 0;
 for i := 1 to Length(Data) do
 begin
  iSum := iSum xor Ord(Data[i]);
 end;
 Result := IntToHex(iSum,2)
end;

procedure send_cmd(str: AnsiString);
begin
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
end;

function build_cmd_str(cmd_in: AnsiString): AnsiString; // generate command string
begin
 cmd_in := '$' + cmd_in;
 Result := cmd_in + '^' + CRC(cmd_in) + #13;
end;

procedure waitans();
begin
  while (last_cmd <> '') and (not Application.Terminated) do Application.ProcessMessages;
end;


procedure TFormMain.SetChargeStatus(AValue: TChargeStatus);
begin
  FChargeStatus:=AValue;
 Refresh;
end;


procedure TFormMain.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    SendButtonClick(Sender);
end;


 procedure TFormMain.SendButtonClick(Sender: TObject);
begin
  if Length(EditSend.Text) > 0 then begin
    if FNet.Connected then begin
       last_cmd := EditSend.Text;  // remember last command sent
       EditSend.Text := build_cmd_str(EditSend.Text);
       MemoText.Append(EditSend.Text);
       FNet.SendMessage(EditSend.Text);
       EditSend.Text := '';
    end;
  end;
end;


 procedure TFormMain.DiconnectButtonClick(Sender: TObject);
begin
  FNet.Disconnect;
  MemoText.Append('Disconnected');
end;


procedure TFormMain.LTCPComponentReceive(aSocket: TLSocket);
var
  s,l,r,rec_cs,calc_cs,la,ppblock,cmd: string;
  pp: TStringArray;
  len: Int64;
  n: Double;
  stime,h,min,sec: integer;

begin
  if aSocket.GetMessage(s) > 0 then begin
//    MemoText.Append(s);      // Debug only

    len := Length(s) - 4;    // Commando length without checksum
    l := LeftStr(s,len);     // Commando String $cc pp pp ...
    r := RightStr(s,3);      // Checksum String xk\r
    rec_cs := LeftStr(r,2);  // received Checksum String
    calc_cs := CRC(l);       // calculated Checksum String
    if rec_cs = calc_cs then  // Antwort verarbeiten
      begin
        case l of
           '$ST 01': begin
                 MemoText.Append('Status: Bereit');
                 ChargeStatus := st_ready;
           end;
           '$ST 02': begin
                MemoText.Append('Status: Verbunden');
                ChargeStatus := st_connected;
           end;
           '$ST 03': begin
                 MemoText.Append('Status: Laden');
                 ChargeStatus := st_charging;
           end;
           '$ST 04','$ST 05': begin
                MemoText.Append('Status: Fehler');
                ChargeStatus := st_error;
           end;
           '$ST fe': begin
                MemoText.Append('Status: Pause');
                ChargeStatus := st_standby;
           end;
           otherwise begin
           la := LeftStr(l,3);
           if la = '$OK' then
             begin
              len := Length(l) - 3;
              ppblock := RightStr(l,len);
              pp := ppblock.Split(' ');            // array of parameters
              len := high(pp);                     // number of parameters
//              MemoText.Append(ppblock + ' received ' + IntToStr(len));  // Debug only
              cmd := Leftstr(last_cmd,2);          // limit to command name
              case cmd of
                 'SC':begin                        // set current
                   set_current := pp[1];
                   MemoText.Append('Current setting: ' +  set_current + ' A');
                   last_cmd := '';
                 end;
                 'GE':begin                        // get set current
                   set_current := pp[1];
                   MemoText.Append('Current set: ' +  set_current + ' A');
                   last_cmd := '';
                 end;
                 'GG':begin                        // get actual current
                    n := StrToFloat(pp[1]);
                    n := n/1000;
                    act_current := Format('%4.2f',[n]);
                    MemoText.Append('Current: ' +  act_current + ' A');
                    last_cmd := '';
                 end;
                 'GS':begin                        // get actual state and elapsed time
                    act_state := pp[1];

                    case act_state of              // wallbox state
                         '1': begin
                              MemoText.Append('Status: Bereit');
                              ChargeStatus := st_ready;
                         end;
                         '2': begin
                              MemoText.Append('Status: Verbunden');
                              ChargeStatus := st_connected;
                         end;
                         '3': begin
                              MemoText.Append('Status: Laden');
                              ChargeStatus := st_charging;
                         end;
                         '4','5': begin
                              MemoText.Append('Status: Fehler');
                              ChargeStatus := st_error;
                         end;
                         '254': begin
                              MemoText.Append('Status: Pause');
                              ChargeStatus := st_standby;
                         end;
                    end;
                    stime := StrToInt(pp[2]);            // elapsed time
                    h := stime div 3600;
                    sec := stime mod 60;
                    min := (stime - (h*3600)) div 60;
                    elapsed_time := Format('%.2d',[h]) + ':' + Format('%.2d',[min])  + ':' +  Format('%.2d',[sec]);
                    MemoText.Append('State: ' +  act_state + ' Session Time: ' + elapsed_time);
                    last_cmd := '';
                  end;
                  'GP':begin                        // get temperature
                    n := StrToFloat(pp[1]);
                    n := n/10;
                    act_temp := FloatToStr(n);
                    MemoText.Append('Temperature: ' +  act_temp +' °C');
                    last_cmd := '';
                  end;
                  'GU':begin                       // get energy usage
                    n := StrToFloat(pp[1]);
                    n := n/3600;                   // Ws -> Wh
                    n := n/1000;                   // Wh -> kWh
                    energy_ses := Format('%4.2f',[n]);
                    n := StrToFloat(pp[2]);
                    n := n/1000;                   // Wh -> kWh
                    energy_tot := Format('%5.2f',[n]);
                    MemoText.Append('Energy Session: ' +  energy_ses + ' kWh Total: ' + energy_tot + ' kWh');
                    last_cmd := '';
                 end;
              end;
             end else  MemoText.Append('?????');
           end;
        end;
      end
    else
     begin
       MemoText.Append('Checksum error....');
     end;

    MemoText.SelStart := Length(MemoText.Lines.Text);
  end;
end;



procedure TFormMain.TimerUpdateTimer(Sender: TObject);
begin

     current_set.caption := set_current + ' A';
     temperature.caption := act_temp + ' °C';
     current_act.caption := act_current + ' A';
     energy_total.caption := energy_tot + ' kWh';

     if  ChargeStatus = st_charging then begin
       charging_time.caption := elapsed_time;
       energy.caption := energy_ses + ' kWh';
     end
     else
     begin
       charging_time.caption := '--:--:--';
       energy.caption := '00,00 kWh';
     end;

     if WB_connected then begin
          Case ChargeStatus of
                   st_ready: begin
                      wallbox_state.Caption := ' Bereit         ';
                   end;
                   st_connected: begin
                      wallbox_state.Caption := ' Verbunden      ';
                   end;
                   st_charging: begin
                      wallbox_state.Caption := ' Laden          ';
                   end;
                   st_error: begin
                      wallbox_state.Caption := ' Fehler         ';
                   end;
                   st_standby: begin
                      wallbox_state.Caption := ' Pause          ';
                   end;
          end;
     end
     else
     begin
        wallbox_state.Caption := ' keine Wallbox  ';
     end;

     if WB_connected then begin // Wallbox connected ?
        send_cmd('GE');         // get current setting
        waitans();
        send_cmd('GG');         // get charge current
        waitans();
        send_cmd('GP');         // get temperatur
        waitans();
        send_cmd('GS');         // get wallbox status
        waitans();
        send_cmd('GU');         // get energy usage
     end;
end;


procedure TFormMain.FormPaint(Sender: TObject);
begin

  Case ChargeStatus of
    st_ready: Canvas.Draw(300,10,Ready.Picture.Bitmap);
    st_connected: Canvas.Draw(300,10,Connected.Picture.Bitmap);
    st_charging: Canvas.Draw(300,10,Charging.Picture.Bitmap);
  end;
end;




procedure TFormMain.set_10AClick(Sender: TObject);
var str : AnsiString;
begin
 waitans();
 str := 'SC 10';
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
end;


procedure TFormMain.set12AClick(Sender: TObject);
var str : AnsiString;
begin
 waitans();
 str := 'SC 12';
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
end;


procedure TFormMain.set16AClick(Sender: TObject);
var str : AnsiString;
begin
 waitans();
 str := 'SC 16';
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
end;

procedure TFormMain.set20AClick(Sender: TObject);
var str : AnsiString;
begin
 waitans();
 str := 'SC 16';
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
end;

procedure TFormMain.pauseClick(Sender: TObject);
var str : AnsiString;
begin

 if ChargeStatus = st_standby then begin
    str := 'FE';
 end
 else begin
    str := 'FS';
 end;
 waitans();
 last_cmd := str;
 str := build_cmd_str(str);
 FNet.SendMessage(str);
end;


initialization
  {$I main.lrs}

end.

