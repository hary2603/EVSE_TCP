program EVSE_CLIENT;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, main;

begin
  Application.Title:='EVSE_TCPIP_IF';
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

