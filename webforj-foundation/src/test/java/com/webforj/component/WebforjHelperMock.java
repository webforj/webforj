package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.CustomObject;
import com.webforj.bbj.BBjVar;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.optiondialog.ConfirmDialog;
import com.webforj.component.optiondialog.FileChooserDialog;
import com.webforj.component.optiondialog.FileSaveDialog;
import com.webforj.component.optiondialog.FileUploadDialog;
import com.webforj.component.optiondialog.InputDialog;
import java.util.ArrayList;
import java.util.Locale;

/** Mock for the WebforjBBjBridge. */
public class WebforjHelperMock implements WebforjBBjBridge {

  @Override
  public CustomObject getEventProxy(Object obj, String method) {
    return new CustomObjectMock();
  }

  @Override
  public CustomObject getEventProxy(Object obj, String method, String eventclassname) {
    return new CustomObjectMock();
  }

  @Override
  public BBjControl createWidget(String classname, BBjWindow wnd) {
    throw new UnsupportedOperationException("Unimplemented method 'createWidget'");
  }

  @Override
  public int msgbox(String msg, int options, String title) {
    throw new UnsupportedOperationException("Unimplemented method 'msgbox'");
  }

  @Override
  public Object invokeMethod(Object object, String method, ArrayList args) {
    throw new UnsupportedOperationException("Unimplemented method 'invokeMethod'");
  }

  @Override
  public Object createInstance(String classname) {
    throw new UnsupportedOperationException("Unimplemented method 'createInstance'");
  }

  @Override
  public ArrayList<BBjVar> call(String pgm, ArrayList<BBjVar> args) {
    throw new UnsupportedOperationException("Unimplemented method 'call'");
  }

  @Override
  public String getWorkingDirectory() {
    return "";
  }

  @Override
  public String getQueryParam(String key) {
    return "";
  }

  @Override
  public int msgbox(ConfirmDialog config) {
    return 0;
  }

  @Override
  public String prompt(InputDialog options) {
    return "";
  }

  @Override
  public String fileChooser(FileChooserDialog options) {
    return "";
  }

  @Override
  public String fileUpload(FileUploadDialog options) {
    return "";
  }


  @Override
  public String fileSave(FileSaveDialog options) {
    return "";
  }

  @Override
  public void sleep(int seconds) {

  }

  @Override
  public String maskString(String value, String mask) {
    throw new UnsupportedOperationException("Unimplemented method 'maskString'");
  }

  @Override
  public String maskNumber(double value, String mask) {
    throw new UnsupportedOperationException("Unimplemented method 'maskNumber'");
  }

  @Override
  public Double parseTime(String time, String mask, Locale locale) {
    throw new UnsupportedOperationException("Unimplemented method 'parseTime'");
  }

  @Override
  public String maskDateTime(int julian, Double time, String mask) {
    throw new UnsupportedOperationException("Unimplemented method 'maskDateTime'");
  }

  @Override
  public Integer parseDate(String date, String mask, Locale locale) {
    throw new UnsupportedOperationException("Unimplemented method 'parseDate'");
  }
}
