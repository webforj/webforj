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
  public void sleep(int seconds) {
    // no-op
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
