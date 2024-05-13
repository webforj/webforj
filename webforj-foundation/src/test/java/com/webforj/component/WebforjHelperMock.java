package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.CustomObject;
import com.webforj.bbj.BBjVar;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.optiondialog.ConfirmDialog;
import java.util.ArrayList;

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

}
