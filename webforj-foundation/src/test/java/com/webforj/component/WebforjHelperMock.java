package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.CustomObject;
import com.webforj.bbj.BBjVar;
import com.webforj.bridge.WebforjBBjBridge;
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
  public String getQueryParam(String key) {
    return "";
  }
}
