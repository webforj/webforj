package com.webforj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.bbj.BBjVar;
import java.util.ArrayList;

public interface WebforjBBjBridge {

  /**
   * delegates the creation of a widget, mostly BBj Plug-Ins written as a BBj Custom Class extending
   * BBjWidget The BBj side uses the constructor that takes a single BBjChildWindow to instantiate
   * the widget.
   *
   * @param classname The classname in the style ::filename.bbj::classname
   * @param wnd Handle to the window where the widget needs to be created
   * @return the BBjControl returned by the BBj side constructor
   */
  BBjControl createWidget(String classname, BBjWindow wnd);

  /**
   * invoke a method of a BBj-side object instance, mostly custom objects.
   *
   * @param object the object instance
   * @param method the method name
   * @param args the arguments as a List
   * @return the result of the method invocation
   */
  @SuppressWarnings("java:S3740") // allow raw types
  Object invokeMethod(Object object, String method, java.util.ArrayList args);

  /**
   * create an instance of a BBj-side object instance, mostly custom objects.
   *
   * @param classname the method name
   * @return the object instance
   */
  Object createInstance(String classname);


  ArrayList<BBjVar> call(String pgm, ArrayList<BBjVar> args);
}
