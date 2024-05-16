package com.webforj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.CustomObject;
import com.webforj.bbj.BBjVar;
import com.webforj.optiondialog.ConfirmDialog;
import com.webforj.optiondialog.FileChooserDialog;
import com.webforj.optiondialog.FileUploadDialog;
import com.webforj.optiondialog.InputDialog;
import java.util.ArrayList;

public interface WebforjBBjBridge {

  /**
   * create and register an event proxy that defers B
   * Bj-side callback invocations to the Java DWCJ
   * side.
   *
   * @param obj - the webforj-side object instance to receive the event
   * @param method - the method name to receive the callback
   * @return the instance of the event proxy on the BBj side that receives the BBj events (which
   *         exposes "onEvent" for the BBj side)
   */
  CustomObject getEventProxy(Object obj, String method);

  /**
   * create and register an event proxy that defers BBj-side callback invocations to the Java DWCJ
   * side.
   *
   * @param obj - the webforj-side object instance to receive the event
   * @param method - the method name to receive the callback
   * @param eventclassname - the BBj event class type (used for BBj Plug-Ins that define custom
   *        event payload objects)
   * @return the instance of the event proxy on the BBj side that receives the BBj events (which
   *         exposes "onEvent" for the BBj side)
   */
  CustomObject getEventProxy(Object obj, String method, String eventclassname);

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
   * invoke the BBj-side MSGBOX function.
   *
   * @param msg the Message
   * @param options the options, see BBj MSGBOX docs
   * @param title the title
   * @return the result of the MSGBOX function result
   *
   * @deprecated since 24.02, for removal in 25.00
   */
  @Deprecated(since = "24.02", forRemoval = true)
  int msgbox(String msg, int options, String title);

  /**
   * invoke the BBj-side MSGBOX function with the given options.
   *
   * @param options the config
   * @return the result of the MSGBOX function
   */
  int msgbox(ConfirmDialog options);

  /**
   * invoke the BBj-side PROMPT function with the given options.
   *
   * @param options the options
   * @return the result of the PROMPT function
   */
  String prompt(InputDialog options);

  /**
   * invoke the BBj-side FILEOPEN(server) function with the given options.
   *
   * @param options the options
   * @return the result of the FILEOPEN function
   */
  String fileChooser(FileChooserDialog options);

  /**
   * invoke the BBj-side FILEOPEN(client) function with the given options.
   *
   * @param options the options
   * @return the result of the FILEOPEN function
   */
  String fileUpload(FileUploadDialog options);

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

  String getWorkingDirectory();

  String getQueryParam(String key);
}
