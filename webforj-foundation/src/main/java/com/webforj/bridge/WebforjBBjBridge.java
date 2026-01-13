package com.webforj.bridge;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.CustomObject;
import com.webforj.bbj.BBjVar;
import java.util.ArrayList;
import java.util.Locale;

public interface WebforjBBjBridge {

  /**
   * create and register an event proxy that defers B Bj-side callback invocations to the Java DWCJ
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
   * invoke the BBj-side WAIT function.
   *
   * @param seconds the number of seconds to wait
   * @since 24.02
   */
  void sleep(int seconds);

  /**
   * Returns the Julian day number for a given date string.
   *
   * @param date the date string formatted according to the BBj date masks.
   * @param mask the date mask which was used to format the date string
   * @param locale the locale to use for parsing the date. The locale is only relevant when passing
   *        a date string containing a week number reference
   *
   * @return the Julian day number
   * @since 24.10
   */
  Integer parseDate(String date, String mask, Locale locale);

  /**
   * Returns the time value for a given time string.
   *
   * @param time the time string formatted according to the BBj time masks.
   * @param mask the date mask which was used to format the time string
   * @param locale the locale to use for parsing the time. The locale is only relevant when parsing
   *        a time string containing a localized am/pm value
   *
   * @return a number greater than or equal to zero and less than 24 and indicates hours and
   *         fractions of hours.
   * @since 24.10
   */
  Double parseTime(String time, String mask, Locale locale);

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
