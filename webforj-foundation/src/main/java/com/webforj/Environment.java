package com.webforj;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WebforjBBjBridge;

import java.util.HashMap;

public final class Environment {

  private static final HashMap<Object, Environment> instanceMap = new HashMap<>();
  private final BBjAPI api;
  private final BBjSysGui sysgui;
  private final WebforjBBjBridge helper;

  private Environment(BBjAPI api, WebforjBBjBridge helper) throws BBjException {
    this.api = api;
    this.sysgui = api.openSysGui("X0");
    this.helper = helper;
  }

  public static void init(BBjAPI api, WebforjBBjBridge helper) throws BBjException {
    Environment env = new Environment(api, helper);
    Environment.instanceMap.put(Thread.currentThread().getName(), env);
  }

  public static void cleanup() {
    Environment.instanceMap.remove(Thread.currentThread().getName());
  }

  public static Environment getCurrent() {
    return Environment.instanceMap.get(Thread.currentThread().getName());
  }

  public BBjAPI getBBjAPI() {
    return this.api;
  }

  public BBjSysGui getSysGui() {
    return this.sysgui;
  }

  public WebforjBBjBridge getWeforjHelper() {
    return helper;
  }


  /*
   * LOGGING: for now we rely on BBj's redirection of err and out into its own logging. In the
   * future we will definitely want to allow more granular debug options and the use of custom
   * loggers that fit a customer's environment Bear with us and consider this a basic solution for
   * the time being. WIP
   */

  public static void logError(String message, Exception e) {
    System.err.println(message); // NOSONAR
    e.printStackTrace(); // NOSONAR
  }

  public static void logError(Exception e) {
    e.printStackTrace(); // NOSONAR
  }

  public static void logError(String message) {
    System.err.println(message); // NOSONAR
  }

}
