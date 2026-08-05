package com.webforj.devtools.hotswap;

import org.hotswap.agent.annotation.LoadEvent;
import org.hotswap.agent.annotation.OnClassLoadEvent;
import org.hotswap.agent.annotation.Plugin;

/**
 * Forwards every class redefinition the HotswapAgent java agent performs to the live reload
 * receiver of the application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@Plugin(name = "Webforj",
    description = "Forwards class redefinitions to the webforJ live reload receiver",
    testedVersions = "2.0.3")
public class WebforjHotswapPlugin {

  static final String RECEIVER_CLASS_NAME =
      "com.webforj.devtools.livereload.receiver.HotswapAgentReceiver";
  static final String RECEIVER_METHOD_NAME = "onClassRedefinition";

  /**
   * Reports one class redefinition to the receiver of the application owning the given classloader.
   *
   * <p>
   * The class bytes travel along, so the receiver can tell a genuine change apart from the same
   * compiled class arriving again through a repeated redefinition of unchanged bytes.
   * </p>
   *
   * @param className the binary name of the redefined class
   * @param classfileBuffer the class bytes the redefinition installs
   * @param classLoader the classloader the class is redefined in
   */
  @OnClassLoadEvent(classNameRegexp = ".*", events = LoadEvent.REDEFINE)
  public static void onClassRedefinition(String className, byte[] classfileBuffer,
      ClassLoader classLoader) {
    if (classLoader == null) {
      return;
    }

    try {
      Class<?> receiverClass = Class.forName(RECEIVER_CLASS_NAME, true, classLoader);
      receiverClass.getMethod(RECEIVER_METHOD_NAME, String.class, byte[].class).invoke(null,
          className, classfileBuffer);
    } catch (ReflectiveOperationException | LinkageError e) {
      // the application does not ship the receiver, so there is nothing to notify
    }
  }
}
