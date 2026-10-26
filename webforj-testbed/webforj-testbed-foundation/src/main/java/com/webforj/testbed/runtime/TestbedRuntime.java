package com.webforj.testbed.runtime;

import com.basis.startup.type.BBjException;
import com.webforj.App;
import com.webforj.Bootstrap;
import com.webforj.Environment;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.environment.ObjectTable;
import com.webforj.testbed.DownloadedFile;
import java.util.List;

/**
 * Stands in for the browser and the server runtime while a test runs.
 *
 * <p>
 * Tests work through {@code WebforjTestbed} and {@code UserInteraction}. This class is the
 * implementation behind them and speaks only in components, event classes and event data.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class TestbedRuntime {
  private static final String NO_SERVICES_PROPERTY = "com.basis.noBBjServices";
  private static final String NO_CLIENT_PROPERTY = "webforj.noClient";
  private static final ThreadLocal<TestbedRuntime> current = new ThreadLocal<>();

  static boolean isRunning() {
    return current.get() != null;
  }

  private final BBjApiMock api = new BBjApiMock();
  private final String previousNoServicesValue;
  private final String previousNoClientValue;

  private TestbedRuntime(String previousNoServicesValue, String previousNoClientValue) {
    this.previousNoServicesValue = previousNoServicesValue;
    this.previousNoClientValue = previousNoClientValue;
  }

  /**
   * Starts a runtime for the current thread.
   *
   * @return the started runtime
   * @throws IllegalStateException if a runtime is already running on this thread
   */
  public static TestbedRuntime start() {
    if (current.get() != null) {
      throw new IllegalStateException("A runtime is already running on this thread.");
    }

    TestbedRuntime runtime = new TestbedRuntime(System.getProperty(NO_SERVICES_PROPERTY),
        System.getProperty(NO_CLIENT_PROPERTY));
    System.setProperty(NO_SERVICES_PROPERTY, "true");
    // the runtime has no browser, so a component reads client state from the server
    System.setProperty(NO_CLIENT_PROPERTY, "true");
    current.set(runtime);

    return runtime;
  }

  /**
   * Returns the runtime of the current thread.
   *
   * @return the current runtime
   * @throws IllegalStateException if no runtime is running on this thread
   */
  public static TestbedRuntime getCurrent() {
    TestbedRuntime runtime = current.get();
    if (runtime == null) {
      throw new IllegalStateException(
          "No test context is set up on this thread. Call WebforjTestbed.setup() or annotate "
              + "the test class with @WebforjTest.");
    }

    return runtime;
  }

  /**
   * Stores an uploaded file in this context's client cache.
   *
   * @param name the client file name
   * @param contents the uploaded bytes, copied into the cache
   */
  public void putUploadedFile(String name, byte[] contents) {
    api.getThinClient().getFileSystem().put(name, contents);
  }

  /**
   * Returns the files the application sent to the user for download, in the order it sent them.
   *
   * @return the downloaded files
   */
  public List<DownloadedFile> getDownloads() {
    return api.getThinClient().getFileSystem().getDownloads();
  }

  /**
   * Initializes the webforJ environment on top of this runtime without launching an application.
   */
  public void initEnvironment() {
    try {
      Environment.init(api.getApi(), 0);
    } catch (BBjException e) {
      throw new IllegalStateException("Failed to initialize the environment", e);
    }
  }

  /**
   * Launches the given application on top of this runtime.
   *
   * @param appClass the application class
   * @return the running application
   */
  public App launch(Class<? extends App> appClass) {
    try {
      return Bootstrap.init(api.getApi(), 0, appClass.getName());
    } catch (Exception e) {
      if (!isCausedByConceiver(e)) {
        throw new IllegalStateException("Failed to launch " + appClass.getName(), e);
      }

      // Without a conceiver registered by an integration, the application boots with the default
      // one, which is what a plain deployment does as well.
      ObjectTable.put(ConceiverProvider.LOOKUP_KEY, new DefaultConceiver());
      try {
        return Bootstrap.init(api.getApi(), 0, appClass.getName());
      } catch (Exception retryFailure) {
        throw new IllegalStateException("Failed to launch " + appClass.getName(), retryFailure);
      }
    }
  }

  /**
   * Stops this runtime and releases everything it holds for the current thread.
   */
  public void stop() {
    try {
      Environment.cleanup();
    } finally {
      try {
        api.getThinClient().getFileSystem().dispose();
      } finally {
        restore(NO_SERVICES_PROPERTY, previousNoServicesValue);
        restore(NO_CLIENT_PROPERTY, previousNoClientValue);

        current.remove();
      }
    }
  }

  /**
   * Returns the scripted answers to option dialogs and the record of the dialogs shown.
   *
   * @return the dialog answers
   */
  public OptionDialogAnswerRegistry getDialogAnswers() {
    return api.getDialogAnswers();
  }

  /**
   * Returns the answers the client gives to scripts.
   *
   * @return the script answers
   */
  public ScriptAnswerRegistry getScriptAnswers() {
    return api.getScriptAnswers();
  }

  /**
   * Fires every running interval once, as if its delay elapsed.
   *
   * @return the number of intervals that fired
   */
  public int runIntervals() {
    int fired = 0;
    for (Object key : api.getTimerKeys()) {
      if (api.fireTimer(key)) {
        fired++;
      }
    }

    return fired;
  }

  private static boolean isCausedByConceiver(Throwable throwable) {
    Throwable cause = throwable;
    while (cause != null) {
      if (cause instanceof ConceiverException) {
        return true;
      }

      cause = cause.getCause();
    }

    return false;
  }

  private static void restore(String property, String previousValue) {
    if (previousValue == null) {
      System.clearProperty(property);
    } else {
      System.setProperty(property, previousValue);
    }
  }
}
