package com.webforj.testbed;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.component.optiondialog.ConfirmDialog;
import com.webforj.component.window.Frame;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.event.NavigateEvent;
import com.webforj.router.history.History;
import com.webforj.router.history.Location;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.history.ParametersBag;
import com.webforj.testbed.runtime.TestbedRuntime;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

/**
 * The context a test runs in, owning the lifecycle of the webforJ {@link Environment}.
 *
 * <p>
 * {@link #setup()} initializes the production {@link Environment} for the current thread against an
 * in memory runtime and {@link #tearDown()} disposes it. The context is not an {@link Environment}
 * itself. It boots the real one, optionally launches the application and the router, and gives the
 * test its entry points. Between setup and teardown, components attach, fire events and navigate
 * exactly as they do in production, and every interaction completes synchronously.
 * </p>
 *
 * <p>
 * Tests based on JUnit annotate the test class with {@code @WebforjTest} instead of calling the
 * lifecycle methods directly.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WebforjTestbed {
  private static final ThreadLocal<WebforjTestbed> current = new ThreadLocal<>();

  private final TestbedRuntime runtime;
  private final App app;
  private Component lastRenderedRoute;
  private Frame frame;

  private WebforjTestbed(TestbedRuntime runtime, App app) {
    this.runtime = runtime;
    this.app = app;
  }

  /**
   * Sets up a test context for the current thread with default options.
   *
   * @return the context
   * @throws IllegalStateException if a context is already set up on the current thread
   */
  public static WebforjTestbed setup() {
    return setup(new WebforjTestbedOptions());
  }

  /**
   * Sets up a test context for the current thread.
   *
   * @param options the context options
   *
   * @return the context
   * @throws IllegalStateException if a context is already set up on the current thread
   */
  public static WebforjTestbed setup(WebforjTestbedOptions options) {
    if (current.get() != null) {
      throw new IllegalStateException(
          "A test context is already set up on this thread. Call WebforjTestbed.tearDown() "
              + "before setting up a new one.");
    }

    TestbedRuntime runtime = TestbedRuntime.start();
    try {
      App app = options.getApp().map(runtime::launch).orElse(null);
      if (app == null) {
        runtime.initEnvironment();
      }

      WebforjTestbed context = new WebforjTestbed(runtime, app);
      current.set(context);

      if (app != null) {
        context.getRouter().ifPresent(context::attachRenderTracker);
      } else if (options.hasRoutes()) {
        context.installRouter(options);
      }

      return context;
    } catch (RuntimeException e) {
      runtime.stop();
      current.remove();
      throw e;
    }
  }

  /**
   * Returns the test context of the current thread.
   *
   * @return the context
   * @throws IllegalStateException if no context is set up on the current thread
   */
  public static WebforjTestbed getCurrent() {
    WebforjTestbed context = current.get();
    if (context == null) {
      throw new IllegalStateException(
          "No test context is set up on this thread. Call WebforjTestbed.setup() or annotate "
              + "the test class with @WebforjTest.");
    }

    return context;
  }

  /**
   * Returns whether a test context is set up on the current thread.
   *
   * @return true when a context is set up
   */
  public static boolean isPresent() {
    return current.get() != null;
  }

  /**
   * Tears down the test context of the current thread.
   *
   * <p>
   * Tearing down an absent context has no effect.
   * </p>
   */
  public static void tearDown() {
    WebforjTestbed context = current.get();
    if (context == null) {
      return;
    }

    try {
      context.runtime.stop();
    } finally {
      current.remove();
    }
  }

  /**
   * Fires every running {@link com.webforj.Interval} once, as if its delay elapsed.
   *
   * @return the number of intervals that fired
   */
  public int runIntervals() {
    return runtime.runIntervals();
  }

  /**
   * Returns the application launched in this context.
   *
   * @return the application, or an empty optional when the context runs without one
   */
  public Optional<App> getApp() {
    return Optional.ofNullable(app);
  }

  /**
   * Returns the router of this context.
   *
   * @return the router, or an empty optional when no routes are configured
   */
  public Optional<Router> getRouter() {
    return Optional.ofNullable(Router.getCurrent());
  }

  /**
   * Returns the option dialogs the application showed so far, in order.
   *
   * @return the shown dialogs
   */
  public List<ShownOptionDialog> getShownOptionDialogs() {
    return runtime.getDialogAnswers().getShown();
  }

  /**
   * Answers the next confirm dialog with the given result.
   *
   * <p>
   * Option dialogs block until the user answers, so a test scripts the answer before the code under
   * test shows the dialog. Each answer serves the next dialog of its kind. A message dialog needs
   * no answer, the user dismisses it. A dialog shown without a scripted answer fails the test.
   * </p>
   *
   * @param result the button the user presses
   */
  public void answerConfirmDialog(ConfirmDialog.Result result) {
    runtime.getDialogAnswers().answer(ShownOptionDialog.Kind.CONFIRM, result);
  }

  /**
   * Answers the next input dialog with the given text, null cancels the dialog.
   *
   * @param text the text the user enters
   */
  public void answerInputDialog(String text) {
    runtime.getDialogAnswers().answer(ShownOptionDialog.Kind.INPUT, text);
  }

  /**
   * Answers the next file chooser dialog with the given path, null cancels the dialog.
   *
   * @param path the path the user chooses
   */
  public void answerFileChooserDialog(String path) {
    runtime.getDialogAnswers().answer(ShownOptionDialog.Kind.FILE_CHOOSER, path);
  }

  /**
   * Answers the next file save dialog with the given path, null cancels the dialog.
   *
   * @param path the path the user chooses
   */
  public void answerFileSaveDialog(String path) {
    runtime.getDialogAnswers().answer(ShownOptionDialog.Kind.FILE_SAVE, path);
  }

  /**
   * Answers the next file upload dialog with the given file name, null cancels the dialog.
   *
   * @param fileName the name of the file the user uploads
   */
  public void answerFileUploadDialog(String fileName) {
    runtime.getDialogAnswers().answer(ShownOptionDialog.Kind.FILE_UPLOAD, fileName);
  }

  /**
   * Scripts the result the next call of {@code executeJs} returns.
   *
   * <p>
   * {@code executeJs} waits for the client inside the call, so the result is scripted before the
   * code under test runs the script. Each result serves the next call, in order. Without a scripted
   * result the call returns null.
   * </p>
   *
   * @param result the result the client returns
   */
  public void answerExecuteJs(Object result) {
    runtime.getScriptAnswers().answerExecuteJs(result);
  }

  /**
   * Completes the oldest script started with {@code executeJsAsync} that still waits for its
   * result, the way the next round trip to the client would.
   *
   * @param result the result the client returns, null allowed
   *
   * @throws IllegalStateException if no script waits for a result
   */
  public void answerExecuteJsAsync(Object result) {
    runtime.getScriptAnswers().answerExecuteJsAsync(result);
  }

  /**
   * Holds a file the user picked on the client of this context, with the given bytes. An upload
   * performed afterwards with the same name reads, copies and deletes these bytes the way it does
   * in production.
   *
   * <pre>{@code
   * context.pickFile("contacts.csv", bytes);
   * UserInteraction.perform(upload, UploadInteraction.upload("contacts.csv"));
   * }</pre>
   *
   * @param name the name of the file on the client
   * @param contents the bytes of the file
   */
  public void pickFile(String name, byte[] contents) {
    runtime.putUploadedFile(name, contents);
  }

  /**
   * Returns the files the application sent to the user for download, in the order it sent them.
   *
   * @return the downloaded files
   */
  public List<DownloadedFile> getDownloads() {
    return runtime.getDownloads();
  }

  /**
   * Returns the root frame of this context.
   *
   * <p>
   * When the application under test created frames, the first one is returned. Otherwise a frame is
   * created on first access.
   * </p>
   *
   * @return the root frame
   */
  public Frame getFrame() {
    if (frame != null) {
      return frame;
    }

    List<Frame> frames = App.getFrames();
    if (!frames.isEmpty()) {
      frame = frames.get(0);

      return frame;
    }

    try {
      frame = new Frame();
    } catch (WebforjAppInitializeException e) {
      throw new IllegalStateException("Failed to create the root frame", e);
    }

    return frame;
  }

  /**
   * Adds the given components to the root frame of this context.
   *
   * <p>
   * Adding a component attaches it and runs its full lifecycle, exactly as adding it to a frame in
   * production does.
   * </p>
   *
   * @param components the components to add
   */
  public void add(Component... components) {
    getFrame().add(components);
  }

  /**
   * Navigates to the route registered for the given component and returns the rendered instance.
   *
   * <p>
   * Route security observers run as part of navigation. When the security manager configured by the
   * application redirects to an authentication or deny location, the component actually rendered is
   * the one at that location, not {@code view}. Navigation then fails naming both the requested
   * view and the view that rendered instead of it, since the caller asked for a specific type. Use
   * {@link #navigate(String)} to observe the redirected component directly.
   * </p>
   *
   * @param <T> the component type
   * @param view the component class the route renders
   *
   * @return the rendered component
   * @throws IllegalStateException if navigation does not end on the given component
   */
  public <T extends Component> T navigate(Class<T> view) {
    return navigate(view, new ParametersBag());
  }

  /**
   * Navigates to the route registered for the given component with the given route parameters and
   * returns the rendered instance.
   *
   * <p>
   * The parameters fill the placeholders of the route, so a component registered under
   * {@code product/:id} navigated with {@code ParametersBag.of("id=42")} renders at
   * {@code product/42}. Redirects by route security fail the same way as in
   * {@link #navigate(Class)}.
   * </p>
   *
   * @param <T> the component type
   * @param view the component class the route renders
   * @param parameters the parameters that fill the route
   *
   * @return the rendered component
   * @throws IllegalStateException if navigation does not end on the given component
   */
  public <T extends Component> T navigate(Class<T> view, ParametersBag parameters) {
    Router router = requireRouter();
    getFrame(); // create if not yet

    beginNavigation();
    AtomicReference<Component> rendered = new AtomicReference<>();
    router.navigate(view, parameters, resolved -> resolved.ifPresent(rendered::set));

    return expectView(view, resolveRendered(rendered.get()), view.getName());
  }

  /**
   * Navigates to the given location and returns the rendered component.
   *
   * <p>
   * When the application's route security redirects the requested location to an authentication or
   * deny location, the component returned is the one rendered at that location, which is how a
   * redirect triggered by route security is observed from a test.
   * </p>
   *
   * @param location the location to navigate to
   *
   * @return the rendered component
   * @throws IllegalStateException if navigation renders no component
   */
  public Component navigate(String location) {
    Router router = requireRouter();
    getFrame();

    beginNavigation();
    AtomicReference<Component> rendered = new AtomicReference<>();
    NavigationOptions options = new NavigationOptions();
    router.navigate(new Location(location), options, resolved -> resolved.ifPresent(rendered::set));

    return expectRendered(resolveRendered(rendered.get()), location);
  }

  private void beginNavigation() {
    lastRenderedRoute = null;
  }

  private Component resolveRendered(Component directlyResolved) {
    return directlyResolved != null ? directlyResolved : lastRenderedRoute;
  }

  private void installRouter(WebforjTestbedOptions options) {
    RouteRegistry registry = options.getRoutePackages().isEmpty() ? new RouteRegistry()
        : RouteRegistry.ofPackage(options.getRoutePackages().toArray(String[]::new));

    for (Class<? extends Component> routeClass : options.getRouteClasses()) {
      registry.register(routeClass);
    }

    options.getRegistryConfigurer().ifPresent(configurer -> configurer.accept(registry));

    Router router = new Router(registry, new MemoryHistory());
    attachRenderTracker(router);
    ObjectTable.put(Router.class.getName(), router);
  }

  private void attachRenderTracker(Router router) {
    router.onNavigate(this::trackNavigatedComponent);
  }

  private void trackNavigatedComponent(NavigateEvent event) {
    lastRenderedRoute = event.getContext().getComponent();
  }

  private Router requireRouter() {
    return getRouter().orElseThrow(() -> new IllegalStateException(
        "No router is available. Configure routes through WebforjTestbedOptions or launch an "
            + "application annotated with @Routify."));
  }

  private <T extends Component> T expectView(Class<T> view, Component rendered, String target) {
    if (rendered == null) {
      throw new IllegalStateException(deniedMessage(target));
    }

    if (!view.isInstance(rendered)) {
      throw new IllegalStateException("Navigation to " + target + " was redirected and rendered "
          + rendered.getClass().getName() + " instead of " + view.getName() + reasonSuffix()
          + ". Use navigate(String) to observe the component that rendered.");
    }

    return view.cast(rendered);
  }

  private Component expectRendered(Component rendered, String target) {
    if (rendered == null) {
      throw new IllegalStateException(deniedMessage(target));
    }

    return rendered;
  }

  private String deniedMessage(String target) {
    return "Navigation to " + target + " rendered no component. The navigation was vetoed, "
        + "either by a route lifecycle observer or by route security with no authentication or "
        + "deny location configured.";
  }

  private String reasonSuffix() {
    return getRouter().map(Router::getHistory).flatMap(History::getLocation)
        .flatMap(location -> location.getQueryParameters().get("reason"))
        .map(reason -> " (reason: " + reason + ")").orElse("");
  }
}
