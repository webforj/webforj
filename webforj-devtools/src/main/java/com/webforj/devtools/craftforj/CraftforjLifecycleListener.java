package com.webforj.devtools.craftforj;

import com.basis.startup.type.BBjException;
import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.Request;
import com.webforj.devtools.craftforj.action.CraftforjActionRegistry;
import com.webforj.devtools.craftforj.appinfo.AppInfoCollector;
import com.webforj.devtools.craftforj.appinfo.action.GetAppInfoAction;
import com.webforj.devtools.craftforj.capabilities.CapabilitiesProvider;
import com.webforj.devtools.craftforj.capabilities.action.GetCapabilitiesAction;
import com.webforj.devtools.craftforj.docs.action.GetDocsAction;
import com.webforj.devtools.craftforj.icons.action.GetIconPoolsAction;
import com.webforj.devtools.craftforj.icons.action.ResolveIconPoolAction;
import com.webforj.devtools.craftforj.inspector.action.ApplyChangesAction;
import com.webforj.devtools.craftforj.inspector.action.ApplyStagedSourceAction;
import com.webforj.devtools.craftforj.inspector.action.DestroyComponentAction;
import com.webforj.devtools.craftforj.inspector.action.DiscardStagedSourceAction;
import com.webforj.devtools.craftforj.inspector.action.GetBeanInfoAction;
import com.webforj.devtools.craftforj.inspector.action.GetComponentFeaturesAction;
import com.webforj.devtools.craftforj.inspector.action.GetComponentMapAction;
import com.webforj.devtools.craftforj.inspector.action.GetSourceAction;
import com.webforj.devtools.craftforj.inspector.action.GetStagedSourceAction;
import com.webforj.devtools.craftforj.inspector.action.GetTranslationsAction;
import com.webforj.devtools.craftforj.inspector.action.PreviewPatchAction;
import com.webforj.devtools.craftforj.inspector.action.SetFeaturePropertyAction;
import com.webforj.devtools.craftforj.inspector.action.StageSourceAction;
import com.webforj.devtools.craftforj.inspector.source.staging.CompileValidator;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.keys.CraftforjKeyStore;
import com.webforj.devtools.craftforj.keys.KeyTransport;
import com.webforj.devtools.craftforj.keys.action.GetKeysAction;
import com.webforj.devtools.craftforj.keys.action.SetKeyAction;
import com.webforj.devtools.craftforj.module.ModuleStore;
import com.webforj.devtools.craftforj.module.action.GetModuleAction;
import com.webforj.devtools.craftforj.router.ActiveRouteTracker;
import com.webforj.devtools.craftforj.router.RouteCollector;
import com.webforj.devtools.craftforj.router.action.GetActiveStateAction;
import com.webforj.devtools.craftforj.router.action.GetRoutesAction;
import com.webforj.devtools.craftforj.router.action.NavigateToRouteAction;
import com.webforj.devtools.craftforj.router.action.SetRouteSecurityAction;
import com.webforj.devtools.craftforj.security.ChannelCredentials;
import com.webforj.devtools.craftforj.security.CraftforjAccessPolicy;
import com.webforj.devtools.craftforj.styles.StylesheetModifier;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import com.webforj.devtools.craftforj.styles.action.ReadStylesheetAction;
import com.webforj.devtools.craftforj.styles.action.WriteStylesheetAction;
import com.webforj.event.page.PageEventOptions;
import com.webforj.router.Router;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Lifecycle listener that initializes craftforJ support for a webforJ app.
 *
 * <p>
 * Injects the craftforJ boot script into the page, registered via SPI
 * ({@code META-INF/services/com.webforj.AppLifecycleListener}). Requests are dispatched to
 * registered action handlers via {@link CraftforjActionRegistry}. Active only when the app runs in
 * debug mode ({@link Environment#isDebug()}).
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CraftforjLifecycleListener implements AppLifecycleListener {
  private static final System.Logger LOGGER =
      System.getLogger(CraftforjLifecycleListener.class.getName());
  private static final String NONCE_PLACEHOLDER = "__WDT_NONCE__";
  private static final String SINK_PLACEHOLDER = "__WDT_SINK__";
  private static final String MANIFEST_PLACEHOLDER = "__WDT_MANIFEST__";
  private static final String EMPTY_MANIFEST = "";
  private static final Map<String, String> scriptCache = new ConcurrentHashMap<>();
  private final ModuleStore moduleStore = new ModuleStore();
  private final ActiveRouteTracker activeRouteTracker;
  private CraftforjActionRegistry actionRegistry;

  /**
   * Creates a new CraftforjLifecycleListener with default dependencies.
   */
  public CraftforjLifecycleListener() {
    this(null, new ActiveRouteTracker());
  }

  /**
   * Creates a new CraftforjLifecycleListener with the given dependencies.
   *
   * @param actionRegistry the action registry, or {@code null} to build one per page
   * @param activeRouteTracker the active route tracker
   */
  CraftforjLifecycleListener(CraftforjActionRegistry actionRegistry,
      ActiveRouteTracker activeRouteTracker) {
    this.actionRegistry = actionRegistry;
    this.activeRouteTracker = activeRouteTracker;
  }

  /**
   * Registers the default action handlers.
   *
   * <p>
   * When licensed, read actions register unconditionally and writing actions register per
   * capability. When unlicensed, only the capabilities action registers.
   * </p>
   *
   * @param capabilitiesProvider the provider used to determine capabilities
   * @param app the running application
   */
  private void registerDefaultActions(CapabilitiesProvider capabilitiesProvider, App app) {
    Path projectRoot = resolveProjectRoot(app);

    if (capabilitiesProvider.isLicensed()) {
      // App info action
      actionRegistry.register(
          new GetAppInfoAction(new AppInfoCollector(app.getClass().getName(), projectRoot)));

      // Inspector actions
      actionRegistry.register(new GetComponentMapAction());
      actionRegistry.register(new GetComponentFeaturesAction());
      actionRegistry.register(new GetTranslationsAction());
      actionRegistry.register(new SetFeaturePropertyAction());
      actionRegistry.register(new DestroyComponentAction());

      actionRegistry.register(new GetSourceAction());
      actionRegistry.register(new GetBeanInfoAction());

      if (capabilitiesProvider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES)) {
        actionRegistry.register(new ApplyChangesAction());
        actionRegistry.register(new PreviewPatchAction());
      }

      if (capabilitiesProvider
          .isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_FREEFORM_CHANGES)) {
        SourceStagingArea stagingArea = new SourceStagingArea();
        actionRegistry
            .register(new StageSourceAction(stagingArea, new CompileValidator(), projectRoot));
        actionRegistry.register(new GetStagedSourceAction(stagingArea));
        actionRegistry.register(new ApplyStagedSourceAction(stagingArea));
        actionRegistry.register(new DiscardStagedSourceAction(stagingArea));
      }

      // Secret key store actions
      CraftforjKeyStore keyStore = CraftforjKeyStore.create();
      KeyTransport keyTransport = new KeyTransport();
      actionRegistry.register(new GetKeysAction(keyStore, keyTransport));
      actionRegistry.register(new SetKeyAction(keyStore, keyTransport));

      // Icon picker actions
      actionRegistry.register(new GetIconPoolsAction());
      actionRegistry.register(new ResolveIconPoolAction());

      // Docs actions
      actionRegistry.register(new GetDocsAction());

      // Panel bundle transport
      actionRegistry.register(new GetModuleAction(moduleStore));

      // Styles actions
      StylesheetResolver stylesheetResolver = new StylesheetResolver(projectRoot);
      StylesheetModifier stylesheetModifier = new StylesheetModifier();
      actionRegistry.register(new ReadStylesheetAction(stylesheetResolver, stylesheetModifier));

      if (capabilitiesProvider.isSupported(CapabilitiesProvider.CAPABILITY_STYLESHEET_CHANGES)) {
        actionRegistry.register(new WriteStylesheetAction(stylesheetResolver, stylesheetModifier));
      }

      // Router actions
      actionRegistry.register(new GetRoutesAction(new RouteCollector(null, activeRouteTracker)));
      actionRegistry.register(new GetActiveStateAction(activeRouteTracker));
      actionRegistry.register(new NavigateToRouteAction());

      if (capabilitiesProvider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES)) {
        actionRegistry.register(new SetRouteSecurityAction());
      }
    }

    // Capabilities action (always registered for communication)
    actionRegistry.register(new GetCapabilitiesAction(capabilitiesProvider.getVersion(),
        capabilitiesProvider.isLicensed(), capabilitiesProvider.getCapabilities(),
        capabilitiesProvider.getCompileGate(), capabilitiesProvider.getHotswapTool(),
        capabilitiesProvider.getHotswapLevel()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    // Only enable craftforJ when debug mode is active
    Environment env = Environment.getCurrent();
    if (env == null || !env.isDebug()) {
      return;
    }

    String clientAddress = resolveClientAddress();
    if (!CraftforjAccessPolicy.isAllowed(env.getConfig(), clientAddress)) {
      LOGGER.log(System.Logger.Level.INFO,
          "craftforJ is not available for client address {0}. Set {1} to allow it.", clientAddress,
          CraftforjAccessPolicy.KEY_HOSTS_ALLOWED);

      return;
    }

    ChannelCredentials credentials = ChannelCredentials.create();
    if (actionRegistry == null) {
      actionRegistry = new CraftforjActionRegistry(credentials);
    }

    boolean licensed = checkLicense(env);
    CapabilitiesProvider capabilitiesProvider = new CapabilitiesProvider(env.getConfig(), licensed);
    registerDefaultActions(capabilitiesProvider, app);

    Page page = Page.getCurrent();

    // Unlicensed pages get an empty manifest so the channel still opens but no module loads.
    page.addInlineJavaScript(renderBootScript(credentials, licensed), true);

    // Listen for craftforJ requests
    PageEventOptions options =
        new PageEventOptions().addData("request", "JSON.stringify(event.detail)");

    page.addEventListener("webforj-devtools-request", event -> {
      actionRegistry.dispatch(page, event);
    }, options);

    // Only attach the route tracker if licensed
    if (licensed) {
      Router router = Router.getCurrent();
      if (router != null) {
        activeRouteTracker.attach(router);
      }
    }
  }

  /**
   * Reads the address of the client the page is served to.
   *
   * @return the client address, or {@code null} when it cannot be read
   */
  private static String resolveClientAddress() {
    try {
      return Request.getCurrent().getIPAddress();
    } catch (RuntimeException e) {
      LOGGER.log(System.Logger.Level.DEBUG, "Could not read the client address", e);

      return null;
    }
  }

  /**
   * Fills the boot script template with the credentials and the module digests for this page.
   *
   * @param credentials the channel credentials
   * @param licensed whether the app may load craftforJ modules at all
   * @return the ready to inject script
   */
  private String renderBootScript(ChannelCredentials credentials, boolean licensed) {
    return loadScript(ModuleStore.BOOT_RESOURCE).replace(NONCE_PLACEHOLDER, credentials.getNonce())
        .replace(SINK_PLACEHOLDER, credentials.getSinkId())
        .replace(MANIFEST_PLACEHOLDER, licensed ? moduleStore.getManifest() : EMPTY_MANIFEST);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillTerminate(App app) {
    activeRouteTracker.detach();
  }

  /**
   * Checks whether a valid WEBFORJ license is available.
   *
   * <p>
   * A successful checkout or an "already checked out" error (code 101) both count as licensed.
   * </p>
   *
   * @param env the current environment
   * @return {@code true} if licensed
   */
  private static boolean checkLicense(Environment env) {
    try {
      env.getBBjAPI().ensureCheckout();
      return true;
    } catch (BBjException e) {
      // Error 101 = already checked out -> still licensed
      boolean licensed = e.getHostErrorNumber() == 101;
      if (!licensed) {
        LOGGER.log(System.Logger.Level.WARNING, "craftforJ license checkout failed", e);
      }

      return licensed;
    }
  }

  /**
   * Resolves the project root for the running application.
   *
   * @param app the running application
   * @return the project root directory
   */
  private static Path resolveProjectRoot(App app) {
    Environment env = Environment.getCurrent();
    return ProjectRootResolver.resolve(env == null ? null : env.getConfig(), app.getClass());
  }

  /**
   * Loads a classpath script, caching it for the JVM lifetime.
   *
   * @param path the classpath resource path
   * @return the script content
   */
  private static String loadScript(String path) {
    return scriptCache.computeIfAbsent(path, p -> {
      try (InputStream is =
          CraftforjLifecycleListener.class.getClassLoader().getResourceAsStream(p)) {
        if (is == null) {
          throw new IllegalStateException("craftforJ script not found: " + p);
        }
        try (BufferedReader reader =
            new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
          return reader.lines().collect(Collectors.joining("\n"));
        }
      } catch (IOException e) {
        throw new IllegalStateException("Failed to load craftforJ script: " + p, e);
      }
    });
  }
}
