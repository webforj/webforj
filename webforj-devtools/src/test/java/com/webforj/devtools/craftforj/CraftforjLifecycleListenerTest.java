package com.webforj.devtools.craftforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.startup.type.BBjException;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.Request;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.action.CraftforjActionRegistry;
import com.webforj.devtools.craftforj.appinfo.action.GetAppInfoAction;
import com.webforj.devtools.craftforj.capabilities.CapabilitiesProvider;
import com.webforj.devtools.craftforj.capabilities.action.GetCapabilitiesAction;
import com.webforj.devtools.craftforj.history.HistoryCapability;
import com.webforj.devtools.craftforj.history.action.GetHistoryAction;
import com.webforj.devtools.craftforj.history.action.RedoHistoryAction;
import com.webforj.devtools.craftforj.history.action.RemoveHistoryAction;
import com.webforj.devtools.craftforj.history.action.UndoHistoryAction;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.inspector.action.ApplyChangesAction;
import com.webforj.devtools.craftforj.inspector.action.ApplyStagedSourceAction;
import com.webforj.devtools.craftforj.inspector.action.GetBeanInfoAction;
import com.webforj.devtools.craftforj.inspector.action.GetComponentMapAction;
import com.webforj.devtools.craftforj.inspector.action.GetSourceAction;
import com.webforj.devtools.craftforj.inspector.action.PreviewPatchAction;
import com.webforj.devtools.craftforj.inspector.action.RemoveComponentSourceAction;
import com.webforj.devtools.craftforj.inspector.action.StageSourceAction;
import com.webforj.devtools.craftforj.inspector.source.SourceFreeformChangesCapability;
import com.webforj.devtools.craftforj.router.ActiveRouteTracker;
import com.webforj.devtools.craftforj.router.action.SetRouteSecurityAction;
import com.webforj.devtools.craftforj.security.ChannelCredentials;
import com.webforj.devtools.craftforj.security.CraftforjAccessPolicy;
import com.webforj.devtools.craftforj.source.SourceChangesCapability;
import com.webforj.devtools.craftforj.styles.StylesheetChangesCapability;
import com.webforj.devtools.craftforj.styles.action.ReadStylesheetAction;
import com.webforj.devtools.craftforj.styles.action.WriteStylesheetAction;
import com.webforj.event.page.PageEvent;
import com.webforj.router.Router;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

class CraftforjLifecycleListenerTest {

  @TempDir
  Path home;

  private CraftforjActionRegistry registry;
  private ActiveRouteTracker tracker;
  private CraftforjLifecycleListener listener;

  @BeforeEach
  void setUp() {
    registry = new CraftforjActionRegistry(ChannelCredentials.create());
    tracker = mock(ActiveRouteTracker.class);
    listener = new CraftforjLifecycleListener(registry, tracker, home);
  }

  private static void enableDevTools(Environment env) {
    enableDevTools(env, "");
  }

  /**
   * Turns craftforJ on and appends the given configuration.
   *
   * @param env the mocked environment
   * @param extraConfig additional configuration in HOCON form
   */
  private static void enableDevTools(Environment env, String extraConfig) {
    when(env.getConfig()).thenReturn(
        ConfigFactory.parseString(CraftforjAccessPolicy.KEY_ENABLED + " = true\n" + extraConfig));
  }

  private static void allowLoopback(MockedStatic<Request> requestMock) {
    Request request = mock(Request.class);
    when(request.getIPAddress()).thenReturn("127.0.0.1");
    requestMock.when(Request::getCurrent).thenReturn(request);
  }

  @Test
  @DisplayName("Should do nothing without an environment")
  void shouldDoNothingWithoutEnvironment() {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class)) {
      envMock.when(Environment::getCurrent).thenReturn(null);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(GetCapabilitiesAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should do nothing when debug mode is off")
  void shouldDoNothingWithoutDebugMode() {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(false);
      envMock.when(Environment::getCurrent).thenReturn(env);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(GetCapabilitiesAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should do nothing when the configuration does not turn craftforJ on")
  void shouldDoNothingWithoutTheEnabledFlag() {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(GetCapabilitiesAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should register the history actions when licensed")
  void shouldRegisterHistoryActionsWhenLicensed() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      pageMock.when(Page::getCurrent).thenReturn(mock(Page.class));

      listener.onWillRun(mock(App.class));

      assertTrue(registry.unregister(GetHistoryAction.ACTION));
      assertTrue(registry.unregister(UndoHistoryAction.ACTION));
      assertTrue(registry.unregister(RedoHistoryAction.ACTION));
      assertTrue(registry.unregister(RemoveHistoryAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should register no history action and make no journal when no write capability is "
      + "supported")
  void shouldNotRegisterHistoryActionsWithoutWriteCapability() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env, """
          webforj.devtools.craftforj {
            source-changes = false
            stylesheet-changes = false
          }
          """);
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      pageMock.when(Page::getCurrent).thenReturn(mock(Page.class));
      routerMock.when(Router::getCurrent).thenReturn(null);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(GetHistoryAction.ACTION));
      assertFalse(registry.unregister(UndoHistoryAction.ACTION));
      assertFalse(registry.unregister(RedoHistoryAction.ACTION));
      assertFalse(registry.unregister(RemoveHistoryAction.ACTION));
      assertTrue(registry.unregister(GetCapabilitiesAction.ACTION));
      assertFalse(Files.exists(home.resolve(".webforj")));
    }
  }

  @Test
  @DisplayName("Should record a write request in the journal the history actions share")
  @SuppressWarnings("rawtypes")
  void shouldRecordWriteRequestInSharedJournal() throws Exception {
    Path projectRoot = home.resolve("project");
    Path sheet = projectRoot.resolve("src/main/resources/static/app.css");
    Files.createDirectories(sheet.getParent());
    Files.writeString(sheet, "a {}\n");
    ChannelCredentials credentials = ChannelCredentials.create();
    CraftforjActionRegistry spied = spy(new CraftforjActionRegistry(credentials));
    CraftforjLifecycleListener writing = new CraftforjLifecycleListener(spied, tracker, home);

    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class);
        MockedConstruction<CapabilitiesProvider> providers =
            mockConstruction(CapabilitiesProvider.class, (provider, context) -> {
              when(provider.isLicensed()).thenReturn(true);
              when(provider.isSupported(anyString())).thenReturn(true);
              when(provider.getCapabilities()).thenReturn(
                  List.of(SourceChangesCapability.KEY, SourceFreeformChangesCapability.KEY,
                      StylesheetChangesCapability.KEY, HistoryCapability.KEY));
            })) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env, ProjectRootResolver.KEY_PROJECT_ROOT + " = \"" + projectRoot + "\"");
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      pageMock.when(Page::getCurrent).thenReturn(mock(Page.class));
      routerMock.when(Router::getCurrent).thenReturn(null);

      writing.onWillRun(mock(App.class));
    }

    ArgumentCaptor<CraftforjActionHandler> captor =
        ArgumentCaptor.forClass(CraftforjActionHandler.class);
    verify(spied, atLeastOnce()).register(captor.capture());
    Map<String, CraftforjActionHandler> handlers = new HashMap<>();
    captor.getAllValues().forEach(handler -> handlers.put(handler.getAction(), handler));
    assertTrue(handlers.keySet()
        .containsAll(List.of(ApplyChangesAction.ACTION, RemoveComponentSourceAction.ACTION,
            SetRouteSecurityAction.ACTION, ApplyStagedSourceAction.ACTION,
            WriteStylesheetAction.ACTION, GetHistoryAction.ACTION, UndoHistoryAction.ACTION,
            RedoHistoryAction.ACTION, RemoveHistoryAction.ACTION)));

    JsonObject response =
        dispatch(spied, credentials, WriteStylesheetAction.ACTION, createAppendParams());
    HistoryJournalInfo journal =
        (HistoryJournalInfo) handlers.get(GetHistoryAction.ACTION).handle(new JsonObject());

    assertTrue(response.getAsJsonObject("data").get("applied").getAsBoolean());
    assertEquals(1, journal.getEntries().size());
    assertEquals(journal.getEntries().get(0).getId(), response.get("historyId").getAsLong());
    assertEquals(sheet.toAbsolutePath().normalize().toString(),
        journal.getEntries().get(0).getFiles().get(0).getPath());
  }

  @Test
  @DisplayName("Should run the writes and keep nothing when the history is not supported")
  @SuppressWarnings("rawtypes")
  void shouldRunWritesWithoutHistory() throws Exception {
    Path projectRoot = home.resolve("project");
    Path sheet = projectRoot.resolve("src/main/resources/static/app.css");
    Files.createDirectories(sheet.getParent());
    Files.writeString(sheet, "a {}\n");
    ChannelCredentials credentials = ChannelCredentials.create();
    CraftforjActionRegistry spied = spy(new CraftforjActionRegistry(credentials));
    CraftforjLifecycleListener writing = new CraftforjLifecycleListener(spied, tracker, home);

    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class);
        MockedConstruction<CapabilitiesProvider> providers =
            mockConstruction(CapabilitiesProvider.class, (provider, context) -> {
              when(provider.isLicensed()).thenReturn(true);
              when(provider.isSupported(anyString()))
                  .thenAnswer(call -> !HistoryCapability.KEY.equals(call.getArgument(0)));
              when(provider.getCapabilities()).thenReturn(List.of(SourceChangesCapability.KEY,
                  SourceFreeformChangesCapability.KEY, StylesheetChangesCapability.KEY));
            })) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env, ProjectRootResolver.KEY_PROJECT_ROOT + " = \"" + projectRoot + "\"");
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      pageMock.when(Page::getCurrent).thenReturn(mock(Page.class));
      routerMock.when(Router::getCurrent).thenReturn(null);

      writing.onWillRun(mock(App.class));
    }

    ArgumentCaptor<CraftforjActionHandler> captor =
        ArgumentCaptor.forClass(CraftforjActionHandler.class);
    verify(spied, atLeastOnce()).register(captor.capture());
    Map<String, CraftforjActionHandler> handlers = new HashMap<>();
    captor.getAllValues().forEach(handler -> handlers.put(handler.getAction(), handler));
    assertTrue(handlers.containsKey(WriteStylesheetAction.ACTION));
    assertFalse(handlers.containsKey(GetHistoryAction.ACTION));

    JsonObject response =
        dispatch(spied, credentials, WriteStylesheetAction.ACTION, createAppendParams());

    assertTrue(response.getAsJsonObject("data").get("applied").getAsBoolean());
    assertTrue(response.get("historyId").isJsonNull());
    assertFalse(Files.exists(home.resolve(".webforj")));
  }

  private static JsonObject createAppendParams() {
    JsonObject change = new JsonObject();
    change.addProperty("type", "APPEND");
    change.addProperty("text", ".card { padding: 8px; }");
    JsonArray changes = new JsonArray();
    changes.add(change);
    JsonObject params = new JsonObject();
    params.addProperty("file", "src/main/resources/static/app.css");
    params.add("changes", changes);

    return params;
  }

  private static JsonObject dispatch(CraftforjActionRegistry target, ChannelCredentials credentials,
      String action, JsonObject params) {
    JsonObject request = new JsonObject();
    request.addProperty("requestId", "r1");
    request.addProperty("action", action);
    request.addProperty("nonce", credentials.getNonce());
    request.add("params", params);
    Page page = mock(Page.class);
    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(Map.of("request", request.toString()));

    target.dispatch(page, event);

    ArgumentCaptor<String> script = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsVoidAsync(script.capture());
    String sent = script.getValue();

    return JsonParser.parseString(sent.substring(sent.indexOf('(') + 1, sent.lastIndexOf(')')))
        .getAsJsonObject();
  }

  @Test
  @DisplayName("Should keep the undo history under the home it is given")
  void shouldKeepHistoryUnderGivenHome() throws BBjException, IOException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      pageMock.when(Page::getCurrent).thenReturn(mock(Page.class));

      listener.onWillRun(mock(App.class));

      try (Stream<Path> journals = Files.list(home.resolve(".webforj/devtools/history"))) {
        assertEquals(1, journals.count());
      }
    }
  }

  @Test
  @DisplayName("Should register all actions and attach the tracker when licensed")
  void shouldRegisterAllActionsWhenLicensed() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);

      Router router = mock(Router.class);
      routerMock.when(Router::getCurrent).thenReturn(router);

      listener.onWillRun(mock(App.class));

      assertTrue(registry.unregister(GetComponentMapAction.ACTION));
      assertTrue(registry.unregister(GetCapabilitiesAction.ACTION));
      assertTrue(registry.unregister(GetAppInfoAction.ACTION));
      assertTrue(registry.unregister(RemoveComponentSourceAction.ACTION));
      verify(page, times(1)).addInlineJavaScript(anyString(), eq(true));
      verify(tracker).attach(router);
    }
  }

  @Test
  @DisplayName("Should not register the writing actions when the features are switched off")
  void shouldNotRegisterWritingActionsWhenFeaturesOff() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env, """
          webforj.devtools.craftforj {
            source-changes = false
            stylesheet-changes = false
          }
          """);
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);
      routerMock.when(Router::getCurrent).thenReturn(null);

      listener.onWillRun(mock(App.class));

      // A feature that is off cannot be reached from the browser at all
      assertFalse(registry.unregister(ApplyChangesAction.ACTION));
      assertFalse(registry.unregister(PreviewPatchAction.ACTION));
      assertFalse(registry.unregister(RemoveComponentSourceAction.ACTION));
      assertFalse(registry.unregister(StageSourceAction.ACTION));
      assertFalse(registry.unregister(WriteStylesheetAction.ACTION));
      assertFalse(registry.unregister(SetRouteSecurityAction.ACTION));

      // Reading stays available, since it writes nothing
      assertTrue(registry.unregister(GetSourceAction.ACTION));
      assertTrue(registry.unregister(ReadStylesheetAction.ACTION));
      assertTrue(registry.unregister(GetBeanInfoAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should not register the staging actions when the assistant is switched off")
  void shouldNotRegisterStagingActionsWhenAiOff() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env, "webforj.devtools.craftforj.ai.enabled = false");
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);
      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);
      routerMock.when(Router::getCurrent).thenReturn(null);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(StageSourceAction.ACTION));

      // The deterministic tooling is untouched by the assistant switch
      assertTrue(registry.unregister(ApplyChangesAction.ACTION));
      assertTrue(registry.unregister(WriteStylesheetAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should only register the capabilities action when unlicensed")
  void shouldOnlyRegisterCapabilitiesWhenUnlicensed() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);

      BBjAPI api = mock(BBjAPI.class);
      BBjException licenseError = mock(BBjException.class);
      when(licenseError.getHostErrorNumber()).thenReturn(999L);
      doThrow(licenseError).when(api).ensureCheckout();
      when(env.getBBjAPI()).thenReturn(api);
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(GetComponentMapAction.ACTION));
      assertFalse(registry.unregister(GetAppInfoAction.ACTION));
      assertTrue(registry.unregister(GetCapabilitiesAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should treat the already-checked-out license error as licensed")
  void shouldTreatAlreadyCheckedOutAsLicensed() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);

      BBjAPI api = mock(BBjAPI.class);
      BBjException alreadyCheckedOut = mock(BBjException.class);
      when(alreadyCheckedOut.getHostErrorNumber()).thenReturn(101L);
      doThrow(alreadyCheckedOut).when(api).ensureCheckout();
      when(env.getBBjAPI()).thenReturn(api);
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);
      routerMock.when(Router::getCurrent).thenReturn(null);

      listener.onWillRun(mock(App.class));

      assertTrue(registry.unregister(GetComponentMapAction.ACTION));
    }
  }

  @Test
  @DisplayName("Should not attach for a client that is neither loopback nor allowed")
  void shouldNotAttachForRemoteClient() {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);
      envMock.when(Environment::getCurrent).thenReturn(env);

      Request request = mock(Request.class);
      when(request.getIPAddress()).thenReturn("203.0.113.7");
      requestMock.when(Request::getCurrent).thenReturn(request);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);

      listener.onWillRun(mock(App.class));

      assertFalse(registry.unregister(GetCapabilitiesAction.ACTION));
      verify(page, never()).addInlineJavaScript(anyString(), eq(true));
    }
  }

  @Test
  @DisplayName("Should inject a boot script that carries this page's credentials")
  void shouldInjectBootScriptWithCredentials() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);

      BBjAPI api = mock(BBjAPI.class);
      BBjException licenseError = mock(BBjException.class);
      when(licenseError.getHostErrorNumber()).thenReturn(999L);
      doThrow(licenseError).when(api).ensureCheckout();
      when(env.getBBjAPI()).thenReturn(api);
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);

      listener.onWillRun(mock(App.class));

      ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
      verify(page).addInlineJavaScript(captor.capture(), eq(true));
      String script = captor.getValue();

      assertFalse(script.contains("__WDT_NONCE__"));
      assertFalse(script.contains("__WDT_SINK__"));
      assertFalse(script.contains("__WDT_MANIFEST__"));
      assertTrue(script.contains("__webforjDevToolsResponse_"));
      assertTrue(script.contains("webforj-devtools-ready"));
    }
  }

  @Test
  @DisplayName("Should name the client modules in the boot script when licensed")
  void shouldStampTheManifestWhenLicensed() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);
      when(env.getBBjAPI()).thenReturn(mock(BBjAPI.class));
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);
      Router router = mock(Router.class);
      routerMock.when(Router::getCurrent).thenReturn(router);

      listener.onWillRun(mock(App.class));

      ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
      verify(page).addInlineJavaScript(captor.capture(), eq(true));
      String script = captor.getValue();

      assertFalse(script.contains("__WDT_MANIFEST__"));
      assertTrue(script.contains("trigger:"));
      assertTrue(script.contains("ui:"));
    }
  }

  @Test
  @DisplayName("Should name no client module in the boot script when unlicensed")
  void shouldStampNoManifestWhenUnlicensed() throws BBjException {
    try (MockedStatic<Environment> envMock = mockStatic(Environment.class);
        MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class)) {
      Environment env = mock(Environment.class);
      when(env.isDebug()).thenReturn(true);
      enableDevTools(env);

      BBjAPI api = mock(BBjAPI.class);
      BBjException licenseError = mock(BBjException.class);
      when(licenseError.getHostErrorNumber()).thenReturn(999L);
      doThrow(licenseError).when(api).ensureCheckout();
      when(env.getBBjAPI()).thenReturn(api);
      envMock.when(Environment::getCurrent).thenReturn(env);
      allowLoopback(requestMock);

      Page page = mock(Page.class);
      pageMock.when(Page::getCurrent).thenReturn(page);

      listener.onWillRun(mock(App.class));

      ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
      verify(page).addInlineJavaScript(captor.capture(), eq(true));

      assertFalse(captor.getValue().contains("trigger:"));
    }
  }
}
