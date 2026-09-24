package com.webforj.matchmedia;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.ObjectTable;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.matchmedia.event.MediaQueryChangeEvent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.concurrent.CompletionException;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

class MediaQueryTest {

  private static final Gson GSON = new Gson();
  private static final String QUERY = "(max-width: 700px)";
  private final Map<String, Object> table = new HashMap<>();
  private final Map<MediaQueryList, String> queryIds = new HashMap<>();
  private final List<Call> calls = new ArrayList<>();
  private final List<JsonObject> commands = new ArrayList<>();
  private final List<String> scripts = new ArrayList<>();
  private final List<EventListener<PageEvent>> pageListeners = new ArrayList<>();
  private MockedStatic<ObjectTable> objectTable;
  private MockedStatic<Page> pages;
  private MockedStatic<Environment> environments;
  private Page page;
  private ListenerRegistration<PageEvent> pageRegistration;

  @BeforeEach
  @SuppressWarnings("unchecked")
  void setUp() {
    objectTable = mockStatic(ObjectTable.class);
    objectTable.when(() -> ObjectTable.contains(anyString()))
        .thenAnswer(invocation -> table.get(invocation.getArgument(0)) != null);
    objectTable.when(() -> ObjectTable.get(anyString()))
        .thenAnswer(invocation -> table.get(invocation.getArgument(0)));
    objectTable.when(() -> ObjectTable.put(anyString(), any())).thenAnswer(invocation -> {
      table.put(invocation.getArgument(0), invocation.getArgument(1));
      return invocation.getArgument(1);
    });
    page = mock(Page.class);
    pageRegistration = mock(ListenerRegistration.class);
    when(page.addEventListener(eq(MediaQueryBridge.EVENT_NAME), any(), any(PageEventOptions.class)))
        .thenAnswer(invocation -> {
          pageListeners.add(invocation.getArgument(1));
          assertEquals("JSON.stringify(event.detail)",
              ((PageEventOptions) invocation.getArgument(2)).getDataMap().get("payload"));
          return pageRegistration;
        });
    when(page.executeJsAsync(anyString())).thenAnswer(invocation -> {
      String script = invocation.getArgument(0);
      scripts.add(script);
      Call call = new Call(requestOf(script), new PendingResult<>());
      calls.add(call);
      return call.answer();
    });
    doAnswer(invocation -> {
      commands.add(requestOf(invocation.getArgument(0)));
      return null;
    }).when(page).executeJsVoidAsync(anyString());
    pages = mockStatic(Page.class);
    pages.when(Page::getCurrent).thenReturn(page);
    environments = mockStatic(Environment.class);
    environments.when(Environment::isPresent).thenReturn(true);
    environments.when(Environment::getCurrent).thenReturn(mock(Environment.class));
  }

  @AfterEach
  void tearDown() {
    environments.close();
    pages.close();
    objectTable.close();
  }

  @Test
  void presenceAndTerminationDoNotInitializeAnUnusedService() {
    assertFalse(MediaQuery.isPresent());
    MediaQuery.ifPresent(service -> {
      throw new AssertionError("Should not initialize a service");
    });
    new MediaQueryLifecycleListener().onWillTerminate(null);
    assertFalse(MediaQuery.isPresent());
    verifyNoInteractions(page);

    assertSame(MediaQuery.getCurrent(), MediaQuery.getCurrent());
    assertTrue(MediaQuery.isPresent());
    verifyNoInteractions(page);
  }

  @Test
  void createsIndependentQueriesAndInitializesTheBridgeOnce() {
    MediaQueryList first = create(QUERY);
    MediaQueryList second = create(QUERY);

    assertNotSame(first, second);
    assertNotEquals(queryIds.get(first), queryIds.get(second));
    assertEquals(QUERY, first.getMedia());
    assertEquals(1, pageListeners.size());
    assertEquals(1,
        scripts.stream().filter(script -> script.contains("const queries = new Map()")).count());
    assertEquals(Boolean.TRUE, table.get(MediaQueryBridge.ASSET_KEY));
  }

  @Test
  void createsOnlyAfterTheBrowserRespondsAndUsesItsSerializedMedia() {
    PendingResult<MediaQueryList> result = MediaQuery.getCurrent().matchMedia("(width>=700px)");
    assertFalse(result.isDone());

    answer(lastCall(), Map.of("media", "(width >= 700px)"));

    assertEquals("(width >= 700px)", valueOf(result).getMedia());
  }

  @Test
  void serializesInputAndLeavesQueryParsingToTheBrowser() {
    String input = "screen and (width > 1px)\"\\\n'";
    MediaQuery.getCurrent().matchMedia(input);
    assertEquals(input, lastCall().request().get("query").getAsString());
    MediaQuery.getCurrent().matchMedia("");
    assertEquals("", lastCall().request().get("query").getAsString());
    assertThrows(NullPointerException.class, () -> MediaQuery.getCurrent().matchMedia(null));
  }

  @Test
  void readsFreshValuesAndCorrelatesOutOfOrderResponses() {
    MediaQueryList query = create(QUERY);
    PendingResult<Boolean> first = query.getMatches();
    Call firstCall = lastCall();
    PendingResult<Boolean> second = query.getMatches();

    answer(lastCall(), false);
    assertEquals(false, valueOf(second));
    assertFalse(first.isDone());
    answer(firstCall, true);
    assertEquals(true, valueOf(first));
  }

  @Test
  void defaultListenersReceiveChangesWithoutAnInitialNotification() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> events = new ArrayList<>();
    MediaQueryListenerRegistration registration = query.onChange(events::add);
    String listenerId = listenerId();

    assertFalse(lastCall().request().get("initial").getAsBoolean());
    assertFalse(registration.whenReady().isDone());
    answer(lastCall(), null);
    assertTrue(registration.whenReady().isDone());
    assertTrue(events.isEmpty());

    notify(query, List.of(listenerId), true, false);
    assertEquals(1, events.size());
    assertSame(query, events.getFirst().getSource());
    assertEquals(QUERY, events.getFirst().getMedia());
    assertTrue(events.getFirst().isMatched());
    assertFalse(events.getFirst().isInitial());
  }

  @Test
  void initialNotificationTargetsOnlyTheRequestingListenerAndPreservesFalse() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> first = new ArrayList<>();
    query.onChange(first::add);
    String firstId = listenerId();
    List<MediaQueryChangeEvent> second = new ArrayList<>();
    query.onChange(second::add, true);
    String secondId = listenerId();

    assertTrue(lastCall().request().get("initial").getAsBoolean());
    notify(query, List.of(secondId), false, true);

    assertTrue(first.isEmpty());
    assertEquals(1, second.size());
    assertTrue(second.getFirst().isInitial());
    assertFalse(second.getFirst().isMatched());

    notify(query, List.of(firstId, secondId), true, false);
    assertEquals(1, first.size());
    assertEquals(2, second.size());
  }

  @Test
  void eventsRemainIsolatedBetweenQueries() {
    MediaQueryList first = create(QUERY);
    MediaQueryList second = create("(prefers-color-scheme: dark)");
    List<MediaQueryChangeEvent> firstEvents = new ArrayList<>();
    List<MediaQueryChangeEvent> secondEvents = new ArrayList<>();
    first.onChange(firstEvents::add);
    String firstId = listenerId();
    second.onChange(secondEvents::add);
    String secondId = listenerId();

    notify(first, List.of(firstId), true, false);
    assertEquals(1, firstEvents.size());
    assertTrue(secondEvents.isEmpty());
    first.destroy();
    notify(second, List.of(secondId), false, false);
    assertEquals(1, secondEvents.size());
  }

  @Test
  void duplicateListenerObjectsHaveIndependentRegistrations() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> events = new ArrayList<>();
    EventListener<MediaQueryChangeEvent> listener = events::add;
    MediaQueryListenerRegistration first = query.onChange(listener);
    String firstId = listenerId();
    query.onChange(listener);
    String secondId = listenerId();

    notify(query, List.of(firstId, secondId), true, false);
    assertEquals(2, events.size());
    first.remove();
    notify(query, List.of(firstId, secondId), false, false);
    assertEquals(3, events.size());
  }

  @Test
  void removalIsIdempotentAndSuppressesQueuedInitialEvents() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> events = new ArrayList<>();
    MediaQueryListenerRegistration old = query.onChange(events::add, true);
    String oldId = listenerId();
    old.remove();
    assertInstanceOf(IllegalStateException.class, failureOf(old.whenReady()));
    query.onChange(events::add);
    String newId = listenerId();
    old.remove();

    notify(query, List.of(oldId), false, true);
    assertTrue(events.isEmpty());
    notify(query, List.of(newId), true, false);
    assertEquals(1, events.size());
    assertEquals(1, commands.stream()
        .filter(request -> request.get("command").getAsString().equals("unsubscribe")).count());
  }

  @Test
  void newlyAddedListenersDoNotReceiveAnOlderQueuedChange() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> first = new ArrayList<>();
    query.onChange(first::add);
    String firstId = listenerId();
    List<MediaQueryChangeEvent> second = new ArrayList<>();
    query.onChange(second::add);

    notify(query, List.of(firstId), true, false);

    assertEquals(1, first.size());
    assertTrue(second.isEmpty());
  }

  @Test
  void readinessReportsBrowserFailureAndRemovesTheFailedListener() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> events = new ArrayList<>();
    MediaQueryListenerRegistration registration = query.onChange(events::add, true);
    String listenerId = listenerId();
    lastCall().answer().complete("{\"ok\":false,\"message\":\"Cannot observe this query\"}");

    assertEquals("Cannot observe this query", failureOf(registration.whenReady()).getMessage());
    notify(query, List.of(listenerId), true, false);
    assertTrue(events.isEmpty());
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = {"", "not json", "[]", "null", "{}", "{\"ok\":\"true\"}",
      "{\"ok\":true,\"value\":{}}"})
  void malformedCreationResponsesFailAndReleaseTheQuery(String response) {
    PendingResult<MediaQueryList> result = MediaQuery.getCurrent().matchMedia(QUERY);
    lastCall().answer().complete(response);

    assertInstanceOf(WebforjRuntimeException.class, failureOf(result));
    assertEquals("destroy", commands.getLast().get("command").getAsString());
  }

  @Test
  void malformedMatchResultsDoNotBecomeFalse() {
    MediaQueryList query = create(QUERY);
    PendingResult<Boolean> result = query.getMatches();
    answer(lastCall(), "false");
    assertInstanceOf(WebforjRuntimeException.class, failureOf(result));
  }

  @Test
  void transportFailuresCompleteCreationExceptionally() {
    doThrow(new IllegalStateException("Disconnected")).when(page).executeJsAsync(anyString());
    PendingResult<MediaQueryList> result = MediaQuery.getCurrent().matchMedia(QUERY);
    assertEquals("Disconnected", failureOf(result).getMessage());
  }

  @Test
  void destroyingAQueryCancelsOutstandingRequestsAndIgnoresLateEvents() {
    MediaQueryList query = create(QUERY);
    List<MediaQueryChangeEvent> events = new ArrayList<>();
    MediaQueryListenerRegistration registration = query.onChange(events::add, true);
    String listenerId = listenerId();
    PendingResult<Boolean> result = query.getMatches();
    Call outstanding = lastCall();

    query.destroy();
    query.destroy();
    registration.remove();
    assertInstanceOf(IllegalStateException.class, failureOf(result));
    assertInstanceOf(IllegalStateException.class, failureOf(registration.whenReady()));
    answer(outstanding, true);
    notify(query, List.of(listenerId), true, false);
    assertTrue(events.isEmpty());
    assertThrows(IllegalStateException.class, query::getMatches);
    assertThrows(IllegalStateException.class, () -> query.onChange(events::add));
    assertEquals(1, commands.size());
    assertEquals("destroy", commands.getFirst().get("command").getAsString());
  }

  @Test
  void terminationCancelsCreationAndReleasesThePageListener() {
    MediaQuery service = MediaQuery.getCurrent();
    PendingResult<MediaQueryList> result = service.matchMedia(QUERY);
    Call pending = lastCall();

    new MediaQueryLifecycleListener().onWillTerminate(null);
    service.destroy();
    assertFalse(MediaQuery.isPresent());
    assertInstanceOf(IllegalStateException.class, failureOf(result));
    assertEquals("destroyAll", commands.getLast().get("command").getAsString());
    assertEquals(pending.request().get("id"), commands.getLast().getAsJsonArray("ids").get(0));
    verify(pageRegistration, times(1)).remove();
    answer(pending, Map.of("media", QUERY));
    assertFalse(MediaQuery.isPresent());
    assertThrows(IllegalStateException.class, () -> service.matchMedia(QUERY));
  }

  @Test
  void cancellationOfCreationReleasesItsBrowserResources() {
    PendingResult<MediaQueryList> result = MediaQuery.getCurrent().matchMedia(QUERY);
    Call pending = lastCall();
    result.cancel();
    answer(pending, Map.of("media", QUERY));
    assertTrue(result.isCancelled());
    assertEquals("destroy", commands.getLast().get("command").getAsString());
  }

  @Test
  void recreatedServiceReusesTheLoadedBridgeAndSurvivesOldDestroyCalls() {
    MediaQuery first = MediaQuery.getCurrent();
    create(QUERY);
    first.destroy();
    MediaQuery second = MediaQuery.getCurrent();
    create(QUERY);
    first.destroy();

    assertNotSame(first, second);
    assertSame(second, MediaQuery.getCurrent());
    assertEquals(2, pageListeners.size());
    assertFalse(scripts.getLast().contains("const queries = new Map()"));
  }

  @Test
  void shutdownReleasesServerResourcesEvenWhenBrowserCleanupFails() {
    MediaQueryList query = create(QUERY);
    doThrow(new IllegalStateException("Disconnected")).when(page).executeJsVoidAsync(anyString());

    assertThrows(IllegalStateException.class, () -> MediaQuery.getCurrent().destroy());
    assertFalse(MediaQuery.isPresent());
    verify(pageRegistration).remove();
    assertThrows(IllegalStateException.class, query::getMatches);
  }

  @Test
  void lifecycleListenerIsDiscoverable() {
    assertTrue(ServiceLoader.load(AppLifecycleListener.class).stream()
        .anyMatch(provider -> provider.type() == MediaQueryLifecycleListener.class));
  }

  private MediaQueryList create(String query) {
    PendingResult<MediaQueryList> result = MediaQuery.getCurrent().matchMedia(query);
    Call call = lastCall();
    answer(call, Map.of("media", query));
    MediaQueryList list = valueOf(result);
    queryIds.put(list, call.request().get("id").getAsString());
    return list;
  }

  private void notify(MediaQueryList query, List<String> listeners, boolean matches,
      boolean initial) {
    String payload = GSON.toJson(Map.of("id", queryIds.get(query), "media", query.getMedia(),
        "listeners", listeners, "matches", matches, "initial", initial));
    pageListeners.getLast().onEvent(
        new PageEvent(page, Map.of("payload", payload), MediaQueryBridge.EVENT_NAME, 1, null));
  }

  private Call lastCall() {
    return calls.getLast();
  }

  private String listenerId() {
    return lastCall().request().get("listener").getAsString();
  }

  private static JsonObject requestOf(String script) {
    String prefix = "window.__webforjMatchMedia.call(";
    return JsonParser
        .parseString(
            script.substring(script.lastIndexOf(prefix) + prefix.length(), script.length() - 1))
        .getAsJsonObject();
  }

  private static void answer(Call call, Object value) {
    JsonObject envelope = new JsonObject();
    envelope.addProperty("ok", true);
    envelope.add("value", GSON.toJsonTree(value));
    call.answer().complete(envelope.toString());
  }

  private static <T> T valueOf(PendingResult<T> result) {
    assertTrue(result.isDone());
    assertFalse(result.isCompletedExceptionally());
    AtomicReference<T> value = new AtomicReference<>();
    result.thenAccept(value::set);
    return value.get();
  }

  private static Throwable failureOf(PendingResult<?> result) {
    AtomicReference<Throwable> failure = new AtomicReference<>();
    result.exceptionally(error -> {
      while (error instanceof CompletionException && error.getCause() != null) {
        error = error.getCause();
      }
      failure.set(error);
      return null;
    });
    assertNotNull(failure.get(), "Expected the pending result to fail");
    return failure.get();
  }

  private record Call(JsonObject request, PendingResult<Object> answer) {}
}
