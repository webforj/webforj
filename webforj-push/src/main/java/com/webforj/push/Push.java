package com.webforj.push;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.environment.ObjectTable;
import com.webforj.push.exception.WebforjPushException;
import com.webforj.router.Router;
import com.webforj.utilities.Assets;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Provides an interface to the push subscription of the browser for the current environment.
 *
 * <p>
 * WebView on Android and WebView on iOS do not implement notifications, see the browser
 * compatibility table of <a href=
 * "https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification">showNotification</a>.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class Push {

  static final String ASSET_KEY = "webforj.push.asset-loaded";
  static final String SCRIPT_URL = "ws://webforj/push/push.min.js";
  static final String WORKER_URL = "ws://webforj/push/push-worker.min.js";
  private static final String STUB =
      "window.__webforjPush = window.__webforjPush || (function() { var q = []; return { _q: q,"
          + " call: function(r) { return new Promise(function(resolve) {"
          + " q.push({ request: r, resolve: resolve }); }); } }; })();";
  private static final String ERROR_PERMISSION_DENIED = "permission-denied";
  private static final String ERROR_UNSUPPORTED = "unsupported";
  private static final Gson GSON = new GsonBuilder().disableHtmlEscaping().create();

  private volatile boolean destroyed = false;

  private Push() {}

  /**
   * Returns the push instance for the current environment.
   *
   * @return the push instance
   */
  public static Push getCurrent() {
    String key = Push.class.getName();
    if (ObjectTable.contains(key)) {
      return (Push) ObjectTable.get(key);
    }

    Push instance = new Push();
    ObjectTable.put(key, instance);

    return instance;
  }

  /**
   * Returns whether a push instance is available for the current environment.
   *
   * @return {@code true} when a push instance is available
   */
  public static boolean isPresent() {
    return getCurrent() != null;
  }

  /**
   * Executes the given consumer with the push instance when one is available.
   *
   * @param consumer the consumer to execute
   */
  public static void ifPresent(Consumer<Push> consumer) {
    if (isPresent()) {
      consumer.accept(getCurrent());
    }
  }

  /**
   * Subscribes the browser to pushes from this deployment. The browser prompts the user for
   * permission when the user has not decided yet.
   *
   * @return the subscription, failing with a {@link WebforjPushException} whose status names the
   *         reason: {@link PushStatus#PERMISSION_DENIED}, {@link PushStatus#UNSUPPORTED} or
   *         {@link PushStatus#UNKNOWN}
   *
   * @throws WebforjPushException before the browser is called, with
   *         {@link PushStatus#NOT_CONFIGURED} when the deployment does not configure push and with
   *         {@link PushStatus#UNSUPPORTED} when the deployment cannot serve push
   */
  public PendingResult<PushSubscription> subscribe() {
    PushConfiguration configuration =
        PushConfiguration.require(Environment.getCurrent().getConfig());

    JsonObject request = toWorkerRequest("subscribe");
    request.addProperty("key", VapidKeyAdapter.toApplicationServerKey(configuration.getKeys()));

    return call(request).thenApply(Push::toSubscription);
  }

  /**
   * Cancels the subscription of the browser to pushes from this deployment.
   *
   * @return the cancelled subscription so the application can delete its stored copy, empty when
   *         the browser had none
   */
  public PendingResult<Optional<PushSubscription>> unsubscribe() {
    return call(toWorkerRequest("unsubscribe")).thenApply(Push::toOptionalSubscription);
  }

  /**
   * Returns the subscription the browser holds for this deployment, for instance to restore a copy
   * the application lost.
   *
   * @return the subscription, empty when the browser has none
   */
  public PendingResult<Optional<PushSubscription>> getSubscription() {
    return call(toWorkerRequest("getSubscription")).thenApply(Push::toOptionalSubscription);
  }

  /**
   * Returns the decision of the user on notifications from this deployment.
   *
   * @return the permission
   */
  public PendingResult<PushPermission> getPermission() {
    JsonObject request = new JsonObject();
    request.addProperty("command", "getPermission");

    return call(request).thenApply(
        value -> PushPermission.fromValue(value.isJsonNull() ? null : value.getAsString()));
  }

  void registerServiceWorker() {
    requireServletDeployment();
    Page page = Page.getCurrent();
    ensureBridge(page);
    page.executeJsVoidAsync(
        "window.__webforjPush.call(" + GSON.toJson(toWorkerRequest("register")) + ");");
  }

  void destroy() {
    destroyed = true;
    ObjectTable.put(Push.class.getName(), null);
  }

  boolean isDestroyed() {
    return destroyed;
  }

  private PendingResult<JsonElement> call(JsonObject request) {
    requireServletDeployment();
    Page page = Page.getCurrent();
    ensureBridge(page);

    return page.executeJsAsync("window.__webforjPush.call(" + GSON.toJson(request) + ")")
        .thenApply(Push::toValue);
  }

  private static void requireServletDeployment() {
    if (Environment.isRunningWithBBjServices()) {
      throw new WebforjPushException(PushStatus.UNSUPPORTED,
          "Push notifications are not supported in this deployment, they require a servlet"
              + " deployment of the application");
    }
  }

  private void ensureBridge(Page page) {
    if (ObjectTable.contains(ASSET_KEY)) {
      return;
    }

    page.executeJsVoidAsync(STUB);
    page.addJavaScript(SCRIPT_URL, true);
    ObjectTable.put(ASSET_KEY, true);
  }

  private static JsonElement toValue(Object result) {
    if (result == null || String.valueOf(result).isBlank()) {
      throw new WebforjPushException(PushStatus.UNKNOWN, "The browser returned no answer");
    }

    JsonElement parsed;
    try {
      parsed = JsonParser.parseString(String.valueOf(result));
    } catch (JsonSyntaxException e) {
      throw new WebforjPushException(PushStatus.UNKNOWN, "The browser returned no answer", e);
    }

    if (!parsed.isJsonObject()) {
      throw new WebforjPushException(PushStatus.UNKNOWN, "The browser returned no answer");
    }

    JsonObject json = parsed.getAsJsonObject();
    if (json.has("ok") && json.get("ok").getAsBoolean()) {
      return json.has("value") ? json.get("value") : JsonNull.INSTANCE;
    }

    String error = json.has("error") ? json.get("error").getAsString() : "";
    String message = json.has("message") ? json.get("message").getAsString() : error;
    PushStatus status = PushStatus.UNKNOWN;

    if (ERROR_PERMISSION_DENIED.equals(error)) {
      status = PushStatus.PERMISSION_DENIED;
    } else if (ERROR_UNSUPPORTED.equals(error)) {
      status = PushStatus.UNSUPPORTED;
    }

    throw new WebforjPushException(status, message);
  }

  private static JsonObject toWorkerRequest(String command) {
    String worker = Assets.resolveWebServerUrl(WORKER_URL);
    String scope = worker.substring(0, worker.lastIndexOf('/') + 1);

    JsonObject request = new JsonObject();
    request.addProperty("command", command);
    request.addProperty("worker", worker + "?" + getWorkerQuery());
    request.addProperty("scope", scope);

    return request;
  }

  private static String getWorkerQuery() {
    StringBuilder query = new StringBuilder("root=")
        .append(URLEncoder.encode(getApplicationRoot(), StandardCharsets.UTF_8));
    query.append("&icons=")
        .append(URLEncoder.encode(Assets.getIconsEndpoint(), StandardCharsets.UTF_8));

    query.append("&v=").append(URLEncoder.encode(PushVersion.get(), StandardCharsets.UTF_8));

    return query.toString();
  }

  private static String getApplicationRoot() {
    Router router = Router.getCurrent();
    if (router != null && router.getRoot().isPresent()) {
      return router.getRoot().get();
    }

    String key = "webforj.router.root";
    var config = Environment.getCurrent().getConfig();
    if (config.hasPath(key) && !config.getIsNull(key) && !config.getString(key).isBlank()) {
      return config.getString(key);
    }

    return Environment.getContextPath();
  }

  private static PushSubscription toSubscription(JsonElement value) {
    if (value == null || !value.isJsonObject()) {
      throw new WebforjPushException(PushStatus.UNKNOWN, "The browser returned no subscription");
    }

    JsonObject json = value.getAsJsonObject();
    return new PushSubscription(toText(json, "endpoint"), toText(json, "p256dh"),
        toText(json, "auth"));
  }

  private static Optional<PushSubscription> toOptionalSubscription(JsonElement value) {
    if (value == null || value.isJsonNull()) {
      return Optional.empty();
    }

    return Optional.of(toSubscription(value));
  }

  private static String toText(JsonObject json, String name) {
    JsonElement element = json.get(name);
    return element == null || element.isJsonNull() ? null : element.getAsString();
  }
}
