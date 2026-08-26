package com.webforj.push;

import com.interaso.webpush.VapidKeys;
import com.interaso.webpush.WebPush;
import com.interaso.webpush.WebPush.SubscriptionState;
import com.interaso.webpush.WebPush.Urgency;
import com.interaso.webpush.WebPushStatusException;
import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.push.exception.WebforjPushException;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Base64;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.function.BiConsumer;

/**
 * Delivers messages to subscribed browsers without blocking the calling thread. Every sender shares
 * one connection pool, creating one wherever it is needed costs nothing.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushSender {

  /**
   * The time a send waits for the push service to answer before it fails with
   * {@link PushStatus#UNREACHABLE}.
   */
  public static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);

  /**
   * The longest time a push service keeps a message for a device that is not reachable, longer
   * values are cut to it.
   */
  public static final Duration MAX_TIME_TO_LIVE = Duration.ofDays(28);

  private static final Logger LOGGER = System.getLogger(PushSender.class.getName());
  private static final Duration CONNECT_TIMEOUT = Duration.ofSeconds(10);

  private final PushConfiguration configuration;
  private final WebPush webPush;
  private final BiConsumer<Environment, Runnable> dispatcher;
  private volatile Duration timeout = DEFAULT_TIMEOUT;

  /**
   * Creates a sender from the configuration of the current environment.
   *
   * @throws WebforjPushException with {@link PushStatus#NOT_CONFIGURED} when the current thread has
   *         no environment or the {@code webforj.push} keys are missing or incomplete
   */
  public PushSender() {
    this(currentConfiguration());
  }

  /**
   * Creates a sender.
   *
   * @param keys the key pair of the deployment
   * @param subject the contact of the deployment, a {@code mailto:} or {@code https://} address
   *
   * @throws WebforjPushException when the keys or the subject are not valid
   */
  public PushSender(PushKeys keys, String subject) {
    this(new PushConfiguration(keys, subject));
  }

  /**
   * Creates a sender from the given configuration.
   *
   * @param configuration the push configuration of the deployment
   *
   * @throws WebforjPushException when the keys are not valid
   */
  public PushSender(PushConfiguration configuration) {
    this(configuration, PushSender::dispatch);
  }

  PushSender(PushConfiguration configuration, BiConsumer<Environment, Runnable> dispatcher) {
    this.configuration = Objects.requireNonNull(configuration, "The configuration is required");
    this.dispatcher = dispatcher;

    VapidKeys vapidKeys = VapidKeyAdapter.toVapidKeys(configuration.getKeys());
    this.webPush = new WebPush(configuration.getSubject(), vapidKeys);
  }

  /**
   * Returns the key pair of the deployment.
   *
   * @return the keys
   */
  public PushKeys getKeys() {
    return configuration.getKeys();
  }

  /**
   * Returns the contact of the deployment.
   *
   * @return the subject
   */
  public String getSubject() {
    return configuration.getSubject();
  }

  /**
   * Sets the time a send waits for the push service to answer.
   *
   * @param timeout the timeout, {@link #DEFAULT_TIMEOUT} by default
   * @return this sender
   */
  public PushSender setTimeout(Duration timeout) {
    this.timeout = Objects.requireNonNull(timeout, "The timeout is required");
    return this;
  }

  /**
   * Returns the time a send waits for the push service to answer.
   *
   * @return the timeout
   */
  public Duration getTimeout() {
    return timeout;
  }

  /**
   * Delivers the message to the push service of the subscription. The call returns at once, the
   * result completes when the push service answers. When called on an application thread the result
   * completes on that thread, so its callbacks may touch components, and it is dropped without
   * completing when that session ends before the answer arrives.
   *
   * @param subscription the browser to reach
   * @param message the message to deliver
   * @return the result, completed when the push service accepted the message, or exceptionally with
   *         a {@link WebforjPushException} carrying {@link PushStatus#SUBSCRIPTION_EXPIRED} when
   *         the push service no longer knows the subscription and the application should delete it,
   *         {@link PushStatus#REJECTED} and the answered status code when the push service refuses
   *         the message, {@link PushStatus#UNREACHABLE} when no answer arrived within the timeout,
   *         and {@link PushStatus#UNKNOWN} for any other failure, such as a subscription or a
   *         message that cannot be encoded
   */
  public PendingResult<Void> send(PushSubscription subscription, PushMessage message) {
    Objects.requireNonNull(subscription, "The subscription is required");
    Objects.requireNonNull(message, "The message is required");

    Environment environment = Environment.getCurrent();
    Push owner = environment == null ? null : Push.getCurrent();
    PendingResult<Void> result = new PendingResult<>();
    Duration deadline = timeout;

    CompletableFuture
        .supplyAsync(() -> toRequest(subscription, message, deadline), Transport.EXECUTOR)
        .thenCompose(request -> Transport.CLIENT.sendAsync(request, BodyHandlers.ofString()))
        .whenComplete((response, failure) -> {
          WebforjPushException error = failure == null ? toFailure(subscription, response)
              : toException(subscription, unwrap(failure));
          complete(environment, owner, result, error);
        });

    return result;
  }

  /**
   * Runs the completion in the given environment, or directly when there is none. A completion the
   * environment cannot take any more is dropped.
   *
   * @param environment the environment the send was called in, {@code null} for none
   * @param completion the completion to run
   */
  static void dispatch(Environment environment, Runnable completion) {
    if (environment == null) {
      completion.run();
      return;
    }

    Environment.runLater(environment, completion).exceptionally(e -> {
      LOGGER.log(Level.DEBUG, "The environment is gone, the completion is dropped", e);
      return null;
    });
  }

  static HttpClient transport() {
    return Transport.CLIENT;
  }

  private HttpRequest toRequest(PushSubscription subscription, PushMessage message,
      Duration deadline) {
    Integer ttl = message.getTimeToLive() == null ? null
        : (int) Math.clamp(message.getTimeToLive().toSeconds(), 0, MAX_TIME_TO_LIVE.toSeconds());
    Urgency urgency = toUrgency(message.getUrgency());

    byte[] body = webPush.getBody(message.toPayload().getBytes(StandardCharsets.UTF_8),
        decode(subscription.getP256dh()), decode(subscription.getAuth()));
    Map<String, String> headers =
        webPush.getHeaders(subscription.getEndpoint(), ttl, message.getTopic(), urgency);

    HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create(subscription.getEndpoint()))
        .timeout(deadline).POST(HttpRequest.BodyPublishers.ofByteArray(body));
    headers.forEach(builder::header);

    return builder.build();
  }

  private WebforjPushException toFailure(PushSubscription subscription,
      HttpResponse<String> response) {
    SubscriptionState state;
    try {
      state = webPush.getSubscriptionState(response.statusCode(), response.body());
    } catch (Exception e) {
      return toException(subscription, e);
    }

    if (state == SubscriptionState.EXPIRED) {
      return new WebforjPushException(PushStatus.SUBSCRIPTION_EXPIRED,
          "The push subscription for " + subscription.getEndpoint() + " expired");
    }

    return null;
  }

  private void complete(Environment environment, Push owner, PendingResult<Void> result,
      WebforjPushException error) {
    if (isGone(owner)) {
      LOGGER.log(Level.DEBUG, "The session ended before the push service answered, dropping");
      return;
    }

    dispatcher.accept(environment, () -> {
      if (isGone(owner)) {
        return;
      }

      if (error == null) {
        result.complete(null);
      } else {
        result.completeExceptionally(error);
      }
    });
  }

  private static boolean isGone(Push owner) {
    return owner != null && owner.isDestroyed();
  }

  private static Throwable unwrap(Throwable failure) {
    return failure instanceof CompletionException && failure.getCause() != null ? failure.getCause()
        : failure;
  }

  private static WebforjPushException toException(PushSubscription subscription, Throwable cause) {
    if (cause instanceof WebPushStatusException status) {
      return new WebforjPushException(PushStatus.REJECTED, status.getStatusCode(),
          "The push service answered " + status.getStatusCode() + " for "
              + subscription.getEndpoint(),
          cause);
    }

    if (cause instanceof IOException) {
      return new WebforjPushException(PushStatus.UNREACHABLE,
          "Could not reach the push service for " + subscription.getEndpoint(), cause);
    }

    return new WebforjPushException(PushStatus.UNKNOWN,
        "Could not deliver the message to " + subscription.getEndpoint(), cause);
  }

  private static byte[] decode(String value) {
    return Base64.getUrlDecoder().decode(value);
  }

  private static PushConfiguration currentConfiguration() {
    Environment environment = Environment.getCurrent();
    if (environment == null) {
      throw new WebforjPushException(PushStatus.NOT_CONFIGURED,
          "No application environment on this thread. Create the PushSender from an application"
              + " thread, or pass a PushConfiguration");
    }

    return PushConfiguration.require(environment.getConfig());
  }

  private static Urgency toUrgency(PushUrgency urgency) {
    if (urgency == null) {
      return null;
    }

    return switch (urgency) {
      case VERY_LOW -> Urgency.VeryLow;
      case LOW -> Urgency.Low;
      case NORMAL -> Urgency.Normal;
      case HIGH -> Urgency.High;
    };
  }

  private static final class Transport {
    static final HttpClient CLIENT =
        HttpClient.newBuilder().connectTimeout(CONNECT_TIMEOUT).build();
    static final Executor EXECUTOR = Executors.newVirtualThreadPerTaskExecutor();

    private Transport() {}
  }
}
