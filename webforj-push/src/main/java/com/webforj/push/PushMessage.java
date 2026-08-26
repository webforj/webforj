package com.webforj.push;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import java.io.Serializable;
import java.time.Duration;
import java.util.Collections;
import java.util.List;

/**
 * A notification to deliver to a subscribed browser.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushMessage implements Serializable {

  private static final Gson GSON = new Gson();

  private final String title;
  private final String body;
  private final String icon;
  private final String tag;
  private final String url;
  private final List<PushAction> actions;
  private final boolean silent;
  private final Duration timeToLive;
  private final PushUrgency urgency;
  private final String topic;

  private PushMessage(Builder builder) {
    this.title = builder.title;
    this.body = builder.body;
    this.icon = builder.icon;
    this.tag = builder.tag;
    this.url = builder.url;
    this.actions = builder.actions;
    this.silent = builder.silent;
    this.timeToLive = builder.timeToLive;
    this.urgency = builder.urgency;
    this.topic = builder.topic;
  }

  /**
   * Starts a message with the given title.
   *
   * @param title a title for the notification, which is shown at the top of the notification window
   * @return the builder
   *
   * @throws IllegalArgumentException when the title is null or blank
   */
  public static Builder create(String title) {
    return new Builder(title);
  }

  /**
   * Returns the title of the notification, which is shown at the top of the notification window.
   *
   * @return the title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Returns the body text of the notification, which is displayed below the title.
   *
   * @return the body text, the default is the empty string
   */
  public String getBody() {
    return body;
  }

  /**
   * Returns the URL of an icon to be displayed in the notification.
   *
   * @return the icon URL
   */
  public String getIcon() {
    return icon;
  }

  /**
   * Returns the identifying tag for the notification.
   *
   * @return the tag, the default is the empty string
   */
  public String getTag() {
    return tag;
  }

  /**
   * Returns the URL a click on the notification opens.
   *
   * @return the URL, {@code null} when a click opens the application root
   */
  public String getUrl() {
    return url;
  }

  /**
   * Returns the actions to display in the notification.
   *
   * @return an unmodifiable list of the actions, the default is an empty list
   */
  public List<PushAction> getActions() {
    return actions;
  }

  /**
   * Returns whether the notification is silent (no sounds or vibrations issued), regardless of the
   * device settings.
   *
   * @return {@code true} when the notification is silent
   */
  public boolean isSilent() {
    return silent;
  }

  /**
   * Returns how long the push service keeps the message for a device that is not reachable.
   *
   * @return the time to live, {@code null} for the push service default of four weeks
   */
  public Duration getTimeToLive() {
    return timeToLive;
  }

  /**
   * Returns the urgency of the message.
   *
   * @return the urgency, {@code null} for the push service default
   */
  public PushUrgency getUrgency() {
    return urgency;
  }

  /**
   * Returns the topic of the message.
   *
   * @return the topic
   */
  public String getTopic() {
    return topic;
  }

  /**
   * Renders the message as the JSON the worker of the browser shows.
   *
   * @return the payload
   */
  String toPayload() {
    JsonObject json = new JsonObject();
    json.addProperty("title", title);
    addIfSet(json, "body", body);
    addIfSet(json, "icon", icon);
    addIfSet(json, "tag", tag);
    addIfSet(json, "url", url);

    if (!actions.isEmpty()) {
      JsonArray array = new JsonArray();
      for (PushAction action : actions) {
        JsonObject item = new JsonObject();
        item.addProperty("action", action.getAction());
        item.addProperty("title", action.getTitle());
        addIfSet(item, "url", action.getUrl());
        array.add(item);
      }
      json.add("actions", array);
    }

    if (silent) {
      json.addProperty("silent", true);
    }

    return GSON.toJson(json);
  }

  private static void addIfSet(JsonObject json, String name, String value) {
    if (value != null && !value.isBlank()) {
      json.addProperty(name, value);
    }
  }

  /**
   * Builds a {@link PushMessage}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private final String title;
    private String body;
    private String icon;
    private String tag;
    private String url;
    private List<PushAction> actions = Collections.emptyList();
    private boolean silent = false;
    private Duration timeToLive;
    private PushUrgency urgency;
    private String topic;

    private Builder(String title) {
      if (title == null || title.isBlank()) {
        throw new IllegalArgumentException("The message title is required");
      }

      this.title = title;
    }

    /**
     * Sets the body text of the notification, which is displayed below the title. The default is
     * the empty string.
     *
     * @param body the body text
     * @return this builder
     */
    public Builder setBody(String body) {
      this.body = body;
      return this;
    }

    /**
     * Sets the URL of an icon to be displayed in the notification.
     *
     * @param icon the icon URL, resolved against the application root when relative
     * @return this builder
     *
     * @throws IllegalArgumentException when the URL uses the {@code context://} protocol
     */
    public Builder setIcon(String icon) {
      if (icon != null && icon.toLowerCase().startsWith("context://")) {
        throw new IllegalArgumentException("The context:// protocol embeds the file into the"
            + " message and push services cap a message at 4 KB. Use icons://, ws:// or a URL"
            + " instead");
      }

      this.icon = icon;
      return this;
    }

    /**
     * Sets an identifying tag for the notification. The default is the empty string.
     *
     * @param tag the tag
     * @return this builder
     */
    public Builder setTag(String tag) {
      this.tag = tag;
      return this;
    }

    /**
     * Sets the URL a click on the notification opens.
     *
     * @param url the URL, resolved against the application root when relative
     * @return this builder
     */
    public Builder setUrl(String url) {
      this.url = url;
      return this;
    }

    /**
     * Sets the actions to display in the notification, for which the default is an empty list.
     *
     * <p>
     * Safari does not implement notification actions and shows the notification without them, see
     * the browser compatibility table of <a href=
     * "https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification#actions">showNotification</a>.
     * </p>
     *
     * @param actions the actions, in display order
     * @return this builder
     */
    public Builder setActions(List<PushAction> actions) {
      this.actions = actions == null ? Collections.emptyList() : List.copyOf(actions);
      return this;
    }

    /**
     * Sets whether the notification is silent (no sounds or vibrations issued), regardless of the
     * device settings. The default is to respect device defaults.
     *
     * @param silent {@code true} for a silent notification
     * @return this builder
     */
    public Builder setSilent(boolean silent) {
      this.silent = silent;
      return this;
    }

    /**
     * Sets how long the push service keeps the message for a device that is not reachable.
     *
     * @param timeToLive the time to live, at most four weeks
     * @return this builder
     */
    public Builder setTimeToLive(Duration timeToLive) {
      this.timeToLive = timeToLive;
      return this;
    }

    /**
     * Sets the urgency of the message.
     *
     * @param urgency the urgency
     * @return this builder
     */
    public Builder setUrgency(PushUrgency urgency) {
      this.urgency = urgency;
      return this;
    }

    /**
     * Sets the topic of the message. A message with the topic of one still waiting at the push
     * service replaces it.
     *
     * @param topic the topic, at most 32 URL safe characters
     * @return this builder
     */
    public Builder setTopic(String topic) {
      this.topic = topic;
      return this;
    }

    /**
     * Builds the message.
     *
     * @return the message
     */
    public PushMessage build() {
      return new PushMessage(this);
    }
  }
}
