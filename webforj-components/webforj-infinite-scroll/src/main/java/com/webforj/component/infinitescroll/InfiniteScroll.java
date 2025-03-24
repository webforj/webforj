package com.webforj.component.infinitescroll;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.component.icons.Icon;
import com.webforj.component.infinitescroll.event.InfiniteScrollEvent;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.utilities.Assets;

/**
 * A component to load content continuously as the user scrolls down, eliminating the need for
 * pagination.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@NodeName("dwc-infinite-scroll")
public class InfiniteScroll extends ElementCompositeContainer
    implements HasText<InfiniteScroll>, HasHtml<InfiniteScroll>, HasClassName<InfiniteScroll>,
    HasStyle<InfiniteScroll>, HasAttribute<InfiniteScroll>, HasSize<InfiniteScroll> {

  private static final String CONTENT_SLOT = "content";

  @PropertyMethods(setter = "setText", getter = "getText")
  private final PropertyDescriptor<String> textProp =
      PropertyDescriptor.property("text", "Loading data");
  private final PropertyDescriptor<String> iconProp =
      PropertyDescriptor.property("icon", "dwc:animated-spinner");
  private final PropertyDescriptor<Boolean> completedProp =
      PropertyDescriptor.property("completed", false);

  /**
   * Creates a new infinite scroll component.
   *
   * @param text the text to display when loading data
   */
  public InfiniteScroll(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new infinite scroll component.
   */
  public InfiniteScroll() {
    super();
  }

  /**
   * Adds the given components to the content slot of the infinite scroll component.
   *
   * <p>
   * Note that the {@code content} slot is different from the The default slot
   * {@link #add(Component...)}. The named slot {@code content} is specifically intended for the
   * loading indicator. By default, if no custom content is provided in this slot, the component
   * renders its fallback markup—a loading icon and text (e.g., "Loading data"). However, if you
   * need to customize the appearance or behavior of the loading indicator, you can supply your own
   * markup in this slot, and it will override the default content.
   * </p>
   *
   * @param component the components to add
   * @return the component itself
   */
  public InfiniteScroll addToContent(Component... component) {
    getElement().add(CONTENT_SLOT, component);
    return this;
  }

  /**
   * Sets the refresh icon to use when loading more data using the given source URl.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code webserver://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   * @return the component itself
   */
  public InfiniteScroll setIcon(String src) {
    set(iconProp, Assets.resolveImageSource(src));
    return this;
  }

  /**
   * Sets the refresh icon to use when loading more data using the given icon pool and name.
   *
   * <p>
   * The method will only use the icon name and pool from the given icon. The icon component itself
   * will not be used.
   * </p>
   *
   * @param icon the icon
   * @return the component itself
   */
  public InfiniteScroll setIcon(Icon icon) {
    set(iconProp, String.format("%s:%s", icon.getPool(), icon.getName()));
    return this;
  }

  /**
   * Gets the refresh icon used when loading more data.
   *
   * @return the icon
   */
  public String getIcon() {
    return get(iconProp);
  }

  /**
   * Sets the completed state of the infinite scroll.
   *
   * <p>
   * This flag indicates that no more items are available for loading. When it is set to true, the
   * component stops observing, effectively preventing any further infinite scroll triggers. This
   * means that even if the user scrolls, the component won’t try to load more data.
   * </p>
   *
   * @param completed true if the infinite scroll is completed, false otherwise
   * @return the component itself
   */
  public InfiniteScroll setCompleted(boolean completed) {
    set(completedProp, completed);
    return this;
  }

  /**
   * Gets the completed state of the infinite scroll.
   *
   * @return true if the infinite scroll is completed, false otherwise
   * @see #setCompleted(boolean)
   */
  public boolean isCompleted() {
    return get(completedProp);
  }

  /**
   * Update the infinite scroll component.
   *
   * <p>
   * This method will refresh the component's state after new content has been added.
   *
   * After the content update (for example, when new items have been appended to the list), calling
   * update resets the loading state, then reinitializes the client observer. This ensures that the
   * component correctly recalculates its boundaries and is ready to trigger further loads if
   * needed.
   * </p>
   *
   * @return the component itself
   */
  public InfiniteScroll update() {
    getElement().callJsFunctionVoidAsync("update");
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public InfiniteScroll setText(String text) {
    setHtml(sanitizeHtml(text));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return sanitizeHtml(getHtml());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public InfiniteScroll setHtml(String html) {
    set(textProp, html);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return get(textProp);
  }

  /**
   * Adds a listener for the scroll event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<InfiniteScrollEvent> addScrollListener(
      EventListener<InfiniteScrollEvent> listener) {
    return addEventListener(InfiniteScrollEvent.class, listener);
  }

  /**
   * Alias for {@link #addScrollListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<InfiniteScrollEvent> onScroll(
      EventListener<InfiniteScrollEvent> listener) {
    return addScrollListener(listener);
  }

  private String sanitizeHtml(String html) {
    return html.replaceAll("\\<[^>]*>", "");
  }

  Element getOriginalElement() {
    return getElement();
  }
}
