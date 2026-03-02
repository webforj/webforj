package com.webforj.component.table.renderer;

import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * A renderer that displays a native HTML button in a table cell. The click event provides access to
 * the row item via {@code e.getItem()}.
 *
 * <pre>{@code
 * NativeButtonRenderer<MusicRecord> renderer = new NativeButtonRenderer<>("Delete", e -> {
 *   MusicRecord record = e.getItem();
 *   // handle delete
 * });
 *
 * table.addColumn("delete", r -> "").setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("button")
public class NativeButtonRenderer<T> extends AbstractElementRenderer<T> {

  /**
   * Creates a new native button renderer with the given content and click listener.
   *
   * @param content the content of the button
   * @param listener the click listener
   */
  public NativeButtonRenderer(String content, EventListener<RendererClickEvent<T>> listener) {
    super(content, listener);
  }

  /**
   * Creates a new native button renderer with the given content.
   *
   * @param content the content of the button
   */
  public NativeButtonRenderer(String content) {
    this(content, null);
  }

  /**
   * Creates a new native button renderer.
   */
  public NativeButtonRenderer() {
    this(null);
  }

  /**
   * Enables or disables the renderer.
   *
   * @param enabled true to enable, false to disable
   *
   * @return this renderer
   */
  public NativeButtonRenderer<T> setEnabled(boolean enabled) {
    String key = "disabled";

    if (enabled) {
      removeAttribute(key);
    } else {
      setAttribute(key, "true");
    }

    return this;
  }

  /**
   * Returns whether the renderer is enabled.
   *
   * @return true if the renderer is enabled, false otherwise
   */
  public boolean isEnabled() {
    return getAttribute("disabled") == null;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute("tabindex", "-1", false);
    return super.build();
  }
}
