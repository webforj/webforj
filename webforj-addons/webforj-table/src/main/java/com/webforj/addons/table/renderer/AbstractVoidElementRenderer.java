package com.webforj.addons.table.renderer;

import com.webforj.addons.table.Table;
import com.webforj.addons.table.event.renderer.RendererClickEvent;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.DwcjRuntimeException;
import java.util.HashMap;
import java.util.Map;

/**
 * The base class for all renderers which render a tag without any content.
 *
 * @param <T> the type of the row data
 *
 * @see AbstractElementRenderer
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class AbstractVoidElementRenderer<T> extends Renderer<T> {
  private String content = null;

  /**
   * {@inheritDoc}
   */
  @Override
  public String build() {
    String theContent = content == null ? "<%= cell.value %>" : content;
    return /* html */"""
        <%1$s
          %2$s
          data-k='%3$s'
          data-c='<%%= cell.column.key %%>'
          data-i='<%%= cell.row.key %%>'
          onclick="this.getRootNode().host['__%3$s__'](this)">
          %4$s
        </%1$s>
        """.formatted(getNodeName(), getAttributesAsString(), getKey(), theContent);
  }

  /**
   * Gets the name of html tag.
   *
   * @return the name of the tag
   */
  public String getNodeName() {
    @SuppressWarnings("rawtypes")
    Class<? extends AbstractVoidElementRenderer> clazz = getClass();

    if (clazz.isAnnotationPresent(NodeName.class)) {
      return clazz.getAnnotation(NodeName.class).value();
    } else {
      throw new DwcjRuntimeException(
          "The renderer class '" + clazz.getSimpleName() + "' is not annotated with @NodeName");
    }
  }

  /**
   * Adds a click listener to the renderer.
   *
   * @param listener the listener to add
   * @return the listener registration
   */
  public ListenerRegistration<RendererClickEvent<T>> addClickListener(
      EventListener<RendererClickEvent<T>> listener) {
    return getEventDispatcher().addListener(RendererClickEvent.class, listener);
  }

  /**
   * Alias for {@link #addClickListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return the listener registration
   */
  public ListenerRegistration<RendererClickEvent<T>> onClick(
      EventListener<RendererClickEvent<T>> listener) {
    return addClickListener(listener);
  }

  /**
   * Sets the content of the tag renderer.
   *
   * @param content the content
   *
   * @return the renderer itself.
   */
  AbstractVoidElementRenderer<T> setContent(String content) {
    this.content = content;
    fireChangeEvent();

    return this;
  }

  /**
   * Gets the content of the tag renderer.
   *
   * @return the content
   */
  String getContent() {
    return content;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    String key = getKey();
    Table<T> table = getTable();

    // Create a click event trigger for the tag renderer
    String func = /* javascript */"""
        component['__%1$s__'] = (render) => {
          const itemKey = render.dataset.i;
          const columnKey = render.dataset.c;
          render.dispatchEvent(new CustomEvent('%1$s-click', {
            bubbles: true,
            composed: true,
            detail: { itemKey, columnKey }
          }));
        };""".formatted(key);

    table.getElement().executeJsAsync(func);

    ElementEventOptions options = new ElementEventOptions();
    options.addData("itemKey", "event.detail.itemKey"); // NOSONAR
    options.addData("columnKey", "event.detail.columnKey"); // NOSONAR

    // Add the click listener to the table to be notified when the tag is clicked
    table.getElement().addEventListener(key + "-click", e -> {
      Map<String, Object> payload = new HashMap<>();
      payload.put("itemKey", e.getData().get("itemKey"));
      payload.put("columnKey", e.getData().get("columnKey"));

      RendererClickEvent<T> event = new RendererClickEvent<>(table, payload);
      getEventDispatcher().dispatchEvent(event);
    }, options);
  }
}
