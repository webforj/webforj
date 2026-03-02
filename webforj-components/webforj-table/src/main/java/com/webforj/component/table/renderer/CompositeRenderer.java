package com.webforj.component.table.renderer;

import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.component.layout.flexlayout.FlexDirection;
import com.webforj.component.layout.flexlayout.FlexJustifyContent;
import com.webforj.component.layout.flexlayout.FlexWrap;
import java.util.ArrayList;
import java.util.List;

/**
 * A renderer that combines multiple sub-renderers in a flex layout within a single cell.
 *
 * <pre>{@code
 * AvatarRenderer<MusicRecord> avatar = new AvatarRenderer<>();
 *
 * TextRenderer<MusicRecord> text = new TextRenderer<>();
 * text.setTheme(Theme.PRIMARY);
 *
 * CompositeRenderer<MusicRecord> renderer = new CompositeRenderer<>(avatar, text);
 *
 * table.addColumn("artist", MusicRecord::getArtist).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class CompositeRenderer<T> extends Renderer<T> {
  private final List<Renderer<T>> renderers = new ArrayList<>();
  private String spacing = "8px";
  private FlexDirection direction = FlexDirection.ROW;
  private FlexAlignment alignment = FlexAlignment.CENTER;
  private FlexJustifyContent justifyContent;
  private FlexWrap wrap;

  /**
   * Creates a new composite renderer with the given sub-renderers.
   *
   * @param renderers the sub-renderers to compose
   */
  @SafeVarargs
  public CompositeRenderer(Renderer<T>... renderers) {
    for (Renderer<T> renderer : renderers) {
      add(renderer);
    }
  }

  /**
   * Adds a sub-renderer to the composition.
   *
   * @param renderer the renderer to add
   * @return this renderer
   */
  public CompositeRenderer<T> add(Renderer<T> renderer) {
    renderers.add(renderer);
    renderer.addChangeListener(e -> fireChangeEvent());
    fireChangeEvent();
    return this;
  }

  /**
   * Sets the spacing (gap) between sub-renderers.
   *
   * @param spacing the CSS gap value (e.g., {@code "8px"}, {@code "var(--dwc-space-s)"})
   * @return this renderer
   */
  public CompositeRenderer<T> setSpacing(String spacing) {
    this.spacing = spacing;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the spacing (gap) between sub-renderers.
   *
   * @return the CSS gap value
   */
  public String getSpacing() {
    return spacing;
  }

  /**
   * Sets the flex direction of the container.
   *
   * @param direction the flex direction
   * @return this renderer
   */
  public CompositeRenderer<T> setDirection(FlexDirection direction) {
    this.direction = direction;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the flex direction.
   *
   * @return the flex direction
   */
  public FlexDirection getDirection() {
    return direction;
  }

  /**
   * Sets the alignment of items in the container.
   *
   * @param alignment the flex alignment
   * @return this renderer
   */
  public CompositeRenderer<T> setAlignment(FlexAlignment alignment) {
    this.alignment = alignment;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the alignment of items.
   *
   * @return the flex alignment
   */
  public FlexAlignment getAlignment() {
    return alignment;
  }

  /**
   * Sets the justify-content property of the container.
   *
   * @param justifyContent the flex justify-content value
   * @return this renderer
   */
  public CompositeRenderer<T> setJustifyContent(FlexJustifyContent justifyContent) {
    this.justifyContent = justifyContent;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the justify-content property.
   *
   * @return the flex justify-content value
   */
  public FlexJustifyContent getJustifyContent() {
    return justifyContent;
  }

  /**
   * Sets the wrap behavior of the container.
   *
   * @param wrap the flex wrap value
   * @return this renderer
   */
  public CompositeRenderer<T> setWrap(FlexWrap wrap) {
    this.wrap = wrap;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the wrap behavior.
   *
   * @return the flex wrap value
   */
  public FlexWrap getWrap() {
    return wrap;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    StringBuilder style = new StringBuilder("display:flex");
    style.append(";flex-direction:").append(direction.getValue());
    style.append(";align-items:").append(alignment.getValue());

    if (justifyContent != null) {
      style.append(";justify-content:").append(justifyContent.getValue());
    }

    if (wrap != null) {
      style.append(";flex-wrap:").append(wrap.getValue());
    }

    style.append(";gap:").append(spacing);

    StringBuilder sb = new StringBuilder();
    sb.append("<span style='").append(style).append("'>");

    for (Renderer<T> renderer : renderers) {
      sb.append(renderer.build());
    }

    sb.append("</span>");
    return sb.toString();
  }

  /** {@inheritDoc} */
  @Override
  protected void onAttach() {
    super.onAttach();
    for (Renderer<T> renderer : renderers) {
      renderer.setTable(getTable());
    }
  }
}
