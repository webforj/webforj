package com.webforj.component.table.renderer;

import com.webforj.component.element.annotation.NodeName;
import com.webforj.utilities.Assets;

/**
 * A renderer that displays an image in a table cell.
 *
 * <p>
 * When {@code src} is not explicitly set, it defaults to the column's value function result. The
 * column should return an image URL. The {@code src} also supports template expressions like
 * {@code <%= cell.value %>} for dynamic URLs.
 * </p>
 *
 * <pre>{@code
 * ImageRenderer<MusicRecord> renderer = new ImageRenderer<>();
 * renderer.setSrc("https://placehold.co/40x40?text=<%= cell.value %>");
 * renderer.setAlt("Cover");
 *
 * table.addColumn("cover", MusicRecord::getArtist).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("img")
public class ImageRenderer<T> extends AbstractVoidElementRenderer<T> {
  private static final String SRC_ATTR = "src";
  private static final String STYLE_ATTR = "style";

  private String src;
  private String alt;
  private String width;
  private String height;


  /**
   * Creates a new image renderer with the given source and alt text.
   *
   * @param src the image source URL
   * @param alt the alt text
   */
  public ImageRenderer(String src, String alt) {
    this(src);
    setAlt(alt);
  }

  /**
   * Creates a new image renderer with the given source.
   *
   * @param src the image source URL
   */
  public ImageRenderer(String src) {
    this();
    setSrc(src);
  }

  /**
   * Creates a new image renderer.
   */
  public ImageRenderer() {
    setContent("");
    updateStyle();
  }

  /**
   * Sets the source of the image.
   *
   * <p>
   * If the URL begins with {@code context://}, it will be resolved as a context URL. If it starts
   * with {@code ws://}, it will be resolved as a web server URL. URLs containing template
   * expressions (e.g., {@code <%= cell.value %>}) are passed through as-is without resolution.
   * </p>
   *
   * @param src the image source URL
   * @return this renderer
   */
  public ImageRenderer<T> setSrc(String src) {
    this.src = src;
    if (src != null && src.contains("<%")) {
      setAttribute(SRC_ATTR, src);
    } else {
      setAttribute(SRC_ATTR, Assets.resolveImageSource(src));
    }
    return this;
  }

  /**
   * Returns the source of the image.
   *
   * @return the source of the image
   */
  public String getSrc() {
    return src;
  }

  /**
   * Sets the alternative text of the image.
   *
   * @param alt the alternative text of the image
   * @return this renderer
   */
  public ImageRenderer<T> setAlt(String alt) {
    this.alt = alt;
    setAttribute("alt", alt);
    return this;
  }

  /**
   * Returns the alternative text of the image.
   *
   * @return the alternative text of the image
   */
  public String getAlt() {
    return alt;
  }

  /**
   * Sets the width of the image.
   *
   * @param width the CSS width value (e.g., {@code "40px"}, {@code "2em"}, {@code "100%"})
   * @return this renderer
   */
  public ImageRenderer<T> setWidth(String width) {
    this.width = width;
    updateStyle();
    return this;
  }

  /**
   * Returns the width of the image.
   *
   * @return the width
   */
  public String getWidth() {
    return width;
  }

  /**
   * Sets the height of the image.
   *
   * @param height the CSS height value (e.g., {@code "40px"}, {@code "2em"}, {@code "100%"})
   * @return this renderer
   */
  public ImageRenderer<T> setHeight(String height) {
    this.height = height;
    updateStyle();
    return this;
  }

  /**
   * Returns the height of the image.
   *
   * @return the height
   */
  public String getHeight() {
    return height;
  }


  /** {@inheritDoc} */
  @Override
  public String build() {
    if (src == null) {
      setAttribute(SRC_ATTR, "<%= cell.value %>", false);
    }

    return super.build();
  }

  private void updateStyle() {
    StringBuilder style = new StringBuilder();
    style.append("display:block;vertical-align:middle;max-height:100%");

    if (width != null) {
      style.append(";width:").append(width);
    }

    if (height != null) {
      style.append(";height:").append(height);
    }


    setAttribute(STYLE_ATTR, style.toString());
  }
}
