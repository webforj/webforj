package com.webforj.component.table.renderer;

import com.webforj.component.element.annotation.NodeName;

/**
 * A renderer that displays a hyperlink in a table cell.
 *
 * <p>
 * When no content is provided, the link text defaults to the column's value function result. The
 * {@code href} supports template expressions like {@code <%= cell.value %>}.
 * </p>
 *
 * <pre>{@code
 * AnchorRenderer<MusicRecord> renderer = new AnchorRenderer<>();
 * renderer.setHref("https://www.google.com/search?q=<%= cell.value %>");
 * renderer.setTarget("_blank");
 *
 * table.addColumn("title", MusicRecord::getTitle).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("a")
public class AnchorRenderer<T> extends AbstractElementRenderer<T> {
  private String href;
  private String target;
  private String download;

  /**
   * Creates a new anchor renderer with the given href, text, and target.
   *
   * @param href the url of the anchor
   * @param content the text
   * @param target the target
   */
  public AnchorRenderer(String href, String content, String target) {
    super(content, null);
    setHref(href);
    setTarget(target);
  }

  /**
   * Creates a new anchor renderer with the given url and text.
   *
   * @param href the url of the anchor
   * @param content the text
   */
  public AnchorRenderer(String href, String content) {
    super(content, null);
    setHref(href);
  }

  /**
   * Creates a new anchor renderer with the given text.
   *
   * @param content the text
   */
  public AnchorRenderer(String content) {
    super(content, null);
  }

  /**
   * Creates a new anchor renderer.
   */
  public AnchorRenderer() {
    this(null);
  }

  /**
   * Sets the href attribute.
   *
   * @param href the href
   * @return this renderer
   */
  public AnchorRenderer<T> setHref(String href) {
    this.href = href;
    setAttribute("href", href);
    return this;
  }

  /**
   * Returns the href attribute.
   *
   * @return the href attribute
   */
  public String getHref() {
    return href;
  }

  /**
   * Alias for {@link #setHref(String)}.
   *
   * @param url the url
   * @return this renderer
   */
  public AnchorRenderer<T> setUrl(String url) {
    return setHref(url);
  }

  /**
   * Alias for {@link #getHref()}.
   *
   * @return the href attribute
   */
  public String getUrl() {
    return getHref();
  }

  /**
   * Sets the target attribute.
   *
   * @param target the target
   * @return this renderer
   */
  public AnchorRenderer<T> setTarget(String target) {
    this.target = target;
    setAttribute("target", target);
    return this;
  }

  /**
   * Returns the target attribute.
   *
   * @return the target attribute
   */
  public String getTarget() {
    return target;
  }

  /**
   * Sets the download attribute, causing the browser to treat the linked URL as a download.
   *
   * @param filename the filename
   * @return this renderer
   */
  public AnchorRenderer<T> setDownload(String filename) {
    this.download = filename;
    setAttribute("download", filename);
    return this;
  }

  /**
   * Returns the download attribute.
   *
   * @return the download attribute
   */
  public String getDownload() {
    return download;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute("tabindex", "-1", false);
    return super.build();
  }
}
