package com.webforj.component.html.elements;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;

/**
 * Component representing a {@code a} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a">Html a Tag</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
@NodeName("a")
public class Anchor extends HtmlComponentContainer<Anchor> {

  private final PropertyDescriptor<String> downloadProp =
      PropertyDescriptor.attribute("download", "");
  private final PropertyDescriptor<String> hrefProp = PropertyDescriptor.attribute("href", "");
  private final PropertyDescriptor<String> targetProp =
      PropertyDescriptor.attribute("target", "_self");

  /**
   * Creates a new empty Anchor.
   */
  public Anchor() {
    super();
  }

  /**
   * Creates a new anchor with the given text, url and target.
   *
   * @param href the url of the anchor
   * @param text the text
   * @param target the target
   */
  public Anchor(String href, String text, String target) {
    super();
    setText(text);
    setHref(href);
    setTarget(target);
  }

  /**
   * Creates a new anchor with the given url and text.
   *
   * @param href the url of the anchor
   * @param text the text
   */
  public Anchor(String href, String text) {
    super();
    setText(text);
    setHref(href);
  }

  /**
   * Creates a new anchor with the given text.
   *
   * @param text the text
   */
  public Anchor(String text) {
    super();
    setText(text);
  }

  /**
   * Creates a new anchor with the given url and child components.
   *
   * @param href the url of the anchor
   * @param components the child components
   */
  public Anchor(String href, Component... components) {
    super(components);
    setHref(href);
  }

  /**
   * Creates a new anchor with the given child components.
   *
   * @param components the child components
   */
  public Anchor(Component... components) {
    super(components);
  }

  /**
   * Sets the download attribute.
   *
   * <p>
   * This method causes the browser to treat the linked URL as a download. The behavior can vary
   * based on the presence or absence of a filename value:
   * <ul>
   * <li>Without a value, the browser suggests a filename/extension, generated from:
   * <ul>
   * <li>The Content-Disposition HTTP header.</li>
   * <li>The final segment in the URL path.</li>
   * <li>The media type (from the Content-Type header, data: URL start, or Blob.type for a blob:
   * URL).</li>
   * </ul>
   * </li>
   * <li>With a filename value, it suggests the value as the filename. '/' and '\' characters are
   * converted to underscores ('_'). Other characters may be adjusted by browsers to comply with
   * filesystem restrictions.</li>
   * </ul>
   * Note:
   * <ul>
   * <li>The download attribute works only for same-origin URLs, or the blob: and data:
   * schemes.</li>
   * <li>Browser handling of downloads varies based on the browser, user settings, and other
   * factors. Users may be prompted before a download, or the file may be saved or opened
   * automatically.</li>
   * <li>If the Content-Disposition header specifies a filename or a disposition of 'inline', it may
   * override or affect the behavior of the download attribute:
   * <ul>
   * <li>A filename in the header takes priority over one in the download attribute.</li>
   * <li>A disposition of 'inline' is overridden by the download attribute in Chrome and Firefox,
   * but not in older Firefox versions (before 82).</li>
   * </ul>
   * </li>
   * </ul>
   * </p>
   *
   * @param filename the filename
   * @return the component itself
   */
  public Anchor setDownload(String filename) {
    set(downloadProp, filename);
    return this;
  }

  /**
   * Gets the download attribute.
   *
   * @return the download attribute
   * @see #setDownload(java.lang.String)
   */
  public String getDownload() {
    return get(downloadProp);
  }

  /**
   * Sets the href attribute.
   *
   * <p>
   * The href attribute specifies the URL of the page the link goes to.
   * </p>
   *
   * @param href the href
   * @return the component itself
   */
  public Anchor setHref(String href) {
    if (href == null) {
      throw new IllegalArgumentException("Href must not be null");
    }

    set(hrefProp, href);
    return this;
  }

  /**
   * Gets the href attribute.
   *
   * @return the href attribute
   * @see #setHref(java.lang.String)
   */
  public String getHref() {
    return get(hrefProp);
  }

  /**
   * Alias for {@link #setHref(java.lang.String)}.
   *
   * @param url the url
   * @return the component itself
   */
  public Anchor setUrl(String url) {
    return setHref(url);
  }

  /**
   * Alias for {@link #getHref()}.
   *
   * @return the href attribute
   * @see #setUrl(java.lang.String)
   */
  public String getUrl() {
    return getHref();
  }

  /**
   * Sets the target attribute.
   *
   * <p>
   * The target attribute specifies where to open the linked document.
   * </p>
   *
   * @param target the target
   * @return the component itself
   */
  public Anchor setTarget(String target) {
    if (target == null) {
      throw new IllegalArgumentException("Target must not be null");
    }

    set(targetProp, target);
    return this;
  }

  /**
   * Gets the target attribute.
   *
   * @return the target attribute
   */
  public String getTarget() {
    return get(targetProp);
  }
}
