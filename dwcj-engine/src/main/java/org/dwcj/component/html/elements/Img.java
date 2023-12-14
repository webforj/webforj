package org.dwcj.component.html.elements;

import org.dwcj.component.element.PropertyDescriptor;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.html.HtmlComponent;
import org.dwcj.utilities.Assets;

/**
 * Component representing a {@code img} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img">HTML img Tag</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("img")
public class Img extends HtmlComponent<Iframe> {

  private final PropertyDescriptor<String> srcProp = PropertyDescriptor.attribute("src", "");
  private final PropertyDescriptor<String> altProp = PropertyDescriptor.attribute("alt", "");

  /**
   * Creates a new img.
   */
  public Img() {
    super();
  }

  /**
   * Creates a new img with the given source and alternative text.
   *
   * @param src the source of the image
   */
  public Img(String src, String alt) {
    this();
    setSrc(src);
    setAlt(alt);
  }

  /**
   * Creates a new img with the given source.
   *
   * @param src the source of the image
   */
  public Img(String src) {
    this();
    setSrc(src);
  }

  /**
   * Sets the source of the image.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code webserver://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   * @return the component itself
   */
  public Img setSrc(String src) {
    String url = src.trim();

    // TODO: this should be test with integration tests.
    if (Assets.isWebServerUrl(src)) {
      url = Assets.resolveWebServerUrl(src);
    } else if (Assets.isContextUrl(src)) {
      String content = Assets.contentOf(Assets.resolveContextUrl(src), Assets.ContentFormat.BASE64);
      url = "data:text/html;base64," + content;
    }

    set(srcProp, url);
    return this;
  }

  /**
   * Returns the source of the image.
   *
   * @return the source of the image
   */
  public String getSrc() {
    return get(srcProp);
  }

  /**
   * Sets the alternative text of the image.
   *
   * @param alt the alternative text of the image
   * @return the component itself
   */
  public Img setAlt(String alt) {
    set(altProp, alt);
    return this;
  }

  /**
   * Returns the alternative text of the image.
   *
   * @return the alternative text of the image
   */
  public String getAlt() {
    return get(altProp);
  }
}
