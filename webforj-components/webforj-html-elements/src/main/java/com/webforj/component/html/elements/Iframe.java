package com.webforj.component.html.elements;

import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Component representing a {@code iframe} element.
 *
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe">HTML iframe
 *      Tag</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("iframe")
public class Iframe extends HtmlComponent<Iframe> {

  // @formatter:off
  /**
   * An enum representing the possible values of the {@code sandbox} attribute.
   */
  public enum Sandbox {
    ALLOW_DOWNLOADS("allow-downloads"),
    ALLOW_DOWNLOADS_WITHOUT_USER_ACTIVATION("allow-downloads-without-user-activation"),
    ALLOW_FORMS("allow-forms"),
    ALLOW_MODALS("allow-modals"),
    ALLOW_ORIENTATION_LOCK("allow-orientation-lock"),
    ALLOW_POINTER_LOCK("allow-pointer-lock"),
    ALLOW_POPUPS("allow-popups"),
    ALLOW_POPUPS_TO_ESCAPE_SANDBOX("allow-popups-to-escape-sandbox"),
    ALLOW_PRESENTATION("allow-presentation"),
    ALLOW_SAME_ORIGIN("allow-same-origin"),
    ALLOW_SCRIPTS("allow-scripts"),
    ALLOW_TOP_NAVIGATION("allow-top-navigation"),
    ALLOW_TOP_NAVIGATION_BY_USER_ACTIVATION("allow-top-navigation-by-user-activation"),
    ALLOW_TOP_NAVIGATION_TO_CUSTOM_PROTOCOLS("allow-top-navigation-to-custom-protocols");

    private final String value;

    Sandbox(String value) {
      this.value = value;
    }

    /**
     * Gets the value of the enum.
     *
     * @return the value of the enum
     */
    public String getValue() {
      return value;
    }
  }
  // @formatter:on

  private List<Sandbox> sandboxValues = new ArrayList<>();
  private final PropertyDescriptor<String> srcProp = PropertyDescriptor.attribute("src", "");
  private final PropertyDescriptor<String> srcDocProp = PropertyDescriptor.attribute("srcdoc", "");
  private final PropertyDescriptor<String> nameProp = PropertyDescriptor.attribute("name", "");
  private final PropertyDescriptor<String> allowProp = PropertyDescriptor.attribute("allow", "");

  /**
   * Creates a new iframe.
   */
  public Iframe() {
    super();
  }

  /**
   * Sets the iframe URL.
   *
   * <p>
   * The URL of the page to embed. Use a value of about:blank to embed an empty page that conforms
   * to the same-origin policy. Also note that programmatically removing an iframe's src attribute
   * (e.g. via Element.removeAttribute()) causes about:blank to be loaded in the frame in Firefox
   * (from version 65), Chromium-based browsers, and Safari/iOS.
   * </p>
   *
   * @param src the source URL
   * @return the component itself
   */
  public Iframe setSrc(String src) {
    set(srcProp, src);
    return this;
  }

  /**
   * Gets the iframe URL.
   *
   * @return the iframe URL
   * @see #setSrc(String)
   */
  public String getSrc() {
    return get(srcProp);
  }

  /**
   * Sets the HTML content of the page to show in the iframe.
   *
   * <p>
   * Inline HTML to embed, overriding the src attribute. If a browser does not support the srcdoc
   * attribute, it will fall back to the URL in the src attribute.
   * </p>
   *
   * @param srcDoc the HTML content
   * @return the component itself
   */
  public Iframe setSrcdoc(String srcDoc) {
    set(srcDocProp, srcDoc);
    return this;
  }

  /**
   * Gets the HTML content of the page to show in the iframe.
   *
   * @return the HTML content
   * @see #setSrcDoc(String)
   */
  public String getSrcdoc() {
    return get(srcDocProp);
  }

  /**
   * Sets the name of the iframe.
   *
   * <p>
   * A targetable name for the embedded browsing context. This can be used in the target attribute
   * of the {@link Anchor}
   * </p>
   *
   * @param name the name of the iframe
   * @return the component itself
   */
  @Override
  public Iframe setName(String name) {
    super.setName(name);
    set(nameProp, name);
    return this;
  }

  /**
   * Gets the name of the iframe.
   *
   * @return the name of the iframe
   * @see #setName(String)
   */
  @Override
  public String getName() {
    return get(nameProp);
  }

  /**
   * Specifies a <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Permissions_Policy">
   * Permissions Policy</a> for the iframe.
   *
   * <p>
   * The policy defines what features are available to the iframe (for example, access to the
   * microphone, camera, battery, web-share, etc.) based on the origin of the request.
   * </p>
   *
   * @param allow the permissions policy
   * @return the component itself
   */
  public Iframe setAllow(String allow) {
    set(allowProp, allow);
    return this;
  }

  /**
   * Gets the permissions policy for the iframe.
   *
   * @return the permissions policy
   * @see #setAllow(String)
   */
  public String getAllow() {
    return get(allowProp);
  }

  /**
   * Sets the sandbox attribute values for the iframe.
   *
   * @param sandboxValues a list of sandbox attribute values
   * @return the component itself
   */
  public Iframe setSandbox(List<Sandbox> sandboxValues) {
    this.sandboxValues = new ArrayList<>(sandboxValues);
    updateSandboxAttribute();
    return this;
  }

  /**
   * Adds a single sandbox attribute value to the iframe.
   *
   * @param sandboxValue a single sandbox attribute value to add
   * @return the component itself
   */
  public Iframe addSandbox(Sandbox sandboxValue) {
    sandboxValues.add(sandboxValue);
    updateSandboxAttribute();
    return this;
  }

  /**
   * Gets the sandbox attribute values for the iframe.
   *
   * @return the list of sandbox attribute values
   */
  public List<Sandbox> getSandbox() {
    return Collections.unmodifiableList(sandboxValues);
  }

  /**
   * Removes a specific sandbox attribute value from the iframe.
   *
   * @param sandboxValue the sandbox attribute value to remove
   * @return the component itself
   */
  public Iframe removeSandboxValue(Sandbox sandboxValue) {
    sandboxValues.remove(sandboxValue);
    updateSandboxAttribute();
    return this;
  }

  /**
   * Gets the sandbox attribute values for the iframe.
   *
   * @return the list of sandbox attribute values
   * @deprecated Use {@link #getSandbox()} instead.
   */
  @Deprecated(since = "25.12", forRemoval = true)
  public List<Sandbox> getSandboxValues() {
    return getSandbox();
  }

  /**
   * Updates the sandbox attribute based on the current sandboxValues.
   */
  private void updateSandboxAttribute() {
    StringBuilder sb = new StringBuilder();

    for (Sandbox sandboxValue : sandboxValues) {
      sb.append(sandboxValue.getValue()).append(" ");
    }

    getElement().setAttribute("sandbox", sb.toString().trim());
  }
}
