package com.webforj.devtools.craftforj.docs.model;

/**
 * Query parameters for looking up component documentation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DocsQuery {

  private String serverComponent;
  private String clientComponent;

  /**
   * Creates a DocsQuery with the given component identifiers.
   *
   * @param serverComponent the server component class name
   * @param clientComponent the client component tag name
   */
  public DocsQuery(String serverComponent, String clientComponent) {
    this.serverComponent = serverComponent;
    this.clientComponent = clientComponent;
  }

  /**
   * Gets the server-side component class name.
   *
   * @return the server component (e.g., "com.webforj.component.button.Button")
   */
  public String getServerComponent() {
    return serverComponent;
  }

  /**
   * Sets the server-side component class name.
   *
   * @param serverComponent the server component
   */
  public void setServerComponent(String serverComponent) {
    this.serverComponent = serverComponent;
  }

  /**
   * Gets the client-side component tag name.
   *
   * @return the client component (e.g., "dwc-button")
   */
  public String getClientComponent() {
    return clientComponent;
  }

  /**
   * Sets the client-side component tag name.
   *
   * @param clientComponent the client component
   */
  public void setClientComponent(String clientComponent) {
    this.clientComponent = clientComponent;
  }
}
