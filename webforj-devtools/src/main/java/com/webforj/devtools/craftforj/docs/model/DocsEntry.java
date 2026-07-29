package com.webforj.devtools.craftforj.docs.model;

/**
 * Entry from the docs-index.json file.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DocsEntry {

  private String title;
  private String since;
  private String javadoc;
  private String docs;
  private String clientComponent;
  private String content;

  /**
   * Gets the component title.
   *
   * @return the title (e.g., "Button")
   */
  public String getTitle() {
    return title;
  }

  /**
   * Sets the component title.
   *
   * @param title the title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Gets the version when this component was introduced.
   *
   * @return the since version (e.g., "23.02")
   */
  public String getSince() {
    return since;
  }

  /**
   * Sets the version when this component was introduced.
   *
   * @param since the since version
   */
  public void setSince(String since) {
    this.since = since;
  }

  /**
   * Gets the full javadoc URL.
   *
   * @return the javadoc URL
   */
  public String getJavadoc() {
    return javadoc;
  }

  /**
   * Sets the full javadoc URL.
   *
   * @param javadoc the javadoc URL
   */
  public void setJavadoc(String javadoc) {
    this.javadoc = javadoc;
  }

  /**
   * Gets the full URL to the docs page.
   *
   * @return the docs URL
   */
  public String getDocs() {
    return docs;
  }

  /**
   * Sets the full URL to the docs page.
   *
   * @param docs the docs URL
   */
  public void setDocs(String docs) {
    this.docs = docs;
  }

  /**
   * Gets the client component tag name.
   *
   * @return the client component (e.g., "dwc-button")
   */
  public String getClientComponent() {
    return clientComponent;
  }

  /**
   * Sets the client component tag name.
   *
   * @param clientComponent the client component
   */
  public void setClientComponent(String clientComponent) {
    this.clientComponent = clientComponent;
  }

  /**
   * Gets the markdown content describing the component.
   *
   * @return the content
   */
  public String getContent() {
    return content;
  }

  /**
   * Sets the markdown content describing the component.
   *
   * @param content the content
   */
  public void setContent(String content) {
    this.content = content;
  }
}
