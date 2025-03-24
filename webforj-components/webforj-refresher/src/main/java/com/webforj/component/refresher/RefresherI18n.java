package com.webforj.component.refresher;

/**
 * The refresher translation object.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
public class RefresherI18n {
  private String pull = "Pull down to refresh";
  private String release = "Release to refresh";
  private String refresh = "Refreshing";

  /**
   * Gets the pull text.
   *
   * @return the pull text
   */
  public String getPull() {
    return pull;
  }

  /**
   * Sets the pull text.
   *
   * @param pull the pull text
   */
  public void setPull(String pull) {
    this.pull = pull;
  }

  /**
   * Gets the release text.
   *
   * @return the release text
   */
  public String getRelease() {
    return release;
  }

  /**
   * Sets the release text.
   *
   * @param release the release text
   */
  public void setRelease(String release) {
    this.release = release;
  }

  /**
   * Gets the refreshing text.
   *
   * @return the refreshing text
   */
  public String getRefresh() {
    return refresh;
  }

  /**
   * Sets the refreshing text.
   *
   * @param refreshing the refreshing text
   */
  public void setRefresh(String refreshing) {
    this.refresh = refreshing;
  }
}
