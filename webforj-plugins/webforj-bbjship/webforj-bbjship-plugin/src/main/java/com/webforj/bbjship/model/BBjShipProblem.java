package com.webforj.bbjship.model;

/**
 * Holds the problem details of a refused request as RFC 9457 defines them.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipProblem {

  private final String type;
  private final String title;
  private final int status;
  private final String detail;

  private BBjShipProblem(Builder builder) {
    this.type = builder.type;
    this.title = builder.title;
    this.status = builder.status;
    this.detail = builder.detail;
  }

  /**
   * Starts a problem with its status code.
   *
   * @param status the HTTP status code
   *
   * @return the builder
   */
  public static Builder create(int status) {
    return new Builder(status);
  }

  /**
   * Gets the URI reference of the problem type.
   *
   * @return the problem type
   */
  public String getType() {
    return type;
  }

  /**
   * Gets the short summary of the problem type.
   *
   * @return the title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Gets the HTTP status code.
   *
   * @return the status code
   */
  public int getStatus() {
    return status;
  }

  /**
   * Gets the explanation of this occurrence of the problem.
   *
   * @return the detail
   */
  public String getDetail() {
    return detail;
  }

  /**
   * Builds a problem.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final int status;
    private String type;
    private String title;
    private String detail;

    private Builder(int status) {
      this.status = status;
    }

    /**
     * Sets the URI reference of the problem type.
     *
     * @param type the problem type
     *
     * @return this builder
     */
    public Builder setType(String type) {
      this.type = type;
      return this;
    }

    /**
     * Sets the short summary of the problem type.
     *
     * @param title the title
     *
     * @return this builder
     */
    public Builder setTitle(String title) {
      this.title = title;
      return this;
    }

    /**
     * Sets the explanation of this occurrence of the problem.
     *
     * @param detail the detail
     *
     * @return this builder
     */
    public Builder setDetail(String detail) {
      this.detail = detail;
      return this;
    }

    /**
     * Builds the problem.
     *
     * @return the problem
     */
    public BBjShipProblem build() {
      return new BBjShipProblem(this);
    }
  }
}
