package com.webforj.bbjship.assets.model;

/**
 * Holds the HTTP answer for one asset request.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class AssetResponse {

  private final int status;
  private final String contentType;
  private final String entityTag;
  private final String cacheControl;
  private final byte[] body;

  private AssetResponse(Builder builder) {
    this.status = builder.status;
    this.contentType = builder.contentType;
    this.entityTag = builder.entityTag;
    this.cacheControl = builder.cacheControl;
    this.body = builder.body;
  }

  /**
   * Starts a response with a status code.
   *
   * @param status the HTTP status code
   *
   * @return the builder
   */
  public static Builder create(int status) {
    return new Builder(status);
  }

  /**
   * Returns the HTTP status code.
   *
   * @return the status code
   */
  public int getStatus() {
    return status;
  }

  /**
   * Returns the content type or null.
   *
   * @return the content type
   */
  public String getContentType() {
    return contentType;
  }

  /**
   * Returns the quoted entity tag or null.
   *
   * @return the ETag header value
   */
  public String getEntityTag() {
    return entityTag;
  }

  /**
   * Returns the cache control directive or null.
   *
   * @return the Cache-Control header value
   */
  public String getCacheControl() {
    return cacheControl;
  }

  /**
   * Returns a copy of the body, empty when the answer carries none.
   *
   * @return the body bytes
   */
  public byte[] getBody() {
    return body.clone();
  }

  /**
   * Builds an asset response.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final int status;
    private String contentType;
    private String entityTag;
    private String cacheControl;
    private byte[] body = new byte[0];

    private Builder(int status) {
      this.status = status;
    }

    /**
     * Sets the content type.
     *
     * @param contentType the content type or null
     *
     * @return this builder
     */
    public Builder setContentType(String contentType) {
      this.contentType = contentType;
      return this;
    }

    /**
     * Sets the quoted entity tag.
     *
     * @param entityTag the ETag header value or null
     *
     * @return this builder
     */
    public Builder setEntityTag(String entityTag) {
      this.entityTag = entityTag;
      return this;
    }

    /**
     * Sets the cache control directive.
     *
     * @param cacheControl the Cache-Control header value or null
     *
     * @return this builder
     */
    public Builder setCacheControl(String cacheControl) {
      this.cacheControl = cacheControl;
      return this;
    }

    /**
     * Sets the body.
     *
     * @param body the body bytes
     *
     * @return this builder
     */
    public Builder setBody(byte[] body) {
      this.body = body.clone();
      return this;
    }

    /**
     * Builds the response.
     *
     * @return the response
     */
    public AssetResponse build() {
      return new AssetResponse(this);
    }
  }
}
