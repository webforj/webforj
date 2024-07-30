package com.webforj.router;

import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

/**
 * Represents a relative URL made up of path segments and query parameters.
 *
 * <p>
 * Location objects are used to represent the URL of a web page, including the path segments, query
 * parameters, and fragment. They can be constructed from a location string or from individual path
 * segments, query parameters, and fragment components.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class Location implements Serializable {
  private final SegmentsBag segments;
  private final ParametersBag parameters;
  private final String fragment;

  /**
   * Constructs a Location object from path segments, query string, and fragment.
   *
   * @param segments the list of path segments
   * @param query the query string
   * @param fragment the fragment
   */
  public Location(List<String> segments, String query, String fragment) {
    this.segments = new SegmentsBag(segments);
    this.parameters = new ParametersBag(query);
    this.fragment = fragment;
  }

  /**
   * Constructs a Location object from path segments and query string.
   *
   * @param segments the list of path segments
   * @param query the query string
   */
  public Location(List<String> segments, String query) {
    this(segments, query, null);
  }

  /**
   * Constructs a Location object from path segments.
   *
   * @param segments the list of path segments
   */
  public Location(List<String> segments) {
    this(segments, null, null);
  }

  /**
   * Constructs a Location object from a location string.
   *
   * @param location the location string
   */
  public Location(String location) {
    URI uri;
    try {
      uri = new URI(location);
      this.segments = new SegmentsBag(uri.getPath());
      this.parameters = new ParametersBag(uri.getQuery());
      this.fragment = uri.getFragment();
    } catch (URISyntaxException e) {
      throw new IllegalArgumentException("Invalid location: " + location, e);
    }
  }

  /**
   * Gets the query parameters.
   *
   * @return the query parameters
   */
  public ParametersBag getQueryParameters() {
    return parameters;
  }

  /**
   * Gets the path segments.
   *
   * @return the path segments
   */
  public SegmentsBag getSegments() {
    return segments;
  }

  /**
   * Gets the path fragment.
   *
   * @return the path fragment
   */
  public String getFragment() {
    return fragment;
  }

  /**
   * Gets the full URI string with query parameters and fragment.
   *
   * @return the full URI string
   */
  public String getFullURI() {
    StringBuilder uriBuilder = new StringBuilder(segments.getPath());
    String query = parameters.getQueryString();
    if (query != null && !query.isEmpty()) {
      uriBuilder.append('?').append(query);
    }

    if (fragment != null) {
      uriBuilder.append('#').append(fragment);
    }

    return uriBuilder.toString();
  }
}
