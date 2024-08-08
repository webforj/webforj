package com.webforj.router.history;

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
   * Constructs a Location object from path segments, query string, and fragment.
   *
   * @param segments the list of path segments
   * @param query the query string
   * @param fragment the fragment
   */
  public Location(SegmentsBag segments, ParametersBag query, String fragment) {
    this.segments = segments;
    this.parameters = query;
    this.fragment = fragment;
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

  /**
   * Gets the full URI string with query parameters and fragment.
   *
   * @return the full URI string
   */
  @Override
  public String toString() {
    return getFullURI();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((segments == null) ? 0 : segments.hashCode());
    result = prime * result + ((parameters == null) ? 0 : parameters.hashCode());
    result = prime * result + ((fragment == null) ? 0 : fragment.hashCode());
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    Location other = (Location) obj;
    if (segments == null) {
      if (other.segments != null) {
        return false;
      }
    } else if (!segments.equals(other.segments)) {
      return false;
    }
    if (parameters == null) {
      if (other.parameters != null) {
        return false;
      }
    } else if (!parameters.equals(other.parameters)) {
      return false;
    }
    if (fragment == null) {
      if (other.fragment != null) {
        return false;
      }
    } else if (!fragment.equals(other.fragment)) {
      return false;
    }
    return true;
  }
}
