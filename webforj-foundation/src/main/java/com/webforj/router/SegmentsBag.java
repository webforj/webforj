
package com.webforj.router;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

/**
 * SegmentsBag is a container for path segments.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class SegmentsBag implements Serializable, Iterable<String> {
  private List<String> segments;

  /**
   * Constructs an empty SegmentsBag object.
   */
  public SegmentsBag() {
    this.segments = new ArrayList<>();
  }

  /**
   * Constructs a SegmentsBag object from a path string.
   *
   * @param path the path string
   */
  public SegmentsBag(String path) {
    this.segments =
        path == null || path.isEmpty() ? new ArrayList<>() : Arrays.asList(path.split("/"));
  }

  /**
   * Constructs a SegmentsBag object from a list of segments.
   *
   * @param segments the list of path segments
   */
  public SegmentsBag(List<String> segments) {
    this.segments = new ArrayList<>(segments);
  }

  /**
   * Creates a new SegmentsBag object.
   *
   * @return a new SegmentsBag object
   */
  public static SegmentsBag of(String path) {
    return new SegmentsBag(path);
  }

  /**
   * Creates a new SegmentsBag object.
   *
   * @return a new SegmentsBag object
   */
  public static SegmentsBag of(List<String> segments) {
    return new SegmentsBag(segments);
  }

  /**
   * Returns all path segments.
   *
   * @return the list of path segments
   */
  public List<String> all() {
    return Collections.unmodifiableList(segments);
  }

  /**
   * Returns the segment at a specific index.
   *
   * @param index the index of the segment
   * @param defaultValue the default value
   *
   * @return the segment at the specified index
   */
  public String get(int index, String defaultValue) {
    return index < segments.size() ? segments.get(index) : defaultValue;
  }

  /**
   * Returns the segment at a specific index.
   *
   * @param index the index of the segment
   * @return the segment at the specified index
   */
  public Optional<String> get(int index) {
    return Optional.ofNullable(get(index, null));
  }

  /**
   * Adds a new path segment.
   *
   * @param index the index to add the segment
   * @param segment the segment to add
   */
  public void add(int index, String segment) {
    segments.add(index, segment);
  }

  /**
   * Adds a new path segment.
   *
   * @param segment the segment to add
   */
  public void add(String segment) {
    segments.add(segment);
  }

  /**
   * Removes a specific path segment.
   *
   * @param segment the segment to remove
   */
  public void remove(String segment) {
    segments.remove(segment);
  }

  /**
   * Removes a path segment at a specific index.
   *
   * @param index the index of the segment to remove
   */
  public void remove(int index) {
    segments.remove(index);
  }

  /**
   * Checks if a specific path segment is present.
   *
   * @param segment the segment to check
   * @return true if the segment is present, false otherwise
   */
  public boolean contains(String segment) {
    return segments.contains(segment);
  }

  /**
   * Returns the number of segments.
   *
   * @return the number of segments
   */
  public int size() {
    return segments.size();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<String> iterator() {
    return Collections.unmodifiableList(segments).iterator();
  }

  /**
   * Returns the path as a string.
   *
   * @return the path string
   */
  public String getPath() {
    return String.join("/", segments);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return getPath();
  }
}
