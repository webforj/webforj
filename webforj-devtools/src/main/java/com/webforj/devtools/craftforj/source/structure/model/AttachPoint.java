package com.webforj.devtools.craftforj.source.structure.model;

import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.util.List;

/**
 * A place on a parent component where a child is attached in source.
 *
 * <p>
 * The point names the parent, the methods that attach into the wanted slot and an optional sibling
 * the child sits next to. The first method is the one written for a new attach call, every method
 * is accepted when an existing attach call is looked up.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class AttachPoint {

  private final SourceLocation parent;
  private final List<String> methods;
  private SourceLocation anchor;
  private boolean before;

  /**
   * Creates an attach point.
   *
   * @param parent the parent component location
   * @param methods the method names attaching into the slot, the first one is written
   */
  public AttachPoint(SourceLocation parent, List<String> methods) {
    this.parent = parent;
    this.methods = List.copyOf(methods);
  }

  /**
   * Gets the parent component location.
   *
   * @return the parent location
   */
  public SourceLocation getParent() {
    return parent;
  }

  /**
   * Gets the method names attaching into the slot.
   *
   * @return the method names
   */
  public List<String> getMethodNames() {
    return methods;
  }

  /**
   * Gets the method name written for a new attach call.
   *
   * @return the first method name
   */
  public String getMethodName() {
    return methods.get(0);
  }

  /**
   * Gets the sibling the child is placed next to.
   *
   * @return the sibling location, or {@code null} to append
   */
  public SourceLocation getAnchor() {
    return anchor;
  }

  /**
   * Sets the sibling the child is placed next to.
   *
   * @param anchor the sibling location, or {@code null} to append
   */
  public void setAnchor(SourceLocation anchor) {
    this.anchor = anchor;
  }

  /**
   * Checks whether the child goes before the sibling.
   *
   * @return {@code true} for before, {@code false} for after
   */
  public boolean isBefore() {
    return before;
  }

  /**
   * Sets whether the child goes before the sibling.
   *
   * @param before {@code true} for before, {@code false} for after
   */
  public void setBefore(boolean before) {
    this.before = before;
  }
}
