package com.webforj.devtools.craftforj.inspector.source.model;

import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;

/**
 * Request to change a component property in source code.
 *
 * <p>
 * This class is sent from the client when the user wants to save property changes to source code.
 * It contains the component ID, full property with all metadata and new value, and source location
 * for fallback when the runtime component is destroyed.
 * </p>
 *
 * <p>
 * The FeatureProperty travels round-trip: server builds it with all metadata (name, featureType,
 * editorType, editorConfig, javaType), client updates only the value, and sends the complete
 * property back for source generation.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ChangeRequest {

  /** Target value for edits applied at the component's usage site. */
  public static final String TARGET_USAGE = "usage";

  /** Target value for edits applied at the component's creation site. */
  public static final String TARGET_DEFINITION = "definition";

  private String componentId;
  private FeatureProperty property;
  private SourceLocation source;
  private String parentId;
  private SourceLocation parentSource;
  private String target;
  private Object originalValue;

  /**
   * Creates a ChangeRequest with the given values.
   *
   * @param componentId the component ID
   * @param property the property
   * @param source the source location
   */
  public ChangeRequest(String componentId, FeatureProperty property, SourceLocation source) {
    this.componentId = componentId;
    this.property = property;
    this.source = source;
  }

  /**
   * Creates a ChangeRequest for a parent-scoped property.
   *
   * @param componentId the component ID
   * @param property the property
   * @param source the source location
   * @param parentId the parent component ID
   * @param parentSource the parent source location for fallback
   */
  public ChangeRequest(String componentId, FeatureProperty property, SourceLocation source,
      String parentId, SourceLocation parentSource) {
    this(componentId, property, source);
    this.parentId = parentId;
    this.parentSource = parentSource;
  }

  /**
   * Gets the server-side component UUID.
   *
   * @return the component ID
   */
  public String getComponentId() {
    return componentId;
  }

  /**
   * Sets the server-side component UUID.
   *
   * @param componentId the component ID
   */
  public void setComponentId(String componentId) {
    this.componentId = componentId;
  }

  /**
   * Gets the full property including metadata and new value.
   *
   * @return the property
   */
  public FeatureProperty getProperty() {
    return property;
  }

  /**
   * Sets the full property including metadata and new value.
   *
   * @param property the property
   */
  public void setProperty(FeatureProperty property) {
    this.property = property;
  }

  /**
   * Gets the source location for fallback (null if component still exists at runtime).
   *
   * @return the source location
   */
  public SourceLocation getSource() {
    return source;
  }

  /**
   * Sets the source location for fallback.
   *
   * @param source the source location
   */
  public void setSource(SourceLocation source) {
    this.source = source;
  }

  /**
   * Gets the feature type from the property.
   *
   * @return the feature type
   */
  public String getFeatureType() {
    return property != null ? property.getFeatureType() : null;
  }

  /**
   * Gets the property name from the property.
   *
   * @return the property name
   */
  public String getPropertyName() {
    return property != null ? property.getName() : null;
  }

  /**
   * Gets the value from the property.
   *
   * @return the property value
   */
  public Object getValue() {
    return property != null ? property.getValue() : null;
  }

  /**
   * Gets the server-side UUID of the parent component for parent-scoped properties.
   *
   * <p>
   * The parent is resolved by the client, which owns the component tree built from the rendered
   * DOM; the server-side hierarchy is incomplete.
   * </p>
   *
   * @return the parent component ID, or null for regular properties
   */
  public String getParentId() {
    return parentId;
  }

  /**
   * Sets the server-side UUID of the parent component.
   *
   * @param parentId the parent component ID
   */
  public void setParentId(String parentId) {
    this.parentId = parentId;
  }

  /**
   * Gets the parent source location for fallback (used when the parent was destroyed).
   *
   * @return the parent source location, or null
   */
  public SourceLocation getParentSource() {
    return parentSource;
  }

  /**
   * Sets the parent source location for fallback.
   *
   * @param parentSource the parent source location
   */
  public void setParentSource(SourceLocation parentSource) {
    this.parentSource = parentSource;
  }

  /**
   * Gets the requested edit target.
   *
   * @return {@link #TARGET_USAGE}, {@link #TARGET_DEFINITION}, or null for the default definition
   *         behavior
   */
  public String getTarget() {
    return target;
  }

  /**
   * Sets the requested edit target.
   *
   * @param target the edit target
   */
  public void setTarget(String target) {
    this.target = target;
  }

  /**
   * Checks whether the client asked for this change to land at the component's usage site.
   *
   * @return true when the usage site is the requested target
   */
  public boolean isUsageTargeted() {
    return TARGET_USAGE.equals(target);
  }

  /**
   * Gets the property value before the change.
   *
   * <p>
   * Usage-site edits verify the call-site argument against this value before rewriting it, so a
   * wrong parameter guess or a stale line can never rewrite an unrelated argument.
   * </p>
   *
   * @return the original value, or null when the client did not send one
   */
  public Object getOriginalValue() {
    return originalValue;
  }

  /**
   * Sets the property value before the change.
   *
   * @param originalValue the original value
   */
  public void setOriginalValue(Object originalValue) {
    this.originalValue = originalValue;
  }

  /**
   * Checks if this request has a valid parent source location for fallback.
   *
   * @return true if the parent source location has basic info
   */
  public boolean hasParentSourceFallback() {
    return parentSource != null && parentSource.hasBasicInfo();
  }

  /**
   * Checks if this request has valid source location for fallback.
   *
   * <p>
   * Only requires basic info (file + line) since variableName can be derived from componentType if
   * needed. The isComplete() check is too strict for fallback scenarios.
   * </p>
   *
   * @return true if source location has basic info
   */
  public boolean hasSourceFallback() {
    return source != null && source.hasBasicInfo();
  }
}
