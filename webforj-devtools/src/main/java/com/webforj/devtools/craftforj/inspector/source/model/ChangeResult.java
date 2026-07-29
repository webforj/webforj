package com.webforj.devtools.craftforj.inspector.source.model;

import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;

/**
 * Result of a source code change (preview or apply).
 *
 * <p>
 * Contains the component ID, the full FeatureProperty that was changed (including all metadata from
 * the round-trip), the source location if successful, and any error message if failed.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ChangeResult {

  private String componentId;
  private FeatureProperty property;
  private SourceLocation source;
  private String error;
  private String resolvedTarget;
  private String replacedExpression;

  /**
   * Gets the component ID this change applies to.
   *
   * @return the component ID
   */
  public String getComponentId() {
    return componentId;
  }

  /**
   * Sets the component ID this change applies to.
   *
   * @param componentId the component ID
   */
  public void setComponentId(String componentId) {
    this.componentId = componentId;
  }

  /**
   * Gets the full property that was changed.
   *
   * @return the property
   */
  public FeatureProperty getProperty() {
    return property;
  }

  /**
   * Sets the full property that was changed.
   *
   * @param property the property
   */
  public void setProperty(FeatureProperty property) {
    this.property = property;
  }

  /**
   * Gets the source location (null if failed).
   *
   * @return the source location
   */
  public SourceLocation getSource() {
    return source;
  }

  /**
   * Sets the source location.
   *
   * @param source the source location
   */
  public void setSource(SourceLocation source) {
    this.source = source;
  }

  /**
   * Gets the error message (null if successful).
   *
   * @return the error message
   */
  public String getError() {
    return error;
  }

  /**
   * Sets the error message.
   *
   * @param error the error message
   */
  public void setError(String error) {
    this.error = error;
  }

  /**
   * Gets the target the change actually resolved to.
   *
   * <p>
   * A usage-targeted preview reports {@link ChangeRequest#TARGET_DEFINITION} here when the property
   * could not be traced to the usage site, so the client can surface the fallback before anything
   * is written.
   * </p>
   *
   * @return the resolved target, or null when the request carried no target
   */
  public String getResolvedTarget() {
    return resolvedTarget;
  }

  /**
   * Sets the target the change actually resolved to.
   *
   * @param resolvedTarget the resolved target
   */
  public void setResolvedTarget(String resolvedTarget) {
    this.resolvedTarget = resolvedTarget;
  }

  /**
   * Gets the computed expression the change overwrites.
   *
   * <p>
   * When the existing setter call's argument computes its value (a method call, an operator
   * expression, a conditional, an object creation or a lambda), writing the change replaces that
   * logic with a fixed value for every instance. The client surfaces this before anything is
   * written.
   * </p>
   *
   * @return the overwritten expression as source text, or null when the change is safe
   */
  public String getReplacedExpression() {
    return replacedExpression;
  }

  /**
   * Sets the computed expression the change overwrites.
   *
   * @param replacedExpression the overwritten expression as source text
   */
  public void setReplacedExpression(String replacedExpression) {
    this.replacedExpression = replacedExpression;
  }

  /**
   * Returns true if this change succeeded.
   *
   * @return true if no error occurred
   */
  public boolean isSuccess() {
    return error == null;
  }

  /**
   * Creates a successful result.
   *
   * @param componentId the component ID
   * @param property the property that was changed
   * @param source the source location where the change was applied
   * @return a successful ChangeResult
   */
  public static ChangeResult success(String componentId, FeatureProperty property,
      SourceLocation source) {
    ChangeResult result = new ChangeResult();
    result.setComponentId(componentId);
    result.setProperty(property);
    result.setSource(source);
    return result;
  }

  /**
   * Creates a failed result.
   *
   * @param componentId the component ID
   * @param property the property that failed to change
   * @param error the error message describing the failure
   * @return a failed ChangeResult
   */
  public static ChangeResult failure(String componentId, FeatureProperty property, String error) {
    ChangeResult result = new ChangeResult();
    result.setComponentId(componentId);
    result.setProperty(property);
    result.setError(error);
    return result;
  }
}
