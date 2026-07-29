package com.webforj.devtools.craftforj.model;

import com.webforj.devtools.craftforj.inspector.model.FeatureGroup;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import java.util.List;

/**
 * Unified metadata for a webforJ component.
 *
 * <p>
 * This class represents all information about a component that the craftforJ needs. It is used for
 * both tree display (showing the component hierarchy) and the inspector panel (showing component
 * properties and features).
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ComponentMeta {

  private String id;
  private String componentType;
  private String compositeComponentType;
  private String displayName;
  private boolean isComposite;
  private SourceLocation source;
  private SourceLocation usageSource;
  private List<FeatureGroup> featureGroups;

  ComponentMeta() {}

  /**
   * Creates a new ComponentMeta.
   *
   * @param id the server-side component UUID
   * @param componentType the fully qualified component class name
   * @param compositeComponentType the type of the bound component for composites (null if not
   *        composite)
   * @param displayName the simple class name for display
   * @param isComposite true if this is a Composite (not ElementComposite)
   * @param source the source location where component was created (null if not available)
   * @param usageSource the source location where the component's enclosing class was used (null if
   *        not available)
   */
  public ComponentMeta(String id, String componentType, String compositeComponentType,
      String displayName, boolean isComposite, SourceLocation source, SourceLocation usageSource) {
    this.id = id;
    this.componentType = componentType;
    this.compositeComponentType = compositeComponentType;
    this.displayName = displayName;
    this.isComposite = isComposite;
    this.source = source;
    this.usageSource = usageSource;
    this.featureGroups = null;
  }

  /**
   * Gets the component ID.
   *
   * @return the server-side component UUID
   */
  public String getId() {
    return id;
  }

  /**
   * Gets the fully qualified component type.
   *
   * @return the component class name (e.g., "com.webforj.component.button.Button")
   */
  public String getComponentType() {
    return componentType;
  }

  /**
   * Gets the composite's bound component type.
   *
   * @return the bound component class name, or null if not a composite
   */
  public String getCompositeComponentType() {
    return compositeComponentType;
  }

  /**
   * Gets the simple class name for display.
   *
   * @return the display name (e.g., "Button")
   */
  public String getDisplayName() {
    return displayName;
  }

  /**
   * Returns whether this is a Composite wrapper.
   *
   * @return true if this is a Composite (not ElementComposite)
   */
  public boolean isComposite() {
    return isComposite;
  }

  /**
   * Gets the source location where this component was created.
   *
   * @return the source location, or null if not available
   */
  public SourceLocation getSource() {
    return source;
  }

  /**
   * Gets the source location where the component's enclosing user class was used.
   *
   * @return the usage source location, or null if not available
   */
  public SourceLocation getUsageSource() {
    return usageSource;
  }

  /**
   * Gets the feature groups for this component.
   *
   * @return the feature groups, or null if not yet loaded
   */
  public List<FeatureGroup> getFeatureGroups() {
    return featureGroups;
  }

  /**
   * Sets the feature groups for this component.
   *
   * @param featureGroups the feature groups
   */
  public void setFeatureGroups(List<FeatureGroup> featureGroups) {
    this.featureGroups = featureGroups;
  }
}
