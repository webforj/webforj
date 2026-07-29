package com.webforj.devtools.craftforj.inspector.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a group of related features displayed together in the craftforJ inspector.
 *
 * <p>
 * Feature groups organize properties into logical sections like "Content", "Appearance",
 * "Validation", etc. Each group has an ID, display label, and a list of properties.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FeatureGroup {

  private final String id;
  private final String label;
  private final List<FeatureProperty> properties;

  /**
   * Creates a new FeatureGroup.
   *
   * @param id the group identifier (e.g., "content", "appearance")
   * @param label the display label (e.g., "Content", "Appearance")
   */
  public FeatureGroup(String id, String label) {
    this.id = id;
    this.label = label;
    this.properties = new ArrayList<>();
  }

  /**
   * Gets the group identifier.
   *
   * @return the group ID
   */
  public String getId() {
    return id;
  }

  /**
   * Gets the display label.
   *
   * @return the label
   */
  public String getLabel() {
    return label;
  }

  /**
   * Gets the list of properties in this group.
   *
   * @return the properties list
   */
  public List<FeatureProperty> getProperties() {
    return properties;
  }

  /**
   * Adds a property to this group.
   *
   * @param property the property to add
   */
  public void addProperty(FeatureProperty property) {
    this.properties.add(property);
  }
}
