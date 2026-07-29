package com.webforj.devtools.craftforj.inspector.model;

/**
 * Defines the available feature categories and their display order.
 *
 * <p>
 * Categories are displayed in the order they are defined in this enum. Each category has a unique
 * identifier and a human-readable label for display in the craftforJ panel.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum FeatureCategory {

  /**
   * Text content properties like text, label, placeholder, helperText, tooltip.
   */
  CONTENT("content", "Content"),

  /**
   * Component interaction state like visible, enabled, readOnly.
   */
  STATE("state", "State"),

  /**
   * Layout properties like flex direction, wrap, justify, align.
   */
  LAYOUT("layout", "Layout"),

  /**
   * Visual appearance and sizing like theme, expanse, classNames, width, height.
   */
  APPEARANCE("appearance", "Appearance"),

  /**
   * Form validation rules like required, pattern, mask, min, max, step.
   */
  VALIDATION("validation", "Validation");

  private final String id;
  private final String label;

  FeatureCategory(String id, String label) {
    this.id = id;
    this.label = label;
  }

  /**
   * Gets the unique identifier for this category.
   *
   * @return the category identifier
   */
  public String getId() {
    return id;
  }

  /**
   * Gets the human-readable label for this category.
   *
   * @return the display label
   */
  public String getLabel() {
    return label;
  }

  /**
   * Gets the display order for this category (based on enum ordinal).
   *
   * @return the display order (0-based)
   */
  public int getOrder() {
    return ordinal();
  }
}
