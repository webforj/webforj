package com.webforj.devtools.craftforj.inspector.contribution.content;

import com.google.auto.service.AutoService;
import com.webforj.component.icons.Icon;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the icon of {@code Icon} components.
 *
 * <p>
 * Exposes the icon pool and name as a single {@code "pool:name"} value edited with the icon picker.
 * Source write-back for this contribution rewrites the icon factory expression in place instead of
 * appending setter calls.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class IconContribution extends ConcernContribution<Icon> {

  /**
   * Creates the Icon contribution.
   */
  public IconContribution() {
    super(Icon.class, "Icon", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::icon);
    setGetter(icon -> icon.getPool() + ":" + icon.getName());
    setSetter((icon, value) -> {
      String text = String.valueOf(value);
      int separator = text.indexOf(':');
      if (separator <= 0 || separator == text.length() - 1) {
        throw new IllegalArgumentException("Icon value must be in 'pool:name' format: " + text);
      }

      icon.setPool(text.substring(0, separator));
      icon.setName(text.substring(separator + 1));
    });
  }
}
