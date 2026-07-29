package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the shrink factor of a flex item, applied via {@code FlexLayout.setItemShrink}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexItemShrinkContribution extends FlexItemContribution {

  /** Creates the flex item shrink contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public FlexItemShrinkContribution() {
    super("flex-shrink", "Shrink", "setItemShrink");

    setBuilderConfig(FeatureProperty.Builder::decimal);
    setGetter(c -> {
      String value = readStyle(c);
      if (value == null) {
        return null;
      }
      try {
        return Double.parseDouble(value);
      } catch (NumberFormatException e) {
        return null;
      }
    });
    setSetter((parent, item, v) -> {
      double shrink =
          (v instanceof Number n) ? n.doubleValue() : Double.parseDouble(String.valueOf(v));
      parent.setItemShrink(shrink, (Component & HasStyle) item);
    });
    setResetter((parent, item) -> clearStyle(item));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return scalarExpression(property);
  }
}
