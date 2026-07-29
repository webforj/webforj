package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the grow factor of a flex item, applied via {@code FlexLayout.setItemGrow}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexItemGrowContribution extends FlexItemContribution {

  /** Creates the flex item grow contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public FlexItemGrowContribution() {
    super("flex-grow", "Grow", "setItemGrow");

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
      double grow =
          (v instanceof Number n) ? n.doubleValue() : Double.parseDouble(String.valueOf(v));
      parent.setItemGrow(grow, (Component & HasStyle) item);
    });
    setResetter((parent, item) -> parent.setItemGrow(0, (Component & HasStyle) item));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return scalarExpression(property);
  }
}
