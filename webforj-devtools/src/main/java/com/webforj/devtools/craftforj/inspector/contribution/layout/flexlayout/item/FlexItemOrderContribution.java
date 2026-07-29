package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the order of a flex item, applied via {@code FlexLayout.setItemOrder}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexItemOrderContribution extends FlexItemContribution {

  /** Creates the flex item order contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public FlexItemOrderContribution() {
    super("order", "Order", "setItemOrder");

    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(c -> {
      String value = readStyle(c);
      if (value == null) {
        return null;
      }
      try {
        return Integer.parseInt(value);
      } catch (NumberFormatException e) {
        return null;
      }
    });
    setSetter((parent, item, v) -> {
      int order = (v instanceof Number n) ? n.intValue() : Integer.parseInt(String.valueOf(v));
      parent.setItemOrder(order, (Component & HasStyle) item);
    });
    setResetter((parent, item) -> parent.setItemOrder(0, (Component & HasStyle) item));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return scalarExpression(property);
  }
}
